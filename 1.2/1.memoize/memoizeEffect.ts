import { Effect, Ref, SynchronizedRef } from "effect";

/**
 * Creates an Effect that, when executed, produces a memoized version
 * of an effectful unary function `f`.
 *
 * @param f The effectful function to memoize. `(arg: A) => Effect<Success, Error, Requirements>`
 * @returns An Effect that resolves to the new memoized function.
 */
export function makeMemoizeSingle<A, Success, Error, Requirements>(
  f: (arg: A) => Effect.Effect<Success, Error, Requirements>
): Effect.Effect<(arg: A) => Effect.Effect<Success, Error, Requirements>, never, never> {
  return Effect.gen(function* () {
    // 1. The cache is managed by a Ref
    const cache = yield* Ref.make(new Map<A, Success>());

    // 2. Return the new, memoized function
    return (arg: A): Effect.Effect<Success, Error, Requirements> => {
      return Effect.gen(function* () {
        const map = yield* Ref.get(cache);
        if (map.has(arg)) {
          // Cache hit: return the cached value directly, wrapped in an Effect
          return map.get(arg)!;
        }

        // Cache miss: run the original effectful function
        const result = yield* f(arg);

        // Update the cache with the new result
        yield* Ref.update(cache, (map) => map.set(arg, result));

        return result;
      });
    };
  });
}


// A node in the cache trie that can hold both children and a result.
// Using a container for the result allows us to distinguish
// "has a cached result of undefined" from "has no result yet".
type TrieNode<R> = {
  readonly children: Map<unknown, TrieNode<R>>;
  result: { value: R } | undefined;
};

// Helper to create a new, empty node.
const makeNode = <R>(): TrieNode<R> => ({
  children: new Map(),
  result: undefined,
});

/**
 * Creates an Effect that, when executed, produces a memoized and TRACED version
 * of an effectful function `f`. The cache uses a trie structure where any node
 * can store a result, elegantly handling functions of any arity, including zero.
 *
 * This implementation is concurrent-safe.
 *
 * @param f The effectful function to memoize. `(...args) => Effect<...>`
 * @param functionName The name to use for the trace spans.
 * @returns An Effect that resolves to the new memoized function.
 */
export function makeMemoizeTrie<Args extends readonly any[], R, E, Req>(
  f: (...args: Args) => Effect.Effect<R, E, Req>,
): Effect.Effect<(...args: Args) => Effect.Effect<R, E, Req>, never, never> {
  // This is the "constructor" Effect. It runs once to set up the cache
  // and create the memoized function.
  return Effect.gen(function* () {
    const cacheRef = yield* SynchronizedRef.make<TrieNode<R>>(makeNode());

    // Effect.fn creates the final function for us. The generator body
    // is the implementation of our memoized function.
    return Effect.fn("makememoizeTrie")(function* (...args: Args) {
      yield* Effect.annotateCurrentSpan("function.arguments", args);

      // We use `modifyEffect` for an atomic "get or compute" operation.
      return yield* SynchronizedRef.modifyEffect(cacheRef, (root) => {
        // 1. Traverse the trie to find the target node for these args.
        let targetNode = root;
        for (const arg of args) {
          const next = targetNode.children.get(arg);
          if (next) {
            targetNode = next;
          } else {
            // Path doesn't exist, so it's a guaranteed cache miss.
            return f(...args).pipe(
              Effect.tap(() => Effect.annotateCurrentSpan("cache.hit", false)),
              Effect.map((result) => {
                const newRoot = updateCache(root, args, result);
                return [result, newRoot] as const;
              })
            );
          }
        }

        // 2. We've reached the target node. Check if it has a result.
        if (targetNode.result) {
          // Cache Hit!
          const effect = Effect.succeed(targetNode.result.value).pipe(
            Effect.tap(() => Effect.annotateCurrentSpan("cache.hit", true))
          );
          // Return the cached result and the UNCHANGED root.
          return effect.pipe(Effect.map((res) => [res, root] as const));
        }

        // 3. Cache Miss at the target node.
        return f(...args).pipe(
          Effect.tap(() => Effect.annotateCurrentSpan("cache.hit", false)),
          Effect.map((result) => {
            const newRoot = updateCache(root, args, result);
            return [result, newRoot] as const;
          })
        );
      });
    });
  });
}

/**
 * A pure helper function to immutably update the trie. It creates a deep
 * copy of the path being written to, ensuring concurrent reads are not affected.
 */
function updateCache<R>(
  root: TrieNode<R>,
  args: readonly any[],
  result: R
): TrieNode<R> {
  const newRoot: TrieNode<R> = { ...root, children: new Map(root.children) };

  let currentNode = newRoot;
  for (const arg of args) {
    const existingNext = currentNode.children.get(arg);
    const newNext: TrieNode<R> = {
      ...makeNode<R>(),
      ...existingNext,
      children: new Map(existingNext?.children),
    };
    currentNode.children.set(arg, newNext);
    currentNode = newNext;
  }

  currentNode.result = { value: result };
  return newRoot;
}
