import { Effect, SynchronizedRef } from "effect";

/**
 * Creates an Effect that, when executed, produces a concurrent-safe, traced,
 * and memoized version of an effectful unary function `f`.
 *
 * @param f The effectful function to memoize. `(arg: A) => Effect<...>`
 * @param functionName The name to use for the trace spans created for each call.
 * @returns An Effect that resolves to the new memoized function.
 */
export function makeMemoizeSingle<A, R, E, Req>(
  f: (arg: A) => Effect.Effect<R, E, Req>,
): Effect.Effect<(arg: A) => Effect.Effect<R, E, Req>, never, never> {
  return Effect.gen(function* () {
    // Use SynchronizedRef for concurrent-safe, atomic updates.
    const cache = yield* SynchronizedRef.make(new Map<A, R>());

    // Effect.fn creates our final traced function.
    return Effect.fn("makeMemoizeSingle")(function* (arg: A) {
      // Add the argument to the span for context.
      yield* Effect.annotateCurrentSpan("function.argument", arg);

      // `modifyEffect` is an atomic "get-or-set". It ensures that if two fibers
      // call this for the same new `arg` at the same time, `f(arg)` is only run ONCE.
      const result = yield* SynchronizedRef.modifyEffect(cache, (map) => {
        if (map.has(arg)) {
          // --- Cache Hit ---
          const effect = Effect.succeed(map.get(arg)! as R).pipe(
            Effect.tap(() => Effect.annotateCurrentSpan("cache.hit", true))
          );
          // Return the result and the UNCHANGED map.
          return effect.pipe(Effect.map((res) => [res, map] as const));
        }

        // --- Cache Miss ---
        // Run the original effectful function.
        return f(arg).pipe(
          Effect.tap(() => Effect.annotateCurrentSpan("cache.hit", false)),
          Effect.map((result) => {
            // Update the map with the new result.
            const newMap = new Map(map).set(arg, result);
            // Return the result and the NEW map.
            return [result, newMap] as const;
          })
        );
      });

      return result;
    });
  });
}

// This is the Applier function from our previous discussions.
// It's the function that exists at each "tier" of the memoization.
interface Applier<R, E, Req> {
  // Call with an argument to get the next tier's Applier.
  (arg: unknown): Effect.Effect<Applier<R, E, Req>, E, Req>;
  // Call with no arguments to get the final result.
  (): Effect.Effect<R, E, Req>;
}

/**
 * Creates an Effect that produces a traced, tiered memoized function of unknown arity.
 *
 * @param f The multi-argument effectful function to memoize.
 * @param functionName The base name for the trace spans.
 */
export function makeMemoizeTiered<Args extends readonly unknown[], R, E, Req>(
  f: (...args: Args) => Effect.Effect<R, E, Req>,
): Effect.Effect<(...args: Args) => Effect.Effect<R, E, Req>, never, never> {
  return Effect.gen(function* () {
    // Build a chain of appliers; memoize next-tier and final result per path.
    const createApplier = (
      collectedArgs: unknown[]
    ): Effect.Effect<Applier<R, E, Req>, never, never> =>
      Effect.gen(function* () {
        const memoizedNext = yield* makeMemoizeSingle((arg: unknown) =>
          createApplier([...collectedArgs, arg])
        );

        // Result cache for this specific prefix of collectedArgs
        const resultCacheRef = yield* SynchronizedRef.make<
          | { readonly hasValue: false }
          | { readonly hasValue: true; readonly value: R }
        >({ hasValue: false });

        const applier: Applier<R, E, Req> = ((...maybeArg: [unknown] | []) => {
          if (maybeArg.length === 0)
            return Effect.gen(function* () {
              const out = yield* SynchronizedRef.modifyEffect(
                resultCacheRef,
                (state) => {
                  if (state.hasValue) {
                    const out: { value: R; hit: boolean } = { value: state.value as R, hit: true };
                    return Effect.succeed([out, state] as const);
                  }
                  return f(...(collectedArgs as unknown as Args)).pipe(
                    Effect.map((value) => {
                      const out: { value: R; hit: boolean } = { value, hit: false };
                      const newState = { hasValue: true as const, value } as const;
                      return [out, newState] as const;
                    })
                  );
                }
              );

              // Annotate whether the ENTIRE tiered call was a cache hit
              // (true only if the final result was cached, which implies all tiers hit)
              yield* Effect.annotateCurrentSpan("cache.hit", out.hit);
              return out.value;
            });
          return memoizedNext(maybeArg[0]);
        }) as Applier<R, E, Req>;
        return applier;
      });

    const rootApplier = yield* createApplier([]);

    const memoized = Effect.fn("makeMemoizeTiered")(function* (
      ...args: Args
    ) {
      // Capture simple argument metadata for the span
      yield* Effect.annotateCurrentSpan("function.args", args);

      let applier = rootApplier;
      for (const arg of args) {
        applier = yield* applier(arg);
      }
      return yield* applier();
    });

    return memoized;
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

      yield* Effect.annotateCurrentSpan("function.args", args);

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
