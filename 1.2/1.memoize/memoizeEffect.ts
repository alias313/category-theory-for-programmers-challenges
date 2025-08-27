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


// A type alias for clarity, representing a node in our cache trie.
type Node = Map<unknown, unknown>;

/**
 * Creates an Effect that, when executed, produces a memoized version of an
 * effectful function `f`. The cache uses a trie (nested map) structure,
 * making it suitable for functions with multiple arguments.
 *
 * This implementation is concurrent-safe.
 *
 * @param f The effectful function to memoize. `(...args) => Effect<...>`
 * @returns An Effect that resolves to the new memoized function.
 */
export function makeMemoizeTrie<Args extends readonly any[], R, E, Req>(
  f: (...args: Args) => Effect.Effect<R, E, Req>
): Effect.Effect<(...args: Args) => Effect.Effect<R, E, Req>, never, never> {
  return Effect.gen(function* () {
    // The root of the trie, managed by a concurrent-safe Ref.
    const cacheRef = yield* SynchronizedRef.make<Node>(new Map());

    // This is the memoized function that will be returned.
    return (...args: Args): Effect.Effect<R, E, Req> => {
      // A special case for zero arguments. We use a unique symbol as the key.
      if (args.length === 0) {
        const ZEROTH_ARG = Symbol.for("effect/memoizeTrie/zerothArgument");
        return SynchronizedRef.modifyEffect(cacheRef, (root) => {
          if (root.has(ZEROTH_ARG)) {
            const result = root.get(ZEROTH_ARG) as R;
            return Effect.succeed([result, root] as const);
          }
          return f(...args).pipe(
            Effect.map((result) => {
              root.set(ZEROTH_ARG, result);
              return [result, root] as const;
            })
          );
        });
      }

      // The main logic for one or more arguments.
      return SynchronizedRef.modifyEffect(cacheRef, (root) => {
        // 1. Synchronously traverse the trie as far as possible without mutating.
        let node: Node = root;
        let pathExists = true;
        for (let i = 0; i < args.length - 1; i++) {
          const next = node.get(args[i]) as Node | undefined;
          if (!next) {
            pathExists = false;
            break;
          }
          node = next;
        }

        const lastArg = args[args.length - 1];

        // 2. Handle a cache hit.
        if (pathExists && node.has(lastArg)) {
          const cachedResult = node.get(lastArg) as R;
          // Return the cached value and unchanged root as an Effect of tuple
          return Effect.succeed([cachedResult, root] as const);
        }

        // 3. Handle a cache miss.
        // The effect to run is the original function `f`.
        const computeEffect = f(...args);
        return computeEffect.pipe(
          Effect.map((result) => {
            // Re-traverse the trie, this time creating nodes where they don't exist.
            let nodeToUpdate: Node = root;
            for (let i = 0; i < args.length - 1; i++) {
              const arg = args[i];
              let next = nodeToUpdate.get(arg) as Node | undefined;
              if (!next) {
                next = new Map();
                nodeToUpdate.set(arg, next);
              }
              nodeToUpdate = next;
            }
            // Set the computed result at the final leaf node.
            nodeToUpdate.set(lastArg, result);
            return [result, root] as const; // Return result and updated root
          })
        );
      });
    };
  });
}
