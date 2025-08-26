// simple memoize that takes in a pure function with arguments of primitive types
export function memoize<Args extends readonly unknown[], R>(
  f: (...args: Args) => R
): (...args: Args) => R {
  const cache = new Map<string, R>();
  return (...args: Args): R => {
    const key = JSON.stringify(args);
    if (cache.has(key)) return cache.get(key)!;
    const r = f(...args);
    cache.set(key, r);
    return r;
  };
}

// memoize with nested map (trie) cache
// Internal cache node type (a plain Map used to build the trie)
type Node = Map<unknown, unknown>;

export function memoizeTrie<Args extends readonly unknown[], R>(
  f: (...args: Args) => R
): (...args: Args) => R {
  const root: Node = new Map();

  return (...args: Args): R => {
    let node: Node = root;

    for (let i = 0; i < args.length - 1; i++) {
      const a = args[i];
      let next = node.get(a) as Node | undefined;
      if (!next) {
        next = new Map();
        node.set(a, next);
      }
      node = next;
    }

    const last = args[args.length - 1];
    if (node.has(last)) {
      return node.get(last) as R;
    }
    const r = f(...args);
    node.set(last, r);
    return r;
  };
}

// memoize with trie cache and a reduce to traverse it 

// Unique sentinel for storing the computed result at the leaf node
const RESULT = Symbol("memoize-result");

export function memoizeTrieReduce<Args extends readonly unknown[], R>(
  f: (...args: Args) => R
): (...args: Args) => R {
  let root: Node = new Map();

  // Local helper: get the child node for a key, creating it if absent
  const getOrInit = (node: Node, key: unknown): Node => {
    const existing = node.get(key) as Node | undefined;
    if (existing !== undefined) return existing;
    const created: Node = new Map();
    node.set(key, created);
    return created;
  };

  return ((...args: Args): R => {
    // Walk the trie along the argument tuple to reach the leaf node
    const leaf: Node = args.reduce<Node>(getOrInit, root);

    // Cached result?
    if (leaf.has(RESULT)) {
      return leaf.get(RESULT) as R;
    }

    // Compute, store at the leaf, and return
    const r = f(...args);
    leaf.set(RESULT, r);
    return r;
  });
}


// Known-arity tiered memoizer using memoize at every tier
export function memoizeTieredKnownArity<A extends readonly unknown[], R>() {
  function createApplier(
    calculator: (...args: A) => R,
    argCount: number,
    collected: unknown[]
  ): (next: unknown) => any {
    const idx = collected.length;
    const isFinal = idx >= argCount - 1;

    if (isFinal) {
      const applyFinal = (value: unknown) => {
        // Make a readonly tuple
        const next = [...collected, value] as unknown as A;
        return calculator(...next);
      };
      return memoize(applyFinal as any);
    } else {
      const applyNonFinal = (value: unknown) =>
        createApplier(calculator, argCount, [...collected, value]);
      return memoize(applyNonFinal as any);
    }
  }

  return function tieredMemoize(
    calculator: (...args: A) => R,
    expectedArgCount: number
  ): (...args: A) => R {
    const firstApplier = createApplier(calculator, expectedArgCount, []);
    return (...args: A): R => {
      const result = args.reduce<any>(
        (applier, arg) => applier(arg),
        firstApplier
      );
      return result as R;
    };
  };
}

// Tiered memoize where arity is not known until call time.
// Uses the top-level memoize at each tier; same signature as memoizeTrie.
export function memoizeTieredUnknownArity<
  Args extends readonly unknown[],
  R
>(f: (...args: Args) => R): (...args: Args) => R {
  // Build a memoized chain of unary appliers.
  function createApplier(collected: unknown[]) {
    // Memoize "apply next arg" at this tier
    const applyNext = memoize((arg: unknown) =>
      createApplier([...collected, arg])
    );

    // Lazily compute and cache the result for this exact collected prefix
    let computed = false;
    let value!: R;
    const compute = () => {
      if (!computed) {
        computed = true;
        value = f(...([...collected] as unknown as Args));
      }
      return value;
    };

    // If called with an arg, return next applier; if called with no args, finalize
    return (...maybeArg: [unknown] | []): any => {
      if (maybeArg.length === 0) return compute();
      return applyNext(maybeArg[0]);
    };
  }

  const initApplier = createApplier([]);

  return (...args: Args): R => {
    // Feed args through the chain, then finalize with a 0-arg call
    const last = args.reduce<any>((applier, arg) => applier(arg), initApplier);
    return last() as R;
  };
}
