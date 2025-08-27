// Unary memoize: caches by argument identity (works for primitives and objects)
export function memoizeSingle<A, R>(f: (arg: A) => R): (arg: A) => R {
  const cache = new Map<A, R>();
  return (arg: A): R => {
    if (cache.has(arg)) return cache.get(arg)!;
    const r = f(arg);
    cache.set(arg, r);
    return r;
  };
}

// simple memoize that uses serialize to create a key
// if it accepted objects, some keys like "toString" would be problematic
type Primitive = string | number | boolean | bigint | symbol | null | undefined


export function memoizeSerialize<Args extends readonly Primitive[], R>(
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

// Tiered memoize using stores (Maps) only.
// Same signature as memoizeTrie; stores the leaf result in the node.
type RecordNode<R> = {
  children: Map<unknown, RecordNode<R>>;
  hasResult: boolean;
  result?: R;
};

export function memoizeTrieRecord<Args extends readonly unknown[], R>(
  f: (...args: Args) => R
): (...args: Args) => R {
  const root: RecordNode<R> = { children: new Map(), hasResult: false };

  return (...args: Args): R => {
    // Walk or build down the trie
    let node = root;
    for (const key of args) {
      let next = node.children.get(key);
      if (!next) {
        next = { children: new Map(), hasResult: false };
        node.children.set(key, next);
      }
      node = next;
    }

    // At the leaf, return cached result or compute and store
    if (node.hasResult) {
      return node.result as R;
    }
    const r = f(...args);
    node.result = r;
    node.hasResult = true;
    return r;
  };
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
      return memoizeSingle(applyFinal as any);
    } else {
      const applyNonFinal = (value: unknown) =>
        createApplier(calculator, argCount, [...collected, value]);
      return memoizeSingle(applyNonFinal as any);
    }
  }

  return function tieredmemoizeSingle(
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
    const applyNext = memoizeSingle((arg: unknown) =>
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
