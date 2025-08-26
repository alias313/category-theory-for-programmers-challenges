// simple memoize that takes in a pure function with arguments of primitive types
type Primitive = string | number | boolean | bigint | symbol | null | undefined

export function memoize<Args extends readonly Primitive[], R>(
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

// memoize with trie cache and a reduce to traverse it 

// Unique sentinel for storing the computed result at the leaf node
const RESULT = Symbol("memoize-result");

// Internal cache node type (a plain Map used to build the trie)
type Node = Map<unknown, unknown>;

export interface Memoized<Args extends readonly unknown[], R> {
  (...args: Args): R;
  clear(): void;
}

export function memoizeTrieReduce<Args extends readonly unknown[], R>(
  f: (...args: Args) => R
): Memoized<Args, R> {
  let root: Node = new Map();

  // Local helper: get the child node for a key, creating it if absent
  const getOrInit = (node: Node, key: unknown): Node => {
    const existing = node.get(key) as Node | undefined;
    if (existing !== undefined) return existing;
    const created: Node = new Map();
    node.set(key, created);
    return created;
  };

  const memo = ((...args: Args): R => {
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
  }) as Memoized<Args, R>;

  memo.clear = () => {
    root = new Map();
  };

  return memo;
}
