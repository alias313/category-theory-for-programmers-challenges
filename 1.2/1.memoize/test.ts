const isPrime = (n: number): boolean => {
  if (n < 2) return false;
  const limit = Math.floor(Math.sqrt(n));
  for (let i = 2; i <= limit; i++) {
    if (n % i === 0) return false;
  }
  return true;
};

// Nested-Map memoize (functional, reduce-based)
// Unique sentinel for storing the computed result at the leaf node
const RESULT = Symbol("memoize-result");

// Internal cache node type (a plain Map used to build the trie)
type Node = Map<unknown, unknown>;

export interface Memoized<Args extends readonly unknown[], R> {
  (...args: Args): R;
  clear(): void;
}

export function memoize<Args extends readonly unknown[], R>(
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
const memoIsPrime = memoize(isPrime);

/*
// Try it out
console.time("first 8-digit prime");
console.log(memoIsPrime(15485863));
console.timeEnd("first 8-digit prime");

console.time("second 8-digit prime");
console.log(memoIsPrime(15485863));
console.timeEnd("second 8-digit prime");

// 15-digit prime
console.time("first 16-digit prime");
console.log(memoIsPrime(1_981_201_020_802_099));
console.timeEnd("first 16-digit prime");

console.time("second 16-digit prime");
console.log(memoIsPrime(1981201020802099));
console.timeEnd("second 16-digit prime");
*/

function timeSync<T>(label: string, fn: () => T): { result: T; ms: number } {
  const t0 = performance.now();
  const result = fn();
  const ms = performance.now() - t0;
  console.log(`${label}: ${ms.toFixed(3)} ms`);
  return { result, ms };
}

// Measure and report speedup
const t1 = timeSync("first 8-digit prime", () => memoIsPrime(15485863));
console.log(t1.result);

const t2 = timeSync("second 8-digit prime", () => memoIsPrime(15485863));
console.log(t2.result);

const speedup1 = t1.ms / t2.ms;
console.log(`second vs first (8-digit): ${speedup1.toFixed(2)}x faster`);

console.log("")

const big = 1_981_201_020_802_099;

const tb1 = timeSync("first 16-digit prime", () => memoIsPrime(big));
console.log(tb1.result);

const tb2 = timeSync("second 16-digit prime", () => memoIsPrime(big));
console.log(tb2.result);

const speedup2 = tb1.ms / tb2.ms;
console.log(`second vs first (16-digit): ${speedup2.toFixed(2)}x faster`);

console.log("\n");

// 2) computeOnce (memoized) — note: memoize by its arguments.
// To truly force "once", use a nullary function and call memoOnce() with no args.

let counter = 0;

const computeOnce = (): number => {
  // Expensive computation
  for (let i = 0; i < 5e7; i++) {}
  return ++counter;
};

const memoOnce = memoize(computeOnce);

// Measure memoized speedup for zero-arg function
const c1 = timeSync("computeOnce first (memo)", () => memoOnce());
console.log("result:", c1.result);

const c2 = timeSync("computeOnce second (memo)", () => memoOnce());
console.log("result:", c2.result);

console.log(
  `second vs first (computeOnce): ${(c1.ms / c2.ms).toFixed(2)}x faster`
);
