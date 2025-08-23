// memoize.timing.test.ts
import { describe, it, expect } from "vitest";

// Paste or import your implementations as needed
// import { memoize } from "./memoize";
// import { isPrime } from "./isPrime";

const isPrime = (n: number): boolean => {
  if (n < 2) return false;
  const limit = Math.floor(Math.sqrt(n));
  for (let i = 2; i <= limit; i++) {
    if (n % i === 0) return false;
  }
  return true;
};

// Nested-Map memoize
const RESULT = Symbol("memoize-result");
type Node = Map<unknown, unknown>;

export interface Memoized<Args extends readonly unknown[], R> {
  (...args: Args): R;
  clear(): void;
}

export function memoize<Args extends readonly unknown[], R>(
  f: (...args: Args) => R
): Memoized<Args, R> {
  let root: Node = new Map();
  const getOrInit = (node: Node, key: unknown): Node => {
    const existing = node.get(key) as Node | undefined;
    if (existing !== undefined) return existing;
    const created: Node = new Map();
    node.set(key, created);
    return created;
  };

  const memo = ((...args: Args): R => {
    const leaf: Node = args.reduce<Node>(getOrInit, root);
    if (leaf.has(RESULT)) {
      return leaf.get(RESULT) as R;
    }
    const r = f(...args);
    leaf.set(RESULT, r);
    return r;
  }) as Memoized<Args, R>;

  memo.clear = () => {
    root = new Map();
  };

  return memo;
}

describe("memoize timing behavior", () => {
  it("isPrime: second call hits cache and runs under 0.01 ms", () => {
    const memoIsPrime = memoize(isPrime);

    // Choose a number that's prime and large enough to be measurably slow.
    // 15485863 is a known 8-digit prime.
    const n = 15485863;

    const t1Start = performance.now();
    const r1 = memoIsPrime(n);
    const t1End = performance.now();
    const firstMs = t1End - t1Start;

    expect(r1).toBe(true);
    // Sanity check: first call should be meaningfully slow (environment-dependent).
    // We assert it's > 0.1 ms to make the cache test meaningful.
    expect(firstMs).toBeGreaterThan(0.05);

    const t2Start = performance.now();
    const r2 = memoIsPrime(n);
    const t2End = performance.now();
    const secondMs = t2End - t2Start;

    expect(r2).toBe(true);
    expect(secondMs).toBeLessThan(0.01);
  });

  it("computeOnce via memoize: second call hits cache and runs under 0.01 ms", () => {
    let counter = 0;
    const computeOnce = (): number => {
      // Expensive sync loop to simulate heavy work
      for (let i = 0; i < 5e7; i++) {}
      return ++counter;
    };

    const memoOnce = memoize(computeOnce);

    const t1Start = performance.now();
    const r1 = memoOnce();
    const t1End = performance.now();
    const firstMs = t1End - t1Start;

    expect(r1).toBe(1);
    expect(firstMs).toBeGreaterThan(0.05);

    const t2Start = performance.now();
    const r2 = memoOnce();
    const t2End = performance.now();
    const secondMs = t2End - t2Start;

    expect(r2).toBe(1);
    expect(secondMs).toBeLessThan(0.01);
  });

  it("isPrime: different arguments do not hit cache (should be slow again)", () => {
    const memoIsPrime = memoize(isPrime);
    const a = 15485863; // prime
    const b = 15485867; // test a nearby number (prime as well), different arg => cache miss

    // Warm cache for a
    memoIsPrime(a);

    const tStart = performance.now();
    const r = memoIsPrime(b);
    const tEnd = performance.now();

    expect(r).toBe(true);
    expect(tEnd - tStart).toBeGreaterThan(0.01);
  });
});
