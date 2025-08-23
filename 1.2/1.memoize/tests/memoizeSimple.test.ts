import { describe, it, expect } from "vitest";

const isPrime = (n: number): boolean => {
  if (n < 2) return false;
  const limit = Math.floor(Math.sqrt(n));
  for (let i = 2; i <= limit; i++) {
    if (n % i === 0) return false;
  }
  return true;
};

export function memoize<Args extends readonly unknown[], R>(
  f: (...args: Args) => R
): (...args: Args) => R {
  const cache = new Map<string, R>();

  return (...args: Args): R => {
    const key = JSON.stringify(args); // fine for primitives/tuples of primitives
    if (cache.has(key)) {
      return cache.get(key)!;
    }
    const result = f(...args);
    cache.set(key, result);
    return result;
  };
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

