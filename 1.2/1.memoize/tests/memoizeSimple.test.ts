import { describe, it, expect } from "vitest";

import { memoize } from "../memoize";

const isPrime = (n: number): boolean => {
  if (n < 2) return false;
  const limit = Math.floor(Math.sqrt(n));
  for (let i = 2; i <= limit; i++) {
    if (n % i === 0) return false;
  }
  return true;
};

describe("memoize timing behavior", () => {
  it("isPrime 16-digit prime: second call hits cache and runs under 0.1 ms", () => {
    const memoIsPrime = memoize(isPrime);
    
    const n = 1_981_201_020_802_099;

    const t1Start = performance.now();
    const r1 = memoIsPrime(n);
    const t1End = performance.now();
    const firstMs = t1End - t1Start;

    expect(r1).toBe(true);
    // Sanity check: first call should be meaningfully slow (environment-dependent).
    // We assert it's > 10 ms to make the cache test meaningful.
    expect(firstMs).toBeGreaterThan(10);

    const t2Start = performance.now();
    const r2 = memoIsPrime(n);
    const t2End = performance.now();
    const secondMs = t2End - t2Start;
    const speedup = secondMs > 0 ? firstMs / secondMs : Infinity;
    const speedupStr = Number.isFinite(speedup) ? `${speedup.toFixed(1)}x` : "∞x";
    console.log(`[memoize:isPrime] firstMs=${firstMs.toFixed(3)}ms secondMs=${secondMs.toFixed(3)}ms speedup=${speedupStr}`);

    expect(r2).toBe(true);
    expect(secondMs).toBeLessThan(0.1);
  });

  it("isPrime: different arguments do not hit cache (should be slow again)", () => {
    const memoIsPrime = memoize(isPrime);

    const a = 15485863; // prime
    const b = 1_981_201_020_802_099; // prime (different arg)

    // Warm cache for a
    memoIsPrime(a);

    const tCachedStart = performance.now();
    const cachedR = memoIsPrime(a);
    const tCachedEnd = performance.now();
    const cachedMs = tCachedEnd - tCachedStart;

    expect(cachedR).toBe(true);

    const tMissStart = performance.now();
    const r = memoIsPrime(b);
    const tMissEnd = performance.now();
    const missMs = tMissEnd - tMissStart;

    const ratio = missMs > 0 ? missMs / cachedMs : Infinity;
    const ratioStr = Number.isFinite(ratio) ? `${ratio.toFixed(1)}x` : "∞x";
    console.log(`[memoize:differentArgs] cachedMs=${cachedMs.toFixed(3)}ms missMs=${missMs.toFixed(3)}ms ratio=${ratioStr}`);

    expect(r).toBe(true);
    expect(missMs).toBeGreaterThan(cachedMs * 1000);
  });
});

