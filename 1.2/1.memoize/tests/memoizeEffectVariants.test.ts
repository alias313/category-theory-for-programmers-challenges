import { describe, it, expect } from "vitest";
import { Effect } from "effect";

import { makeMemoizeSingle, makeMemoizeTrie } from "../memoizeEffect.js";

const isPrime = (n: number): boolean => {
  if (n < 2) return false;
  const limit = Math.floor(Math.sqrt(n));
  for (let i = 2; i <= limit; i++) {
    if (n % i === 0) return false;
  }
  return true;
};

const isPrimeEffect = (...ns: number[]) =>
  Effect.sync(() => ns.every(isPrime));

const PRIMES = [
  1_981_201_020_802_099,
  144_403_552_893_599,
  29_100_036_564_239,
  1_012_020_412_021,
  166_666_666_667,
  10_999_999_999,
] as const;

describe("makeMemoizeTiered timing behavior (multi-arg)", () => {
  it("all primes: second call hits cache and is much faster", () => {
    const memoAllPrime = Effect.runSync(
      makeMemoizeTrie((...xs: readonly number[]) => isPrimeEffect(...xs))
    );

    const t1Start = performance.now();
    const r1 = Effect.runSync(memoAllPrime(...PRIMES));
    const t1End = performance.now();
    const firstMs = t1End - t1Start;

    expect(r1).toBe(true);

    const t2Start = performance.now();
    const r2 = Effect.runSync(memoAllPrime(...PRIMES));
    const t2End = performance.now();
    const secondMs = t2End - t2Start;

    const speedup = secondMs > 0 ? firstMs / secondMs : Infinity;
    const speedupStr = Number.isFinite(speedup) ? `${speedup.toFixed(1)}x` : "∞x";
    console.log(`[effect:tiered:all-primes] firstMs=${firstMs.toFixed(3)}ms secondMs=${secondMs.toFixed(3)}ms speedup=${speedupStr}`);

    expect(r2).toBe(true);
    expect(secondMs).toBeLessThan(firstMs / 50);
  });

  it("same primes, different order => cache miss and slower than cached call", () => {
    const memoAllPrime = Effect.runSync(
      makeMemoizeTrie((...xs: readonly number[]) => isPrimeEffect(...xs))
    );
    const reversed = [...PRIMES].reverse();

    // Warm and measure cached time for the original order
    Effect.runSync(memoAllPrime(...PRIMES));
    const tCachedStart = performance.now();
    const cachedR = Effect.runSync(memoAllPrime(...PRIMES));
    const tCachedEnd = performance.now();
    const cachedMs = tCachedEnd - tCachedStart;
    expect(cachedR).toBe(true);

    // Now call with a different order to force cache miss
    const tMissStart = performance.now();
    const missR = Effect.runSync(memoAllPrime(...reversed));
    const tMissEnd = performance.now();
    const missMs = tMissEnd - tMissStart;

    const ratio = missMs > 0 ? missMs / cachedMs : Infinity;
    const ratioStr = Number.isFinite(ratio) ? `${ratio.toFixed(1)}x` : "∞x";
    console.log(`[effect:tiered:different-order] cachedMs=${cachedMs.toFixed(3)}ms missMs=${missMs.toFixed(3)}ms ratio=${ratioStr}`);

    expect(missR).toBe(true);
    expect(missMs).toBeGreaterThan(cachedMs * 50);
  });
});

describe("makeMemoizeSingle timing behavior (unary)", () => {
  it("16-digit prime: second call hits cache and is much faster", () => {
    const memoIsPrime = Effect.runSync(
      makeMemoizeSingle((n: number) => Effect.sync(() => isPrime(n)))
    );
    const n = 1_981_201_020_802_099;

    const t1Start = performance.now();
    const r1 = Effect.runSync(memoIsPrime(n));
    const t1End = performance.now();
    const firstMs = t1End - t1Start;

    expect(r1).toBe(true);

    const t2Start = performance.now();
    const r2 = Effect.runSync(memoIsPrime(n));
    const t2End = performance.now();
    const secondMs = t2End - t2Start;

    const speedup = secondMs > 0 ? firstMs / secondMs : Infinity;
    const speedupStr = Number.isFinite(speedup) ? `${speedup.toFixed(1)}x` : "∞x";
    console.log(`[effect:single:isPrime] firstMs=${firstMs.toFixed(3)}ms secondMs=${secondMs.toFixed(3)}ms speedup=${speedupStr}`);

    expect(r2).toBe(true);
    expect(secondMs).toBeLessThan(firstMs / 50);
  });
});

