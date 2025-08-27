import { describe, it, expect } from "vitest";

import { memoizeSingle, memoizeSerialize, memoizeTrie, memoizeTrieReduce, memoizeTieredKnownArity, memoizeTieredUnknownArity, memoizeTrieRecord } from "../memoize";

const isPrime = (...ns: number[]): boolean => {
  const single = (n: number): boolean => {
    if (n < 2) return false;
    const limit = Math.floor(Math.sqrt(n));
    for (let i = 2; i <= limit; i++) {
      if (n % i === 0) return false;
    }
    return true;
  };
  for (const n of ns) {
    if (!single(n)) return false;
  }
  return true;
};

type MemoizeLike<Args extends readonly unknown[], R> = (
  f: (...args: Args) => R
) => (...args: Args) => R;

// Adapters to fit various memoizers to variadic number[] signature
const singleVariadicImpl: MemoizeLike<readonly number[], boolean> = (f) => {
  const memo = memoizeSingle((key: string) => f(...(JSON.parse(key) as number[])));
  return (...args: readonly number[]) => memo(JSON.stringify(args));
};

const tieredKnownArityImpl: MemoizeLike<readonly number[], boolean> = (f) =>
  memoizeTieredKnownArity<readonly number[], boolean>()(f, 6);

const implementations: Array<[string, MemoizeLike<readonly number[], boolean>]> = [
  ["memoizeSingle", singleVariadicImpl],
  ["memoizeSerialize", memoizeSerialize],
  ["memoizeTrie", memoizeTrie],
  ["memoizeTrieReduce", memoizeTrieReduce],
  ["memoizeTrieRecord", memoizeTrieRecord],
  ["memoizeTieredKnownArity", tieredKnownArityImpl],
  ["memoizeTieredUnknownArity", memoizeTieredUnknownArity],
];

const PRIMES = [
  1_981_201_020_802_099,
  144_403_552_893_599,
  29_100_036_564_239,
  1_012_020_412_021,
  166_666_666_667,
  10_999_999_999,
] as const;

describe.each(implementations)("%s timing behavior (multi-arg)", (name, memoizeImpl) => {
  it("all primes: second call hits cache and is much faster", () => {
    const memoAllPrime = memoizeImpl(isPrime);

    const t1Start = performance.now();
    const r1 = memoAllPrime(...PRIMES);
    const t1End = performance.now();
    const firstMs = t1End - t1Start;

    expect(r1).toBe(true);

    const t2Start = performance.now();
    const r2 = memoAllPrime(...PRIMES);
    const t2End = performance.now();
    const secondMs = t2End - t2Start;

    const speedup = secondMs > 0 ? firstMs / secondMs : Infinity;
    const speedupStr = Number.isFinite(speedup) ? `${speedup.toFixed(1)}x` : "∞x";
    console.log(`[${name}:all-primes] firstMs=${firstMs.toFixed(3)}ms secondMs=${secondMs.toFixed(3)}ms speedup=${speedupStr}`);

    expect(r2).toBe(true);
    expect(secondMs).toBeLessThan(firstMs / 50);
  });

  it("same primes, different order => cache miss and slower than cached call", () => {
    const memoAllPrime = memoizeImpl(isPrime);
    const reversed = [...PRIMES].reverse();

    // Warm and measure cached time for the original order
    memoAllPrime(...PRIMES);
    const tCachedStart = performance.now();
    const cachedR = memoAllPrime(...PRIMES);
    const tCachedEnd = performance.now();
    const cachedMs = tCachedEnd - tCachedStart;
    expect(cachedR).toBe(true);

    // Now call with a different order to force cache miss
    const tMissStart = performance.now();
    const missR = memoAllPrime(...reversed);
    const tMissEnd = performance.now();
    const missMs = tMissEnd - tMissStart;

    const ratio = missMs > 0 ? missMs / cachedMs : Infinity;
    const ratioStr = Number.isFinite(ratio) ? `${ratio.toFixed(1)}x` : "∞x";
    console.log(`[${name}:different-order] cachedMs=${cachedMs.toFixed(3)}ms missMs=${missMs.toFixed(3)}ms ratio=${ratioStr}`);

    expect(missR).toBe(true);
    expect(missMs).toBeGreaterThan(cachedMs * 50);
  });
});

