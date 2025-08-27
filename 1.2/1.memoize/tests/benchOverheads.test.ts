import { describe, it, expect } from "vitest";

import {
  memoizeSingle,
  memoizeSerialize,
  memoizeTrie,
  memoizeTrieReduce,
  memoizeTrieRecord,
  memoizeTieredKnownArity,
  memoizeTieredUnknownArity,
} from "../memoize";

type MemoizeLike<Args extends readonly unknown[], R> = (
  f: (...args: Args) => R
) => (...args: Args) => R;

// Cheap payload so memoization overhead dominates
const payload = (...xs: number[]): number => xs.length;

// Baseline: cache only by the first argument using memoizeSingle
// Still calls f with all args; only the first arg is used as the cache key.
const singleFirstArgBaseline: MemoizeLike<readonly number[], number> = (f) => {
  const memo = memoizeSingle((first: number) => {
    // Return a function that applies the rest of the arguments
    return (...rest: readonly number[]) => f(first, ...rest);
  });
  return (...args: readonly number[]) => {
    const [first, ...rest] = args as number[];
    const cont = memo(first);
    return cont(...rest);
  };
};

// Factories that produce a MemoizeLike based on arity k when needed
const variants: Array<
  [
    string,
    (k: number) => MemoizeLike<readonly number[], number>
  ]
> = [
  ["memoizeSingle(first-arg)", () => singleFirstArgBaseline],
  ["memoizeSerialize", () => memoizeSerialize],
  ["memoizeTrie", () => memoizeTrie],
  ["memoizeTrieReduce", () => memoizeTrieReduce],
  ["memoizeTrieRecord", () => memoizeTrieRecord],
  [
    "memoizeTieredKnownArity",
    (k) => (f) => memoizeTieredKnownArity<readonly number[], number>()(f, k),
  ],
  ["memoizeTieredUnknownArity", () => memoizeTieredUnknownArity],
];

function bench(label: string, fn: () => void, iters: number) {
  const t0 = performance.now();
  for (let i = 0; i < iters; i++) fn();
  const t1 = performance.now();
  const totalMs = t1 - t0;
  const usPerOp = (totalMs * 1000) / iters;
  console.log(`${label}: total=${totalMs.toFixed(2)}ms (~${usPerOp.toFixed(2)} µs/op)`);
  expect(Number.isFinite(totalMs)).toBe(true);
}

function makeArgs(base: number, k: number): number[] {
  const out: number[] = [];
  for (let i = 0; i < k; i++) out.push(base + i);
  return out;
}

describe("Memoization overheads: hits vs misses and arity scaling", () => {
  // Choose modest iteration counts to keep the suite fast
  const ARITIES = [1, 3, 6] as const;
  const HIT_ITERS = 50_000;
  const MISS_ITERS = 20_000;

  for (const k of ARITIES) {
    describe(`arity k=${k}`, () => {
      for (const [name, factory] of variants) {
        it(`${name}: cache hit (lookup)`, () => {
          const memo = factory(k)(payload);
          const args = makeArgs(10, k);
          // Warm cache
          memo(...args);
          // Measure lookups
          bench(`${name} hit k=${k}`, () => {
            memo(...args);
          }, HIT_ITERS);
        });

        it(`${name}: cache build (miss)`, () => {
          const memo = factory(k)(payload);
          const tail = makeArgs(200, k - 1);
          // Ensure each iteration is a miss by varying the FIRST arg uniquely
          let x = 0;
          bench(`${name} build k=${k}`, () => {
            // Create a fresh first argument to avoid hits
            const args = [1_000_000 + (x++), ...tail] as const;
            (memo as any)(...args);
          }, MISS_ITERS);
        });
      }
    });
  }
});

