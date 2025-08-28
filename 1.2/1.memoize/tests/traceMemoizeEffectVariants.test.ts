import { describe, it, expect } from "vitest";
import { Effect } from "effect";
import { NodeSdk } from "@effect/opentelemetry";
import { BatchSpanProcessor, ConsoleSpanExporter, SimpleSpanProcessor } from "@opentelemetry/sdk-trace-base";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";

import { makeMemoizeTiered, makeMemoizeTrie } from "../memoizeEffect.js";

const isPrime = (n: number): boolean => {
  if (n < 2) return false;
  const limit = Math.floor(Math.sqrt(n));
  for (let i = 2; i <= limit; i++) {
    if (n % i === 0) return false;
  }
  return true;
};

const isPrimeEffect = (...ns: number[]) => Effect.sync(() => ns.every(isPrime));

const PRIMES = [
  1_981_201_020_802_099,
  144_403_552_893_599,
  29_100_036_564_239,
  1_012_020_412_021,
  166_666_666_667,
  10_999_999_999,
] as const;

type EffectMemoizeLike<Args extends readonly unknown[], R, E = never, Req = never> = (
  f: (...args: Args) => Effect.Effect<R, E, Req>
) => Effect.Effect<(...args: Args) => Effect.Effect<R, E, Req>, never, never>;

// Adapter for makeMemoizeTrie to normalize its type parameters
const trieAdapter: EffectMemoizeLike<readonly number[], boolean> = (f) =>
  makeMemoizeTrie<readonly number[], boolean, never, never>(f);

const implementations: Array<[
  string,
  EffectMemoizeLike<readonly number[], boolean>
]> = [
  ["cachedFunction", Effect.fn("cachedFunction")(Effect.cachedFunction)],
  ["makeMemoizeTrie", trieAdapter],
  ["makeMemoizeTiered", makeMemoizeTiered],
];

const makeTracingLayer = () => {
  const isOtlp = process.env.TRACING_MODE === "otlp";
  const exporter = isOtlp ? new OTLPTraceExporter() : new ConsoleSpanExporter();
  const spanProcessor = isOtlp
    ? new BatchSpanProcessor(exporter)
    : new SimpleSpanProcessor(exporter);
  return NodeSdk.layer(() => ({
    resource: { serviceName: "memoize-tests" },
    spanProcessor,
  }));
};

const run = <A, E>(effect: Effect.Effect<A, E, never>) =>
  process.env.RUN_TRACE
    ? Effect.runPromise(Effect.scoped(effect.pipe(Effect.provide(makeTracingLayer()))))
    : Effect.runPromise(effect);

describe.each(implementations)("%s timing behavior (multi-arg)", (name, makeMemoizeImpl) => {
  it("all primes: second call hits cache and is much faster", async () => {
    const memoAllPrime = await run(
      makeMemoizeImpl((...xs: readonly number[]) => isPrimeEffect(...xs))
    );

    const t1Start = performance.now();
    const r1 = await run(memoAllPrime(...PRIMES));
    const t1End = performance.now();
    const firstMs = t1End - t1Start;

    expect(r1).toBe(true);

    const t2Start = performance.now();
    const r2 = await run(memoAllPrime(...PRIMES));
    const t2End = performance.now();
    const secondMs = t2End - t2Start;

    const speedup = secondMs > 0 ? firstMs / secondMs : Infinity;
    const speedupStr = Number.isFinite(speedup) ? `${speedup.toFixed(1)}x` : "∞x";
    console.log(`[effect:${name}:all-primes] firstMs=${firstMs.toFixed(3)}ms secondMs=${secondMs.toFixed(3)}ms speedup=${speedupStr}`);

    expect(r2).toBe(true);
    // Tracing adds overhead; require a modest speedup
    expect(secondMs).toBeLessThan(firstMs / 2);
  });

  it("same primes, different order => cache miss and slower than cached call", async () => {
    const memoAllPrime = await run(
      makeMemoizeImpl((...xs: readonly number[]) => isPrimeEffect(...xs))
    );
    const reversed = [...PRIMES].reverse();

    // Warm and measure cached time for the original order
    await run(memoAllPrime(...PRIMES));
    const tCachedStart = performance.now();
    const cachedR = await run(memoAllPrime(...PRIMES));
    const tCachedEnd = performance.now();
    const cachedMs = tCachedEnd - tCachedStart;
    expect(cachedR).toBe(true);

    // Now call with a different order to force cache miss
    const tMissStart = performance.now();
    const missR = await run(memoAllPrime(...reversed));
    const tMissEnd = performance.now();
    const missMs = tMissEnd - tMissStart;

    const ratio = missMs > 0 ? missMs / cachedMs : Infinity;
    const ratioStr = Number.isFinite(ratio) ? `${ratio.toFixed(1)}x` : "∞x";
    console.log(`[effect:${name}:different-order] cachedMs=${cachedMs.toFixed(3)}ms missMs=${missMs.toFixed(3)}ms ratio=${ratioStr}`);

    expect(missR).toBe(true);
    // Tracing adds overhead; require a modest slowdown for misses vs cached
    expect(missMs).toBeGreaterThan(cachedMs * 2);
  });
});

