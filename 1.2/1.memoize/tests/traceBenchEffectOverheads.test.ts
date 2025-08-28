import { describe, it, expect } from "vitest";
import { Effect } from "effect";
import { makeMemoizeTiered, makeMemoizeTrie } from "../memoizeEffect.js";
import { NodeSdk } from "@effect/opentelemetry";
import { BatchSpanProcessor, ConsoleSpanExporter, SimpleSpanProcessor } from "@opentelemetry/sdk-trace-base";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";

type EffectMemoizeLike<Args extends readonly unknown[], R, E = never, Req = never> = (
  f: (...args: Args) => Effect.Effect<R, E, Req>
) => Effect.Effect<(...args: Args) => Effect.Effect<R, E, Req>, never, never>;

// Cheap payload so memoization overhead dominates
const payloadEffect = (...xs: number[]) => Effect.sync(() => xs.length);

function makeArgs(base: number, k: number): number[] {
  const out: number[] = [];
  for (let i = 0; i < k; i++) out.push(base + i);
  return out;
}

const makeTracingLayer = () => {
  const isOtlp = process.env.TRACING_MODE === "otlp";
  const exporter = isOtlp ? new OTLPTraceExporter() : new ConsoleSpanExporter();
  const spanProcessor = isOtlp
    ? new BatchSpanProcessor(exporter)
    : new SimpleSpanProcessor(exporter);
  return NodeSdk.layer(() => ({
    resource: { serviceName: "memoize-effect-bench" },
    spanProcessor,
  }));
};

const run = <A, E>(effect: Effect.Effect<A, E, never>) =>
  process.env.RUN_TRACE
    ? Effect.runPromise(effect.pipe(Effect.provide(makeTracingLayer())))
    : Effect.runPromise(effect);

function repeatEffect(
  iters: number,
  factory: () => Effect.Effect<unknown, never, never>
): Effect.Effect<void, never, never> {
  return Effect.gen(function* () {
    for (let i = 0; i < iters; i++) {
      yield* factory();
    }
  });
}

describe("Effect memoization overheads: hits vs misses and arity scaling", () => {
  // Keep counts smaller than sync version due to async runtime
  const ARITIES = [1, 3, 6] as const;
  const HIT_ITERS = 20000;
  const MISS_ITERS = 5000;

  const variants: Array<[
    string,
    EffectMemoizeLike<readonly number[], number>
  ]> = [
    ["makeMemoizeTrie", makeMemoizeTrie],
    ["makeMemoizeTiered", makeMemoizeTiered],
  ];

  for (const k of ARITIES) {
    describe(`arity k=${k}`, () => {
      for (const [name, makeMemoizeImpl] of variants) {
        it(`${name}: cache hit (lookup)`, async () => {
          const memo = await run(
            makeMemoizeImpl((...xs: readonly number[]) => payloadEffect(...(xs as number[])))
          );
          const args = makeArgs(10, k) as readonly number[];
          // Warm cache
          await run(memo(...args));

          const t0 = performance.now();
          await run(
            repeatEffect(HIT_ITERS, () => memo(...args).pipe(Effect.as(undefined)))
          );
          const t1 = performance.now();
          const totalMs = t1 - t0;
          const usPerOp = (totalMs * 1000) / HIT_ITERS;
          console.log(`${name} hit k=${k}: total=${totalMs.toFixed(2)}ms (~${usPerOp.toFixed(2)} µs/op)`);
          expect(Number.isFinite(totalMs)).toBe(true);
        });

        it(`${name}: cache build (miss)`, async () => {
          const memo = await run(
            makeMemoizeImpl((...xs: readonly number[]) => payloadEffect(...(xs as number[])))
          );
          const tail = makeArgs(200, k - 1);

          const t0 = performance.now();
          await run(
            Effect.gen(function* () {
              for (let i = 0; i < MISS_ITERS; i++) {
                const args = [1_000_000 + i, ...tail] as const;
                yield* memo(...args);
              }
            })
          );
          const t1 = performance.now();
          const totalMs = t1 - t0;
          const usPerOp = (totalMs * 1000) / MISS_ITERS;
          console.log(`${name} build k=${k}: total=${totalMs.toFixed(2)}ms (~${usPerOp.toFixed(2)} µs/op)`);
          expect(Number.isFinite(totalMs)).toBe(true);
        });
      }
    });
  }
});

