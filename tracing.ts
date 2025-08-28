import { NodeSdk } from "@effect/opentelemetry";
import { BatchSpanProcessor, ConsoleSpanExporter } from "@opentelemetry/sdk-trace-base";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";

export type TracingOptions = {
  serviceName?: string;
};

export function makeConsoleTracingLayer(options: TracingOptions = {}) {
  const serviceName = options.serviceName ?? "example";
  return NodeSdk.layer(() => ({
    resource: { serviceName },
    spanProcessor: new BatchSpanProcessor(new ConsoleSpanExporter()),
  }));
}

export function makeOtlpHttpTracingLayer(
  options: TracingOptions & { url?: string } = {},
) {
  const serviceName = options.serviceName ?? "example";
  const url = options.url;
  return NodeSdk.layer(() => ({
    resource: { serviceName },
    spanProcessor: new BatchSpanProcessor(
      new OTLPTraceExporter(url ? { url } : undefined),
    ),
  }));
}

