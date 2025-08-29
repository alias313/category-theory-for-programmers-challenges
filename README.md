# Category Theory for Programmers challenges
My solutions for challenges posed in the book.

Credit to the idea of tiered memoizers to [this article](https://medium.com/@joedski/memoization-of-multi-parametered-functions-in-javascript-8508e89ba545)

Also, memoization in Haskell from [this article](https://functional.computer/blog/memotries)

## Tracing and benchmarks

You can run the tracing tests to emit spans either to the console or to an OTLP endpoint.

1. Create a `.env` file at the repo root (optional for console):

```
# Enable tracing in trace test scripts
RUN_TRACE=1

# Use OTLP tracing exporter
# TRACING_MODE=otlp

# Axiom token for the axiom script (if you use it)
# AXIOM_TOKEN=your-axiom-api-token-here
```

2. Console tracing:

```
pnpm run trace:memoize:console
```

3. OTLP tracing (respects `.env`):

```
pnpm run trace:memoize:otlp
```

4. Send to Axiom (reads `AXIOM_TOKEN` from `.env`):

```
pnpm run trace:memoize:axiom
```
