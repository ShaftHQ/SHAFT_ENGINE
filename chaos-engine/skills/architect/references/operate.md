# Run it in production

## 11 Failure modes

- Every remote call: timeout, bounded retries with exponential backoff and
  jitter, only for idempotent ops.
- Repeated failure: circuit breaker; isolate pools (bulkhead); degrade
  gracefully (cached/partial answer) instead of failing whole request.
- Ask per dependency: slow? down? wrong data? duplicate delivery? Write the
  behaviour for each; test the top one.
- Shared mutable state across threads/tasks: avoid; else one owner + lock or
  queue. Tests must not race (no shared instance fields in parallel methods).

## 12 Observability

- Structured logs (key=value/JSON) with correlation id; no secrets/PII.
- Metrics: RED for services (rate, errors, duration), USE for resources.
- Trace spans across process hops (OpenTelemetry where present).
- New user-facing path: define SLO + alert on error budget burn, not on
  every error.

## 13 Security

- Draw trust boundaries; every input crossing one is validated and typed.
- STRIDE-lite per boundary: spoofing, tampering, repudiation, disclosure,
  denial of service, elevation. Note mitigations in the PR.
- Least privilege for tokens, workflows, DB users. Secrets from env/vault,
  never in code, logs, or tests.
- Parameterized queries, output encoding, safe deserialization; pin and scan
  dependencies (OWASP ASVS level 1 as floor).

## 14 Performance and scale

- Measure before optimizing: profile or benchmark the real path.
- Set a budget (latency, memory, cost) from card 2; fix the top bottleneck
  only; re-measure.
- Usual suspects: N+1 queries, unbounded lists, sync I/O in hot loop,
  missing index, chatty network. Cache only with an invalidation rule.
- Scale stateless first; push state to a store; add backpressure to queues.

## 15 Data and state

- Schema change: expand (add nullable), migrate, switch reads, contract
  (drop) in separate deploys. Never break the running version.
- One writer per fact; others read or subscribe. Pick consistency per use
  (strong vs eventual) and write it down.
- Migrations reversible or explicitly one-way (card 4).
