# OpenTelemetry GenAI agent spans

Phase-3 harness evidence (issue 6518). A run emits local spans that follow the
OpenTelemetry GenAI agent span conventions
(<https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-agent-spans/>).
`invoke_agent` is named `invoke_agent {gen_ai.agent.name}`. `execute_tool` is
named `execute_tool {gen_ai.tool.name}` and requires a parent span. Every span
carries `gen_ai.operation.name` and `gen_ai.provider.name`. Emission stays
behind harness eval task `reg-genai-spans-6518`. The file is
`.chaos-engine-state/genai-spans/index.json`. No network exporter is started.

## Surfaces

| Piece | Path |
| --- | --- |
| Module | [`genai_spans.py`](../genai_spans.py) |
| CLI | `python3 chaos-engine/genai_spans.py emit\|summary` (repo-only) |
| Unittest | `python3 -m unittest tests.scripts.test_chaos_engine_genai_spans_6518 -v` (repo-only) |
| PR Gate check | `genai-spans-contract` (surface `genai-spans`) |
| Suite task | `reg-genai-spans-6518` |

## Rules

1. `emit` refuses when the suite manifest drops this task.
2. `invoke_agent` requires an agent slug. `execute_tool` requires a tool slug and a known parent span.
3. Span kind is the module constant `SPAN_KIND`.
4. Field text is refused when it is not a short slug or holds a secret, an absolute path, a URL, or a backtick.

## Related

- Budget checks stay in [context rot](context-rot.md).
- The task is listed by [harness eval suite](harness-eval-suite.md).
