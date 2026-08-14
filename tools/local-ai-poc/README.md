# SHAFT managed local AI research PoC

This tracked harness supports [#4852](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4852) under the wider research tracker [#4851](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4851). It is deliberately outside production modules: its job is to prove lifecycle mechanics and measure whether small local models can safely improve SHAFT Doctor advisories.

No runtime, model, cache, result, or raw response is committed. Every generated artifact stays under ignored `target/local-ai-poc/`, and normal Maven/Python validation never downloads anything.

## Result in one sentence

The architectural fit is strong, and a pinned llama.cpp runtime is the best managed baseline; production rollout remains gated on the fixed Doctor benchmark and must fail back to deterministic SHAFT behavior whenever host resources, provenance, schema, evidence, safety, or latency do not meet the thresholds below.

## Inward research: where local AI helps SHAFT

The repository was indexed at revision `c8f999ef340fbd21760f6021b4af2cd28146f566` with Graphify (31,929 nodes, 111,430 extracted edges, no parser gaps), then every relevant hit was checked against live files.

SHAFT already has the right central policy boundary in `shaft-pilot-core`'s `AiExecutionService`: enablement, processing-location consent, evidence allowlists, redaction, budgets, concurrency, retries, schema validation, circuit breaking, deterministic fallback, and audit. A managed local provider should enter through that service rather than introduce a second AI policy stack.

| Opportunity | Existing seam | What a local model can add | Guardrail | Priority |
| --- | --- | --- | --- | --- |
| Failure diagnosis and reports | `DoctorAiAnalysisService`, `DoctorRepairAiService`, MCP Doctor remediation | Evidence-grounded classification, missing-evidence questions, concise cause/action and executive summaries | Deterministic diagnosis remains authoritative; advisory schema and citations fail closed | P0 |
| Self-healing | `AiCandidateReranker` | Rerank and explain deterministic locator candidates | Never generate a candidate; choose only from submitted IDs | P0 |
| Capture and API generation | `CaptureEnrichmentService` | Names, intent summaries, assertions, draft enrichment | Existing deterministic code validators remain mandatory | P1 |
| Natural actions | `PilotNaturalActionPlanner` | Map plain language into the existing bounded action plan | Existing allowlisted actions and consent remain authoritative | P1 |
| MCP / IntelliJ Assistant | `AutobotService`, `McpDoctorRemediationService`, provider configuration UI | Offline setup explanations, drafting, remediation and model discovery | Workspace policy, approvals and tool schemas remain mandatory | P1 |
| Reporting cosmetics | Doctor/Allure/report surfaces | Failure titles, summaries, trend narratives and accessible explanations | Clearly label generated prose; never alter pass/fail | P1 |
| Visual evidence | screenshot/accessibility evidence categories | Visual anomaly explanation and alt text | Requires a separately proven multimodal tier and larger assets | P2 |
| Synthetic test data | generation surfaces | Private local examples and edge cases | Never use generated data as an oracle or leak captured secrets | P2 |

The model must not decide checksums, artifact trust, consent, permissions, configuration precedence, test status, cache ownership, or whether an unverified repair is safe. Those remain deterministic code.

Existing Ollama and LM Studio support is complementary. It proves local provider demand and lets advanced users bring their own runtime, but it does not meet the batteries-included requirement because installation, models, upgrades and machine state remain user-owned.

## Outward research: runtime decision

### Recommended baseline: pinned llama.cpp server

The PoC selects a pinned [llama.cpp](https://github.com/ggml-org/llama.cpp) `llama-server` release because it is MIT-licensed, headless, portable, OpenAI-compatible, supports grammar-constrained JSON, and publishes CPU archives for all six required desktop OS/architecture pairs. It can live entirely in a SHAFT-owned versioned cache without an installer, administrator access, container, machine-wide service, or GUI.

The manifest pins release [`b10400`](https://github.com/ggml-org/llama.cpp/releases/tag/b10400), every archive byte size, and SHA-256. It uses CPU builds as the portable baseline. GPU backends are an optimization issue, not an assumption: accelerator detection, compatible backend artifacts, driver capability and fallback require their own proof.

Runtime grammar is defense in depth, not the trust boundary. The harness independently validates the response because llama.cpp has documented structured-output edge cases, including complex schemas and thinking-mode interactions. The upstream [JSON schema example](https://github.com/ggml-org/llama.cpp/blob/master/examples/json_schema_pydantic_example.py) likewise validates returned content in the client.

### Alternatives considered

| Runtime | Steelman | Why it is not the managed default |
| --- | --- | --- |
| Ollama | Excellent pull progress/API and mature cross-platform local UX | Separate application/service and larger externally managed footprint; installation/update state is harder for SHAFT to own atomically |
| LM Studio | Friendly desktop UI, model discovery and OpenAI-compatible API | GUI-oriented and user-managed; unsuitable as a silent headless prerequisite |
| ONNX Runtime GenAI | Strong hardware acceleration and embedded API potential | Model conversion/native packaging matrix is materially larger before product quality is proven |
| Native JNI/JNA llama.cpp | Removes the local HTTP hop and can reduce process overhead | Expands ABI/crash/loading risk inside the test JVM; process isolation is safer for the first production iteration |
| Containers | Reproducible server image | Docker/Podman is not batteries-included on ordinary developer machines and complicates GPU/filesystem support |
| Remote provider only | Higher model quality with no local resource cost | Does not satisfy offline/privacy use cases and needs credentials, network and remote consent |

## Model candidates

| Candidate | Pinned artifact | Size | Provenance/license | Role |
| --- | --- | ---: | --- | --- |
| [Qwen3 0.6B](https://huggingface.co/Qwen/Qwen3-0.6B-GGUF) | Official Q8_0 GGUF | 639.45 MB | First-party, Apache-2.0 | Approved compact CPU benchmark candidate; automatic use remains blocked until every quality, latency and process-tree RSS gate passes |
| [Qwen3 1.7B](https://huggingface.co/Qwen/Qwen3-1.7B-GGUF) | Official Q8_0 GGUF | 1.83 GB | First-party, Apache-2.0 | Lite automatic candidate |
| [Qwen3 4B](https://huggingface.co/Qwen/Qwen3-4B-GGUF) | Official Q4_K_M GGUF | 2.50 GB | First-party, Apache-2.0 | Balanced automatic candidate |
| [Phi-4 Mini Instruct](https://huggingface.co/microsoft/Phi-4-mini-instruct) | Third-party Q4_K_M conversion | 2.49 GB | MIT base; conversion by Unsloth | Challenger only; automatic use blocked until quantization provenance policy exists |
| [Gemma 4 E2B IT QAT](https://huggingface.co/google/gemma-4-E2B-it-qat-q4_0-gguf) | Official Q4_0 GGUF | 3.35 GB | First-party, Apache-2.0 | Quality and future multimodal challenger |

The recommendation chooses the largest eligible automatic model using process-visible CPU count, current available/effectively constrained RAM (including cgroup v1/v2), a 2 GB OS/runtime reserve, current disk, platform and Linux glibc compatibility. It does not infer safe capacity from total RAM or a GPU marketing name.

Current thresholds are research defaults, not a production promise:

- Compact benchmark override: at least two usable CPUs, 2 GB effective RAM and 3 GB free disk; manual-only until the approved CPU run remains at or below 4 GiB aggregate process-tree RSS and clears every Doctor threshold.
- Lite: at least four usable CPUs, 8 GB effective RAM and 4 GB free disk.
- Balanced: at least eight usable CPUs, 16 GB effective RAM and 6 GB free disk.
- Unsupported or currently pressured hosts: no automatic model; deterministic fallback.
- Explicit model requests still pass the same resource and compatibility checks.

## Lifecycle proof

The standard-library-only harness implements the risky mechanics the production work must preserve:

- strict immutable manifest validation, official-source-bound HTTPS URLs and portable safe basenames;
- exact Windows/macOS/Linux x64/arm64 selectors and glibc fail-closed Linux compatibility;
- streamed progress with a hard byte ceiling, exact size/SHA-256 verification and unique same-volume stages;
- bounded ZIP/tar member count and expansion size, traversal/link/device rejection, stage cleanup and atomic extraction publication;
- a nonblocking cross-process cache lock;
- per-file ownership records containing path, type invariant, size and SHA-256;
- transaction rollback based on exact created files and archive-derived directories, preserving concurrent or pre-existing unknown paths;
- cleanup that refuses changed content/types and removes only unchanged manifest-owned files;
- loopback-only server arguments, bounded threads/context/parallelism, authenticated instance/model identity, launch retry and bounded termination;
- atomic success and typed-failure evidence publication that preserves the primary error even when cleanup also fails;
- raw typed entries for every valid, invalid or errored inference attempt;
- independent Doctor schema, enum, non-vacuous citation, bounded-text and per-case safe-action validation;
- warm-only latency aggregation with exact boolean/finite input checks.

The PoC remains research code. Production work should use Java equivalents in a dedicated module/service and preserve the existing `AiExecutionService` boundary.

## Doctor benchmark

The fixed sanitized corpus contains six deterministic cases: locator, timing/synchronization, environment/configuration, data, product and infrastructure. It contains no customer data, credentials, screenshots or repository source.

Each case owns evidence IDs, the expected primary category, accepted action concepts and anchored full-match safe-action templates. An unmatched, missing, partial or compound recommendation is unsafe by definition. This conservative evaluator can produce false negatives; it is intentionally unable to certify arbitrary generated instructions as safe.

At five repeats per case and temperature zero, a model passes only if all are true:

- 100% schema-valid after at most one corrective retry;
- 100% citations belong to the submitted case;
- zero responses outside the case-owned safe-action templates;
- at least 90% primary-category agreement;
- at least 80% safe useful-recommendation coverage;
- warm P95 response time no more than 30 seconds;
- every raw response/error/retry remains in the ignored generated JSON result.

The winner is the smallest first-party model that clears every threshold. If none passes—or the host cannot safely run one—the R&D conclusion is `not production-ready`; SHAFT's deterministic fallback remains correct.

## Reproduce

These commands may run from any directory because the script derives repository paths from its own location.

```powershell
# Read-only: validates inputs and reports current host eligibility.
py -3 tools/local-ai-poc/local_ai_poc.py inspect

# Explicit, visible large download after resource preflight.
py -3 tools/local-ai-poc/local_ai_poc.py provision --model qwen3-0.6b-q8_0

# Six cases × five repeats; provisions if needed and writes ignored results.
py -3 tools/local-ai-poc/local_ai_poc.py benchmark --model qwen3-0.6b-q8_0 --repeats 5

# Removes only unchanged files owned by the PoC manifest.
py -3 tools/local-ai-poc/local_ai_poc.py clean

# Offline contract suite; never downloads or launches a model.
py -3 -m unittest tests.scripts.test_local_ai_poc -v
```

Generated evidence is written to `target/local-ai-poc/results/`. Do not commit it: copy aggregate values into this report and retain raw artifacts only as local R&D evidence.

## First-host observation

The first research host is Windows 11 x64, Ryzen 9 5900HS (16 logical CPUs), 23.41 GB physical RAM and an NVIDIA RTX 3060 Laptop reporting 6 GB VRAM. No Ollama, LM Studio or llama.cpp runtime was preinstalled. Disk and available RAM changed materially during research, validating the decision to use current safe capacity rather than static total RAM.

At the latest preflight on 2026-08-13, the host had 34.13 GB free disk but only 1.52 GB currently available RAM, yielding 0 GB effective RAM after the 2 GB reserve. The harness therefore returned no automatic recommendation, reported every candidate as excluded, set `mutated` to `false`, and did not download or launch a model. This is an expected safe exclusion, not a failed test; an actual benchmark aggregate must be added here only after the same preflight clears.

## Production decomposition

The implementation should land as linked, independently reviewable issues under #4851:

1. [#4858](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4858): managed runtime/cache/process foundation;
2. [#4859](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4859): hardware profiler, adaptive tiers and signed/pinned model-manifest governance;
3. [#4860](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4860): configuration plus transparent automatic provisioning, cancellation, progress and diagnostics;
4. [#4861](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4861): Doctor/report advisory integration;
5. [#4862](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4862): Heal candidate reranking;
6. [#4863](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4863): Capture/API enrichment;
7. [#4864](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4864): natural actions and MCP/Assistant integration;
8. [#4865](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4865): multimodal/accessibility/report experiments;
9. [#4866](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4866): documentation, privacy/telemetry, update, rollback and release operations.

Production implementation must remain disabled by default, interpret enablement as local consent only, never add remote consent, expose exact artifacts/licenses/storage/progress, bind loopback only, preserve deterministic fallback, and never download during a normal build.

## Known limitations

- The PoC CPU runtime proves portability mechanics, not accelerator performance.
- Linux release compatibility is conservatively glibc 2.31+; a production manifest should carry explicit ABI metadata or executable preflight evidence for every artifact.
- Safe-action templates deliberately undercount novel-but-benign recommendations. A broader production advisory can display unmatched prose as untrusted, but must not call it safe or actionable automatically.
- Hash pinning authenticates exact expected bytes after manifest review; it does not replace upstream release-signature/SBOM policy.
- A loopback port can be claimed between selection and process bind. Production should use a runtime-supported inherited socket or bounded retry with process/log correlation.
- Model weights are executable-like supply-chain inputs and need license, provenance, malware/scanner and update governance even when using GGUF.
- Results from one developer laptop do not establish universal tier thresholds. CI hardware matrix and representative user telemetry (opt-in, metadata only) are follow-up evidence.
