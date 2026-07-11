package com.shaft.capture.generate;

import tools.jackson.core.JsonPointer;
import tools.jackson.core.util.DefaultIndenter;
import tools.jackson.core.util.DefaultPrettyPrinter;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.SerializationFeature;
import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.node.ObjectNode;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureReadiness;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.EventContext;
import com.shaft.capture.model.ExternalTestDataReference;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.capture.privacy.CapturePrivacyClassifier;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.AccessDeniedException;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Deterministic pipeline that validates, renders, compiles, replays, and reports generated SHAFT tests.
 */
public final class CaptureGenerator {
    private static final Pattern JAVA_IDENTIFIER = Pattern.compile("[A-Za-z_$][A-Za-z0-9_$]*");
    private static final Pattern JAVA_PACKAGE = Pattern.compile(
            "[A-Za-z_$][A-Za-z0-9_$]*(\\.[A-Za-z_$][A-Za-z0-9_$]*)*");
    private static final Pattern WINDOWS_PERSONAL_PATH = Pattern.compile(
            "(?i)(?:file:/+)?[A-Z]:[/\\\\](?:Users|Documents and Settings)[/\\\\][^/\\\\\\s\"']+");
    private static final Pattern UNIX_PERSONAL_PATH = Pattern.compile(
            "(?:file:/+)?/(?:Users|home)/[^/\\s\"']+");
    private static final Pattern BEARER_SECRET = Pattern.compile("(?i)Bearer\\s+[A-Za-z0-9._~+/-]{8,}");
    private static final Pattern JWT_SECRET = Pattern.compile(
            "eyJ[A-Za-z0-9_-]{6,}\\.[A-Za-z0-9_-]{6,}\\.[A-Za-z0-9_-]{6,}");
    private static final Pattern API_KEY_SECRET = Pattern.compile(
            "(?i)(?:sk|api|token)[-_][A-Za-z0-9_-]{12,}");
    private static final Pattern NAMED_SECRET = Pattern.compile(
            "(?i)(?:password|authorization|cookie|api[-_]?key|access[-_]?token)"
                    + "\\s*[:=]\\s*[\"']?[^\\s\"']{6,}");
    private static final Pattern INDEXED_LOCATOR = Pattern.compile("\\[\\d+]|:nth-(?:child|of-type)\\(");
    private static final Pattern EVIDENCE_ID = Pattern.compile(
            "\\b(?:event-\\d+|checkpoint-[A-Za-z0-9._-]+|action-\\d+)\\b");
    private static final ObjectMapper JSON = JsonMapper.builder()
            .enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS)
            .build();
    private static final DefaultPrettyPrinter PRINTER = printer();

    private final CaptureJsonCodec codec;
    private final LocatorRanker locatorRanker;
    private final GeneratedTestValidator validator;
    private final CaptureEnrichmentService enrichmentService;

    /**
     * Creates the default generation pipeline.
     */
    public CaptureGenerator() {
        this(new CaptureJsonCodec(), new LocatorRanker(), new GeneratedTestValidator(),
                new CaptureEnrichmentService());
    }

    /**
     * Creates an injectable generation pipeline.
     *
     * @param codec Capture session codec
     * @param locatorRanker deterministic locator ranker
     * @param validator generated test validator
     * @param enrichmentService optional enrichment preview service
     */
    public CaptureGenerator(
            CaptureJsonCodec codec,
            LocatorRanker locatorRanker,
            GeneratedTestValidator validator,
            CaptureEnrichmentService enrichmentService) {
        this.codec = Objects.requireNonNull(codec, "codec");
        this.locatorRanker = Objects.requireNonNull(locatorRanker, "locatorRanker");
        this.validator = Objects.requireNonNull(validator, "validator");
        this.enrichmentService = Objects.requireNonNull(enrichmentService, "enrichmentService");
    }

    /**
     * Generates, validates, and reports one SHAFT TestNG test.
     *
     * @param request generation options
     * @return generated artifacts and report
     */
    public CaptureGenerationResult generate(CaptureGenerationRequest request) {
        return generate(request, CodegenBackend.WEBDRIVER);
    }

    /**
     * Generates, validates, and reports one SHAFT TestNG test for a selected GUI backend.
     *
     * @param request generation options
     * @param backend generated SHAFT GUI backend
     * @return generated artifacts and report
     */
    @SuppressWarnings("PMD.NPathComplexity")
    public CaptureGenerationResult generate(CaptureGenerationRequest request, CodegenBackend backend) {
        Objects.requireNonNull(request, "request");
        CodegenBackend targetBackend = backend == null ? CodegenBackend.WEBDRIVER : backend;
        Path sessionPath = request.sessionPath().toAbsolutePath().normalize();
        Path outputRoot = request.outputDirectory().toAbsolutePath().normalize();
        Path privacyRoot = privacyAllowedRoot(sessionPath, outputRoot);
        Path reportPath = outputRoot.resolve("target/shaft-capture/generation-report.json");
        CaptureSession session = null;
        ArtifactPaths paths = null;
        try {
            session = codec.read(sessionPath);
            validatePackage(request.packageName());
            String deterministicClassName = request.className().isBlank()
                    ? defaultClassName(session)
                    : request.className();
            validateJavaIdentifier(deterministicClassName, "Generated class name");
            String deterministicMethodName = defaultMethodName(session);
            paths = artifactPaths(outputRoot, request.packageName(), deterministicClassName);

            GenerationState state = analyze(session, sessionPath, targetBackend);
            Map<String, String> elementNames = defaultElementNames(state.targets());
            String deterministicSource = renderSource(session, request.packageName(), deterministicClassName,
                    deterministicMethodName, state.targets(), state.data(), elementNames, List.of(), targetBackend,
                    request.fallbackLocators(), Set.of());
            String fingerprint = fingerprint(codec.write(session), deterministicSource);
            Set<Long> appliedControlFlowGuards = Set.of();
            if (request.controlFlowMode() == CaptureGenerationRequest.ControlFlowMode.PREVIEW) {
                writeControlFlowPreview(
                        request.controlFlowPreviewPath().toAbsolutePath().normalize(),
                        new CaptureControlFlowPreview(
                                CaptureControlFlowPreview.CURRENT_SCHEMA_VERSION,
                                session.sessionId(),
                                fingerprint,
                                state.controlFlow()),
                        request.overwrite());
            } else if (request.controlFlowMode() == CaptureGenerationRequest.ControlFlowMode.APPLY) {
                CaptureControlFlowPreview preview = readControlFlowPreview(request.controlFlowPreviewPath());
                List<String> controlFlowErrors = validateControlFlowPreview(preview, session.sessionId(), fingerprint,
                        state.controlFlow());
                if (!controlFlowErrors.isEmpty()) {
                    state.unsupported().addAll(controlFlowErrors);
                } else {
                    appliedControlFlowGuards = approvedOptionalGuardSequences(preview);
                    state = state.withControlFlow(appliedControlFlow(state.controlFlow(), appliedControlFlowGuards));
                }
            }

            CaptureGenerationReport.Enrichment enrichment = CaptureGenerationReport.Enrichment.notRequested();
            CaptureEnrichmentPreview.Proposal appliedProposal = CaptureEnrichmentPreview.Proposal.empty();
            if (request.enrichmentMode() == CaptureGenerationRequest.EnrichmentMode.PREVIEW) {
                CaptureEnrichmentPreview preview = enrichmentService.preview(
                        session,
                        fingerprint,
                        deterministicClassName,
                        deterministicMethodName,
                        elementNames,
                        request.aiApprovalPolicy());
                List<String> previewPrivacy = privacyFindings(writeJson(preview), privacyRoot);
                if (!previewPrivacy.isEmpty()) {
                    state.unsupported().addAll(previewPrivacy);
                    enrichment = new CaptureGenerationReport.Enrichment(
                            CaptureGenerationReport.Enrichment.EnrichmentStatus.REJECTED,
                            relative(outputRoot, request.enrichmentPreviewPath().toAbsolutePath().normalize()),
                            preview.diff(),
                            preview.provider());
                } else {
                    ensureWritable(request.enrichmentPreviewPath().toAbsolutePath().normalize(), request.overwrite());
                    atomicWrite(request.enrichmentPreviewPath().toAbsolutePath().normalize(), writeJson(preview));
                    enrichment = new CaptureGenerationReport.Enrichment(
                            CaptureGenerationReport.Enrichment.EnrichmentStatus.PREVIEWED,
                            relative(outputRoot, request.enrichmentPreviewPath().toAbsolutePath().normalize()),
                            preview.diff(),
                            preview.provider());
                }
            } else if (request.enrichmentMode() == CaptureGenerationRequest.EnrichmentMode.APPLY) {
                CaptureEnrichmentPreview preview = readPreview(request.enrichmentPreviewPath());
                List<String> proposalErrors = validateProposal(preview, fingerprint, state.targets());
                if (!proposalErrors.isEmpty()) {
                    state.unsupported().addAll(proposalErrors);
                    enrichment = new CaptureGenerationReport.Enrichment(
                            CaptureGenerationReport.Enrichment.EnrichmentStatus.REJECTED,
                            relative(outputRoot, request.enrichmentPreviewPath().toAbsolutePath().normalize()),
                            preview.diff(),
                            preview.provider());
                } else {
                    appliedProposal = preview.proposal();
                    enrichment = new CaptureGenerationReport.Enrichment(
                            CaptureGenerationReport.Enrichment.EnrichmentStatus.APPLIED,
                            relative(outputRoot, request.enrichmentPreviewPath().toAbsolutePath().normalize()),
                            preview.diff(),
                            preview.provider());
                }
            }

            String className = appliedProposal.className().isBlank()
                    ? deterministicClassName
                    : appliedProposal.className();
            String methodName = appliedProposal.methodName().isBlank()
                    ? deterministicMethodName
                    : appliedProposal.methodName();
            Map<String, String> finalElementNames = mergeElementNames(elementNames, appliedProposal.elementNames());
            if (!className.equals(deterministicClassName)) {
                paths = artifactPaths(outputRoot, request.packageName(), className);
            }
            String source = renderSource(session, request.packageName(), className, methodName,
                    state.targets(), state.data(), finalElementNames, appliedProposal.assertions(), targetBackend,
                    request.fallbackLocators(), appliedControlFlowGuards);
            String dataJson = writeJson(state.data().root());

            List<String> privacy = new ArrayList<>();
            privacy.addAll(privacyFindings(codec.write(session), privacyRoot));
            privacy.addAll(privacyFindings(source, privacyRoot));
            privacy.addAll(privacyFindings(dataJson, privacyRoot));
            privacy.stream().distinct().forEach(state.unsupported()::add);
            validateOutputs(paths, request.overwrite(), state.unsupported());

            if (!state.unsupported().isEmpty()) {
                CaptureGenerationReport report = report(
                        session,
                        paths,
                        state,
                        CaptureGenerationReport.Status.FAILED,
                        CaptureGenerationReport.Validation.skipped("Generation failed before compilation."),
                        CaptureGenerationReport.Validation.skipped("Generation failed before replay."),
                        enrichment);
                writeReportIfPossible(reportPath, report);
                return result(paths.source(), paths.data(), reportPath,
                        request.enrichmentPreviewPath(), report);
            }

            atomicWrite(paths.source(), source);
            atomicWrite(paths.data(), dataJson);
            CaptureGenerationReport.Validation compilation = request.compile()
                    ? validator.compile(paths.source(), paths.classes())
                    : CaptureGenerationReport.Validation.skipped("Compilation was not requested.");
            CaptureGenerationReport.Validation replay = CaptureGenerationReport.Validation.skipped(
                    "Replay was not requested.");
            if (request.replay()
                    && compilation.status() == CaptureGenerationReport.Validation.ValidationStatus.PASSED) {
                replay = validator.replay(
                        request.packageName() + "." + className,
                        paths.classes(),
                        outputRoot.resolve("src/test/resources"),
                        outputRoot,
                        request.replayTimeout());
            } else if (request.replay()) {
                replay = CaptureGenerationReport.Validation.skipped(
                        "Replay was skipped because compilation failed.");
            }
            boolean successful = compilation.status()
                    != CaptureGenerationReport.Validation.ValidationStatus.FAILED
                    && replay.status() != CaptureGenerationReport.Validation.ValidationStatus.FAILED;
            CaptureGenerationReport report = report(
                    session,
                    paths,
                    state,
                    successful ? CaptureGenerationReport.Status.SUCCESS : CaptureGenerationReport.Status.FAILED,
                    compilation,
                    replay,
                    enrichment);
            String reportJson = writeJson(report);
            List<String> reportPrivacy = privacyFindings(reportJson, privacyRoot);
            if (!reportPrivacy.isEmpty()) {
                CaptureGenerationReport privacyFailure = report(
                        session,
                        paths,
                        state.withUnsupported(reportPrivacy),
                        CaptureGenerationReport.Status.FAILED,
                        compilation,
                        replay,
                        enrichment);
                atomicWrite(reportPath, writeJson(privacyFailure));
                return result(paths.source(), paths.data(), reportPath,
                        request.enrichmentPreviewPath(), privacyFailure);
            }
            atomicWrite(reportPath, reportJson);
            return result(paths.source(), paths.data(), reportPath,
                    request.enrichmentPreviewPath(), report);
        } catch (RuntimeException exception) {
            String sessionId = session == null ? "" : session.sessionId();
            ArtifactPaths safePaths = paths == null
                    ? artifactPaths(outputRoot, "generated.capture", "CapturedJourneyTest")
                    : paths;
            GenerationState failure = GenerationState.failure(safeMessage(exception));
            CaptureGenerationReport report = report(
                    sessionId,
                    safePaths,
                    failure,
                    CaptureGenerationReport.Status.FAILED,
                    CaptureGenerationReport.Validation.skipped("Generation failed before compilation."),
                    CaptureGenerationReport.Validation.skipped("Generation failed before replay."),
                    CaptureGenerationReport.Enrichment.notRequested());
            writeReportIfPossible(reportPath, report);
            return result(safePaths.source(), safePaths.data(), reportPath,
                    request.enrichmentPreviewPath(), report);
        }
    }

    @SuppressWarnings("PMD.NPathComplexity")
    private GenerationState analyze(CaptureSession session, Path sessionPath, CodegenBackend backend) {
        List<String> unsupported = new ArrayList<>();
        List<String> flaky = new ArrayList<>();
        List<String> fallback = new ArrayList<>();
        List<String> required = new ArrayList<>();
        List<String> warnings = new ArrayList<>();
        Map<String, MutableTargetPlan> targets = new LinkedHashMap<>();
        Set<Long> verificationSequences = new HashSet<>();
        Set<String> knownWindows = new LinkedHashSet<>();
        if (!session.events().isEmpty()) {
            knownWindows.add(session.events().getFirst().context().page().logicalWindowId());
        }
        for (CaptureEvent event : session.events()) {
            String eventId = eventId(event);
            if (event.context().replayStatus() == EventContext.ReplayStatus.UNSUPPORTED) {
                unsupported.add(eventId + ": the recorder marked this step unsupported. "
                        + "Record a supported equivalent action.");
            } else if (event.context().replayStatus() == EventContext.ReplayStatus.FAILED
                    || event.context().replayStatus() == EventContext.ReplayStatus.SKIPPED) {
                flaky.add(eventId + ": recorded replay status is " + event.context().replayStatus() + ".");
            }
            validateEvent(event, unsupported, required, knownWindows, backend);
            if (event instanceof CaptureEvent.VerificationEvent) {
                verificationSequences.add(event.context().sequence());
            }
            target(event).ifPresent(target -> {
                if (target.locatorCandidates().isEmpty()) {
                    unsupported.add(eventId + ": element " + target.logicalElementId()
                            + " has no locator evidence. Re-record the step with a visible target.");
                    return;
                }
                LocatorRanker.LocatorSelection selection =
                        locatorRanker.select(target, event.context(), interaction(event));
                MutableTargetPlan existing = targets.computeIfAbsent(
                        target.logicalElementId(),
                        ignored -> new MutableTargetPlan(target, event.context(), selection));
                existing.eventIds.add(eventId);
                if (selection.selected().score() > existing.selection.selected().score()) {
                    existing.target = target;
                    existing.context = event.context();
                    existing.selection = selection;
                }
            });
        }
        for (Checkpoint checkpoint : session.checkpoints()) {
            if (checkpoint.kind() == Checkpoint.CheckpointKind.ASSERTION
                    && !verificationSequences.contains(checkpoint.sequence())) {
                String description = checkpoint.description().isBlank()
                        ? ""
                        : " (" + checkpoint.description() + ")";
                unsupported.add("checkpoint-" + checkpoint.id() + description + ": assertion checkpoints require a "
                        + "VerificationEvent at sequence " + checkpoint.sequence()
                        + ". Record the expected target and value explicitly so generated code uses SHAFT "
                        + "assertion builders only.");
            }
        }
        List<TargetPlan> immutableTargets = targets.entrySet().stream()
                .map(entry -> entry.getValue().immutable(entry.getKey()))
                .sorted(Comparator.comparing(TargetPlan::logicalElementId))
                .toList();
        for (TargetPlan target : immutableTargets) {
            if (!target.selection().alternatives().isEmpty()) {
                LocatorRanker.ScoredLocator alternative = target.selection().alternatives().getFirst();
                fallback.add(target.logicalElementId() + ": "
                        + alternative.candidate().strategy() + " "
                        + alternative.candidate().expression() + " (score " + alternative.score() + ")");
            }
            LocatorCandidate selected = target.selection().selected().candidate();
            if (selected.uniquenessCount() != 1 || !selected.stable()
                    || target.selection().selected().score() < 500) {
                flaky.add(target.logicalElementId() + ": selected "
                        + selected.strategy() + " locator has score "
                        + target.selection().selected().score() + ".");
            }
            brittleLocatorWarning(target).ifPresent(warnings::add);
        }
        DataPlan data = data(session, sessionPath, required, unsupported);
        warnings.addAll(missingAssertionWarnings(session, verificationSequences));
        warnings.addAll(waitReviewWarnings(session.events()));
        warnings.addAll(testDataReviewWarnings(data.references()));
        List<CaptureGenerationReport.ControlFlowSuggestion> controlFlow =
                controlFlowSuggestions(session.events(), session.checkpoints());
        if (session.status() != CaptureSession.SessionStatus.COMPLETED) {
            warnings.add("The source Capture session is INCOMPLETE: the recording ended unexpectedly "
                    + "(it was never stopped cleanly), so the generated test may be missing the end of "
                    + "the journey. Re-record the flow and confirm the recording reports COMPLETED "
                    + "before generating code from it.");
        }
        if (session.events().isEmpty()) {
            warnings.add("The source Capture session contains no events.");
        }
        return new GenerationState(
                immutableTargets,
                data,
                CaptureReadiness.from(session),
                unsupported,
                flaky,
                fallback,
                controlFlow,
                required,
                warnings);
    }

    private static void validateEvent(
            CaptureEvent event,
            List<String> unsupported,
            List<String> required,
            Set<String> knownWindows,
            CodegenBackend backend) {
        String id = eventId(event);
        if (hasShadowContext(event.context())) {
            unsupported.add(id + ": shadow-root replay requires a recorded host-selector chain. "
                    + "Re-record after enabling shadow-host evidence.");
        }
        if (event instanceof CaptureEvent.ClickEvent value) {
            if (value.button() != CaptureEvent.MouseButton.PRIMARY) {
                unsupported.add(id + ": only primary-button clicks are generated in v1.");
            }
            if (value.clickCount() > 2) {
                unsupported.add(id + ": click counts above two are unsupported.");
            }
        } else if (event instanceof CaptureEvent.SelectEvent value
                && value.mode() == CaptureEvent.SelectMode.INDEX) {
            unsupported.add(id + ": index-based selection is volatile. Record value or visible text.");
        } else if (event instanceof CaptureEvent.SelectEvent value && isSecret(value.value())) {
            unsupported.add(id + ": secret select values are not generated because selection diagnostics "
                    + "could expose them.");
        } else if (event instanceof CaptureEvent.WindowEvent value) {
            if (value.action() == CaptureEvent.WindowAction.OPEN_TAB
                    || value.action() == CaptureEvent.WindowAction.OPEN_WINDOW) {
                knownWindows.add(value.logicalWindowId());
            } else if (value.action() == CaptureEvent.WindowAction.SWITCH
                    && !knownWindows.contains(value.logicalWindowId())) {
                unsupported.add(id + ": logical window " + value.logicalWindowId()
                        + " was not opened earlier in the session.");
            }
        } else if (event instanceof CaptureEvent.WaitEvent value) {
            boolean elementCondition = switch (value.condition()) {
                case ELEMENT_PRESENT, ELEMENT_VISIBLE, ELEMENT_CLICKABLE, ELEMENT_ABSENT -> true;
                default -> false;
            };
            if (elementCondition && value.target() == null) {
                unsupported.add(id + ": element wait requires target evidence.");
            }
            if ((value.condition() == CaptureEvent.WaitCondition.URL_CONTAINS
                    || value.condition() == CaptureEvent.WaitCondition.TITLE_CONTAINS)
                    && value.expected() == null) {
                unsupported.add(id + ": URL/title wait requires an expected data reference.");
            } else if ((value.condition() == CaptureEvent.WaitCondition.URL_CONTAINS
                    || value.condition() == CaptureEvent.WaitCondition.TITLE_CONTAINS)
                    && isSecret(value.expected())) {
                unsupported.add(id + ": secret URL/title wait values are not generated because wait "
                        + "diagnostics could expose them.");
            }
        } else if (event instanceof CaptureEvent.VerificationEvent value) {
            validateVerification(id, value.verification(), value.target(), value.expected(),
                    value.context(), unsupported);
        } else if (event instanceof CaptureEvent.FrameEvent value
                && backend == CodegenBackend.PLAYWRIGHT
                && value.action() != CaptureEvent.FrameAction.TOP) {
            unsupported.add(id + ": Playwright codegen cannot replay WebDriver frame switching yet.");
        } else if (event instanceof CaptureEvent.AlertEvent value
                && value.action() == CaptureEvent.AlertAction.VERIFY_TEXT
                && isSecret(value.text())) {
            unsupported.add(id + ": secret alert text cannot be asserted without exposing it in reports.");
        } else if (event instanceof CaptureEvent.NavigationEvent value
                && value.targetUrl().contains("[data:")) {
            required.add(id + ": replace sanitized URL data placeholders before replay.");
        } else if (event instanceof CaptureEvent.NetworkEvent) {
            // P2: NetworkEvent requires ApiCaptureGenerator; UI-only codegen skips network capture
            unsupported.add(id + ": network capture requires ApiCaptureGenerator (P2 feature).");
        }
    }

    private static void validateVerification(
            String eventId,
            CaptureEvent.VerificationKind verification,
            ElementSnapshot target,
            ExternalTestDataReference expected,
            EventContext context,
            List<String> unsupported) {
        boolean targetRequired = switch (verification) {
            case URL_EQUALS, URL_CONTAINS, TITLE_EQUALS, TITLE_CONTAINS, PAGE_TEXT_CONTAINS -> false;
            default -> true;
        };
        boolean expectedRequired = switch (verification) {
            case TEXT_EQUALS, TEXT_CONTAINS, ATTRIBUTE_EQUALS, URL_EQUALS, URL_CONTAINS,
                 TITLE_EQUALS, TITLE_CONTAINS, PAGE_TEXT_CONTAINS -> true;
            default -> false;
        };
        if (targetRequired && target == null) {
            unsupported.add(eventId + ": " + verification + " requires target evidence.");
        }
        if (expectedRequired && expected == null) {
            unsupported.add(eventId + ": " + verification + " requires an expected data reference.");
        }
        if (expectedRequired && isSecret(expected)) {
            unsupported.add(eventId + ": secret values cannot be rendered as assertion expectations.");
        }
        if (verification == CaptureEvent.VerificationKind.ATTRIBUTE_EQUALS
                && extensionText(context, "attributeName").isBlank()) {
            unsupported.add(eventId + ": ATTRIBUTE_EQUALS requires context.extensions.attributeName.");
        }
    }

    private static Optional<String> brittleLocatorWarning(TargetPlan target) {
        LocatorCandidate candidate = target.selection().selected().candidate();
        String expression = candidate.expression();
        boolean xpath = candidate.strategy() == LocatorCandidate.LocatorStrategy.XPATH;
        boolean brittle = (xpath && isAbsoluteXpath(expression)) || INDEXED_LOCATOR.matcher(expression).find();
        if (!brittle) {
            return Optional.empty();
        }
        String evidence = String.join(",", target.eventIds());
        String summary = "Brittle " + candidate.strategy() + " locator selected for "
                + target.logicalElementId() + ": " + expression + ".";
        return Optional.of(reviewWarning(
                "LOCATOR",
                "WARNING",
                evidence.isBlank() ? target.logicalElementId() : evidence,
                summary,
                semanticLocatorRecommendation(target.target())));
    }

    private static List<String> missingAssertionWarnings(CaptureSession session, Set<Long> verificationSequences) {
        List<String> warnings = new ArrayList<>();
        for (CaptureEvent event : session.events()) {
            if (!needsPostActionAssertion(event) || hasLaterAssertion(event.context().sequence(), verificationSequences)) {
                continue;
            }
            warnings.add(reviewWarning(
                    "ASSERTION",
                    "WARNING",
                    eventId(event),
                    "No recorded SHAFT assertion-builder verification follows a navigation or form-submission action.",
                    "Record a verification for the post-action page state."));
        }
        return warnings;
    }

    private static boolean hasLaterAssertion(long sequence, Set<Long> assertionSequences) {
        return assertionSequences.stream().anyMatch(assertion -> assertion > sequence);
    }

    private static boolean needsPostActionAssertion(CaptureEvent event) {
        if (event instanceof CaptureEvent.NavigationEvent) {
            return true;
        }
        if (!(event instanceof CaptureEvent.ClickEvent)) {
            return false;
        }
        return target(event).map(CaptureGenerator::looksLikeFormSubmission).orElse(false);
    }

    private static boolean looksLikeFormSubmission(ElementSnapshot target) {
        String type = target.normalizedAttributes().getOrDefault("type", "");
        String text = (target.accessibleName() + " " + target.label() + " "
                + target.normalizedAttributes().getOrDefault("value", "")).toLowerCase(Locale.ROOT);
        return "submit".equalsIgnoreCase(type)
                || text.contains("submit")
                || text.contains("pay")
                || text.contains("checkout")
                || text.contains("place order")
                || text.contains("confirm")
                || text.contains("sign in")
                || text.contains("log in")
                || text.contains("continue");
    }

    private static List<String> waitReviewWarnings(List<CaptureEvent> events) {
        List<String> warnings = new ArrayList<>();
        for (CaptureEvent event : events) {
            if (event instanceof CaptureEvent.WaitEvent value
                    && value.condition() == CaptureEvent.WaitCondition.FIXED_DURATION) {
                warnings.add(reviewWarning(
                        "WAIT",
                        "WARNING",
                        eventId(event),
                        "Generated replay uses a fixed-duration wait of " + value.timeout().toMillis() + " ms.",
                        "Replace it with a SHAFT action, wait condition, or assertion tied to page state."));
            }
        }
        return warnings;
    }

    private static List<String> testDataReviewWarnings(Map<String, ExternalTestDataReference> references) {
        List<String> warnings = new ArrayList<>();
        for (ExternalTestDataReference reference : references.values()) {
            if (reference.source() == ExternalTestDataReference.DataSource.JSON
                    && isSecret(reference)) {
                warnings.add(reviewWarning(
                        "TEST_DATA",
                        "WARNING",
                        reference.id(),
                        "Sensitive data reference " + reference.id()
                                + " is written to generated JSON test data.",
                        "Use an environment variable, secret provider, or masked fixture before committing."));
            }
        }
        return warnings;
    }

    private static List<CaptureGenerationReport.ControlFlowSuggestion> controlFlowSuggestions(
            List<CaptureEvent> events,
            List<Checkpoint> checkpoints) {
        Map<String, CaptureGenerationReport.ControlFlowSuggestion> suggestions = new LinkedHashMap<>();
        repeatedGroupSuggestion(events).ifPresent(suggestion -> suggestions.put(suggestion.id(), suggestion));
        for (CaptureEvent event : events) {
            if (optionalCloseClick(event)) {
                CaptureGenerationReport.ControlFlowSuggestion suggestion = suggestion(
                        "control-flow-optional-guard-" + event.context().sequence(),
                        CaptureGenerationReport.ControlFlowKind.OPTIONAL_GUARD,
                        List.of(eventId(event)),
                        "Optional modal or banner close action can be guarded without changing the default replay.",
                        "Review the preview, then apply it to wrap this close action in an if-displayed guard.");
                suggestions.put(suggestion.id(), suggestion);
            }
        }
        for (int index = 0; index + 1 < events.size(); index++) {
            CaptureEvent event = events.get(index);
            if (event.context().replayStatus() == EventContext.ReplayStatus.FAILED
                    || event.context().replayStatus() == EventContext.ReplayStatus.SKIPPED) {
                CaptureGenerationReport.ControlFlowSuggestion suggestion = suggestion(
                        "control-flow-recovery-" + event.context().sequence(),
                        CaptureGenerationReport.ControlFlowKind.RECOVERY_REVIEW,
                        List.of(eventId(event), eventId(events.get(index + 1))),
                        "A recorded failed or skipped action is followed by another step that may be recovery logic.",
                        "Review the follow-up step manually before turning it into generated control flow.");
                suggestions.put(suggestion.id(), suggestion);
            }
        }
        for (Checkpoint checkpoint : checkpoints) {
            if (checkpoint.kind() == Checkpoint.CheckpointKind.RECOVERY) {
                CaptureGenerationReport.ControlFlowSuggestion suggestion = suggestion(
                        "control-flow-recovery-checkpoint-" + checkpoint.id(),
                        CaptureGenerationReport.ControlFlowKind.RECOVERY_REVIEW,
                        List.of("checkpoint-" + checkpoint.id()),
                        "A RECOVERY checkpoint marks this section as manual-review control flow.",
                        "Review the marked recovery path before adding generated branching.");
                suggestions.put(suggestion.id(), suggestion);
            }
        }
        return List.copyOf(suggestions.values());
    }

    private static Optional<CaptureGenerationReport.ControlFlowSuggestion> repeatedGroupSuggestion(
            List<CaptureEvent> events) {
        int maxLength = Math.min(5, events.size() / 2);
        for (int length = maxLength; length >= 2; length--) {
            for (int start = 0; start + length * 2 <= events.size(); start++) {
                List<String> first = eventSignatures(events.subList(start, start + length));
                List<String> second = eventSignatures(events.subList(start + length, start + length * 2));
                if (first.equals(second)) {
                    List<String> evidence = events.subList(start, start + length * 2).stream()
                            .map(CaptureGenerator::eventId)
                            .toList();
                    return Optional.of(suggestion(
                            "control-flow-repeat-" + events.get(start).context().sequence() + "-" + length,
                            CaptureGenerationReport.ControlFlowKind.REPEATED_GROUP,
                            evidence,
                            "Repeated " + length + "-step action group detected with identical recorded steps.",
                            "Approve only if this should become a helper method or loop; otherwise keep replay linear."));
                }
            }
        }
        return Optional.empty();
    }

    private static List<String> eventSignatures(List<CaptureEvent> events) {
        return events.stream().map(CaptureGenerator::eventSignature).toList();
    }

    private static String eventSignature(CaptureEvent event) {
        String target = target(event).map(ElementSnapshot::logicalElementId).orElse("");
        if (event instanceof CaptureEvent.ClickEvent value) {
            return "click:" + target + ":" + value.button() + ":" + value.clickCount();
        }
        if (event instanceof CaptureEvent.TypeEvent value) {
            return "type:" + target + ":" + value.value().id();
        }
        if (event instanceof CaptureEvent.ClearEvent) {
            return "clear:" + target;
        }
        if (event instanceof CaptureEvent.SelectEvent value) {
            return "select:" + target + ":" + value.mode() + ":" + value.value().id();
        }
        if (event instanceof CaptureEvent.ToggleEvent value) {
            return "toggle:" + target + ":" + value.checked();
        }
        if (event instanceof CaptureEvent.UploadEvent value) {
            return "upload:" + target + ":" + value.file().id();
        }
        if (event instanceof CaptureEvent.KeyboardEvent value) {
            return "keyboard:" + target + ":" + String.join("+", value.keys());
        }
        if (event instanceof CaptureEvent.NavigationEvent value) {
            return "navigation:" + value.action() + ":" + value.targetUrl();
        }
        if (event instanceof CaptureEvent.WaitEvent value) {
            return "wait:" + target + ":" + value.condition();
        }
        if (event instanceof CaptureEvent.VerificationEvent value) {
            return "verification:" + target + ":" + value.verification() + ":" + value.negated();
        }
        return event.getClass().getSimpleName() + ":" + target;
    }

    private static boolean optionalCloseClick(CaptureEvent event) {
        return event instanceof CaptureEvent.ClickEvent value
                && value.button() == CaptureEvent.MouseButton.PRIMARY
                && value.clickCount() == 1
                && looksLikeOptionalClose(value.target());
    }

    private static boolean looksLikeOptionalClose(ElementSnapshot target) {
        String text = (target.logicalElementId() + " " + target.accessibleName() + " " + target.label() + " "
                + String.join(" ", target.normalizedAttributes().values())).toLowerCase(Locale.ROOT);
        boolean closeAction = text.contains("close")
                || text.contains("dismiss")
                || text.contains("accept cookie")
                || text.contains("accept all")
                || text.contains("got it")
                || text.contains("no thanks");
        boolean optionalSurface = text.contains("cookie")
                || text.contains("banner")
                || text.contains("modal")
                || text.contains("dialog")
                || text.contains("overlay")
                || text.contains("popup")
                || text.contains("toast")
                || text.contains("consent");
        return closeAction && (optionalSurface || "button".equals(target.role()));
    }

    private static CaptureGenerationReport.ControlFlowSuggestion suggestion(
            String id,
            CaptureGenerationReport.ControlFlowKind kind,
            List<String> evidenceIds,
            String summary,
            String recommendation) {
        return new CaptureGenerationReport.ControlFlowSuggestion(
                id, kind, evidenceIds, summary, recommendation, false);
    }

    private static boolean isAbsoluteXpath(String value) {
        String locator = value == null ? "" : value.trim();
        return (locator.startsWith("/") && !locator.startsWith("//")) || locator.startsWith("(/");
    }

    private static DataPlan data(
            CaptureSession session,
            Path sessionPath,
            List<String> required,
            List<String> unsupported) {
        Map<String, ExternalTestDataReference> references = new TreeMap<>();
        session.dataReferences().forEach(reference -> references.put(reference.id(), reference));
        session.events().forEach(event -> eventReferences(event)
                .forEach(reference -> references.put(reference.id(), reference)));
        Map<String, String> keys = new LinkedHashMap<>();
        Map<String, String> environment = new LinkedHashMap<>();
        Set<String> usedKeys = new HashSet<>();
        Set<String> usedEnvironmentNames = new HashSet<>();
        ObjectNode root = JSON.createObjectNode();
        root.put("schemaVersion", "1.0");
        ObjectNode values = root.putObject("values");
        Map<Path, JsonNode> sourceFiles = new HashMap<>();
        for (ExternalTestDataReference reference : references.values()) {
            String key = uniqueDataKey(reference.logicalName(), usedKeys);
            keys.put(reference.id(), key);
            if (reference.source() == ExternalTestDataReference.DataSource.JSON) {
                Path source = sessionPath.getParent().resolve(reference.relativePath()).normalize();
                try {
                    JsonNode sourceRoot = sourceFiles.computeIfAbsent(source, CaptureGenerator::readJsonUnchecked);
                    JsonNode value = sourceRoot.at(JsonPointer.compile(reference.jsonPointer()));
                    if (value.isMissingNode() || value.isNull()) {
                        required.add(reference.id() + ": provide JSON test data for " + reference.logicalName() + ".");
                    } else if (value.isObject() || value.isArray()) {
                        unsupported.add(reference.id() + ": generated test data values must be scalar.");
                    } else {
                        values.set(key, value.deepCopy());
                    }
                } catch (RuntimeException exception) {
                    required.add(reference.id() + ": source test data file is unavailable.");
                }
            } else if (reference.source() == ExternalTestDataReference.DataSource.ENVIRONMENT
                    || reference.source() == ExternalTestDataReference.DataSource.SECRET_PROVIDER) {
                String name = uniqueEnvironmentName(reference.id(), usedEnvironmentNames);
                environment.put(reference.id(), name);
                required.add(reference.id() + ": set environment variable " + name + " before replay.");
            } else if (reference.source() == ExternalTestDataReference.DataSource.FILE_FIXTURE) {
                required.add(reference.id() + ": provide fixture src/test/resources/"
                        + reference.relativePath() + " before replay.");
            }
        }
        return new DataPlan(root, Map.copyOf(keys), Map.copyOf(environment), Map.copyOf(references));
    }

    private static JsonNode readJsonUnchecked(Path path) {
        try {
            return JSON.readTree(Files.readString(path, StandardCharsets.UTF_8));
        } catch (IOException exception) {
            throw new IllegalStateException("External test data could not be read.", exception);
        }
    }

    private static String renderSource(
            CaptureSession session,
            String packageName,
            String className,
            String methodName,
            List<TargetPlan> targets,
            DataPlan data,
            Map<String, String> elementNames,
            List<CaptureEnrichmentPreview.AssertionSuggestion> extraAssertions,
            CodegenBackend backend,
            boolean fallbackLocators,
            Set<Long> optionalGuardSequences) {
        boolean fallbackReplay = fallbackLocators && hasFallbackTargets(targets);
        StringBuilder source = new StringBuilder();
        line(source, "package " + packageName + ";");
        line(source, "");
        if (backend == CodegenBackend.WEBDRIVER) {
            line(source, "import com.shaft.driver.DriverFactory;");
            line(source, "import com.shaft.driver.SHAFT;");
        } else {
            line(source, "import com.shaft.driver.SHAFT;");
        }
        if (needsByImport(targets, fallbackReplay)) {
            line(source, "import org.openqa.selenium.By;");
        }
        line(source, "import org.testng.annotations.AfterMethod;");
        line(source, "import org.testng.annotations.BeforeMethod;");
        line(source, "import org.testng.annotations.Test;");
        line(source, "");
        line(source, "import java.nio.file.Path;");
        line(source, "import java.util.HashMap;");
        line(source, "import java.util.Map;");
        line(source, "");
        line(source, "public class " + className + " {");
        line(source, "    // Capture review: readiness=" + CaptureReadiness.from(session).state()
                + ", events=" + session.events().size()
                + ", fallback locators=" + fallbackLocatorCount(targets) + ".");
        String sessionGoal = sessionGoal(session);
        if (!sessionGoal.isBlank()) {
            line(source, "    // Capture goal: " + safeComment(sessionGoal));
        }
        line(source, "    private SHAFT.GUI." + backend.driverClassName() + " driver;");
        line(source, "    private SHAFT.TestData.JSON testData;");
        line(source, "    private final Map<String, String> windows = new HashMap<>();");
        line(source, "");
        line(source, "    @BeforeMethod");
        line(source, "    public void setUp() {");
        if (backend == CodegenBackend.WEBDRIVER) {
            line(source, "        driver = new SHAFT.GUI.WebDriver(DriverFactory.DriverType."
                    + driverType(session.browser().browserName()) + ");");
        } else {
            line(source, "        SHAFT.Properties.playwright.set().browserName(\""
                    + javaString(playwrightBrowserName(session.browser().browserName())) + "\");");
            line(source, "        driver = new SHAFT.GUI.Playwright();");
        }
        line(source, "        testData = new SHAFT.TestData.JSON(\"" + javaString(dataFileName(className)) + "\");");
        if (!session.events().isEmpty()) {
            line(source, "        windows.put(\""
                    + javaString(session.events().getFirst().context().page().logicalWindowId())
                    + "\", " + currentWindowHandleExpression(backend) + ");");
        }
        line(source, "    }");
        line(source, "");
        line(source, "    @Test");
        line(source, "    public void " + methodName + "() throws Exception {");
        Map<Long, List<Checkpoint>> checkpoints = checkpoints(session.checkpoints());
        List<FlowSegment> flows = flowSegments(session.checkpoints(), session.events(), methodName);
        Map<Long, FlowSegment> flowsByFirstSequence = flowsByFirstSequence(flows);
        Set<Long> flowEventSequences = flowEventSequences(flows);
        Map<Long, List<CaptureEnrichmentPreview.AssertionSuggestion>> assertions =
                extraAssertions(extraAssertions);
        Map<Long, CaptureEvent> eventsBySequence = new HashMap<>();
        session.events().forEach(event -> eventsBySequence.put(event.context().sequence(), event));
        for (CaptureEvent event : session.events()) {
            FlowSegment flow = flowsByFirstSequence.get(event.context().sequence());
            if (flow != null) {
                line(source, "        " + flow.methodName() + "();");
                continue;
            }
            if (flowEventSequences.contains(event.context().sequence())) {
                continue;
            }
            renderEventWithAnnotations(source, event, targets, elementNames, data, backend, fallbackReplay,
                    optionalGuardSequences, checkpoints, assertions, eventsBySequence);
        }
        line(source, "    }");
        line(source, "");
        for (FlowSegment flow : flows) {
            line(source, "    private void " + flow.methodName() + "() throws Exception {");
            for (CaptureEvent event : flow.events()) {
                renderEventWithAnnotations(source, event, targets, elementNames, data, backend, fallbackReplay,
                        optionalGuardSequences, checkpoints, assertions, eventsBySequence);
            }
            line(source, "    }");
            line(source, "");
        }
        line(source, "    private String requiredData(String key) {");
        line(source, "        String value = testData.getTestData(\"values.\" + key);");
        line(source, "        if (value == null) {");
        line(source, "            throw new IllegalStateException(\"Missing generated test data: \" + key);");
        line(source, "        }");
        line(source, "        return value;");
        line(source, "    }");
        line(source, "");
        line(source, "    private String requiredEnvironment(String name) {");
        line(source, "        String value = System.getenv(name);");
        line(source, "        if (value == null || value.isBlank()) {");
        line(source, "            throw new IllegalStateException(\"Missing required environment variable: \" + name);");
        line(source, "        }");
        line(source, "        return value;");
        line(source, "    }");
        line(source, "");
        if (fallbackReplay) {
            renderFallbackHelper(source);
        }
        line(source, "    @AfterMethod(alwaysRun = true)");
        line(source, "    public void tearDown() {");
        line(source, "        if (driver != null) {");
        line(source, "            driver.quit();");
        line(source, "        }");
        line(source, "    }");
        line(source, "}");
        return source.toString();
    }

    private static boolean hasFallbackTargets(List<TargetPlan> targets) {
        return targets.stream().anyMatch(target -> !target.selection().alternatives().isEmpty());
    }

    private static long fallbackLocatorCount(List<TargetPlan> targets) {
        return targets.stream()
                .filter(target -> !target.selection().alternatives().isEmpty())
                .count();
    }

    private static String sessionGoal(CaptureSession session) {
        JsonNode value = session.extensions().get("sessionGoal");
        return value == null ? "" : value.asText("").trim();
    }

    private static void renderFallbackHelper(StringBuilder source) {
        line(source, "    private By captureReplayLocator(By primary, By... alternatives) {");
        line(source, "        if (driver.element().getElementsCount(primary) > 0) {");
        line(source, "            return primary;");
        line(source, "        }");
        line(source, "        for (By alternative : alternatives) {");
        line(source, "            if (driver.element().getElementsCount(alternative) > 0) {");
        line(source, "                return alternative;");
        line(source, "            }");
        line(source, "        }");
        line(source, "        return primary;");
        line(source, "    }");
        line(source, "");
    }

    private static void renderEventWithAnnotations(
            StringBuilder source,
            CaptureEvent event,
            List<TargetPlan> targets,
            Map<String, String> elementNames,
            DataPlan data,
            CodegenBackend backend,
            boolean fallbackReplay,
            Set<Long> optionalGuardSequences,
            Map<Long, List<Checkpoint>> checkpoints,
            Map<Long, List<CaptureEnrichmentPreview.AssertionSuggestion>> assertions,
            Map<Long, CaptureEvent> eventsBySequence) {
        String userDescription = extensionText(event.context(), "userDescription");
        if (!userDescription.isBlank()) {
            line(source, "        // Captured step: " + safeComment(userDescription));
        }
        if (optionalGuardSequences.contains(event.context().sequence())
                && event instanceof CaptureEvent.ClickEvent value) {
            renderOptionalGuardedClick(source, value, targets, fallbackReplay);
        } else {
            renderEvent(source, event, targets, elementNames, data, backend, fallbackReplay);
        }
        for (Checkpoint checkpoint : checkpoints.getOrDefault(event.context().sequence(), List.of())) {
            if (flowBoundary(checkpoint)) {
                continue;
            }
            String description = checkpoint.description().isBlank()
                    ? ""
                    : " " + safeComment(checkpoint.description());
            line(source, "        // Recorded checkpoint " + safeComment(checkpoint.id())
                    + " (" + checkpoint.kind() + ")." + description);
        }
        for (CaptureEnrichmentPreview.AssertionSuggestion assertion :
                assertions.getOrDefault(event.context().sequence(), List.of())) {
            renderSuggestedAssertion(source, eventsBySequence.get(assertion.eventSequence()),
                    assertion, targets, elementNames, backend, fallbackReplay);
        }
    }

    private static void renderEvent(
            StringBuilder source,
            CaptureEvent event,
            List<TargetPlan> targets,
            Map<String, String> elementNames,
            DataPlan data,
            CodegenBackend backend,
            boolean fallbackReplay) {
        String locator = target(event)
                .map(target -> locatorReference(target, targets, fallbackReplay))
                .orElse("");
        if (event instanceof CaptureEvent.NavigationEvent value) {
            String statement = switch (value.action()) {
                case OPEN -> "driver.browser().navigateToURL(\"" + javaString(value.targetUrl()) + "\");";
                case BACK -> "driver.browser().navigateBack();";
                case FORWARD -> "driver.browser().navigateForward();";
                case REFRESH -> "driver.browser().refreshCurrentPage();";
            };
            line(source, "        " + statement);
        } else if (event instanceof CaptureEvent.ClickEvent value) {
            line(source, "        driver.element()."
                    + (value.clickCount() == 2 ? "doubleClick" : "click")
                    + "(" + locator + ");");
        } else if (event instanceof CaptureEvent.TypeEvent value) {
            line(source, "        driver.element()."
                    + (isSecret(value.value()) ? "typeSecure" : "type")
                    + "(" + locator + ", " + dataExpression(value.value(), data) + ");");
        } else if (event instanceof CaptureEvent.ClearEvent) {
            line(source, "        driver.element().clear(" + locator + ");");
        } else if (event instanceof CaptureEvent.SelectEvent value) {
            line(source, "        driver.element().select(" + locator + ", "
                    + dataExpression(value.value(), data) + ");");
        } else if (event instanceof CaptureEvent.ToggleEvent value) {
            line(source, "        driver.element().click(" + locator + ");");
        } else if (event instanceof CaptureEvent.UploadEvent value) {
            line(source, "        driver.element().typeFileLocationForUpload(" + locator + ", "
                    + dataExpression(value.file(), data) + ");");
        } else if (event instanceof CaptureEvent.KeyboardEvent value) {
            if (value.target() == null) {
                line(source, "        // Global keyboard shortcut omitted by SHAFT-only codegen.");
            } else if (containsNonTextKey(value.keys())) {
                line(source, "        // Targeted keyboard shortcut omitted by SHAFT-only codegen.");
            } else {
                line(source, "        driver.element().typeAppend(" + locator + ", "
                        + keyboardTextExpression(value.keys()) + ");");
            }
        } else if (event instanceof CaptureEvent.WindowEvent value) {
            renderWindow(source, value, backend);
        } else if (event instanceof CaptureEvent.FrameEvent value) {
            if (value.action() == CaptureEvent.FrameAction.TOP) {
                line(source, "        driver.element().switchToDefaultContent();");
            } else if (value.action() == CaptureEvent.FrameAction.EXIT) {
                line(source, "        driver.element().switchToParentFrame();");
            } else if (value.target() != null) {
                line(source, "        driver.element().switchToIframe(" + locator + ");");
            } else {
                line(source, "        driver.element().switchToIframe(SHAFT.GUI.Locator.id(\""
                        + javaString(value.logicalFrameId()) + "\"));");
            }
        } else if (event instanceof CaptureEvent.AlertEvent value) {
            renderAlert(source, value, data, backend);
        } else if (event instanceof CaptureEvent.WaitEvent value) {
            renderWait(source, value, locator, data, backend);
        } else if (event instanceof CaptureEvent.VerificationEvent value) {
            renderVerification(source, value.verification(), value.target(), value.expected(),
                    value.negated(), value.context(), locator, data, backend);
        }
    }

    private static void renderOptionalGuardedClick(
            StringBuilder source,
            CaptureEvent.ClickEvent value,
            List<TargetPlan> targets,
            boolean fallbackReplay) {
        String locator = locatorReference(value.target(), targets, fallbackReplay);
        line(source, "        if (driver.element().getElementsCount(" + locator + ") > 0) {");
        line(source, "            driver.element()."
                + (value.clickCount() == 2 ? "doubleClick" : "click")
                + "(" + locator + ");");
        line(source, "        }");
    }

    private static void renderWindow(StringBuilder source, CaptureEvent.WindowEvent value, CodegenBackend backend) {
        switch (value.action()) {
            case OPEN_TAB, OPEN_WINDOW -> {
                line(source, "        driver.browser()."
                        + (value.action() == CaptureEvent.WindowAction.OPEN_TAB ? "openNewTab" : "openNewWindow")
                        + "(\"about:blank\");");
                line(source, "        windows.put(\"" + javaString(value.logicalWindowId())
                        + "\", " + currentWindowHandleExpression(backend) + ");");
            }
            case SWITCH -> line(source, "        driver.browser().switchToWindow(windows.get(\""
                    + javaString(value.logicalWindowId()) + "\"));");
            case CLOSE -> line(source, "        driver.browser().closeCurrentWindow();");
        }
    }

    private static void renderAlert(
            StringBuilder source,
            CaptureEvent.AlertEvent value,
            DataPlan data,
            CodegenBackend backend) {
        switch (value.action()) {
            case ACCEPT -> line(source, "        driver.browser().acceptAlert();");
            case DISMISS -> line(source, "        driver.browser().dismissAlert();");
            case TYPE -> line(source, "        driver.browser().typeIntoPromptAlert("
                    + dataExpression(value.text(), data) + ");");
            case VERIFY_TEXT -> line(source, "        driver.browser().assertThat().alertText().isEqualTo("
                    + dataExpression(value.text(), data) + ");");
        }
    }

    private static void renderWait(
            StringBuilder source,
            CaptureEvent.WaitEvent value,
            String locator,
            DataPlan data,
            CodegenBackend backend) {
        long seconds = Math.max(1, value.timeout().toSeconds());
        if (backend == CodegenBackend.PLAYWRIGHT) {
            renderPlaywrightWait(source, value, locator, data);
            return;
        }
        switch (value.condition()) {
            case ELEMENT_PRESENT -> line(source, "        driver.element().assertThat("
                    + locator + ").exists();");
            case ELEMENT_VISIBLE -> line(source, "        driver.element().assertThat("
                    + locator + ").isVisible();");
            case ELEMENT_CLICKABLE -> {
                line(source, "        driver.element().assertThat(" + locator + ").isVisible();");
                line(source, "        driver.element().assertThat(" + locator + ").isEnabled();");
            }
            case ELEMENT_ABSENT -> line(source, "        driver.element().assertThat("
                    + locator + ").doesNotExist();");
            case URL_CONTAINS -> nativeAssertion(source, "driver.browser().assertThat().url()",
                    "contains", dataExpression(value.expected(), data));
            case TITLE_CONTAINS -> nativeAssertion(source, "driver.browser().assertThat().title()",
                    "contains", dataExpression(value.expected(), data));
            case DOCUMENT_READY -> line(source, "        driver.browser().waitForLazyLoading();");
            case FIXED_DURATION -> line(source, "        driver.browser().waitForLazyLoading();");
        }
    }

    private static void renderPlaywrightWait(
            StringBuilder source,
            CaptureEvent.WaitEvent value,
            String locator,
            DataPlan data) {
        switch (value.condition()) {
            case ELEMENT_PRESENT, ELEMENT_VISIBLE, ELEMENT_CLICKABLE ->
                    line(source, "        driver.element().assertThat(" + locator + ").isVisible();");
            case ELEMENT_ABSENT ->
                    line(source, "        driver.element().assertThat(" + locator + ").doesNotExist();");
            case URL_CONTAINS -> nativeAssertion(source, "driver.browser().assertThat().url()",
                    "contains", dataExpression(value.expected(), data));
            case TITLE_CONTAINS -> nativeAssertion(source, "driver.browser().assertThat().title()",
                    "contains", dataExpression(value.expected(), data));
            case DOCUMENT_READY -> line(source, "        driver.browser().waitForLazyLoading();");
            case FIXED_DURATION -> line(source, "        driver.browser().waitForLazyLoading();");
        }
    }

    private static void renderVerification(
            StringBuilder source,
            CaptureEvent.VerificationKind verification,
            ElementSnapshot target,
            ExternalTestDataReference expected,
            boolean negated,
            EventContext context,
            String locator,
            DataPlan data,
            CodegenBackend backend) {
        switch (verification) {
            case ELEMENT_PRESENT -> line(source, "        driver.element().assertThat(" + locator + ")."
                    + (negated ? "doesNotExist" : "exists") + "();");
            case ELEMENT_VISIBLE -> {
                if (negated) {
                    line(source, "        driver.element().assertThat(" + locator + ").isHidden();");
                } else {
                    line(source, "        driver.element().assertThat(" + locator + ").isVisible();");
                }
            }
            case ELEMENT_ENABLED -> line(source, "        driver.element().assertThat(" + locator + ")."
                    + (negated ? "isDisabled" : "isEnabled") + "();");
            case ELEMENT_SELECTED -> line(source, "        driver.element().assertThat(" + locator + ")."
                    + (negated ? "isNotSelected" : "isSelected") + "();");
            case TEXT_EQUALS -> nativeAssertion(source,
                    "driver.element().assertThat(" + locator + ").text()",
                    negated ? "doesNotEqual" : "isEqualTo", dataExpression(expected, data));
            case TEXT_CONTAINS -> nativeAssertion(source,
                    "driver.element().assertThat(" + locator + ").text()",
                    negated ? "doesNotContain" : "contains", dataExpression(expected, data));
            case ATTRIBUTE_EQUALS -> nativeAssertion(source,
                    "driver.element().assertThat(" + locator + ").attribute(\""
                            + javaString(extensionText(context, "attributeName")) + "\")",
                    negated ? "doesNotEqual" : "isEqualTo", dataExpression(expected, data));
            case URL_EQUALS -> nativeAssertion(source, "driver.browser().assertThat().url()",
                    negated ? "doesNotEqual" : "isEqualTo", dataExpression(expected, data));
            case URL_CONTAINS -> nativeAssertion(source, "driver.browser().assertThat().url()",
                    negated ? "doesNotContain" : "contains", dataExpression(expected, data));
            case TITLE_EQUALS -> nativeAssertion(source, "driver.browser().assertThat().title()",
                    negated ? "doesNotEqual" : "isEqualTo", dataExpression(expected, data));
            case TITLE_CONTAINS -> nativeAssertion(source, "driver.browser().assertThat().title()",
                    negated ? "doesNotContain" : "contains", dataExpression(expected, data));
            case PAGE_TEXT_CONTAINS -> nativeAssertion(source, "driver.browser().assertThat().text()",
                    negated ? "doesNotContain" : "contains", dataExpression(expected, data));
            case ELEMENT_IMAGE_MATCHES -> line(source, "        driver.element().assertThat(" + locator + ")."
                    + (negated ? "doesNotMatchReferenceImage" : "matchesReferenceImage") + "();");
        }
    }

    private static void nativeAssertion(
            StringBuilder source,
            String builder,
            String comparison,
            String expected) {
        line(source, "        " + builder + "." + comparison + "(" + expected + ");");
    }

    private static void renderSuggestedAssertion(
            StringBuilder source,
            CaptureEvent event,
            CaptureEnrichmentPreview.AssertionSuggestion suggestion,
            List<TargetPlan> targets,
            Map<String, String> elementNames,
            CodegenBackend backend,
            boolean fallbackReplay) {
        Optional<ElementSnapshot> target = target(event);
        if (target.isEmpty()) {
            return;
        }
        CaptureEvent.VerificationKind verification =
                CaptureEvent.VerificationKind.valueOf(suggestion.verification());
        String locator = locatorReference(target.get(), targets, fallbackReplay);
        renderVerification(source, verification, target.get(), null, suggestion.negated(),
                event.context(), locator, null, backend);
    }

    private static String locatorExpression(TargetPlan plan) {
        LocatorCandidate candidate = plan.selection().selected().candidate();
        return locatorExpression(plan.target(), candidate);
    }

    private static String locatorExpression(ElementSnapshot target, LocatorCandidate candidate) {
        String name = !target.accessibleName().isBlank() ? target.accessibleName() : target.label();
        return switch (candidate.strategy()) {
            case ROLE, ACCESSIBLE_NAME, LABEL -> semanticLocator(target, name, candidate);
            case TEST_ID, CSS -> "SHAFT.GUI.Locator.cssSelector(\"" + javaString(candidate.expression()) + "\")";
            case ID -> "SHAFT.GUI.Locator.id(\"" + javaString(candidate.expression()) + "\")";
            case NAME -> "SHAFT.GUI.Locator.name(\"" + javaString(candidate.expression()) + "\")";
            case XPATH -> "By.xpath(\"" + javaString(candidate.expression()) + "\")";
        };
    }

    private static boolean needsByImport(List<TargetPlan> targets, boolean fallbackReplay) {
        return fallbackReplay || targets.stream().anyMatch(CaptureGenerator::usesNativeBy);
    }

    private static boolean usesNativeBy(TargetPlan plan) {
        LocatorCandidate candidate = plan.selection().selected().candidate();
        if (candidate.strategy() == LocatorCandidate.LocatorStrategy.XPATH) {
            return true;
        }
        if (candidate.strategy() == LocatorCandidate.LocatorStrategy.ROLE
                || candidate.strategy() == LocatorCandidate.LocatorStrategy.ACCESSIBLE_NAME
                || candidate.strategy() == LocatorCandidate.LocatorStrategy.LABEL) {
            String name = !plan.target().accessibleName().isBlank()
                    ? plan.target().accessibleName()
                    : plan.target().label();
            return semanticName(name, candidate).isBlank() && !isInput(plan.target()) && !isClickable(plan.target());
        }
        return false;
    }

    private static String locatorReference(
            ElementSnapshot target,
            List<TargetPlan> targets,
            boolean fallbackReplay) {
        return targets.stream()
                .filter(plan -> plan.logicalElementId().equals(target.logicalElementId()))
                .findFirst()
                .map(plan -> locatorExpression(plan, fallbackReplay))
                .orElseGet(() -> target.locatorCandidates().isEmpty()
                        ? "By.xpath(\"//*\")"
                        : locatorExpression(target, target.locatorCandidates().getFirst()));
    }

    private static String locatorExpression(TargetPlan plan, boolean fallbackReplay) {
        String primary = locatorExpression(plan);
        if (!fallbackReplay || plan.selection().alternatives().isEmpty()) {
            return primary;
        }
        String alternatives = plan.selection().alternatives().stream()
                .map(alternative -> locatorExpression(plan.target(), alternative.candidate()))
                .reduce((left, right) -> left + ", " + right)
                .orElse("");
        if (alternatives.isBlank()) {
            return primary;
        }
        return "captureReplayLocator(" + primary + ", " + alternatives + ")";
    }

    private static String semanticLocator(
            ElementSnapshot target,
            String name,
            LocatorCandidate candidate) {
        String semanticName = name;
        semanticName = semanticName(semanticName, candidate);
        if (isInput(target)) {
            return "SHAFT.GUI.Locator.inputField(\"" + javaString(semanticName) + "\")";
        }
        if (isClickable(target)) {
            return "SHAFT.GUI.Locator.clickableField(\"" + javaString(semanticName) + "\")";
        }
        if (!semanticName.isBlank()) {
            return "SHAFT.GUI.Locator.hasAnyTagName().containsText(\"" + javaString(semanticName) + "\").build()";
        }
        return "By.xpath(\"" + javaString(candidate.expression()) + "\")";
    }

    private static String semanticName(String name, LocatorCandidate candidate) {
        if (candidate.strategy() == LocatorCandidate.LocatorStrategy.ROLE
                && candidate.expression().contains(":")) {
            return candidate.expression().substring(candidate.expression().indexOf(':') + 1);
        }
        return name.isBlank() ? candidate.expression() : name;
    }

    private static String dataExpression(ExternalTestDataReference reference, DataPlan data) {
        if (reference == null) {
            return "\"\"";
        }
        return switch (reference.source()) {
            case JSON -> "requiredData(\"" + javaString(data.keys().get(reference.id())) + "\")";
            case ENVIRONMENT, SECRET_PROVIDER -> "requiredEnvironment(\""
                    + javaString(data.environment().get(reference.id())) + "\")";
            case FILE_FIXTURE -> "Path.of(\"src/test/resources/"
                    + javaString(reference.relativePath()) + "\").toString()";
        };
    }

    private static String keyboardTextExpression(List<String> values) {
        StringBuilder text = new StringBuilder();
        for (String value : values) {
            text.append(keyboardText(value));
        }
        return "\"" + javaString(text.toString()) + "\"";
    }

    private static boolean containsNonTextKey(List<String> values) {
        return values.stream().anyMatch(CaptureGenerator::nonTextKey);
    }

    private static boolean nonTextKey(String value) {
        String normalized = value == null ? "" : value.trim().toUpperCase(Locale.ROOT);
        return Set.of("NULL", "CANCEL", "HELP", "BACK_SPACE", "CLEAR", "SHIFT", "CONTROL", "ALT",
                "PAUSE", "ESCAPE", "PAGE_UP", "PAGE_DOWN", "END", "HOME", "ARROW_LEFT", "LEFT",
                "ARROW_UP", "UP", "ARROW_RIGHT", "RIGHT", "ARROW_DOWN", "DOWN", "INSERT", "DELETE",
                "NUMPAD0", "NUMPAD1", "NUMPAD2", "NUMPAD3", "NUMPAD4", "NUMPAD5", "NUMPAD6",
                "NUMPAD7", "NUMPAD8", "NUMPAD9", "F1", "F2", "F3", "F4", "F5", "F6", "F7",
                "F8", "F9", "F10", "F11", "F12", "META", "COMMAND", "ZENKAKU_HANKAKU")
                .contains(normalized);
    }

    private static String keyboardText(String value) {
        String normalized = value == null ? "" : value.trim().toUpperCase(Locale.ROOT);
        return switch (normalized) {
            case "ENTER", "RETURN" -> "\n";
            case "TAB" -> "\t";
            case "SPACE" -> " ";
            default -> value == null ? "" : value;
        };
    }

    private static Map<String, String> defaultElementNames(List<TargetPlan> targets) {
        Map<String, String> names = new LinkedHashMap<>();
        Set<String> used = new HashSet<>();
        for (TargetPlan target : targets) {
            String base = constantName(target.logicalElementId()) + "_LOCATOR";
            String name = base;
            int suffix = 2;
            while (!used.add(name)) {
                name = base + "_" + suffix++;
            }
            names.put(target.logicalElementId(), name);
        }
        return Map.copyOf(names);
    }

    private static final Set<String> RESERVED_TEST_METHOD_NAMES = Set.of(
            "setUp", "tearDown", "requiredData", "requiredEnvironment", "captureReplayLocator");

    private static String defaultClassName(CaptureSession session) {
        return javaClassName(defaultNameSeed(session)) + "Test";
    }

    private static String defaultMethodName(CaptureSession session) {
        String goal = sessionGoal(session);
        if (!goal.isBlank()) {
            String intentName = javaMethodName(goal);
            if (!RESERVED_TEST_METHOD_NAMES.contains(intentName)) {
                return intentName;
            }
        }
        return "replay" + javaClassName(defaultNameSeed(session));
    }

    private static String defaultNameSeed(CaptureSession session) {
        String goal = sessionGoal(session);
        if (!goal.isBlank()) {
            return goal;
        }
        for (Checkpoint checkpoint : session.checkpoints()) {
            if (!checkpoint.description().isBlank()) {
                return checkpoint.description();
            }
        }
        for (CaptureEvent event : session.events()) {
            String title = event.context().page().title();
            if (!title.isBlank()) {
                return title;
            }
            String urlSeed = urlNameSeed(event.context().page().url());
            if (!urlSeed.isBlank()) {
                return urlSeed;
            }
        }
        return session.sessionId();
    }

    private static String urlNameSeed(String url) {
        if (url == null || url.isBlank()) {
            return "";
        }
        String path = url.replaceFirst("^[a-zA-Z][a-zA-Z0-9+.-]*://[^/]+/?", "");
        path = path.replaceFirst("[?#].*$", "").replace('/', ' ').trim();
        return path.isBlank() ? "" : path;
    }

    private static Map<String, String> mergeElementNames(
            Map<String, String> deterministic,
            Map<String, String> proposed) {
        Map<String, String> result = new LinkedHashMap<>(deterministic);
        proposed.forEach((key, value) -> {
            if (result.containsKey(key) && value != null && !value.isBlank()) {
                result.put(key, value);
            }
        });
        return Map.copyOf(result);
    }

    private static List<String> validateProposal(
            CaptureEnrichmentPreview preview,
            String fingerprint,
            List<TargetPlan> targets) {
        List<String> errors = new ArrayList<>();
        if (!CaptureEnrichmentPreview.CURRENT_SCHEMA_VERSION.equals(preview.schemaVersion())) {
            errors.add("AI enrichment preview uses an unsupported schema version.");
        }
        if (!fingerprint.equals(preview.deterministicFingerprint())) {
            errors.add("AI enrichment preview does not match the deterministic source.");
        }
        validateOptionalIdentifier(preview.proposal().className(), "AI class name", errors);
        validateOptionalIdentifier(preview.proposal().methodName(), "AI method name", errors);
        Map<String, TargetPlan> byId = targets.stream()
                .collect(java.util.stream.Collectors.toMap(TargetPlan::logicalElementId, item -> item));
        Set<String> names = new HashSet<>();
        preview.proposal().elementNames().forEach((id, name) -> {
            if (!byId.containsKey(id)) {
                errors.add("AI element name references unknown target " + id + ".");
            }
            if (!JAVA_IDENTIFIER.matcher(name).matches()) {
                errors.add("AI element name for " + id + " is not a Java identifier.");
            }
            if (!names.add(name)) {
                errors.add("AI element names must be unique.");
            }
        });
        Map<Long, TargetPlan> bySequence = new HashMap<>();
        for (TargetPlan target : targets) {
            for (String eventId : target.eventIds()) {
                bySequence.put(Long.parseLong(eventId.substring("event-".length())), target);
            }
        }
        for (CaptureEnrichmentPreview.AssertionSuggestion assertion : preview.proposal().assertions()) {
            TargetPlan target = bySequence.get(assertion.eventSequence());
            if (target == null) {
                errors.add("AI assertion references an event without captured target state: event-"
                        + assertion.eventSequence() + ".");
                continue;
            }
            boolean observed = switch (assertion.verification()) {
                case "ELEMENT_PRESENT" -> true;
                case "ELEMENT_VISIBLE" -> target.target().visible();
                case "ELEMENT_ENABLED" -> target.target().enabled();
                case "ELEMENT_SELECTED" -> target.target().selected();
                default -> false;
            };
            if (observed == assertion.negated()) {
                errors.add("AI assertion contradicts captured state at event-"
                        + assertion.eventSequence() + ".");
            }
        }
        return List.copyOf(errors);
    }

    private static CaptureEnrichmentPreview readPreview(Path path) {
        try {
            return JSON.readValue(Files.readString(path, StandardCharsets.UTF_8),
                    CaptureEnrichmentPreview.class);
        } catch (IOException exception) {
            throw new IllegalStateException("AI enrichment preview could not be read.", exception);
        }
    }

    private static void writeControlFlowPreview(
            Path path,
            CaptureControlFlowPreview preview,
            boolean overwrite) {
        ensureWritable(path, overwrite);
        atomicWrite(path, writeJson(preview));
    }

    private static CaptureControlFlowPreview readControlFlowPreview(Path path) {
        try {
            return JSON.readValue(Files.readString(path, StandardCharsets.UTF_8),
                    CaptureControlFlowPreview.class);
        } catch (IOException exception) {
            throw new IllegalStateException("Control-flow preview could not be read.", exception);
        }
    }

    private static List<String> validateControlFlowPreview(
            CaptureControlFlowPreview preview,
            String sessionId,
            String fingerprint,
            List<CaptureGenerationReport.ControlFlowSuggestion> current) {
        List<String> errors = new ArrayList<>();
        if (!CaptureControlFlowPreview.CURRENT_SCHEMA_VERSION.equals(preview.schemaVersion())) {
            errors.add("Control-flow preview uses an unsupported schema version.");
        }
        if (!sessionId.equals(preview.sessionId())) {
            errors.add("Control-flow preview does not match the Capture session.");
        }
        if (!fingerprint.equals(preview.deterministicFingerprint())) {
            errors.add("Control-flow preview does not match the deterministic source.");
        }
        Set<String> currentIds = current.stream()
                .map(CaptureGenerationReport.ControlFlowSuggestion::id)
                .collect(java.util.stream.Collectors.toSet());
        for (CaptureGenerationReport.ControlFlowSuggestion suggestion : preview.suggestions()) {
            if (!currentIds.contains(suggestion.id())) {
                errors.add("Control-flow preview references unknown suggestion " + suggestion.id() + ".");
            }
        }
        return List.copyOf(errors);
    }

    private static Set<Long> approvedOptionalGuardSequences(CaptureControlFlowPreview preview) {
        Set<Long> sequences = new HashSet<>();
        for (CaptureGenerationReport.ControlFlowSuggestion suggestion : preview.suggestions()) {
            if (suggestion.kind() == CaptureGenerationReport.ControlFlowKind.OPTIONAL_GUARD) {
                suggestion.evidenceIds().stream()
                        .filter(id -> id.startsWith("event-"))
                        .map(id -> id.substring("event-".length()))
                        .map(Long::parseLong)
                        .forEach(sequences::add);
            }
        }
        return Set.copyOf(sequences);
    }

    private static List<CaptureGenerationReport.ControlFlowSuggestion> appliedControlFlow(
            List<CaptureGenerationReport.ControlFlowSuggestion> suggestions,
            Set<Long> appliedSequences) {
        List<CaptureGenerationReport.ControlFlowSuggestion> result = new ArrayList<>();
        for (CaptureGenerationReport.ControlFlowSuggestion suggestion : suggestions) {
            boolean applied = suggestion.kind() == CaptureGenerationReport.ControlFlowKind.OPTIONAL_GUARD
                    && suggestion.evidenceIds().stream()
                    .filter(id -> id.startsWith("event-"))
                    .map(id -> id.substring("event-".length()))
                    .map(Long::parseLong)
                    .anyMatch(appliedSequences::contains);
            result.add(new CaptureGenerationReport.ControlFlowSuggestion(
                    suggestion.id(), suggestion.kind(), suggestion.evidenceIds(),
                    suggestion.summary(), suggestion.recommendation(), applied));
        }
        return List.copyOf(result);
    }

    private static CaptureGenerationReport report(
            CaptureSession session,
            ArtifactPaths paths,
            GenerationState state,
            CaptureGenerationReport.Status status,
            CaptureGenerationReport.Validation compilation,
            CaptureGenerationReport.Validation replay,
            CaptureGenerationReport.Enrichment enrichment) {
        return report(session == null ? "" : session.sessionId(), paths, state, status,
                compilation, replay, enrichment);
    }

    private static CaptureGenerationReport report(
            String sessionId,
            ArtifactPaths paths,
            GenerationState state,
            CaptureGenerationReport.Status status,
            CaptureGenerationReport.Validation compilation,
            CaptureGenerationReport.Validation replay,
            CaptureGenerationReport.Enrichment enrichment) {
        List<CaptureGenerationReport.LocatorDecision> decisions = state.targets().stream()
                .map(target -> new CaptureGenerationReport.LocatorDecision(
                        target.eventIds(),
                        target.logicalElementId(),
                        target.selection().selected().candidate().strategy().name(),
                        target.selection().selected().candidate().expression(),
                        target.selection().selected().score(),
                        target.selection().selected().breakdown(),
                        target.selection().alternatives().stream()
                                .map(item -> item.candidate().strategy() + " "
                                        + item.candidate().expression() + " (score " + item.score() + ")")
                                .toList()))
                .toList();
        CaptureReadiness readiness = reportReadiness(state);
        return new CaptureGenerationReport(
                CaptureGenerationReport.CURRENT_SCHEMA_VERSION,
                sessionId,
                status,
                relative(paths.root(), paths.source()),
                relative(paths.root(), paths.data()),
                readiness.state(),
                readiness.warnings(),
                decisions,
                state.unsupported().stream().distinct().sorted().toList(),
                state.flaky().stream().distinct().sorted().toList(),
                state.fallback().stream().distinct().sorted().toList(),
                state.controlFlow().stream()
                        .sorted(Comparator.comparing(CaptureGenerationReport.ControlFlowSuggestion::id))
                        .toList(),
                state.required().stream().distinct().sorted().toList(),
                reportWarnings(paths, state, replay),
                compilation,
                replay,
                enrichment);
    }

    private static List<String> reportWarnings(
            ArtifactPaths paths,
            GenerationState state,
            CaptureGenerationReport.Validation replay) {
        List<String> warnings = new ArrayList<>(state.warnings());
        warnings.addAll(controlFlowReviewWarnings(state.controlFlow()));
        warnings.addAll(replayReviewWarnings(paths.root(), paths.source(), replay));
        return warnings.stream().distinct().sorted().toList();
    }

    private static List<String> controlFlowReviewWarnings(
            List<CaptureGenerationReport.ControlFlowSuggestion> suggestions) {
        List<String> warnings = new ArrayList<>();
        for (CaptureGenerationReport.ControlFlowSuggestion suggestion : suggestions) {
            String severity = suggestion.kind() == CaptureGenerationReport.ControlFlowKind.RECOVERY_REVIEW
                    ? "WARNING"
                    : "INFO";
            warnings.add(reviewWarning(
                    "CONTROL_FLOW",
                    severity,
                    String.join(",", suggestion.evidenceIds()),
                    suggestion.summary(),
                    suggestion.recommendation()));
        }
        return warnings;
    }

    private static CaptureReadiness reportReadiness(GenerationState state) {
        List<String> warnings = new ArrayList<>(state.readiness().warnings());
        warnings.addAll(state.unsupported());
        warnings.addAll(state.required());
        warnings.addAll(state.flaky());
        warnings.addAll(state.fallback());
        CaptureReadiness.State stateValue = state.unsupported().isEmpty() && state.required().isEmpty()
                ? state.readiness().state()
                : CaptureReadiness.State.BLOCKED;
        if (stateValue == CaptureReadiness.State.READY && !warnings.isEmpty()) {
            stateValue = CaptureReadiness.State.RISKY;
        }
        return new CaptureReadiness(stateValue, warnings.stream().distinct().sorted().toList());
    }

    /**
     * Overwrite protection applies to user-owned generated code artifacts only. Status artifacts
     * (generation report, review, workbench) describe the current attempt and are always
     * refreshed; protecting them made every retry after a failed attempt fail on the failed
     * attempt's own leftover report.
     */
    private static void validateOutputs(
            ArtifactPaths paths,
            boolean overwrite,
            List<String> unsupported) {
        for (Path path : List.of(paths.source(), paths.data())) {
            if (Files.exists(path) && !overwrite) {
                unsupported.add("output: " + relative(paths.root(), path)
                        + " already exists. Supply an explicit output directory and overwrite approval.");
            }
        }
    }

    private static void ensureWritable(Path path, boolean overwrite) {
        if (Files.exists(path) && !overwrite) {
            throw new IllegalStateException("Enrichment preview already exists and overwrite was not approved.");
        }
    }

    private static void writeReportIfPossible(Path reportPath, CaptureGenerationReport report) {
        try {
            atomicWrite(reportPath, writeJson(report));
        } catch (RuntimeException ignored) {
            // The returned in-memory report remains available when the filesystem is not writable.
        }
    }

    private static CaptureGenerationResult result(
            Path sourcePath,
            Path testDataPath,
            Path reportPath,
            Path enrichmentPreviewPath,
            CaptureGenerationReport report) {
        CaptureGenerationResult result = new CaptureGenerationResult(
                sourcePath, testDataPath, reportPath, enrichmentPreviewPath, report);
        CaptureReview review = review(report);
        writeReviewIfPossible(result.reviewPath(), review);
        writeWorkbenchIfPossible(result.reviewUiPath(), sourcePath, report, review);
        return result;
    }

    private static void writeReviewIfPossible(Path reviewPath, CaptureReview review) {
        try {
            atomicWrite(reviewPath, writeJson(review));
        } catch (RuntimeException ignored) {
            // Review output is additive; generation result remains authoritative.
        }
    }

    private static void writeWorkbenchIfPossible(
            Path workbenchPath,
            Path sourcePath,
            CaptureGenerationReport report,
            CaptureReview review) {
        try {
            CaptureWorkbenchHtml.write(workbenchPath, sourcePath, report, review, true);
        } catch (RuntimeException ignored) {
            // Workbench output is additive; generation result remains authoritative.
        }
    }

    private static CaptureReview review(CaptureGenerationReport report) {
        List<String> blockers = new ArrayList<>();
        blockers.addAll(report.unsupportedEvents());
        blockers.addAll(report.requiredUserInputs());
        addValidationBlocker(blockers, "compilation", report.compilation());
        addValidationBlocker(blockers, "replay", report.replay());

        List<String> risks = new ArrayList<>();
        risks.addAll(report.flakySteps());
        risks.addAll(report.fallbackLocators());
        risks.addAll(report.warnings());
        List<CaptureReviewFinding> findings = reviewFindings(report.warnings());

        List<String> suggestions = new ArrayList<>();
        if (!report.unsupportedEvents().isEmpty()) {
            suggestions.add("Re-record unsupported events before committing generated source.");
        }
        if (!report.requiredUserInputs().isEmpty()) {
            suggestions.add("Provide the listed external inputs before replay.");
        }
        if (!report.flakySteps().isEmpty() || !report.fallbackLocators().isEmpty()) {
            suggestions.add("Review weak locator and replay-risk diagnostics.");
        }
        if (!report.controlFlowSuggestions().isEmpty()) {
            suggestions.add("Review deterministic control-flow suggestions before applying optional guards.");
        }
        if (!findings.isEmpty()) {
            suggestions.add("Resolve deterministic generated-code review findings before committing the test.");
        }
        if (suggestions.isEmpty()) {
            suggestions.add("Generated test is ready for review.");
        }

        int score = 100
                - blockers.size() * 25
                - report.flakySteps().size() * 10
                - report.fallbackLocators().size() * 5
                - report.controlFlowSuggestions().size() * 3
                - report.warnings().size() * 5
                - findings.size() * 5;
        return new CaptureReview(
                CaptureReview.CURRENT_SCHEMA_VERSION,
                report.sessionId(),
                score,
                blockers.stream().distinct().sorted().toList(),
                risks.stream().distinct().sorted().toList(),
                findings,
                suggestions,
                report.enrichment().provider());
    }

    private static void addValidationBlocker(
            List<String> blockers,
            String label,
            CaptureGenerationReport.Validation validation) {
        if (validation.status() == CaptureGenerationReport.Validation.ValidationStatus.FAILED) {
            blockers.add(label + ": " + String.join("; ", validation.diagnostics()));
        }
    }

    private static List<String> replayReviewWarnings(
            Path outputRoot,
            Path sourcePath,
            CaptureGenerationReport.Validation replay) {
        if (replay.status() != CaptureGenerationReport.Validation.ValidationStatus.FAILED) {
            return List.of();
        }
        Path traceRoot = outputRoot.resolve("target/shaft-traces").toAbsolutePath().normalize();
        if (!Files.isDirectory(traceRoot)) {
            return List.of();
        }
        try (java.util.stream.Stream<Path> paths = Files.walk(traceRoot, 4)) {
            List<String> warnings = new ArrayList<>();
            for (Path trace : paths
                    .filter(Files::isRegularFile)
                    .filter(path -> path.getFileName() != null
                            && "shaft-trace.json".equals(path.getFileName().toString()))
                    .sorted(Comparator.comparing(Path::toString))
                    .toList()) {
                warnings.addAll(traceReviewWarnings(sourcePath, trace));
            }
            return warnings.stream().distinct().toList();
        } catch (IOException exception) {
            return List.of();
        }
    }

    private static List<String> traceReviewWarnings(Path sourcePath, Path tracePath) {
        try {
            JsonNode trace = JSON.readTree(Files.readString(tracePath, StandardCharsets.UTF_8));
            List<String> warnings = new ArrayList<>();
            failedAction(trace.path("actions")).ifPresent(action -> warnings.add(reviewWarning(
                    "REPLAY_TRACE",
                    "ERROR",
                    "trace action " + text(action.path("id")),
                    "Replay failure maps to generated step " + sourceStep(sourcePath, trace.path("source"))
                            + " and trace action " + text(action.path("id")) + " "
                            + text(action.path("name")) + " using " + text(action.path("locator")) + ".",
                    "Inspect the mapped generated step and trace action before committing the replay.")));
            warnings.addAll(networkDependencyWarnings(trace.path("network")));
            return warnings;
        } catch (IOException | RuntimeException exception) {
            return List.of();
        }
    }

    private static Optional<JsonNode> failedAction(JsonNode actions) {
        if (!actions.isArray()) {
            return Optional.empty();
        }
        for (JsonNode action : actions) {
            if ("failed".equalsIgnoreCase(text(action.path("status")))) {
                return Optional.of(action);
            }
        }
        return Optional.empty();
    }

    private static List<String> networkDependencyWarnings(JsonNode network) {
        if (!network.isArray()) {
            return List.of();
        }
        List<String> warnings = new ArrayList<>();
        for (JsonNode entry : network) {
            int status = entry.path("status").asInt(-1);
            String failure = text(entry.path("failureReason"));
            if (status >= 400 || status <= 0 || !failure.isBlank()) {
                String detail = (text(entry.path("method")) + " " + status + " " + text(entry.path("url"))
                        + (failure.isBlank() ? "" : " " + failure)).trim();
                warnings.add(reviewWarning(
                        "NETWORK_DEPENDENCY",
                        "WARNING",
                        "network",
                        "Network/API dependency failed during replay: " + detail + ".",
                        "Record/replay this dependency through #3065 HTTP contract replay."));
            }
        }
        return warnings;
    }

    private static String sourceStep(Path sourcePath, JsonNode source) {
        String lineText = text(source.path("line"));
        String snippet = text(source.path("snippet"));
        if (!lineText.isBlank() && Files.isRegularFile(sourcePath)) {
            try {
                int line = Integer.parseInt(lineText);
                List<String> lines = Files.readAllLines(sourcePath, StandardCharsets.UTF_8);
                if (line > 0 && line <= lines.size()) {
                    snippet = lines.get(line - 1).trim();
                }
            } catch (IOException | NumberFormatException ignored) {
                // Trace source snippets are best-effort.
            }
        }
        return lineText.isBlank()
                ? safeReviewText(snippet)
                : "line " + lineText + (snippet.isBlank() ? "" : " (" + safeReviewText(snippet) + ")");
    }

    private static List<CaptureReviewFinding> reviewFindings(List<String> warnings) {
        List<CaptureReviewFinding> findings = new ArrayList<>();
        for (String warning : warnings) {
            if (warning == null || !warning.startsWith("review/")) {
                continue;
            }
            int space = warning.indexOf(' ');
            if (space < 0) {
                continue;
            }
            String[] header = warning.substring("review/".length(), space).split("/");
            String category = header.length > 0 ? header[0] : "";
            String severity = header.length > 1 ? header[1] : "WARNING";
            String body = warning.substring(space + 1);
            int colon = body.indexOf(':');
            if (category.isBlank() || colon < 0) {
                continue;
            }
            String evidence = body.substring(0, colon).trim();
            String summary = body.substring(colon + 1).trim();
            String recommendation = "";
            int recommendationIndex = summary.indexOf(" Recommendation: ");
            if (recommendationIndex >= 0) {
                recommendation = summary.substring(recommendationIndex + " Recommendation: ".length()).trim();
                summary = summary.substring(0, recommendationIndex).trim();
            }
            findings.add(new CaptureReviewFinding(
                    category.toLowerCase(Locale.ROOT) + "-" + (findings.size() + 1),
                    category,
                    severity,
                    summary,
                    evidenceIds(evidence),
                    recommendation));
        }
        return List.copyOf(findings);
    }

    private static List<String> evidenceIds(String evidence) {
        List<String> ids = new ArrayList<>();
        Matcher matcher = EVIDENCE_ID.matcher(evidence == null ? "" : evidence);
        while (matcher.find()) {
            ids.add(matcher.group());
        }
        return ids.stream().distinct().toList();
    }

    private static String semanticLocatorRecommendation(ElementSnapshot target) {
        String semantic = target.accessibleName().isBlank() ? target.label() : target.accessibleName();
        if (!semantic.isBlank()) {
            return "Prefer semantic locator text \"" + safeReviewText(semantic) + "\" when unique.";
        }
        if (target.normalizedAttributes().containsKey("data-testid")) {
            return "Prefer the captured data-testid locator when unique.";
        }
        return "Prefer role, label, test-id, or stable CSS locator evidence.";
    }

    private static String reviewWarning(
            String category,
            String severity,
            String evidence,
            String summary,
            String recommendation) {
        return "review/" + safeReviewText(category).toUpperCase(Locale.ROOT)
                + "/" + safeReviewText(severity).toUpperCase(Locale.ROOT)
                + " " + safeReviewText(evidence)
                + ": " + safeReviewText(summary)
                + " Recommendation: " + safeReviewText(recommendation);
    }

    private static String text(JsonNode node) {
        return node == null || node.isMissingNode() || node.isNull() ? "" : node.asText("");
    }

    private static String safeReviewText(String value) {
        String normalized = value == null ? "" : value.replaceAll("\\s+", " ").trim();
        return normalized.length() <= 240 ? normalized : normalized.substring(0, 237) + "...";
    }

    /**
     * The user's working area for one generation spans the recording and the output directory:
     * output directories are normally subfolders like {@code generated-tests}, so recorded
     * {@code file://} fixture URLs elsewhere in the same project must not read as privacy leaks.
     * The root never widens to the user home (or above): there the strict output-root behavior
     * remains so genuinely personal paths keep blocking generation.
     *
     * @param sessionPath normalized recording path
     * @param outputRoot normalized generation output root
     * @return allowed root for personal-path privacy findings
     */
    static Path privacyAllowedRoot(Path sessionPath, Path outputRoot) {
        Path sessionDirectory = sessionPath.getParent();
        if (sessionDirectory == null) {
            return outputRoot;
        }
        Path common = commonAncestor(sessionDirectory, outputRoot);
        if (common == null || common.getNameCount() == 0) {
            return outputRoot;
        }
        Path home = Path.of(System.getProperty("user.home", "")).toAbsolutePath().normalize();
        if (home.startsWith(common)) {
            return outputRoot;
        }
        return common;
    }

    private static Path commonAncestor(Path first, Path second) {
        Path candidate = first.toAbsolutePath().normalize();
        Path target = second.toAbsolutePath().normalize();
        while (candidate != null && !target.startsWith(candidate)) {
            candidate = candidate.getParent();
        }
        return candidate;
    }

    private static List<String> privacyFindings(String content, Path allowedRoot) {
        List<String> findings = new ArrayList<>();
        if (content == null || content.isBlank()) {
            return findings;
        }
        addPersonalPathFinding(findings, WINDOWS_PERSONAL_PATH, content, allowedRoot,
                "privacy: generated artifacts contain an absolute personal Windows path.");
        addPersonalPathFinding(findings, UNIX_PERSONAL_PATH, content, allowedRoot,
                "privacy: generated artifacts contain an absolute personal POSIX path.");
        addFinding(findings, BEARER_SECRET, content,
                "privacy: generated artifacts contain a bearer credential.");
        addFinding(findings, JWT_SECRET, content,
                "privacy: generated artifacts contain a JWT-like credential.");
        addFinding(findings, API_KEY_SECRET, content,
                "privacy: generated artifacts contain an API-key-like value.");
        addFinding(findings, NAMED_SECRET, content,
                "privacy: generated artifacts contain a named secret value.");
        var sanitized = new CapturePrivacyClassifier().sanitizeText(content);
        if (!sanitized.value().equals(content)) {
            findings.add("privacy: generated artifacts contain a configured secret-pattern match.");
        }
        return findings.stream().distinct().toList();
    }

    private static void addFinding(List<String> findings, Pattern pattern, String content, String message) {
        if (pattern.matcher(content).find()) {
            findings.add(message);
        }
    }

    /**
     * Flags personal home-directory paths only when they escape the generation root. The recording
     * and its generated artifacts already live inside that root, so a root-contained absolute path
     * (for example a recorded {@code file://} fixture URL inside the user's own workspace) is not a
     * privacy leak, while any personal path outside the root still blocks generation.
     */
    private static void addPersonalPathFinding(
            List<String> findings,
            Pattern pattern,
            String content,
            Path allowedRoot,
            String message) {
        Matcher matcher = pattern.matcher(content);
        while (matcher.find()) {
            if (!withinAllowedRoot(pathToken(content, matcher.start()), allowedRoot)) {
                findings.add(message);
                return;
            }
        }
    }

    private static String pathToken(String content, int start) {
        int end = start;
        while (end < content.length() && "\"' \t\r\n".indexOf(content.charAt(end)) < 0) {
            end++;
        }
        return content.substring(start, end);
    }

    private static boolean withinAllowedRoot(String token, Path allowedRoot) {
        if (allowedRoot == null) {
            return false;
        }
        // Recorded file:// URLs percent-encode spaces and other characters; decode before the
        // containment comparison so a workspace path like "My%20Files" matches its real directory.
        String candidate = percentDecode(token.replaceFirst("(?i)^file:/*", ""));
        if (!candidate.matches("(?i)^[A-Z]:.*")) {
            candidate = "/" + candidate.replaceFirst("^/+", "");
        }
        try {
            return Path.of(candidate).toAbsolutePath().normalize()
                    .startsWith(allowedRoot.toAbsolutePath().normalize());
        } catch (InvalidPathException exception) {
            return false;
        }
    }

    private static String percentDecode(String value) {
        if (value.indexOf('%') < 0) {
            return value;
        }
        try {
            // '+' is a literal in file URLs, not an encoded space; protect it from URLDecoder.
            return java.net.URLDecoder.decode(value.replace("+", "%2B"), StandardCharsets.UTF_8);
        } catch (IllegalArgumentException malformedEscape) {
            return value;
        }
    }

    private static ArtifactPaths artifactPaths(Path root, String packageName, String className) {
        Path source = root.resolve("src/test/java")
                .resolve(packageName.replace('.', '/'))
                .resolve(className + ".java")
                .normalize();
        Path data = root.resolve("src/test/resources/testDataFiles")
                .resolve(dataFileName(className))
                .normalize();
        Path classes = root.resolve("target/shaft-capture/classes").normalize();
        ensureWithin(root, source);
        ensureWithin(root, data);
        ensureWithin(root, classes);
        return new ArtifactPaths(root, source, data, classes);
    }

    private static void ensureWithin(Path root, Path target) {
        if (!target.toAbsolutePath().normalize().startsWith(root.toAbsolutePath().normalize())) {
            throw new IllegalArgumentException("Generated artifact path escapes the output directory.");
        }
    }

    private static String relative(Path root, Path target) {
        try {
            return root.toAbsolutePath().normalize()
                    .relativize(target.toAbsolutePath().normalize())
                    .toString()
                    .replace('\\', '/');
        } catch (IllegalArgumentException exception) {
            return target.getFileName() == null ? "" : target.getFileName().toString();
        }
    }

    private static String writeJson(Object value) {
        try {
            return JSON.writer().with(PRINTER).writeValueAsString(value) + "\n";
        } catch (RuntimeException exception) {
            throw new IllegalStateException("Generated JSON could not be serialized.", exception);
        }
    }

    private static void atomicWrite(Path destination, String content) {
        Path absolute = destination.toAbsolutePath().normalize();
        Path parent = absolute.getParent();
        if (parent == null) {
            throw new IllegalArgumentException("Generated artifact requires a parent directory.");
        }
        Path temporary = null;
        try {
            Files.createDirectories(parent);
            temporary = Files.createTempFile(parent, "." + absolute.getFileName(), ".tmp");
            Files.writeString(temporary, content, StandardCharsets.UTF_8);
            moveReplacing(temporary, absolute);
        } catch (IOException exception) {
            throw new IllegalStateException("Generated artifact could not be written atomically.", exception);
        } finally {
            if (temporary != null) {
                try {
                    Files.deleteIfExists(temporary);
                } catch (IOException ignored) {
                    // Best-effort cleanup of an unpublished temporary artifact.
                }
            }
        }
    }

    private static void moveReplacing(Path temporary, Path destination) throws IOException {
        boolean atomic = true;
        for (int attempt = 1; attempt <= 50; attempt++) {
            try {
                if (atomic) {
                    Files.move(temporary, destination, StandardCopyOption.ATOMIC_MOVE,
                            StandardCopyOption.REPLACE_EXISTING);
                } else {
                    Files.move(temporary, destination, StandardCopyOption.REPLACE_EXISTING);
                }
                return;
            } catch (AtomicMoveNotSupportedException ignored) {
                atomic = false;
            } catch (AccessDeniedException exception) {
                if (attempt == 50) {
                    throw exception;
                }
                try {
                    Thread.sleep(10);
                } catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt();
                    throw new IOException("Interrupted while publishing generated artifact.", interrupted);
                }
            }
        }
    }

    private static String fingerprint(String sessionJson, String source) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] bytes = digest.digest((sessionJson + "\n" + source).getBytes(StandardCharsets.UTF_8));
            return java.util.HexFormat.of().formatHex(bytes);
        } catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException("SHA-256 is unavailable.", exception);
        }
    }

    private static Optional<ElementSnapshot> target(CaptureEvent event) {
        if (event instanceof CaptureEvent.ClickEvent value) {
            return Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.TypeEvent value) {
            return Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.ClearEvent value) {
            return Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.SelectEvent value) {
            return Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.ToggleEvent value) {
            return Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.UploadEvent value) {
            return Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.KeyboardEvent value) {
            return Optional.ofNullable(value.target());
        }
        if (event instanceof CaptureEvent.FrameEvent value) {
            return Optional.ofNullable(value.target());
        }
        if (event instanceof CaptureEvent.WaitEvent value) {
            return Optional.ofNullable(value.target());
        }
        if (event instanceof CaptureEvent.VerificationEvent value) {
            return Optional.ofNullable(value.target());
        }
        // P2: NetworkEvent requires ApiCaptureGenerator; UI-only codegen skips network capture
        if (event instanceof CaptureEvent.NetworkEvent) {
            return Optional.empty();
        }
        return Optional.empty();
    }

    private static List<ExternalTestDataReference> eventReferences(CaptureEvent event) {
        List<ExternalTestDataReference> references = new ArrayList<>();
        if (event instanceof CaptureEvent.TypeEvent value) {
            references.add(value.value());
        } else if (event instanceof CaptureEvent.SelectEvent value) {
            references.add(value.value());
        } else if (event instanceof CaptureEvent.UploadEvent value) {
            references.add(value.file());
        } else if (event instanceof CaptureEvent.AlertEvent value && value.text() != null) {
            references.add(value.text());
        } else if (event instanceof CaptureEvent.WaitEvent value && value.expected() != null) {
            references.add(value.expected());
        } else if (event instanceof CaptureEvent.VerificationEvent value && value.expected() != null) {
            references.add(value.expected());
        }
        // P2: NetworkEvent requires ApiCaptureGenerator; UI-only codegen skips network capture
        // } else if (event instanceof CaptureEvent.NetworkEvent value && value.request().body() != null) {
        //     // Network request/response bodies handled in P2 ApiCaptureGenerator
        // }
        return references;
    }

    private static boolean interaction(CaptureEvent event) {
        return event instanceof CaptureEvent.ClickEvent
                || event instanceof CaptureEvent.TypeEvent
                || event instanceof CaptureEvent.ClearEvent
                || event instanceof CaptureEvent.SelectEvent
                || event instanceof CaptureEvent.ToggleEvent
                || event instanceof CaptureEvent.UploadEvent
                || event instanceof CaptureEvent.KeyboardEvent
                || event instanceof CaptureEvent.FrameEvent;
    }

    private static boolean isSecret(ExternalTestDataReference reference) {
        return reference != null
                && (reference.classification() == ExternalTestDataReference.DataClassification.SECRET
                || reference.classification() == ExternalTestDataReference.DataClassification.SENSITIVE);
    }

    private static boolean hasShadowContext(EventContext context) {
        JsonNode shadowHosts = context.extensions().get("shadowHosts");
        return shadowHosts != null && shadowHosts.isArray() && !shadowHosts.isEmpty();
    }

    private static String extensionText(EventContext context, String name) {
        JsonNode value = context.extensions().get(name);
        return value == null ? "" : value.asText("");
    }

    private static boolean isInput(ElementSnapshot target) {
        return "textbox".equals(target.role())
                || "combobox".equals(target.role())
                || "input".equals(target.tagName())
                || "textarea".equals(target.tagName())
                || "select".equals(target.tagName());
    }

    private static boolean isClickable(ElementSnapshot target) {
        return Set.of("button", "link", "checkbox", "radio").contains(target.role())
                || Set.of("button", "a").contains(target.tagName());
    }

    private static String eventId(CaptureEvent event) {
        return "event-" + event.context().sequence();
    }

    private static Map<Long, List<Checkpoint>> checkpoints(List<Checkpoint> checkpoints) {
        Map<Long, List<Checkpoint>> result = new TreeMap<>();
        checkpoints.forEach(checkpoint ->
                result.computeIfAbsent(checkpoint.sequence(), ignored -> new ArrayList<>()).add(checkpoint));
        return result;
    }

    private static List<FlowSegment> flowSegments(
            List<Checkpoint> checkpoints,
            List<CaptureEvent> events,
            String testMethodName) {
        List<FlowSegment> flows = new ArrayList<>();
        Set<String> usedNames = new HashSet<>(Set.of(
                testMethodName,
                "setUp",
                "tearDown",
                "requiredData",
                "requiredEnvironment",
                "captureReplayLocator",
                "matchesCaptureTarget"));
        Checkpoint open = null;
        for (Checkpoint checkpoint : checkpoints) {
            if (checkpoint.kind() == Checkpoint.CheckpointKind.FLOW_START) {
                open = checkpoint;
            } else if (checkpoint.kind() == Checkpoint.CheckpointKind.FLOW_END && open != null) {
                Checkpoint start = open;
                List<CaptureEvent> flowEvents = events.stream()
                        .filter(event -> event.context().sequence() > start.sequence())
                        .filter(event -> event.context().sequence() <= checkpoint.sequence())
                        .toList();
                if (!flowEvents.isEmpty()) {
                    String name = flowName(open);
                    flows.add(new FlowSegment(
                            uniqueFlowMethodName(name, usedNames),
                            flowEvents));
                }
                open = null;
            }
        }
        return List.copyOf(flows);
    }

    private static Map<Long, FlowSegment> flowsByFirstSequence(List<FlowSegment> flows) {
        Map<Long, FlowSegment> result = new HashMap<>();
        flows.forEach(flow -> result.put(flow.events().getFirst().context().sequence(), flow));
        return result;
    }

    private static Set<Long> flowEventSequences(List<FlowSegment> flows) {
        Set<Long> result = new HashSet<>();
        flows.forEach(flow -> flow.events().forEach(event -> result.add(event.context().sequence())));
        return result;
    }

    private static boolean flowBoundary(Checkpoint checkpoint) {
        return checkpoint.kind() == Checkpoint.CheckpointKind.FLOW_START
                || checkpoint.kind() == Checkpoint.CheckpointKind.FLOW_END;
    }

    private static String flowName(Checkpoint checkpoint) {
        return checkpoint.description().isBlank() ? "captured flow" : checkpoint.description();
    }

    private static String uniqueFlowMethodName(String name, Set<String> used) {
        String base = javaMethodName(name);
        String candidate = base;
        int suffix = 2;
        while (!used.add(candidate)) {
            candidate = base + suffix++;
        }
        return candidate;
    }

    private static Map<Long, List<CaptureEnrichmentPreview.AssertionSuggestion>> extraAssertions(
            List<CaptureEnrichmentPreview.AssertionSuggestion> assertions) {
        Map<Long, List<CaptureEnrichmentPreview.AssertionSuggestion>> result = new TreeMap<>();
        assertions.stream()
                .sorted(Comparator.comparingLong(CaptureEnrichmentPreview.AssertionSuggestion::eventSequence)
                        .thenComparing(CaptureEnrichmentPreview.AssertionSuggestion::verification))
                .forEach(assertion -> result.computeIfAbsent(
                        assertion.eventSequence(), ignored -> new ArrayList<>()).add(assertion));
        return result;
    }

    private static String javaClassName(String value) {
        StringBuilder result = new StringBuilder();
        for (String part : splitWords(value)) {
            if (!part.isBlank()) {
                result.append(Character.toUpperCase(part.charAt(0)));
                if (part.length() > 1) {
                    result.append(part.substring(1).toLowerCase(Locale.ROOT));
                }
            }
        }
        if (result.isEmpty()) {
            result.append("CapturedJourney");
        }
        if (!Character.isJavaIdentifierStart(result.charAt(0))) {
            result.insert(0, "Capture");
        }
        return result.toString();
    }

    private static String javaMethodName(String value) {
        String className = javaClassName(value);
        return Character.toLowerCase(className.charAt(0)) + className.substring(1);
    }

    private static String constantName(String value) {
        String normalized = value == null ? "ELEMENT" : value.replaceAll("[^A-Za-z0-9]+", "_")
                .replaceAll("^_+|_+$", "")
                .toUpperCase(Locale.ROOT);
        if (normalized.isBlank()) {
            normalized = "ELEMENT";
        }
        if (!Character.isJavaIdentifierStart(normalized.charAt(0))) {
            normalized = "ELEMENT_" + normalized;
        }
        return normalized;
    }

    private static String uniqueDataKey(String value, Set<String> used) {
        String base = value == null ? "value" : value.trim().toLowerCase(Locale.ROOT)
                .replaceAll("[^a-z0-9]+", "_")
                .replaceAll("^_+|_+$", "");
        if (base.isBlank()) {
            base = "value";
        }
        if (Character.isDigit(base.charAt(0))) {
            base = "value_" + base;
        }
        String candidate = base;
        int suffix = 2;
        while (!used.add(candidate)) {
            candidate = base + "_" + suffix++;
        }
        return candidate;
    }

    /**
     * Environment-variable names must stay stable across re-recordings of the same journey, so the
     * recorder's per-event sequence suffix ("data.password-4" -> "data.password") is stripped
     * before deriving the name; duplicates within one generation are de-duplicated with a numeric
     * suffix instead.
     */
    private static String uniqueEnvironmentName(String id, Set<String> used) {
        String stableId = id == null ? "" : id.replaceFirst("-\\d+$", "");
        String base = "SHAFT_CAPTURE_" + constantName(stableId);
        String candidate = base;
        int suffix = 2;
        while (!used.add(candidate)) {
            candidate = base + "_" + suffix++;
        }
        return candidate;
    }

    private static String dataFileName(String className) {
        return className.replaceAll("([a-z0-9])([A-Z])", "$1-$2")
                .replace('_', '-')
                .toLowerCase(Locale.ROOT) + ".json";
    }

    private static String driverType(String browserName) {
        return "edge".equalsIgnoreCase(browserName) ? "EDGE" : "CHROME";
    }

    private static String playwrightBrowserName(String browserName) {
        String normalized = browserName == null || browserName.isBlank() ? "chromium" : browserName.toLowerCase(Locale.ROOT);
        return "edge".equals(normalized) ? "chromium" : normalized;
    }

    private static String currentWindowHandleExpression(CodegenBackend backend) {
        return "driver.browser().getWindowHandle()";
    }

    private static List<String> splitWords(String value) {
        if (value == null) {
            return List.of();
        }
        return java.util.Arrays.stream(value.split("[^A-Za-z0-9]+"))
                .filter(part -> !part.isBlank())
                .toList();
    }

    private static void validatePackage(String packageName) {
        if (!JAVA_PACKAGE.matcher(packageName).matches()) {
            throw new IllegalArgumentException("Generated package name is invalid.");
        }
    }

    private static void validateJavaIdentifier(String value, String label) {
        if (!JAVA_IDENTIFIER.matcher(value).matches()) {
            throw new IllegalArgumentException(label + " is not a valid Java identifier.");
        }
    }

    private static void validateOptionalIdentifier(String value, String label, List<String> errors) {
        if (value != null && !value.isBlank() && !JAVA_IDENTIFIER.matcher(value).matches()) {
            errors.add(label + " is not a Java identifier.");
        }
    }

    private static String javaString(String value) {
        if (value == null) {
            return "";
        }
        return value.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\r", "\\r")
                .replace("\n", "\\n")
                .replace("\t", "\\t");
    }

    private static String safeComment(String value) {
        return (value == null ? "" : value).replace("*/", "* /")
                .replace('\r', ' ')
                .replace('\n', ' ');
    }

    private static String safeMessage(RuntimeException exception) {
        String message = exception.getMessage();
        return message == null || message.isBlank() ? "Generation failed." : message;
    }

    private static void line(StringBuilder source, String value) {
        source.append(value).append('\n');
    }

    private static DefaultPrettyPrinter printer() {
        DefaultPrettyPrinter printer = new DefaultPrettyPrinter();
        DefaultIndenter indenter = new DefaultIndenter("  ", "\n");
        printer.indentArraysWith(indenter);
        printer.indentObjectsWith(indenter);
        return printer;
    }

    /**
     * SHAFT GUI backend used by generated capture replay code.
     */
    public enum CodegenBackend {
        WEBDRIVER("WebDriver"),
        PLAYWRIGHT("Playwright");

        private final String driverClassName;

        CodegenBackend(String driverClassName) {
            this.driverClassName = driverClassName;
        }

        String driverClassName() {
            return driverClassName;
        }

        /**
         * Parses a backend name, defaulting to WebDriver.
         *
         * @param value backend name
         * @return parsed backend
         */
        public static CodegenBackend from(String value) {
            if (value == null || value.isBlank()) {
                return WEBDRIVER;
            }
            String normalized = value.trim().toLowerCase(Locale.ROOT);
            if ("playwright".equals(normalized) || "shaft-playwright".equals(normalized)) {
                return PLAYWRIGHT;
            }
            return WEBDRIVER;
        }
    }

    private record ArtifactPaths(Path root, Path source, Path data, Path classes) {
    }

    private record DataPlan(
            ObjectNode root,
            Map<String, String> keys,
            Map<String, String> environment,
            Map<String, ExternalTestDataReference> references) {
    }

    private record TargetPlan(
            String logicalElementId,
            ElementSnapshot target,
            EventContext context,
            LocatorRanker.LocatorSelection selection,
            List<String> eventIds) {
    }

    private record FlowSegment(
            String methodName,
            List<CaptureEvent> events) {
    }

    private static final class MutableTargetPlan {
        private ElementSnapshot target;
        private EventContext context;
        private LocatorRanker.LocatorSelection selection;
        private final List<String> eventIds = new ArrayList<>();

        private MutableTargetPlan(
                ElementSnapshot target,
                EventContext context,
                LocatorRanker.LocatorSelection selection) {
            this.target = target;
            this.context = context;
            this.selection = selection;
        }

        private TargetPlan immutable(String logicalElementId) {
            return new TargetPlan(logicalElementId, target, context, selection, List.copyOf(eventIds));
        }
    }

    private record GenerationState(
            List<TargetPlan> targets,
            DataPlan data,
            CaptureReadiness readiness,
            List<String> unsupported,
            List<String> flaky,
            List<String> fallback,
            List<CaptureGenerationReport.ControlFlowSuggestion> controlFlow,
            List<String> required,
            List<String> warnings) {
        private static GenerationState failure(String message) {
            ObjectNode root = JSON.createObjectNode();
            root.put("schemaVersion", "1.0");
            root.putObject("values");
            return new GenerationState(
                    List.of(),
                    new DataPlan(root, Map.of(), Map.of(), Map.of()),
                    CaptureReadiness.ready(),
                    new ArrayList<>(List.of(message)),
                    new ArrayList<>(),
                    new ArrayList<>(),
                    new ArrayList<>(),
                    new ArrayList<>(),
                    new ArrayList<>());
        }

        private GenerationState withUnsupported(List<String> additional) {
            List<String> merged = new ArrayList<>(unsupported);
            merged.addAll(additional);
            return new GenerationState(targets, data, readiness, merged, flaky, fallback, controlFlow,
                    required, warnings);
        }

        private GenerationState withControlFlow(
                List<CaptureGenerationReport.ControlFlowSuggestion> suggestions) {
            return new GenerationState(targets, data, readiness, unsupported, flaky, fallback, suggestions,
                    required, warnings);
        }
    }
}
