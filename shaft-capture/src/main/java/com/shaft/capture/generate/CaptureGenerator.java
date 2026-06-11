package com.shaft.capture.generate;

import com.fasterxml.jackson.core.JsonPointer;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureEvent;
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
    private static final ObjectMapper JSON = new ObjectMapper()
            .enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS);
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
        Objects.requireNonNull(request, "request");
        Path sessionPath = request.sessionPath().toAbsolutePath().normalize();
        Path outputRoot = request.outputDirectory().toAbsolutePath().normalize();
        Path reportPath = outputRoot.resolve("target/shaft-capture/generation-report.json");
        CaptureSession session = null;
        ArtifactPaths paths = null;
        try {
            session = codec.read(sessionPath);
            validatePackage(request.packageName());
            String deterministicClassName = request.className().isBlank()
                    ? javaClassName(session.sessionId()) + "Test"
                    : request.className();
            validateJavaIdentifier(deterministicClassName, "Generated class name");
            String deterministicMethodName = "replay" + javaClassName(session.sessionId());
            paths = artifactPaths(outputRoot, request.packageName(), deterministicClassName);

            GenerationState state = analyze(session, sessionPath);
            Map<String, String> elementNames = defaultElementNames(state.targets());
            String deterministicSource = renderSource(session, request.packageName(), deterministicClassName,
                    deterministicMethodName, state.targets(), state.data(), elementNames, List.of());
            String fingerprint = fingerprint(codec.write(session), deterministicSource);

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
                List<String> previewPrivacy = privacyFindings(writeJson(preview));
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
                    state.targets(), state.data(), finalElementNames, appliedProposal.assertions());
            String dataJson = writeJson(state.data().root());

            List<String> privacy = new ArrayList<>();
            privacy.addAll(privacyFindings(codec.write(session)));
            privacy.addAll(privacyFindings(source));
            privacy.addAll(privacyFindings(dataJson));
            privacy.stream().distinct().forEach(state.unsupported()::add);
            validateOutputs(paths, reportPath, request.overwrite(), state.unsupported());

            if (!state.unsupported().isEmpty()) {
                CaptureGenerationReport report = report(
                        session,
                        paths,
                        state,
                        CaptureGenerationReport.Status.FAILED,
                        CaptureGenerationReport.Validation.skipped("Generation failed before compilation."),
                        CaptureGenerationReport.Validation.skipped("Generation failed before replay."),
                        enrichment);
                writeReportIfPossible(reportPath, report, request.overwrite());
                return new CaptureGenerationResult(paths.source(), paths.data(), reportPath,
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
            List<String> reportPrivacy = privacyFindings(reportJson);
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
                return new CaptureGenerationResult(paths.source(), paths.data(), reportPath,
                        request.enrichmentPreviewPath(), privacyFailure);
            }
            atomicWrite(reportPath, reportJson);
            return new CaptureGenerationResult(paths.source(), paths.data(), reportPath,
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
            writeReportIfPossible(reportPath, report, request.overwrite());
            return new CaptureGenerationResult(safePaths.source(), safePaths.data(), reportPath,
                    request.enrichmentPreviewPath(), report);
        }
    }

    private GenerationState analyze(CaptureSession session, Path sessionPath) {
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
            validateEvent(event, unsupported, required, knownWindows);
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
                unsupported.add("checkpoint-" + checkpoint.id() + ": assertion checkpoints require a "
                        + "VerificationEvent at sequence " + checkpoint.sequence()
                        + ". Record the expected target and value explicitly.");
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
        }
        DataPlan data = data(session, sessionPath, required, unsupported);
        if (session.status() != CaptureSession.SessionStatus.COMPLETED) {
            warnings.add("The source Capture session is incomplete.");
        }
        if (session.events().isEmpty()) {
            warnings.add("The source Capture session contains no events.");
        }
        return new GenerationState(
                immutableTargets,
                data,
                unsupported,
                flaky,
                fallback,
                required,
                warnings);
    }

    private static void validateEvent(
            CaptureEvent event,
            List<String> unsupported,
            List<String> required,
            Set<String> knownWindows) {
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
        } else if (event instanceof CaptureEvent.AlertEvent value
                && value.action() == CaptureEvent.AlertAction.VERIFY_TEXT
                && isSecret(value.text())) {
            unsupported.add(id + ": secret alert text cannot be asserted without exposing it in reports.");
        } else if (event instanceof CaptureEvent.NavigationEvent value
                && value.targetUrl().contains("[data:")) {
            required.add(id + ": replace sanitized URL data placeholders before replay.");
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
            case URL_EQUALS, URL_CONTAINS, TITLE_EQUALS -> false;
            default -> true;
        };
        boolean expectedRequired = switch (verification) {
            case TEXT_EQUALS, TEXT_CONTAINS, ATTRIBUTE_EQUALS, URL_EQUALS, URL_CONTAINS, TITLE_EQUALS -> true;
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
                    } else if (value.isContainerNode()) {
                        unsupported.add(reference.id() + ": generated test data values must be scalar.");
                    } else {
                        values.set(key, value.deepCopy());
                    }
                } catch (RuntimeException exception) {
                    required.add(reference.id() + ": source test data file is unavailable.");
                }
            } else if (reference.source() == ExternalTestDataReference.DataSource.ENVIRONMENT
                    || reference.source() == ExternalTestDataReference.DataSource.SECRET_PROVIDER) {
                String name = environmentName(reference.id());
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
            List<CaptureEnrichmentPreview.AssertionSuggestion> extraAssertions) {
        StringBuilder source = new StringBuilder();
        line(source, "package " + packageName + ";");
        line(source, "");
        line(source, "import com.shaft.driver.DriverFactory;");
        line(source, "import com.shaft.driver.SHAFT;");
        line(source, "import com.shaft.gui.internal.locator.Role;");
        line(source, "import org.openqa.selenium.By;");
        line(source, "import org.openqa.selenium.Keys;");
        line(source, "import org.openqa.selenium.WindowType;");
        line(source, "import org.openqa.selenium.support.ui.ExpectedConditions;");
        line(source, "import org.testng.annotations.AfterMethod;");
        line(source, "import org.testng.annotations.BeforeMethod;");
        line(source, "import org.testng.annotations.Test;");
        line(source, "");
        line(source, "import java.nio.file.Path;");
        line(source, "import java.util.HashMap;");
        line(source, "import java.util.Map;");
        line(source, "");
        line(source, "public class " + className + " {");
        for (TargetPlan target : targets) {
            line(source, "    private static final By " + elementNames.get(target.logicalElementId()) + " = "
                    + locatorExpression(target) + ";");
        }
        if (!targets.isEmpty()) {
            line(source, "");
        }
        line(source, "    private SHAFT.GUI.WebDriver driver;");
        line(source, "    private SHAFT.TestData.JSON testData;");
        line(source, "    private final Map<String, String> windows = new HashMap<>();");
        line(source, "");
        line(source, "    @BeforeMethod");
        line(source, "    public void setUp() {");
        line(source, "        driver = new SHAFT.GUI.WebDriver(DriverFactory.DriverType."
                + driverType(session.browser().browserName()) + ");");
        line(source, "        testData = new SHAFT.TestData.JSON(\"" + javaString(dataFileName(className)) + "\");");
        if (!session.events().isEmpty()) {
            line(source, "        windows.put(\""
                    + javaString(session.events().getFirst().context().page().logicalWindowId())
                    + "\", driver.getDriver().getWindowHandle());");
        }
        line(source, "    }");
        line(source, "");
        line(source, "    @Test");
        line(source, "    public void " + methodName + "() throws Exception {");
        Map<Long, List<Checkpoint>> checkpoints = checkpoints(session.checkpoints());
        Map<Long, List<CaptureEnrichmentPreview.AssertionSuggestion>> assertions =
                extraAssertions(extraAssertions);
        Map<Long, CaptureEvent> eventsBySequence = new HashMap<>();
        session.events().forEach(event -> eventsBySequence.put(event.context().sequence(), event));
        for (CaptureEvent event : session.events()) {
            renderEvent(source, event, targets, elementNames, data);
            for (Checkpoint checkpoint : checkpoints.getOrDefault(event.context().sequence(), List.of())) {
                line(source, "        // Recorded checkpoint " + safeComment(checkpoint.id())
                        + " (" + checkpoint.kind() + ").");
            }
            for (CaptureEnrichmentPreview.AssertionSuggestion assertion :
                    assertions.getOrDefault(event.context().sequence(), List.of())) {
                renderSuggestedAssertion(source, eventsBySequence.get(assertion.eventSequence()),
                        assertion, targets, elementNames);
            }
        }
        line(source, "    }");
        line(source, "");
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
        line(source, "    @AfterMethod(alwaysRun = true)");
        line(source, "    public void tearDown() {");
        line(source, "        if (driver != null) {");
        line(source, "            driver.quit();");
        line(source, "        }");
        line(source, "    }");
        line(source, "}");
        return source.toString();
    }

    private static void renderEvent(
            StringBuilder source,
            CaptureEvent event,
            List<TargetPlan> targets,
            Map<String, String> elementNames,
            DataPlan data) {
        String locator = target(event)
                .map(ElementSnapshot::logicalElementId)
                .map(elementNames::get)
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
            line(source, "        if (driver.getDriver().findElement(" + locator + ").isSelected() != "
                    + value.checked() + ") {");
            line(source, "            driver.element().click(" + locator + ");");
            line(source, "        }");
        } else if (event instanceof CaptureEvent.UploadEvent value) {
            line(source, "        driver.element().typeFileLocationForUpload(" + locator + ", "
                    + dataExpression(value.file(), data) + ");");
        } else if (event instanceof CaptureEvent.KeyboardEvent value) {
            String keys = keys(value.keys());
            if (value.target() == null) {
                line(source, "        new org.openqa.selenium.interactions.Actions(driver.getDriver())"
                        + ".sendKeys(" + keys + ").perform();");
            } else {
                line(source, "        driver.element().typeAppend(" + locator + ", " + keys + ");");
            }
        } else if (event instanceof CaptureEvent.WindowEvent value) {
            renderWindow(source, value);
        } else if (event instanceof CaptureEvent.FrameEvent value) {
            if (value.action() == CaptureEvent.FrameAction.TOP) {
                line(source, "        driver.element().switchToDefaultContent();");
            } else if (value.action() == CaptureEvent.FrameAction.EXIT) {
                line(source, "        driver.getDriver().switchTo().parentFrame();");
            } else if (value.target() != null) {
                line(source, "        driver.element().switchToIframe(" + locator + ");");
            } else {
                line(source, "        driver.element().switchToIframe(By.id(\""
                        + javaString(value.logicalFrameId()) + "\"));");
            }
        } else if (event instanceof CaptureEvent.AlertEvent value) {
            renderAlert(source, value, data);
        } else if (event instanceof CaptureEvent.WaitEvent value) {
            renderWait(source, value, locator, data);
        } else if (event instanceof CaptureEvent.VerificationEvent value) {
            renderVerification(source, value.verification(), value.target(), value.expected(),
                    value.negated(), value.context(), locator, data);
        }
    }

    private static void renderWindow(StringBuilder source, CaptureEvent.WindowEvent value) {
        switch (value.action()) {
            case OPEN_TAB, OPEN_WINDOW -> {
                line(source, "        driver.getDriver().switchTo().newWindow(WindowType."
                        + (value.action() == CaptureEvent.WindowAction.OPEN_TAB ? "TAB" : "WINDOW") + ");");
                line(source, "        windows.put(\"" + javaString(value.logicalWindowId())
                        + "\", driver.getDriver().getWindowHandle());");
            }
            case SWITCH -> line(source, "        driver.browser().switchToWindow(windows.get(\""
                    + javaString(value.logicalWindowId()) + "\"));");
            case CLOSE -> line(source, "        driver.browser().closeCurrentWindow();");
        }
    }

    private static void renderAlert(
            StringBuilder source,
            CaptureEvent.AlertEvent value,
            DataPlan data) {
        switch (value.action()) {
            case ACCEPT -> line(source, "        driver.alert().acceptAlert();");
            case DISMISS -> line(source, "        driver.alert().dismissAlert();");
            case TYPE -> {
                if (isSecret(value.text())) {
                    line(source, "        driver.getDriver().switchTo().alert().sendKeys("
                            + dataExpression(value.text(), data) + ");");
                } else {
                    line(source, "        driver.alert().typeIntoPromptAlert("
                            + dataExpression(value.text(), data) + ");");
                }
            }
            case VERIFY_TEXT -> line(source, "        SHAFT.Validations.assertThat()"
                    + ".object(driver.alert().getAlertText()).isEqualTo("
                    + dataExpression(value.text(), data) + ").perform();");
        }
    }

    private static void renderWait(
            StringBuilder source,
            CaptureEvent.WaitEvent value,
            String locator,
            DataPlan data) {
        long seconds = Math.max(1, value.timeout().toSeconds());
        switch (value.condition()) {
            case ELEMENT_PRESENT -> waitUntil(source, "presenceOfElementLocated(" + locator + ")", seconds);
            case ELEMENT_VISIBLE -> waitUntil(source, "visibilityOfElementLocated(" + locator + ")", seconds);
            case ELEMENT_CLICKABLE -> waitUntil(source, "elementToBeClickable(" + locator + ")", seconds);
            case ELEMENT_ABSENT -> waitUntil(source, "invisibilityOfElementLocated(" + locator + ")", seconds);
            case URL_CONTAINS -> waitUntil(source, "urlContains(" + dataExpression(value.expected(), data) + ")",
                    seconds);
            case TITLE_CONTAINS -> waitUntil(source,
                    "titleContains(" + dataExpression(value.expected(), data) + ")", seconds);
            case DOCUMENT_READY -> line(source, "        driver.browser().waitForLazyLoading();");
            case FIXED_DURATION -> line(source, "        Thread.sleep(" + value.timeout().toMillis() + "L);");
        }
    }

    private static void waitUntil(StringBuilder source, String condition, long seconds) {
        line(source, "        driver.element().waitUntil(ExpectedConditions." + condition
                + ", java.time.Duration.ofSeconds(" + seconds + "));");
    }

    private static void renderVerification(
            StringBuilder source,
            CaptureEvent.VerificationKind verification,
            ElementSnapshot target,
            ExternalTestDataReference expected,
            boolean negated,
            EventContext context,
            String locator,
            DataPlan data) {
        switch (verification) {
            case ELEMENT_PRESENT -> line(source, "        driver.assertThat().element(" + locator + ")."
                    + (negated ? "doesNotExist" : "exists") + "().perform();");
            case ELEMENT_VISIBLE -> {
                if (negated) {
                    stateAssertion(source, "driver.getDriver().findElement(" + locator + ").isDisplayed()", true);
                } else {
                    line(source, "        driver.assertThat().element(" + locator + ").isVisible().perform();");
                }
            }
            case ELEMENT_ENABLED -> line(source, "        driver.assertThat().element(" + locator + ")."
                    + (negated ? "isDisabled" : "isEnabled") + "().perform();");
            case ELEMENT_SELECTED -> line(source, "        driver.assertThat().element(" + locator + ")."
                    + (negated ? "isNotSelected" : "isSelected") + "().perform();");
            case TEXT_EQUALS -> nativeAssertion(source,
                    "driver.assertThat().element(" + locator + ").text()",
                    negated ? "doesNotEqual" : "isEqualTo", dataExpression(expected, data));
            case TEXT_CONTAINS -> nativeAssertion(source,
                    "driver.assertThat().element(" + locator + ").text()",
                    negated ? "doesNotContain" : "contains", dataExpression(expected, data));
            case ATTRIBUTE_EQUALS -> nativeAssertion(source,
                    "driver.assertThat().element(" + locator + ").attribute(\""
                            + javaString(extensionText(context, "attributeName")) + "\")",
                    negated ? "doesNotEqual" : "isEqualTo", dataExpression(expected, data));
            case URL_EQUALS -> nativeAssertion(source, "driver.assertThat().browser().url()",
                    negated ? "doesNotEqual" : "isEqualTo", dataExpression(expected, data));
            case URL_CONTAINS -> nativeAssertion(source, "driver.assertThat().browser().url()",
                    negated ? "doesNotContain" : "contains", dataExpression(expected, data));
            case TITLE_EQUALS -> nativeAssertion(source, "driver.assertThat().browser().title()",
                    negated ? "doesNotEqual" : "isEqualTo", dataExpression(expected, data));
        }
    }

    private static void nativeAssertion(
            StringBuilder source,
            String builder,
            String comparison,
            String expected) {
        line(source, "        " + builder + "." + comparison + "(" + expected + ").perform();");
    }

    private static void stateAssertion(StringBuilder source, String actual, boolean expectedFalse) {
        line(source, "        SHAFT.Validations.assertThat().object(" + actual + ")."
                + (expectedFalse ? "isFalse" : "isTrue") + "().perform();");
    }

    private static void renderSuggestedAssertion(
            StringBuilder source,
            CaptureEvent event,
            CaptureEnrichmentPreview.AssertionSuggestion suggestion,
            List<TargetPlan> targets,
            Map<String, String> elementNames) {
        Optional<ElementSnapshot> target = target(event);
        if (target.isEmpty()) {
            return;
        }
        String locator = elementNames.get(target.get().logicalElementId());
        CaptureEvent.VerificationKind verification =
                CaptureEvent.VerificationKind.valueOf(suggestion.verification());
        renderVerification(source, verification, target.get(), null, suggestion.negated(),
                event.context(), locator, null);
    }

    private static String locatorExpression(TargetPlan plan) {
        LocatorCandidate candidate = plan.selection().selected().candidate();
        ElementSnapshot target = plan.target();
        String name = !target.accessibleName().isBlank() ? target.accessibleName() : target.label();
        return switch (candidate.strategy()) {
            case ROLE, ACCESSIBLE_NAME, LABEL -> semanticLocator(target, name, candidate);
            case TEST_ID, CSS -> "By.cssSelector(\"" + javaString(candidate.expression()) + "\")";
            case ID -> "SHAFT.GUI.Locator.hasAnyTagName().hasId(\""
                    + javaString(candidate.expression()) + "\").build()";
            case NAME -> "SHAFT.GUI.Locator.hasAnyTagName().hasAttribute(\"name\", \""
                    + javaString(candidate.expression()) + "\").build()";
            case XPATH -> "By.xpath(\"" + javaString(candidate.expression()) + "\")";
        };
    }

    private static String semanticLocator(
            ElementSnapshot target,
            String name,
            LocatorCandidate candidate) {
        String semanticName = name;
        if (candidate.strategy() == LocatorCandidate.LocatorStrategy.ROLE
                && candidate.expression().contains(":")) {
            semanticName = candidate.expression().substring(candidate.expression().indexOf(':') + 1);
        } else if (semanticName.isBlank()) {
            semanticName = candidate.expression();
        }
        if (isInput(target)) {
            return "SHAFT.GUI.Locator.inputField(\"" + javaString(semanticName) + "\")";
        }
        if (isClickable(target)) {
            return "SHAFT.GUI.Locator.clickableField(\"" + javaString(semanticName) + "\")";
        }
        String role = target.role().toUpperCase(Locale.ROOT).replace('-', '_');
        if (Set.of("BUTTON", "LINK", "TEXTBOX", "CHECKBOX", "RADIO", "COMBOBOX", "HEADING",
                "IMAGE", "LIST", "LISTITEM", "TABLE", "TABLE_ROW", "TABLE_CELL",
                "TABLE_COLUMNHEADER").contains(role)) {
            String suffix = semanticName.isBlank() ? "" : ".hasText(\"" + javaString(semanticName) + "\")";
            return "SHAFT.GUI.Locator.hasRole(Role." + role + ")" + suffix + ".build()";
        }
        return "By.xpath(\"" + javaString(candidate.expression()) + "\")";
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

    private static String keys(List<String> values) {
        List<String> rendered = values.stream().map(CaptureGenerator::key).toList();
        return rendered.size() == 1
                ? rendered.getFirst()
                : "Keys.chord(" + String.join(", ", rendered) + ")";
    }

    private static String key(String value) {
        String normalized = value == null ? "" : value.trim().toUpperCase(Locale.ROOT);
        if (Set.of("NULL", "CANCEL", "HELP", "BACK_SPACE", "TAB", "CLEAR", "RETURN", "ENTER",
                "SHIFT", "CONTROL", "ALT", "PAUSE", "ESCAPE", "SPACE", "PAGE_UP", "PAGE_DOWN",
                "END", "HOME", "ARROW_LEFT", "LEFT", "ARROW_UP", "UP", "ARROW_RIGHT", "RIGHT",
                "ARROW_DOWN", "DOWN", "INSERT", "DELETE", "SEMICOLON", "EQUALS", "NUMPAD0",
                "NUMPAD1", "NUMPAD2", "NUMPAD3", "NUMPAD4", "NUMPAD5", "NUMPAD6", "NUMPAD7",
                "NUMPAD8", "NUMPAD9", "MULTIPLY", "ADD", "SEPARATOR", "SUBTRACT", "DECIMAL",
                "DIVIDE", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10",
                "F11", "F12", "META", "COMMAND", "ZENKAKU_HANKAKU").contains(normalized)) {
            return "Keys." + normalized;
        }
        return "\"" + javaString(value) + "\"";
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
        return new CaptureGenerationReport(
                CaptureGenerationReport.CURRENT_SCHEMA_VERSION,
                sessionId,
                status,
                relative(paths.root(), paths.source()),
                relative(paths.root(), paths.data()),
                decisions,
                state.unsupported().stream().distinct().sorted().toList(),
                state.flaky().stream().distinct().sorted().toList(),
                state.fallback().stream().distinct().sorted().toList(),
                state.required().stream().distinct().sorted().toList(),
                state.warnings().stream().distinct().sorted().toList(),
                compilation,
                replay,
                enrichment);
    }

    private static void validateOutputs(
            ArtifactPaths paths,
            Path reportPath,
            boolean overwrite,
            List<String> unsupported) {
        for (Path path : List.of(paths.source(), paths.data(), reportPath)) {
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

    private static void writeReportIfPossible(
            Path reportPath,
            CaptureGenerationReport report,
            boolean overwrite) {
        try {
            if (!Files.exists(reportPath) || overwrite) {
                atomicWrite(reportPath, writeJson(report));
            }
        } catch (RuntimeException ignored) {
            // The returned in-memory report remains available when the filesystem is not writable.
        }
    }

    private static List<String> privacyFindings(String content) {
        List<String> findings = new ArrayList<>();
        if (content == null || content.isBlank()) {
            return findings;
        }
        addFinding(findings, WINDOWS_PERSONAL_PATH, content,
                "privacy: generated artifacts contain an absolute personal Windows path.");
        addFinding(findings, UNIX_PERSONAL_PATH, content,
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
            return JSON.writer(PRINTER).writeValueAsString(value) + "\n";
        } catch (IOException exception) {
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

    private static String environmentName(String id) {
        return "SHAFT_CAPTURE_" + constantName(id);
    }

    private static String dataFileName(String className) {
        return className.replaceAll("([a-z0-9])([A-Z])", "$1-$2")
                .replace('_', '-')
                .toLowerCase(Locale.ROOT) + ".json";
    }

    private static String driverType(String browserName) {
        return "edge".equalsIgnoreCase(browserName) ? "EDGE" : "CHROME";
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
            List<String> unsupported,
            List<String> flaky,
            List<String> fallback,
            List<String> required,
            List<String> warnings) {
        private static GenerationState failure(String message) {
            ObjectNode root = JSON.createObjectNode();
            root.put("schemaVersion", "1.0");
            root.putObject("values");
            return new GenerationState(
                    List.of(),
                    new DataPlan(root, Map.of(), Map.of(), Map.of()),
                    new ArrayList<>(List.of(message)),
                    new ArrayList<>(),
                    new ArrayList<>(),
                    new ArrayList<>(),
                    new ArrayList<>());
        }

        private GenerationState withUnsupported(List<String> additional) {
            List<String> merged = new ArrayList<>(unsupported);
            merged.addAll(additional);
            return new GenerationState(targets, data, merged, flaky, fallback, required, warnings);
        }
    }
}
