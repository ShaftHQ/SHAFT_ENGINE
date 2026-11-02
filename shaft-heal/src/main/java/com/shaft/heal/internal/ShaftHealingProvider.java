package com.shaft.heal.internal;

import com.shaft.gui.internal.healing.HealingActionOutcome;
import com.shaft.gui.internal.healing.HealingExplanation;
import com.shaft.gui.internal.healing.HealingFingerprintObservation;
import com.shaft.gui.internal.healing.HealingFingerprintSeed;
import com.shaft.gui.internal.healing.HealingObservation;
import com.shaft.gui.internal.healing.HealingProvider;
import com.shaft.gui.internal.healing.HealingRequest;
import com.shaft.gui.internal.healing.HealingResolution;
import com.shaft.heal.HealingConfiguration;
import com.shaft.heal.ShaftHeal;
import com.shaft.heal.model.HealingDecision;
import com.shaft.heal.model.HealingContext;
import com.shaft.heal.model.HealingPlatform;
import com.shaft.heal.model.HealingReport;
import com.shaft.heal.model.LocatorFingerprint;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Default deterministic SHAFT Heal provider.
 *
 * <p>The provider derives and validates candidates but never executes the
 * intended user action. SHAFT Engine owns action execution and reports the
 * outcome through {@link #recordOutcome(HealingActionOutcome)}.</p>
 */
public class ShaftHealingProvider implements HealingProvider {
    // Issue #4027: delay between re-suggestion rungs once the hard total budget is engaged. Only
    // relevant when healing.ladder.budgetSeconds > 0; the default (0) never reaches this loop.
    private static final Duration LADDER_POLL_INTERVAL = Duration.ofSeconds(1);
    private final Map<String, PendingRecovery> pendingRecoveries = new ConcurrentHashMap<>();
    private final ResuggestionLadder ladder;

    /**
     * Creates the ServiceLoader provider.
     */
    public ShaftHealingProvider() {
        this(ResuggestionLadder.system());
    }

    /**
     * Creates a provider with an injectable re-suggestion ladder (issue #4027), letting tests
     * control the clock driving the hard total budget.
     *
     * @param ladder re-suggestion ladder
     */
    ShaftHealingProvider(ResuggestionLadder ladder) {
        this.ladder = ladder;
    }

    @Override
    public Optional<HealingResolution> resolve(HealingRequest request) {
        HealingConfiguration configuration = HealingConfiguration.current();
        HealingHistoryStore history = new HealingHistoryStore(configuration);
        HealingReportWriter writer = new HealingReportWriter(configuration);
        String attemptId = UUID.randomUUID().toString();
        String pageKey = HealingSupport.pageKey(request.driver());
        String originalLocator = HealingSupport.locator(request.originalLocator());
        HealingContext contextMetadata = HealingSupport.context(
                request.driver(),
                request.frameLocator(),
                request.shadowHostLocator(),
                request.shadowContentLocator());
        String context = contextMetadata.stableKey();
        Optional<HistoryRecord> retained = history.find(pageKey, originalLocator, context);
        if (retained.isEmpty()) {
            writer.publish(report(
                    attemptId,
                    request,
                    pageKey,
                    context,
                    contextMetadata,
                    List.of(),
                    new HealingDecision(
                            HealingDecision.Status.NO_HISTORY,
                            "",
                            0,
                            "No previously verified fingerprint exists for this logical element.",
                            true,
                            false),
                    HealingReport.ProviderMetadata.disabled(),
                    false,
                    false,
                    HealingReport.LadderMetadata.disabled(),
                    request.visibilityRequired()));
            return Optional.empty();
        }

        LadderRun run = runLadder(request, configuration, retained.get());
        HealingDecisionEngine.DecisionResult result = run.attempt().result();
        AiCandidateReranker.RerankResult reranked = run.attempt().reranked();
        HealingReport report = report(
                attemptId,
                request,
                pageKey,
                context,
                contextMetadata,
                rankedReports(reranked.candidates()),
                result.decision(),
                reranked.metadata(),
                reranked.remoteEvidenceSent(),
                result.selected() != null,
                new HealingReport.LadderMetadata(
                        run.rungs(), run.elapsed().toMillis(), configuration.ladderBudget().toSeconds()),
                request.visibilityRequired());
        writer.publish(report);
        if (result.selected() == null) {
            return Optional.empty();
        }
        pendingRecoveries.put(attemptId, new PendingRecovery(
                report,
                result.selected(),
                retained.get(),
                configuration,
                request.driver(),
                request.originalLocator()));
        return Optional.of(new HealingResolution(
                attemptId,
                List.of(result.selected().element()),
                result.selected().locator()));
    }

    /**
     * Runs the deterministic candidate-extraction-and-decision pass once, or -- when
     * {@code healing.ladder.budgetSeconds} is positive -- repeatedly across
     * {@link ResuggestionLadder} rungs until a candidate is accepted or the hard total budget is
     * exhausted (issue #4027). {@code Duration#ZERO} (the default) never engages the ladder,
     * preserving today's exact single-attempt behavior for every existing caller.
     */
    private LadderRun runLadder(
            HealingRequest request, HealingConfiguration configuration, HistoryRecord retained) {
        Duration budget = configuration.ladderBudget();
        if (budget.isZero()) {
            return new LadderRun(attempt(request, configuration, retained), 1, Duration.ZERO);
        }
        AtomicReference<AttemptResult> lastAttempt = new AtomicReference<>();
        ResuggestionLadder.Result<AttemptResult> ladderResult = ladder.run(budget, LADDER_POLL_INTERVAL, () -> {
            AttemptResult attemptResult = attempt(request, configuration, retained);
            lastAttempt.set(attemptResult);
            return attemptResult.result().selected() != null ? Optional.of(attemptResult) : Optional.empty();
        });
        AttemptResult finalAttempt = ladderResult.value().orElseGet(lastAttempt::get);
        return new LadderRun(finalAttempt, ladderResult.rungs(), ladderResult.elapsed());
    }

    /**
     * Runs exactly one deterministic re-suggestion rung: extract candidates from the retained
     * fingerprint, apply optional visual evidence, decide, and rerank when triggered.
     */
    private static AttemptResult attempt(
            HealingRequest request, HealingConfiguration configuration, HistoryRecord retained) {
        List<RankedCandidate> candidates = new CandidateExtractor(configuration)
                .extract(
                        request.driver(),
                        retained.fingerprint(),
                        request.frameLocator(),
                        request.shadowHostLocator());
        candidates = new VisualEvidenceService(configuration)
                .apply(candidates, retained.visualReference());
        HealingDecisionEngine.DecisionResult result = HealingDecisionEngine.decide(
                candidates, configuration, request.visibilityRequired());
        AiCandidateReranker.RerankResult reranked = rerankIfTriggered(
                candidates, configuration, result.decision());
        if (reranked.applied()) {
            result = HealingDecisionEngine.decide(
                    reranked.candidates(), configuration, request.visibilityRequired());
        }
        return new AttemptResult(result, reranked);
    }

    private record AttemptResult(
            HealingDecisionEngine.DecisionResult result, AiCandidateReranker.RerankResult reranked) {
    }

    private record LadderRun(AttemptResult attempt, int rungs, Duration elapsed) {
    }

    @Override
    public Optional<HealingExplanation> explain(String attemptId) {
        PendingRecovery pending = pendingRecoveries.get(attemptId);
        if (pending == null) {
            return Optional.empty();
        }
        HealingReport report = pending.report();
        HealingReport.ProviderMetadata provider = report.provider();
        String providerStatus = provider.enabled()
                ? provider.provider() + ":" + provider.status()
                : "deterministic";
        return Optional.of(new HealingExplanation(
                attemptId,
                pending.originalLocator(),
                pending.selected().locator(),
                report.decision().confidence(),
                pending.configuration().minimumConfidence(),
                pending.selected().report().evidence(),
                providerStatus,
                report.decision().reason()));
    }

    @Override
    public void observe(HealingObservation observation) {
        HealingConfiguration configuration = HealingConfiguration.current();
        String pageKey = HealingSupport.pageKey(observation.driver());
        String originalLocator = HealingSupport.locator(observation.originalLocator());
        HealingContext contextMetadata = HealingSupport.context(
                observation.driver(),
                observation.frameLocator(),
                observation.shadowHostLocator(),
                observation.shadowContentLocator());
        String context = contextMetadata.stableKey();
        String historyKey = HealingHistoryStore.key(pageKey, originalLocator, context);
        LocatorFingerprint fingerprint = new FingerprintExtractor(configuration)
                .extract(observation.driver(), observation.element());
        String visualReference = new VisualEvidenceService(configuration)
                .saveReference(historyKey, observation.element());
        new HealingHistoryStore(configuration).save(
                pageKey,
                originalLocator,
                context,
                contextMetadata,
                fingerprint,
                visualReference);
    }

    @Override
    public void observeFingerprint(HealingFingerprintObservation observation) {
        HealingConfiguration configuration = HealingConfiguration.current();
        String pageKey = HealingSupport.pageKeyFromUrl(observation.pageUrl());
        String originalLocator = HealingSupport.locator(observation.originalLocator());
        // Generation-time seeding has no live driver, frame, or shadow root -- only the top-level
        // page context applies (issue #4161). HealingSupport.context() tolerates a null driver and
        // null locators, so the resulting context matches a later live top-level resolution.
        HealingContext contextMetadata = HealingSupport.context(null, null, null, null);
        String context = contextMetadata.stableKey();
        LocatorFingerprint fingerprint = toFingerprint(observation.fingerprint());
        new HealingHistoryStore(configuration).save(
                pageKey,
                originalLocator,
                context,
                contextMetadata,
                fingerprint,
                "");
    }

    private static LocatorFingerprint toFingerprint(HealingFingerprintSeed seed) {
        return new LocatorFingerprint(
                LocatorFingerprint.CURRENT_SCHEMA_VERSION,
                seed.tagName(),
                seed.accessibleName(),
                seed.associatedLabel(),
                seed.visibleText(),
                seed.id(),
                seed.name(),
                seed.role(),
                seed.type(),
                seed.placeholder(),
                seed.title(),
                seed.testIds(),
                seed.semanticAttributes(),
                "",
                HealingPlatform.WEB,
                Map.of(),
                "",
                seed.displayed(),
                seed.enabled(),
                seed.selected(),
                "");
    }

    @Override
    public void recordOutcome(HealingActionOutcome outcome) {
        PendingRecovery pending = pendingRecoveries.remove(outcome.attemptId());
        if (pending == null) {
            return;
        }
        HealingReport.ActionMetadata action = new HealingReport.ActionMetadata(
                outcome.action(),
                true,
                outcome.successful() ? "PASSED" : "FAILED",
                outcome.verification(),
                outcome.failure());
        HealingReport previous = pending.report();
        HealingReport updated = new HealingReport(
                previous.schemaVersion(),
                previous.attemptId(),
                previous.timestamp(),
                previous.originalLocator(),
                previous.failureCategory(),
                previous.pageKey(),
                previous.context(),
                previous.contextMetadata(),
                previous.candidates(),
                previous.decision(),
                previous.provider(),
                previous.privacy(),
                action,
                previous.ladder(),
                previous.visibilityRequired());
        new HealingReportWriter(pending.configuration()).publish(updated);
        if (outcome.successful()) {
            HealingHistoryStore store = new HealingHistoryStore(pending.configuration());
            store.recordOutcome(
                    pending.history().pageKey(),
                    pending.history().originalLocator(),
                    pending.history().context(),
                    "healed-and-passed");
            String visualReference = new VisualEvidenceService(pending.configuration())
                    .saveReference(pending.history().key(), pending.selected().element());
            store.save(
                    pending.history().pageKey(),
                    pending.history().originalLocator(),
                    pending.history().context(),
                    pending.history().contextMetadata(),
                    pending.selected().report().fingerprint(),
                    visualReference.isBlank() ? pending.history().visualReference() : visualReference);
        }
    }

    @Override
    public void clear(WebDriver driver) {
        pendingRecoveries.entrySet().removeIf(entry -> entry.getValue().driver() == driver);
        ShaftHeal.clear();
    }

    private static HealingReport report(
            String attemptId,
            HealingRequest request,
            String pageKey,
            String context,
            HealingContext contextMetadata,
            List<com.shaft.heal.model.HealingCandidate> candidates,
            HealingDecision decision,
            HealingReport.ProviderMetadata provider,
            boolean remoteEvidenceSent,
            boolean recoveryUsed,
            HealingReport.LadderMetadata ladder,
            boolean visibilityRequired) {
        HealingReport.ActionMetadata action = new HealingReport.ActionMetadata(
                request.action(),
                recoveryUsed,
                recoveryUsed ? "PENDING" : "NOT_EXECUTED",
                "UNVERIFIABLE",
                "");
        return new HealingReport(
                HealingReport.CURRENT_SCHEMA_VERSION,
                attemptId,
                Instant.now().toString(),
                HealingSupport.locator(request.originalLocator()),
                "LOCATOR_NOT_FOUND",
                pageKey,
                context,
                contextMetadata,
                candidates,
                decision,
                provider,
                new HealingReport.PrivacyMetadata(
                        "Whitelist-only semantic evidence; values, cookies, authorization data, and full DOM are excluded.",
                        0,
                        remoteEvidenceSent),
                action,
                ladder,
                visibilityRequired);
    }

    private static List<com.shaft.heal.model.HealingCandidate> rankedReports(List<RankedCandidate> candidates) {
        return candidates.stream()
                .sorted(HealingDecisionEngine.rankingOrder())
                .map(RankedCandidate::report)
                .toList();
    }

    private static AiCandidateReranker.RerankResult rerankIfTriggered(
            List<RankedCandidate> candidates,
            HealingConfiguration configuration,
            HealingDecision decision) {
        if (!configuration.aiEnabled()) {
            return new AiCandidateReranker.RerankResult(
                    candidates, HealingReport.ProviderMetadata.disabled(), false, false);
        }
        if (!aiTriggered(configuration.aiTrigger(), decision.status(), candidates)) {
            return new AiCandidateReranker.RerankResult(
                    candidates,
                    new HealingReport.ProviderMetadata(
                            true,
                            "none",
                            "",
                            "SKIPPED",
                            "AI reranking not triggered for " + decision.status() + "."),
                    false,
                    false);
        }
        return new AiCandidateReranker(configuration).apply(candidates);
    }

    private static boolean aiTriggered(
            HealingConfiguration.AiTrigger trigger,
            HealingDecision.Status status,
            List<RankedCandidate> candidates) {
        if (candidates.isEmpty()) {
            return false;
        }
        return switch (trigger) {
            case ALWAYS -> true;
            case BELOW_THRESHOLD -> status == HealingDecision.Status.BELOW_THRESHOLD;
            case AMBIGUOUS -> status == HealingDecision.Status.AMBIGUOUS
                    || status == HealingDecision.Status.BELOW_THRESHOLD;
            case NEVER -> false;
        };
    }

    private record PendingRecovery(
            HealingReport report,
            RankedCandidate selected,
            HistoryRecord history,
            HealingConfiguration configuration,
            WebDriver driver,
            By originalLocator) {
    }
}
