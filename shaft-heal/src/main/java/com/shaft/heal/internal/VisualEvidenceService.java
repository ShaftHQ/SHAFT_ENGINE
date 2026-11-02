package com.shaft.heal.internal;

import com.shaft.gui.internal.healing.HealingVisualProvider;
import com.shaft.heal.HealingConfiguration;
import com.shaft.heal.model.HealingCandidate;
import com.shaft.heal.model.HealingScore;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.ServiceLoader;

final class VisualEvidenceService {
    private final HealingConfiguration configuration;

    VisualEvidenceService(HealingConfiguration configuration) {
        this.configuration = configuration;
    }

    String saveReference(String historyKey, WebElement element) {
        if (!configuration.visualEnabled()) {
            return "";
        }
        try {
            Path directory = configuration.historyPath().getParent().resolve("visual");
            Files.createDirectories(directory);
            Path target = directory.resolve(historyKey + ".png");
            Files.write(target, element.getScreenshotAs(OutputType.BYTES));
            return target.toString();
        } catch (IOException | WebDriverException exception) {
            return "";
        }
    }

    List<RankedCandidate> apply(List<RankedCandidate> candidates, String visualReference) {
        if (!configuration.visualEnabled() || visualReference == null || visualReference.isBlank()) {
            return candidates;
        }
        Optional<HealingVisualProvider> provider = provider();
        Path reference = Path.of(visualReference);
        if (provider.isEmpty() || !Files.isRegularFile(reference)) {
            return candidates;
        }
        byte[] referenceImage;
        try {
            referenceImage = Files.readAllBytes(reference);
        } catch (IOException exception) {
            return candidates;
        }
        return candidates.stream().map(candidate -> {
            Double visualScore = compare(provider.get(), referenceImage, candidate.element());
            if (visualScore == null) {
                return candidate;
            }
            HealingScore old = candidate.report().score();
            double finalScore = old.deterministicScore() * 0.90 + visualScore * 0.10;
            HealingScore updatedScore = new HealingScore(
                    old.deterministicScore(),
                    visualScore,
                    old.providerScore(),
                    finalScore,
                    old.evidenceScores());
            HealingCandidate oldReport = candidate.report();
            HealingCandidate updatedReport = new HealingCandidate(
                    oldReport.candidateId(),
                    oldReport.proposedLocator(),
                    oldReport.fingerprint(),
                    updatedScore,
                    oldReport.evidence(),
                    oldReport.unique(),
                    oldReport.visible(),
                    oldReport.interactable(),
                    oldReport.contextMatched());
            return new RankedCandidate(candidate.element(), candidate.locator(), updatedReport);
        }).toList();
    }

    private static Optional<HealingVisualProvider> provider() {
        List<HealingVisualProvider> providers = ServiceLoader.load(HealingVisualProvider.class)
                .stream()
                .map(ServiceLoader.Provider::get)
                .sorted(Comparator.comparing(HealingVisualProvider::id))
                .toList();
        return providers.size() == 1 ? Optional.of(providers.getFirst()) : Optional.empty();
    }

    private static Double compare(
            HealingVisualProvider provider,
            byte[] referenceImage,
            WebElement candidate) {
        try {
            double score = provider.similarity(referenceImage, candidate.getScreenshotAs(OutputType.BYTES));
            return Math.max(0, Math.min(1, score));
        } catch (RuntimeException exception) {
            return null;
        }
    }
}
