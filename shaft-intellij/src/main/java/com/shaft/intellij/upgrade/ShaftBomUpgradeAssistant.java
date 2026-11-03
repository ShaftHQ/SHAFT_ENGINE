package com.shaft.intellij.upgrade;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Detects a newer SHAFT Engine BOM/version in Maven coordinates and proposes a safe bump (W3.6 / #7604).
 * Pure text helpers — no network in unit tests; callers supply the latest known version.
 */
public final class ShaftBomUpgradeAssistant {
    private static final Pattern PROPERTY = Pattern.compile(
            "<(shaft\\.engine\\.version|shaftVersion)>\\s*([^<]+?)\\s*</\\1>",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
    private static final Pattern SHAFT_ARTIFACT = Pattern.compile(
            "<groupId>\\s*io\\.github\\.shafthq\\s*</groupId>\\s*<artifactId>\\s*[^<]*shaft[^<]*\\s*</artifactId>\\s*<version>\\s*([^<]+?)\\s*</version>",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
    private static final Pattern ANY_SHAFT_MENTION = Pattern.compile(
            "io\\.github\\.shafthq",
            Pattern.CASE_INSENSITIVE);

    private ShaftBomUpgradeAssistant() {
        throw new IllegalStateException("Utility class");
    }

    public record Suggestion(
            @NotNull String currentVersion,
            @NotNull String latestVersion,
            @NotNull String summary,
            boolean bumpRecommended
    ) {
    }

    public static @NotNull Optional<String> detectShaftVersion(@Nullable String pomXml) {
        if (pomXml == null || pomXml.isBlank() || !ANY_SHAFT_MENTION.matcher(pomXml).find()) {
            return Optional.empty();
        }
        Matcher prop = PROPERTY.matcher(pomXml);
        if (prop.find()) {
            return Optional.of(prop.group(2).trim());
        }
        Matcher dep = SHAFT_ARTIFACT.matcher(pomXml);
        if (dep.find()) {
            String version = dep.group(1).trim();
            if (version.startsWith("${") && version.endsWith("}")) {
                return Optional.empty();
            }
            return Optional.of(version);
        }
        return Optional.empty();
    }

    public static int compareVersions(@NotNull String left, @NotNull String right) {
        String[] a = left.split("\\.");
        String[] b = right.split("\\.");
        int n = Math.max(a.length, b.length);
        for (int i = 0; i < n; i++) {
            int ai = i < a.length ? parsePart(a[i]) : 0;
            int bi = i < b.length ? parsePart(b[i]) : 0;
            if (ai != bi) {
                return Integer.compare(ai, bi);
            }
        }
        return 0;
    }

    public static @NotNull Optional<Suggestion> suggest(
            @Nullable String pomXml,
            @NotNull String latestKnownVersion,
            @Nullable String behaviourDiffSummary
    ) {
        Optional<String> current = detectShaftVersion(pomXml);
        if (current.isEmpty()) {
            return Optional.empty();
        }
        boolean newer = compareVersions(latestKnownVersion, current.get()) > 0;
        String diff = behaviourDiffSummary == null || behaviourDiffSummary.isBlank()
                ? "Review the SHAFT Engine changelog before bumping."
                : behaviourDiffSummary.trim();
        String summary = newer
                ? "SHAFT " + current.get() + " → " + latestKnownVersion + ". " + diff
                + " Offer: bump dependency, run a smoke test, keep rollback to " + current.get() + "."
                : "Already on SHAFT " + current.get() + " (latest known " + latestKnownVersion + ").";
        return Optional.of(new Suggestion(current.get(), latestKnownVersion, summary, newer));
    }

    /** Replace the first shaft version property or matching version tag; returns original when unchanged. */
    public static @NotNull String applyBump(@NotNull String pomXml, @NotNull String newVersion) {
        Matcher prop = PROPERTY.matcher(pomXml);
        if (prop.find()) {
            return pomXml.substring(0, prop.start(2)) + newVersion + pomXml.substring(prop.end(2));
        }
        Matcher dep = SHAFT_ARTIFACT.matcher(pomXml);
        if (dep.find()) {
            return pomXml.substring(0, dep.start(1)) + newVersion + pomXml.substring(dep.end(1));
        }
        return pomXml;
    }

    private static int parsePart(String part) {
        String digits = part.replaceAll("[^0-9].*$", "");
        if (digits.isEmpty()) {
            return 0;
        }
        try {
            return Integer.parseInt(digits);
        } catch (NumberFormatException ex) {
            return 0;
        }
    }
}
