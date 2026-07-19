package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Dependabot's {@code github-actions} package-ecosystem only scans {@code /.github/workflows}
 * (see docs.github.com "For GitHub Actions, use the value '/'. Dependabot will search the
 * /.github/workflows directory..."), so it never sees the project-generator templates under
 * {@code shaft-engine/src/main/resources/examples/.github/workflows/}. Those templates would
 * otherwise silently drift from the main repo's own workflows (issue #3798).
 *
 * <p>This guard is the fallback freshness mechanism: it parses every {@code uses:} action
 * reference from both the main repo workflows and the templates, and fails whenever a template
 * pins an action to an older major/minor/patch version than the main repo currently uses for
 * that same action. Every Dependabot bump on the main workflows therefore forces this test red
 * until the templates are synced back up.
 */
class GeneratedWorkflowActionVersionFreshnessTest {

    private static final Pattern USES_LINE = Pattern.compile("^\\s*(?:-\\s*)?uses:\\s*([^\\s#]+)\\s*(?:#.*)?$");

    @Test
    void templateActionVersionsShouldNotTrailTheMainRepoWorkflows() throws IOException {
        Path repoRoot = findRepoRoot();
        Map<String, Version> mainRepoVersions = collectActionVersions(repoRoot.resolve(".github/workflows"));
        Map<String, Version> templateVersions = collectActionVersions(repoRoot.resolve(
                "shaft-engine/src/main/resources/examples/.github/workflows"));

        assertTrue(!mainRepoVersions.isEmpty(), "Expected to find at least one 'uses:' reference "
                + "under .github/workflows -- the parser or the discovered path is likely wrong.");
        assertTrue(!templateVersions.isEmpty(), "Expected to find at least one 'uses:' reference "
                + "under the example project templates -- the parser or the discovered path is likely wrong.");

        List<String> staleActions = new ArrayList<>();
        for (Map.Entry<String, Version> templateEntry : templateVersions.entrySet()) {
            Version mainRepoVersion = mainRepoVersions.get(templateEntry.getKey());
            if (mainRepoVersion == null) {
                // Action only used by templates (not by the main repo's own workflows) -- nothing
                // to compare against, so it is out of scope for this freshness guard.
                continue;
            }
            if (templateEntry.getValue().isOlderThan(mainRepoVersion)) {
                staleActions.add(templateEntry.getKey() + ": template pins " + templateEntry.getValue()
                        + " but the main repo now uses " + mainRepoVersion);
            }
        }

        if (!staleActions.isEmpty()) {
            fail("Generated project templates under shaft-engine/src/main/resources/examples/.github/workflows/ "
                    + "pin older GitHub Action versions than .github/workflows/*.yml. Sync the templates to match "
                    + "the main repo's current versions (see issue #3798):\n  - "
                    + String.join("\n  - ", staleActions));
        }
    }

    /**
     * Parses every {@code uses: owner/action@version} reference under {@code workflowsDirectory},
     * keeping the newest version seen per action name. Local composite actions ({@code ./...})
     * and Docker actions ({@code docker://...}) are skipped -- they carry no comparable version.
     */
    private static Map<String, Version> collectActionVersions(Path workflowsDirectory) throws IOException {
        Map<String, Version> versionsByAction = new HashMap<>();
        if (!Files.isDirectory(workflowsDirectory)) {
            return versionsByAction;
        }
        try (Stream<Path> workflowFiles = Files.list(workflowsDirectory)) {
            for (Path workflowFile : workflowFiles.filter(GeneratedWorkflowActionVersionFreshnessTest::isYaml).toList()) {
                for (String line : Files.readAllLines(workflowFile)) {
                    Matcher matcher = USES_LINE.matcher(line);
                    if (!matcher.matches()) {
                        continue;
                    }
                    String reference = matcher.group(1);
                    if (reference.startsWith("./") || reference.startsWith("docker://")) {
                        continue;
                    }
                    int at = reference.lastIndexOf('@');
                    if (at < 0) {
                        continue;
                    }
                    String action = reference.substring(0, at);
                    Version version = Version.parse(reference.substring(at + 1));
                    if (version == null) {
                        continue;
                    }
                    versionsByAction.merge(action, version, (existing, candidate) ->
                            candidate.isOlderThan(existing) ? existing : candidate);
                }
            }
        }
        return versionsByAction;
    }

    private static boolean isYaml(Path path) {
        String name = path.getFileName().toString();
        return name.endsWith(".yml") || name.endsWith(".yaml");
    }

    /**
     * Walks up from the current working directory until a directory that looks like the repo
     * root is found (contains both {@code .github} and {@code shaft-engine}). Handles both
     * "run from the reactor root" and "run from the shaft-mcp module" (the default Surefire
     * working directory for {@code mvn -pl shaft-mcp ... test}).
     */
    private static Path findRepoRoot() {
        Path candidate = Path.of("").toAbsolutePath();
        for (int depth = 0; depth < 8 && candidate != null; depth++, candidate = candidate.getParent()) {
            if (Files.isDirectory(candidate.resolve(".github"))
                    && Files.isDirectory(candidate.resolve("shaft-engine"))) {
                return candidate;
            }
        }
        throw new IllegalStateException("Could not locate the SHAFT_ENGINE repo root (a directory containing both "
                + ".github/ and shaft-engine/) by walking up from " + Path.of("").toAbsolutePath());
    }

    /**
     * A dotted numeric version such as {@code v7}, {@code v1.14.0}, or {@code v5.0.1}. Missing
     * trailing components compare as zero, so {@code v7} == {@code v7.0.0}.
     */
    private record Version(List<Integer> parts, String raw) {
        static Version parse(String raw) {
            String normalized = raw.startsWith("v") ? raw.substring(1) : raw;
            String[] segments = normalized.split("\\.");
            List<Integer> parts = new ArrayList<>();
            for (String segment : segments) {
                if (!segment.chars().allMatch(Character::isDigit) || segment.isEmpty()) {
                    // Not a plain numeric tag (e.g. a commit SHA or a branch name) -- not comparable.
                    return null;
                }
                parts.add(Integer.parseInt(segment));
            }
            if (parts.isEmpty()) {
                return null;
            }
            return new Version(parts, raw);
        }

        boolean isOlderThan(Version other) {
            int length = Math.max(parts.size(), other.parts.size());
            for (int i = 0; i < length; i++) {
                int mine = i < parts.size() ? parts.get(i) : 0;
                int theirs = i < other.parts.size() ? other.parts.get(i) : 0;
                if (mine != theirs) {
                    return mine < theirs;
                }
            }
            return false;
        }

        @Override
        public String toString() {
            return raw;
        }
    }
}
