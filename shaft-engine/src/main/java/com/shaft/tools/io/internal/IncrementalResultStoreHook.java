package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * Wires {@link IncrementalResultStore} into the end of a SHAFT execution (#7335), driven by the
 * {@code allure.incrementalStore*} properties. Never fails the run: problems are logged.
 */
public final class IncrementalResultStoreHook {
    private static final Path STORE_ROOT = Path.of("target", "passed-results");

    private IncrementalResultStoreHook() {
    }

    /**
     * Records and/or merges the incremental pass store, as configured.
     *
     * @param executionStartMs run start; older results in the results folder are leftovers and are not stored
     */
    public static void afterExecution(long executionStartMs) {
        var allure = SHAFT.Properties.allure;
        if (!allure.incrementalStore() && !allure.incrementalStoreMerge()) {
            return;
        }
        Path results = Path.of(SHAFT.Properties.paths.allureResults());
        Path journey = STORE_ROOT.resolve(allure.incrementalStoreJourney().replaceAll("[^A-Za-z0-9._-]", "_"));
        KeyFactory keys = new KeyFactory(Path.of("").toAbsolutePath(), allure.incrementalStoreHashGlobs());
        try {
            if (allure.incrementalStore()) {
                int stored = IncrementalResultStore.record(results, journey, executionStartMs, keys::keyFor);
                ReportManager.logDiscrete("Incremental result store: recorded " + stored + " pass(es) in " + journey + ".");
            }
            if (allure.incrementalStoreMerge()) {
                var merge = IncrementalResultStore.merge(journey, results, keys::keyFor);
                ReportManager.logDiscrete("Incremental result store: merged " + merge.merged().size() + " valid pass(es); stale (rerun): "
                        + merge.stale() + "; flagged (newer failure): " + merge.flagged() + ".");
            }
        } catch (IOException | RuntimeException e) {
            ReportManager.logDiscrete("Incremental result store skipped: " + e.getMessage());
        }
    }

    /** Builds the current {@link IncrementalResultStore.ValidityKey} per test class, caching the shared parts. */
    static final class KeyFactory {
        private final Path root;
        private final String globs;
        private final Map<String, IncrementalResultStore.ValidityKey> cache = new HashMap<>();
        private Set<Path> shared = Set.of();
        private String gitSha = "";
        private String appSha = "";
        private boolean sharedComputed;

        KeyFactory(Path root, String globs) {
            this.root = root;
            this.globs = globs;
        }

        IncrementalResultStore.ValidityKey keyFor(String testClass) {
            return cache.computeIfAbsent(testClass, this::compute);
        }

        private IncrementalResultStore.ValidityKey compute(String testClass) {
            try {
                if (!sharedComputed) {
                    sharedComputed = true;
                    shared = IncrementalResultStore.filesMatching(root, globs);
                    gitSha = gitSha(root);
                    appSha = appSha();
                }
                Set<Path> files = new LinkedHashSet<>(shared);
                if (!testClass.isEmpty()) {
                    String relative = testClass.replace('.', '/') + ".java";
                    files.add(Path.of("src", "test", "java").resolve(relative));
                    files.add(Path.of("src", "main", "java").resolve(relative));
                }
                return new IncrementalResultStore.ValidityKey(gitSha, appSha, IncrementalResultStore.filesHash(root, files),
                        SHAFT.Properties.internal.shaftEngineVersion());
            } catch (IOException e) {
                throw new IllegalStateException(e);
            }
        }

        private static String appSha() throws IOException {
            String app = SHAFT.Properties.mobile.app();
            if (app == null || app.isBlank() || app.contains("://")) {
                return app == null ? "" : app.trim();
            }
            Path appPath = Path.of(app);
            return Files.isRegularFile(appPath) ? IncrementalResultStore.sha256Hex(appPath) : "";
        }

        private static String gitSha(Path root) {
            try {
                Process process = new ProcessBuilder("git", "rev-parse", "HEAD").directory(root.toFile())
                        .redirectErrorStream(true).start();
                String out = new String(process.getInputStream().readAllBytes(), java.nio.charset.StandardCharsets.UTF_8).trim();
                return process.waitFor(10, TimeUnit.SECONDS) && process.exitValue() == 0 ? out : "";
            } catch (IOException e) {
                return "";
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                return "";
            }
        }
    }
}
