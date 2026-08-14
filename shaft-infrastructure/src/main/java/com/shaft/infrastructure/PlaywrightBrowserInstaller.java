package com.shaft.infrastructure;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Duration;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/** Runs the packaged Playwright installer against a verified loopback-only artifact mirror. */
final class PlaywrightBrowserInstaller {
    private static final Set<String> REMOVED_ENVIRONMENT = Set.of(
            "HTTP_PROXY", "HTTPS_PROXY", "ALL_PROXY", "http_proxy", "https_proxy", "all_proxy",
            "PLAYWRIGHT_DOWNLOAD_CONNECTION_TIMEOUT", "PLAYWRIGHT_DOWNLOAD_HOST",
            "PLAYWRIGHT_CHROMIUM_DOWNLOAD_HOST", "PLAYWRIGHT_FIREFOX_DOWNLOAD_HOST",
            "PLAYWRIGHT_WEBKIT_DOWNLOAD_HOST", "PLAYWRIGHT_BROWSERS_PATH", "PLAYWRIGHT_NODEJS_PATH");
    private final CommandRunner commandRunner;

    PlaywrightBrowserInstaller(CommandRunner commandRunner) {
        this.commandRunner = Objects.requireNonNull(commandRunner, "commandRunner");
    }

    void install(Path nodeExecutable, Path driverRoot, Path browserRoot,
                 Map<PlaywrightArtifactManifest.Artifact, Path> archives,
                 Path log, Duration timeout) throws IOException {
        install(nodeExecutable, driverRoot, browserRoot, archives, log, timeout, PlaywrightHostPlatform.WIN64);
    }

    void install(Path nodeExecutable, Path driverRoot, Path browserRoot,
                 Map<PlaywrightArtifactManifest.Artifact, Path> archives,
                 Path log, Duration timeout, PlaywrightHostPlatform hostPlatform) throws IOException {
        Objects.requireNonNull(timeout, "timeout");
        if (timeout.isZero() || timeout.isNegative()) throw new IllegalArgumentException("timeout must be positive.");
        Path node = nodeExecutable.toAbsolutePath().normalize();
        Path cli = driverRoot.toAbsolutePath().normalize().resolve("package/cli.js");
        Path destination = browserRoot.toAbsolutePath().normalize();
        VerifiedArtifactStore.requireUnlinkedAncestors(node);
        VerifiedArtifactStore.requireUnlinkedAncestors(cli);
        VerifiedArtifactStore.requireUnlinkedAncestors(destination);
        if (!java.nio.file.Files.isRegularFile(node, java.nio.file.LinkOption.NOFOLLOW_LINKS)
                || !java.nio.file.Files.isRegularFile(cli, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("Managed Node and the packaged Playwright CLI must be regular unlinked files.");
        }

        Map<String, Path> mirrorPaths = validateArchives(archives, hostPlatform.requiredArtifacts());
        if (log != null) {
            Path normalizedLog = log.toAbsolutePath().normalize();
            VerifiedArtifactStore.requireUnlinkedAncestors(normalizedLog);
            java.nio.file.Files.createDirectories(normalizedLog.getParent());
        }
        try (VerifiedArtifactMirror mirror = VerifiedArtifactMirror.open(mirrorPaths)) {
            String base = mirror.baseUri().toString();
            Map<String, String> environment = new LinkedHashMap<>();
            environment.put("PLAYWRIGHT_NODEJS_PATH", node.toString());
            environment.put("PLAYWRIGHT_BROWSERS_PATH", destination.toString());
            environment.put("PLAYWRIGHT_DOWNLOAD_HOST", base);
            environment.put("PLAYWRIGHT_CHROMIUM_DOWNLOAD_HOST", base);
            environment.put("PLAYWRIGHT_FIREFOX_DOWNLOAD_HOST", base);
            environment.put("PLAYWRIGHT_WEBKIT_DOWNLOAD_HOST", base);
            environment.put("PLAYWRIGHT_SKIP_BROWSER_GC", "1");
            ReportingSetupService.ProcessResult result = commandRunner.run(
                    List.of(node.toString(), cli.toString(), "install", "chromium", "firefox", "webkit"),
                    log, timeout, environment, REMOVED_ENVIRONMENT);
            if (result.exitCode() != 0) {
                throw new IOException("Playwright browser installation failed; see " + log);
            }
        }
    }

    private static Map<String, Path> validateArchives(
            Map<PlaywrightArtifactManifest.Artifact, Path> archives, Set<String> requiredArtifacts) throws IOException {
        Objects.requireNonNull(archives, "archives");
        Set<String> names = archives.keySet().stream().map(PlaywrightArtifactManifest.Artifact::name)
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
        if (!names.equals(requiredArtifacts) || archives.size() != requiredArtifacts.size()) {
            throw new IOException("Playwright installation requires the exact reviewed host artifact set.");
        }
        Map<String, Path> mirrorPaths = new LinkedHashMap<>();
        for (Map.Entry<PlaywrightArtifactManifest.Artifact, Path> entry : archives.entrySet()) {
            PlaywrightArtifactManifest.Artifact artifact = entry.getKey();
            Path archive = entry.getValue().toAbsolutePath().normalize();
            VerifiedArtifactStore.requireUnlinkedAncestors(archive);
            if (!java.nio.file.Files.isRegularFile(archive, java.nio.file.LinkOption.NOFOLLOW_LINKS)
                    || java.nio.file.Files.size(archive) != artifact.size()
                    || !("sha256:" + VerifiedArtifactStore.digest(archive)).equals(artifact.checksum())) {
                throw new IOException("Playwright artifact changed after verification: " + artifact.name());
            }
            String requestPath = artifact.source().getRawPath();
            String legacyPrefix = "/dbazure/download/playwright";
            if (requestPath.startsWith(legacyPrefix + '/')) requestPath = requestPath.substring(legacyPrefix.length());
            if (mirrorPaths.put(requestPath, archive) != null) {
                throw new IOException("Duplicate Playwright mirror request path: " + requestPath);
            }
        }
        return mirrorPaths;
    }

    @FunctionalInterface
    interface CommandRunner {
        ReportingSetupService.ProcessResult run(List<String> command, Path log, Duration timeout,
                                                Map<String, String> environment,
                                                Set<String> removedEnvironment) throws IOException;
    }
}
