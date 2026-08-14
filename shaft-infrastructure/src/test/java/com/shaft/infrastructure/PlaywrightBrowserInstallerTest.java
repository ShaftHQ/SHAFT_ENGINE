package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.net.HttpURLConnection;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PlaywrightBrowserInstallerTest {
    @Test
    void acceptsTheExactUbuntuArtifactSetWithoutWinldd(@TempDir Path temp) throws Exception {
        Path node = Files.writeString(temp.resolve("node"), "node");
        Path cli = temp.resolve("driver/package/cli.js");
        Files.createDirectories(cli.getParent());
        Files.writeString(cli, "cli");
        Map<PlaywrightArtifactManifest.Artifact, Path> archives = verifiedArchives(
                temp, PlaywrightArtifactManifest.load().requirePlatform("ubuntu24.04-x64"));
        AtomicReference<Map<String, String>> environment = new AtomicReference<>();
        PlaywrightBrowserInstaller installer = new PlaywrightBrowserInstaller(
                (command, log, timeout, childEnvironment, removed) -> {
                    environment.set(Map.copyOf(childEnvironment));
                    return new ReportingSetupService.ProcessResult(0, "installed");
                });

        installer.install(node, temp.resolve("driver"), temp.resolve("browsers"), archives,
                temp.resolve("install.log"), Duration.ofMinutes(3),
                PlaywrightHostPlatform.UBUNTU_24_04_X64);

        assertTrue(environment.get().get("PLAYWRIGHT_BROWSERS_PATH").endsWith("browsers"));
    }

    @Test
    void installsThroughVerifiedLoopbackMirrorWithChildOnlyEnvironment(@TempDir Path temp) throws Exception {
        Path node = Files.writeString(temp.resolve("node.exe"), "node");
        Path driver = temp.resolve("driver");
        Path cli = driver.resolve("package/cli.js");
        Files.createDirectories(cli.getParent());
        Files.writeString(cli, "cli");
        Path browsers = temp.resolve("browsers-staging");
        List<PlaywrightArtifactManifest.Artifact> manifest = PlaywrightArtifactManifest.load()
                .requirePlatform("win64");
        Map<PlaywrightArtifactManifest.Artifact, Path> archives = new LinkedHashMap<>();
        for (PlaywrightArtifactManifest.Artifact artifact : manifest) {
            Path archive = Files.writeString(temp.resolve(artifact.name() + ".zip"), artifact.name());
            archives.put(new PlaywrightArtifactManifest.Artifact(artifact.name(), artifact.revision(),
                    artifact.source(), "sha256:" + VerifiedArtifactStore.digest(archive), Files.size(archive)), archive);
        }
        AtomicReference<Map<String, String>> childEnvironment = new AtomicReference<>();
        AtomicReference<Set<String>> removedEnvironment = new AtomicReference<>();
        PlaywrightBrowserInstaller installer = new PlaywrightBrowserInstaller(
                (command, log, timeout, environment, removed) -> {
                    childEnvironment.set(Map.copyOf(environment));
                    removedEnvironment.set(Set.copyOf(removed));
                    assertEquals(List.of(node.toString(), cli.toString(), "install", "chromium", "firefox", "webkit"),
                            command);
                    String mirror = environment.get("PLAYWRIGHT_DOWNLOAD_HOST");
                    HttpURLConnection connection = (HttpURLConnection) java.net.URI.create(mirror)
                            .resolve("builds/firefox/1538/firefox-win64.zip").toURL().openConnection();
                    assertEquals(200, connection.getResponseCode());
                    assertEquals("firefox", new String(connection.getInputStream().readAllBytes()));
                    connection.disconnect();
                    return new ReportingSetupService.ProcessResult(0, "installed");
                });

        installer.install(node, driver, browsers, archives, temp.resolve("install.log"), Duration.ofMinutes(3));

        Map<String, String> environment = childEnvironment.get();
        assertEquals(browsers.toString(), environment.get("PLAYWRIGHT_BROWSERS_PATH"));
        assertEquals(node.toString(), environment.get("PLAYWRIGHT_NODEJS_PATH"));
        assertEquals(environment.get("PLAYWRIGHT_DOWNLOAD_HOST"),
                environment.get("PLAYWRIGHT_CHROMIUM_DOWNLOAD_HOST"));
        assertEquals(environment.get("PLAYWRIGHT_DOWNLOAD_HOST"),
                environment.get("PLAYWRIGHT_FIREFOX_DOWNLOAD_HOST"));
        assertEquals(environment.get("PLAYWRIGHT_DOWNLOAD_HOST"),
                environment.get("PLAYWRIGHT_WEBKIT_DOWNLOAD_HOST"));
        assertEquals("1", environment.get("PLAYWRIGHT_SKIP_BROWSER_GC"));
        assertTrue(removedEnvironment.get().contains("HTTPS_PROXY"));
        assertTrue(removedEnvironment.get().contains("HTTP_PROXY"));
        assertFalse(environment.values().stream().anyMatch(value -> value.contains("cdn.playwright.dev")));
    }

    private static Map<PlaywrightArtifactManifest.Artifact, Path> verifiedArchives(
            Path temp, List<PlaywrightArtifactManifest.Artifact> manifest) throws Exception {
        Map<PlaywrightArtifactManifest.Artifact, Path> archives = new LinkedHashMap<>();
        for (PlaywrightArtifactManifest.Artifact artifact : manifest) {
            Path archive = Files.writeString(temp.resolve(artifact.name() + ".zip"), artifact.name());
            archives.put(new PlaywrightArtifactManifest.Artifact(artifact.name(), artifact.revision(),
                    artifact.source(), "sha256:" + VerifiedArtifactStore.digest(archive), Files.size(archive)), archive);
        }
        return archives;
    }
}
