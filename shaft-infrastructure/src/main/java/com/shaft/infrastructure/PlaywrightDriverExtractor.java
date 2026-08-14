package com.shaft.infrastructure;

import com.microsoft.playwright.impl.driver.Driver;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Comparator;
import java.util.Map;

/** Extracts the version-matched packaged Playwright driver without bundled Node or global properties. */
final class PlaywrightDriverExtractor {
    private PlaywrightDriverExtractor() { }

    static void extract(Path nodeExecutable, Path destination) throws IOException {
        Path node = nodeExecutable.toAbsolutePath().normalize();
        Path target = destination.toAbsolutePath().normalize();
        VerifiedArtifactStore.requireUnlinkedAncestors(node);
        VerifiedArtifactStore.requireUnlinkedAncestors(target);
        if (!Files.isRegularFile(node, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("Managed portable Node executable is not a regular unlinked file: " + node);
        }
        if (Files.exists(target, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("Playwright driver extraction destination already exists: " + target);
        }

        Driver driver = null;
        boolean complete = false;
        try {
            driver = Driver.createAndInstall(Map.of("PLAYWRIGHT_NODEJS_PATH", node.toString()), Boolean.FALSE);
            Path source = driver.driverDir().toAbsolutePath().normalize();
            requireVersionMatchedPackage(source);
            Files.createDirectories(target.getParent());
            Files.createDirectory(target);
            try (var entries = Files.walk(source)) {
                for (Path entry : entries.toList()) {
                    if (Files.isSymbolicLink(entry)) {
                        throw new IOException("Packaged Playwright driver contains a symbolic link: " + entry);
                    }
                    Path relative = source.relativize(entry);
                    Path output = target.resolve(relative.toString()).normalize();
                    if (!output.startsWith(target)) {
                        throw new IOException("Packaged Playwright driver escapes its destination: " + entry);
                    }
                    if (Files.isDirectory(entry, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
                        Files.createDirectories(output);
                    } else {
                        Files.copy(entry, output, StandardCopyOption.COPY_ATTRIBUTES);
                    }
                }
            }
            complete = true;
        } catch (RuntimeException failure) {
            throw new IOException("Unable to extract the packaged Playwright driver.", failure);
        } finally {
            if (!complete) deleteTree(target);
            if (driver != null) deleteTree(driver.driverDir());
        }
    }

    private static void requireVersionMatchedPackage(Path driverRoot) throws IOException {
        Path cli = driverRoot.resolve("package/cli.js");
        Path browsers = driverRoot.resolve("package/browsers.json");
        if (!Files.isRegularFile(cli, java.nio.file.LinkOption.NOFOLLOW_LINKS)
                || !Files.isRegularFile(browsers, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("Packaged Playwright driver is incomplete.");
        }
        String metadata = Files.readString(browsers);
        for (String required : java.util.List.of("\"revision\": \"1234\"", "\"revision\": \"1538\"",
                "\"revision\": \"2336\"", "\"revision\": \"1011\"")) {
            if (!metadata.contains(required)) {
                throw new IOException("Packaged Playwright driver does not match release "
                        + PlaywrightSetupPlanner.PLAYWRIGHT_VERSION + '.');
            }
        }
    }

    private static void deleteTree(Path root) throws IOException {
        if (root == null || Files.notExists(root, java.nio.file.LinkOption.NOFOLLOW_LINKS)) return;
        try (var entries = Files.walk(root)) {
            for (Path entry : entries.sorted(Comparator.reverseOrder()).toList()) Files.deleteIfExists(entry);
        }
    }
}
