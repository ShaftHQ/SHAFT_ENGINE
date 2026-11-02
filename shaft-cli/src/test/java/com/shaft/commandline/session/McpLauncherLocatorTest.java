package com.shaft.commandline.session;

import com.shaft.commandline.mcp.McpException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for {@link McpLauncherLocator}'s discovery order, driven entirely against temp
 * directories via its package-private overridable constructor. No real process is launched.
 */
class McpLauncherLocatorTest {

    @Test
    void envOverrideWithSiblingArgsFileUsesArgfileLaunch(@TempDir Path tempDir) throws IOException {
        Path jar = tempDir.resolve("shaft-mcp.jar");
        Files.createFile(jar);
        Path argsFile = tempDir.resolve("shaft-mcp.args");
        Files.createFile(argsFile);

        McpLauncherLocator locator = new McpLauncherLocator(
                Map.of("SHAFT_MCP_JAR", jar.toString()), tempDir.resolve("no-versions"), tempDir.resolve("no-dev"));

        LaunchSpec spec = locator.locate();

        assertEquals(jar.toString(), spec.jarPath());
        assertEquals("@" + argsFile, spec.baseCommand().get(spec.baseCommand().size() - 1));
    }

    @Test
    void envOverridePointingDirectlyAtArgsFileUsesItAsArgfile(@TempDir Path tempDir) throws IOException {
        Path argsFile = tempDir.resolve("custom.args");
        Files.createFile(argsFile);

        McpLauncherLocator locator = new McpLauncherLocator(
                Map.of("SHAFT_MCP_JAR", argsFile.toString()),
                tempDir.resolve("no-versions"),
                tempDir.resolve("no-dev"));

        LaunchSpec spec = locator.locate();

        assertEquals(List.of(spec.baseCommand().get(0), "@" + argsFile), spec.baseCommand());
    }

    @Test
    void envOverridePointingAtPlainJarUsesClasspathLaunch(@TempDir Path tempDir) throws IOException {
        Path jar = tempDir.resolve("standalone-shaft-mcp.jar");
        Files.createFile(jar);

        McpLauncherLocator locator = new McpLauncherLocator(
                Map.of("SHAFT_MCP_JAR", jar.toString()), tempDir.resolve("no-versions"), tempDir.resolve("no-dev"));

        LaunchSpec spec = locator.locate();

        assertEquals(List.of(spec.baseCommand().get(0), "-cp", jar.toString(), "com.shaft.mcp.ShaftMcpApplication"),
                spec.baseCommand());
    }

    @Test
    void envOverridePointingAtMissingFileThrows(@TempDir Path tempDir) {
        Path missing = tempDir.resolve("does-not-exist.jar");

        McpLauncherLocator locator = new McpLauncherLocator(
                Map.of("SHAFT_MCP_JAR", missing.toString()),
                tempDir.resolve("no-versions"),
                tempDir.resolve("no-dev"));

        assertThrows(McpException.class, locator::locate);
    }

    @Test
    void versionsDirectorySelectsNewestSubdirWithArgsFile(@TempDir Path tempDir) throws IOException {
        Path versionsRoot = tempDir.resolve("versions");
        Path older = versionsRoot.resolve("1.0.0");
        Files.createDirectories(older);
        Files.createFile(older.resolve("shaft-mcp.args"));

        Path newer = versionsRoot.resolve("2.0.0");
        Files.createDirectories(newer);
        Files.createFile(newer.resolve("shaft-mcp.args"));

        // Lexicographically largest but has no shaft-mcp.args, so it must be skipped.
        Path incomplete = versionsRoot.resolve("3.0.0");
        Files.createDirectories(incomplete);

        McpLauncherLocator locator = new McpLauncherLocator(Map.of(), versionsRoot, tempDir.resolve("no-dev"));

        LaunchSpec spec = locator.locate();

        Path expectedArgs = newer.resolve("shaft-mcp.args");
        assertEquals(List.of(spec.baseCommand().get(0), "@" + expectedArgs), spec.baseCommand());
        assertEquals(newer.resolve("shaft-mcp.jar").toString(), spec.jarPath());
    }

    @Test
    void nothingFoundThrowsActionableException(@TempDir Path tempDir) throws IOException {
        Path emptyVersions = tempDir.resolve("versions");
        Files.createDirectories(emptyVersions);
        Path emptyDev = tempDir.resolve("dev");
        Files.createDirectories(emptyDev);

        McpLauncherLocator locator = new McpLauncherLocator(Map.of(), emptyVersions, emptyDev);

        McpException exception = assertThrows(McpException.class, locator::locate);
        assertTrue(exception.getMessage().contains("SHAFT_MCP_JAR"));
    }

    @Test
    void blankEnvOverrideUsesXdgVersionsDirectory(@TempDir Path tempDir) throws IOException {
        org.junit.jupiter.api.Assumptions.assumeTrue(
                System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("linux"),
                "XDG data home is the Linux installer root");
        Path version = tempDir.resolve("xdg/shafthq/shaft-mcp/versions/9.9.9");
        Files.createDirectories(version);
        Files.createFile(version.resolve("shaft-mcp.args"));

        McpLauncherLocator locator = new McpLauncherLocator(
                Map.of("SHAFT_MCP_JAR", "  ", "XDG_DATA_HOME", tempDir.resolve("xdg").toString()),
                null,
                tempDir.resolve("no-dev"));

        LaunchSpec spec = locator.locate();

        assertEquals(version.resolve("shaft-mcp.jar").toString(), spec.jarPath());
        assertTrue(spec.baseCommand().get(1).endsWith(version.resolve("shaft-mcp.args").toString()));
    }

    @Test
    void devJarWithoutAManifestNamesTheMissingEntry(@TempDir Path tempDir) throws IOException {
        Path thinJar = thinJar(tempDir, "shaft-mcp-1.0.0.jar", null);

        McpException failure = assertThrows(McpException.class,
                () -> new McpLauncherLocator(Map.of(), tempDir.resolve("no-versions"), devBase(thinJar)).locate());

        assertTrue(failure.getMessage().contains("META-INF/shaft-mcp/runtime-dependencies.txt"));
    }

    @Test
    void corruptDevJarFailsBeforeDependencyResolution(@TempDir Path tempDir) throws IOException {
        Path thinJar = tempDir.resolve("checkout/shaft-mcp/target/shaft-mcp-1.0.0.jar");
        Files.createDirectories(thinJar.getParent());
        Files.writeString(thinJar, "not-a-zip");

        McpException failure = assertThrows(McpException.class,
                () -> new McpLauncherLocator(Map.of(), tempDir.resolve("no-versions"), devBase(thinJar)).locate());

        assertTrue(failure.getMessage().contains("Failed to read"));
    }

    @Test
    void devManifestSkipsUnusableCoordinatesAndRejectsAnEmptyResolution(@TempDir Path tempDir) throws IOException {
        String manifest = """
                The following files have been resolved:
                
                not-a-coordinate
                org.example:demo:pom:1.0:compile
                org.example:demo:jar:1.0:test
                org.example:demo:jar:tests:1.0:test
                org.example:missing:jar:cls:1.0:compile
                """;
        Path thinJar = thinJar(tempDir, "shaft-mcp-2.0.0.jar", manifest);

        McpException failure = assertThrows(McpException.class,
                () -> new McpLauncherLocator(Map.of(), tempDir.resolve("no-versions"), devBase(thinJar)).locate());

        assertTrue(failure.getMessage().contains("no resolvable runtime dependencies"));
    }

    @Test
    void devFallbackWritesAnArgfileWhenALocalRepositoryJarResolves(@TempDir Path tempDir) throws IOException {
        Path repositoryJar = localRepositoryJar();
        org.junit.jupiter.api.Assumptions.assumeTrue(repositoryJar != null,
                "Maven local repository jar required to prove dependency resolution");
        Path relative = Path.of(System.getProperty("user.home"), ".m2", "repository").relativize(repositoryJar);
        int count = relative.getNameCount();
        String artifact = relative.getName(count - 3).toString();
        String version = relative.getName(count - 2).toString();
        String group = relative.subpath(0, count - 3).toString().replace(File.separatorChar, '.');
        String manifest = "The following files have been resolved:\n"
                + group + ":" + artifact + ":jar:" + version + ":compile\n"
                + "org.example:missing:jar:1.0:compile\n";
        Path thinJar = thinJar(tempDir, "shaft-mcp-3.0.0.jar", manifest);

        LaunchSpec spec = new McpLauncherLocator(Map.of(), tempDir.resolve("no-versions"), devBase(thinJar)).locate();

        assertEquals(thinJar.toString(), spec.jarPath());
        Path argsFile = thinJar.resolveSibling("shaft-mcp-dev.args");
        String args = Files.readString(argsFile);
        assertTrue(args.startsWith("-cp" + System.lineSeparator()));
        assertTrue(args.contains(repositoryJar.toString().replace('\\', '/')));
        assertTrue(args.contains("com.shaft.mcp.ShaftMcpApplication"));
        assertEquals("@" + argsFile.toAbsolutePath().normalize(), spec.baseCommand().get(1));
    }

    private static Path devBase(Path thinJar) {
        return thinJar.getParent().getParent().getParent();
    }

    private static Path thinJar(Path tempDir, String name, String manifest) throws IOException {
        Path thinJar = tempDir.resolve("checkout/shaft-mcp/target").resolve(name);
        Files.createDirectories(thinJar.getParent());
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(thinJar))) {
            if (manifest != null) {
                zip.putNextEntry(new ZipEntry("META-INF/shaft-mcp/runtime-dependencies.txt"));
                zip.write(manifest.getBytes(StandardCharsets.UTF_8));
                zip.closeEntry();
            } else {
                zip.putNextEntry(new ZipEntry("META-INF/MANIFEST.MF"));
                zip.write("Manifest-Version: 1.0\n".getBytes(StandardCharsets.UTF_8));
                zip.closeEntry();
            }
        }
        return thinJar;
    }

    private static Path localRepositoryJar() {
        Path repository = Path.of(System.getProperty("user.home"), ".m2", "repository");
        return Arrays.stream(System.getProperty("java.class.path").split(File.pathSeparator))
                .map(Path::of)
                .filter(path -> path.startsWith(repository) && path.getFileName().toString().endsWith(".jar"))
                .filter(path -> {
                    Path relative = repository.relativize(path);
                    if (relative.getNameCount() < 3) {
                        return false;
                    }
                    String artifact = relative.getName(relative.getNameCount() - 3).toString();
                    String version = relative.getName(relative.getNameCount() - 2).toString();
                    return path.getFileName().toString().equals(artifact + "-" + version + ".jar");
                })
                .findFirst()
                .orElse(null);
    }
}
