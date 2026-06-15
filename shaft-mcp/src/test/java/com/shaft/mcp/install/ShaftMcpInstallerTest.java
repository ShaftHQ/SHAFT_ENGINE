package com.shaft.mcp.install;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftMcpInstallerTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @ParameterizedTest
    @MethodSource("targets")
    void installsAndConfiguresEveryTarget(
            String flag,
            ShaftMcpInstaller.OperatingSystem operatingSystem,
            @TempDir Path temp) throws Exception {
        TestSetup setup = setup(temp, operatingSystem, "1.2.3", "jar-one");

        int exitCode = ShaftMcpInstaller.run(new String[]{flag}, setup.environment());

        assertEquals(0, exitCode);
        Path installed = setup.installedJar();
        assertTrue(Files.isRegularFile(installed));
        assertEquals("jar-one", Files.readString(installed));
        assertEquals(installed, setup.probedJar().get());
        assertConfigured(flag, setup, installed);
    }

    static Stream<Arguments> targets() {
        return Stream.of(
                Arguments.of("--codex", ShaftMcpInstaller.OperatingSystem.WINDOWS),
                Arguments.of("--codex-app", ShaftMcpInstaller.OperatingSystem.MACOS),
                Arguments.of("--claude", ShaftMcpInstaller.OperatingSystem.LINUX),
                Arguments.of("--claude-desktop", ShaftMcpInstaller.OperatingSystem.WINDOWS),
                Arguments.of("--copilot", ShaftMcpInstaller.OperatingSystem.MACOS),
                Arguments.of("--copilot-vscode", ShaftMcpInstaller.OperatingSystem.LINUX));
    }

    @Test
    @EnabledOnOs(OS.WINDOWS)
    void windowsCommandRunnerExecutesMavenBatchFile() {
        var result = new ShaftMcpInstaller.SystemCommandRunner(
                ShaftMcpInstaller.OperatingSystem.WINDOWS)
                .run(List.of("mvn", "--version"));

        assertEquals(0, result.exitCode(), result.output());
        assertTrue(result.output().contains("Apache Maven"), result.output());
    }

    @Test
    void repeatedInstallationReusesVerifiedJar(@TempDir Path temp) throws Exception {
        TestSetup setup = setup(temp, ShaftMcpInstaller.OperatingSystem.LINUX, "1.2.3", "same-jar");
        assertEquals(0, ShaftMcpInstaller.run(new String[]{"--copilot"}, setup.environment()));
        FileTime sentinel = FileTime.fromMillis(1_700_000_000_000L);
        Files.setLastModifiedTime(setup.installedJar(), sentinel);

        assertEquals(0, ShaftMcpInstaller.run(new String[]{"--copilot"}, setup.environment()));

        assertEquals(sentinel, Files.getLastModifiedTime(setup.installedJar()));
    }

    @Test
    void versionUpgradeKeepsOldJarAndPointsConfigurationToNewJar(@TempDir Path temp) throws Exception {
        TestSetup first = setup(temp, ShaftMcpInstaller.OperatingSystem.LINUX, "1.0.0", "old-jar");
        assertEquals(0, ShaftMcpInstaller.run(new String[]{"--copilot"}, first.environment()));
        TestSetup second = setup(temp, ShaftMcpInstaller.OperatingSystem.LINUX, "2.0.0", "new-jar");

        assertEquals(0, ShaftMcpInstaller.run(new String[]{"--copilot"}, second.environment()));

        assertEquals("old-jar", Files.readString(first.installedJar()));
        assertEquals("new-jar", Files.readString(second.installedJar()));
        JsonNode entry = JSON.readTree(Files.readString(second.copilotConfig()))
                .path("mcpServers").path("shaft-mcp");
        assertEquals(second.installedJar().toString(), entry.path("args").get(1).asText());
    }

    @Test
    void duplicateNamedEntryIsReplacedWithoutRemovingOtherServers(@TempDir Path temp) throws Exception {
        TestSetup setup = setup(temp, ShaftMcpInstaller.OperatingSystem.MACOS, "1.2.3", "jar");
        Files.createDirectories(setup.copilotConfig().getParent());
        Files.writeString(setup.copilotConfig(), """
                {
                  "mcpServers": {
                    "shaft-mcp": {"command": "old", "args": []},
                    "other": {"command": "keep", "args": []}
                  }
                }
                """);

        assertEquals(0, ShaftMcpInstaller.run(new String[]{"--copilot"}, setup.environment()));

        JsonNode servers = JSON.readTree(Files.readString(setup.copilotConfig())).path("mcpServers");
        assertEquals(2, servers.size());
        assertEquals("keep", servers.path("other").path("command").asText());
        assertEquals(setup.javaExecutable().toString(),
                servers.path("shaft-mcp").path("command").asText());
    }

    @Test
    void projectConfigurationConflictLeavesUserConfigurationUntouched(@TempDir Path temp) throws Exception {
        TestSetup setup = setup(temp, ShaftMcpInstaller.OperatingSystem.LINUX, "1.2.3", "jar");
        Path projectConfig = setup.workingDirectory().resolve(".vscode").resolve("mcp.json");
        Files.createDirectories(projectConfig.getParent());
        Files.writeString(projectConfig, """
                {"servers":{"shaft-mcp":{"command":"project","args":[]}}}
                """);
        Path userConfig = setup.vsCodeConfig();
        Files.createDirectories(userConfig.getParent());
        byte[] original = "{\"servers\":{\"keep\":{}}}\r\n".getBytes(StandardCharsets.UTF_8);
        Files.write(userConfig, original);

        int exitCode = ShaftMcpInstaller.run(
                new String[]{"--copilot-vscode"}, setup.environment());

        assertEquals(5, exitCode);
        assertArrayEquals(original, Files.readAllBytes(userConfig));
        assertFalse(Files.exists(setup.installedJar()));
    }

    @Test
    void rejectsUnsupportedDesktopOperatingSystem(@TempDir Path temp) throws Exception {
        TestSetup setup = setup(temp, ShaftMcpInstaller.OperatingSystem.LINUX, "1.2.3", "jar");

        int exitCode = ShaftMcpInstaller.run(
                new String[]{"--claude-desktop"}, setup.environment());

        assertEquals(3, exitCode);
        assertFalse(Files.exists(setup.installedJar()));
    }

    @Test
    void malformedJsonIsRestoredByteForByte(@TempDir Path temp) throws Exception {
        TestSetup setup = setup(temp, ShaftMcpInstaller.OperatingSystem.WINDOWS, "1.2.3", "jar");
        Path configuration = setup.claudeDesktopConfig();
        Files.createDirectories(configuration.getParent());
        byte[] original = """
                {"mcpServers":{"shaft-mcp":{},"shaft-mcp":{"command":"duplicate"}}}
                """.getBytes(StandardCharsets.UTF_8);
        Files.write(configuration, original);

        int exitCode = ShaftMcpInstaller.run(
                new String[]{"--claude-desktop"}, setup.environment());

        assertEquals(5, exitCode);
        assertArrayEquals(original, Files.readAllBytes(configuration));
    }

    @Test
    void failedClientCommandRollsBackExactOriginalBytes(@TempDir Path temp) throws Exception {
        TestSetup setup = setup(temp, ShaftMcpInstaller.OperatingSystem.WINDOWS, "1.2.3", "jar");
        Path configuration = setup.codexConfig();
        Files.createDirectories(configuration.getParent());
        byte[] original = "# original\r\n[mcp_servers.keep]\r\ncommand = \"keep\"\r\n"
                .getBytes(StandardCharsets.UTF_8);
        Files.write(configuration, original);
        setup.runner().failCodexAdd = true;

        int exitCode = ShaftMcpInstaller.run(new String[]{"--codex"}, setup.environment());

        assertEquals(5, exitCode);
        assertArrayEquals(original, Files.readAllBytes(configuration));
        try (var backups = Files.list(configuration.getParent())) {
            assertFalse(backups.anyMatch(path -> path.getFileName().toString().contains("shaft-mcp-backup")));
        }
    }

    @Test
    void rejectsMultipleFlagsInvalidJavaAndUnavailableClient(@TempDir Path temp) throws Exception {
        TestSetup setup = setup(temp, ShaftMcpInstaller.OperatingSystem.LINUX, "1.2.3", "jar");
        assertEquals(2, ShaftMcpInstaller.run(
                new String[]{"--codex", "--claude"}, setup.environment()));

        var invalidJava = environment(
                setup,
                24,
                setup.runner(),
                setup.probedJar(),
                true);
        assertEquals(3, ShaftMcpInstaller.run(new String[]{"--codex"}, invalidJava));

        setup.runner().mavenVersion = "3.8.9";
        assertEquals(3, ShaftMcpInstaller.run(new String[]{"--codex"}, setup.environment()));
        setup.runner().mavenVersion = "3.9.12";

        setup.runner().unavailableCommands.add("copilot");
        assertEquals(3, ShaftMcpInstaller.run(new String[]{"--copilot"}, setup.environment()));
    }

    @Test
    void failedProbeDoesNotChangeClientConfiguration(@TempDir Path temp) throws Exception {
        TestSetup setup = setup(temp, ShaftMcpInstaller.OperatingSystem.LINUX, "1.2.3", "jar");
        Path configuration = setup.copilotConfig();
        Files.createDirectories(configuration.getParent());
        byte[] original = "{\"mcpServers\":{\"keep\":{}}}\n".getBytes(StandardCharsets.UTF_8);
        Files.write(configuration, original);
        var environment = new ShaftMcpInstaller.InstallerEnvironment(
                setup.environment().operatingSystem(),
                setup.environment().environmentVariables(),
                setup.environment().userHome(),
                setup.environment().workingDirectory(),
                setup.environment().javaExecutable(),
                setup.environment().javaFeatureVersion(),
                setup.environment().sourceJar(),
                setup.environment().version(),
                setup.environment().applicationDataRoot(),
                setup.runner(),
                (javaExecutable, jar) -> {
                    throw new ShaftMcpInstaller.ProbeFailure(new IOException("probe failed"));
                },
                true);

        int exitCode = ShaftMcpInstaller.run(new String[]{"--copilot"}, environment);

        assertEquals(4, exitCode);
        assertArrayEquals(original, Files.readAllBytes(configuration));
    }

    private static TestSetup setup(
            Path temp,
            ShaftMcpInstaller.OperatingSystem operatingSystem,
            String version,
            String jarContent) throws IOException {
        Path root = temp.resolve(operatingSystem.name().toLowerCase()).resolve(version);
        Path home = root.resolve("home");
        Path work = root.resolve("work").resolve("repo");
        Path appData = root.resolve("application-data").resolve("shaft-mcp");
        Path source = root.resolve("bootstrap").resolve("shaft-mcp.jar");
        Path java = root.resolve("jdk").resolve("bin")
                .resolve(operatingSystem == ShaftMcpInstaller.OperatingSystem.WINDOWS ? "java.exe" : "java");
        Files.createDirectories(source.getParent());
        Files.createDirectories(java.getParent());
        Files.createDirectories(work);
        Files.writeString(source, jarContent);
        Files.writeString(java, "fake-java");

        Map<String, String> variables = new HashMap<>();
        variables.put("APPDATA", root.resolve("appdata").toString());
        variables.put("LOCALAPPDATA", root.resolve("localappdata").toString());
        variables.put("XDG_CONFIG_HOME", root.resolve("xdg-config").toString());
        variables.put("XDG_DATA_HOME", root.resolve("xdg-data").toString());
        variables.put("CODEX_HOME", home.resolve(".codex").toString());
        variables.put("COPILOT_HOME", home.resolve(".copilot").toString());

        AtomicReference<Path> probedJar = new AtomicReference<>();
        FakeCommandRunner runner = new FakeCommandRunner(operatingSystem, variables, home);
        var environment = new ShaftMcpInstaller.InstallerEnvironment(
                operatingSystem,
                variables,
                home,
                work,
                java,
                25,
                source,
                version,
                appData,
                runner,
                (javaExecutable, jar) -> probedJar.set(jar),
                true);
        return new TestSetup(environment, runner, probedJar);
    }

    private static ShaftMcpInstaller.InstallerEnvironment environment(
            TestSetup setup,
            int javaFeature,
            FakeCommandRunner runner,
            AtomicReference<Path> probe,
            boolean desktopInstalled) {
        var current = setup.environment();
        return new ShaftMcpInstaller.InstallerEnvironment(
                current.operatingSystem(),
                current.environmentVariables(),
                current.userHome(),
                current.workingDirectory(),
                current.javaExecutable(),
                javaFeature,
                current.sourceJar(),
                current.version(),
                current.applicationDataRoot(),
                runner,
                (javaExecutable, jar) -> probe.set(jar),
                desktopInstalled);
    }

    private static void assertConfigured(String flag, TestSetup setup, Path installed) throws Exception {
        if ("--codex".equals(flag) || "--codex-app".equals(flag)) {
            assertTrue(Files.readString(setup.codexConfig()).contains("[mcp_servers.shaft-mcp]"));
            assertEquals(installed, setup.runner().configuredJar);
            return;
        }
        Path configuration = switch (flag) {
            case "--claude" -> setup.claudeCodeConfig();
            case "--claude-desktop" -> setup.claudeDesktopConfig();
            case "--copilot" -> setup.copilotConfig();
            case "--copilot-vscode" -> setup.vsCodeConfig();
            default -> throw new IllegalArgumentException(flag);
        };
        JsonNode root = JSON.readTree(Files.readString(configuration));
        JsonNode entry = "--copilot-vscode".equals(flag)
                ? root.path("servers").path("shaft-mcp")
                : root.path("mcpServers").path("shaft-mcp");
        assertEquals(setup.javaExecutable().toString(), entry.path("command").asText());
        assertEquals(List.of("-jar", installed.toString()),
                JSON.convertValue(entry.path("args"), JSON.getTypeFactory().constructCollectionType(List.class, String.class)));
    }

    private record TestSetup(
            ShaftMcpInstaller.InstallerEnvironment environment,
            FakeCommandRunner runner,
            AtomicReference<Path> probedJar) {

        Path installedJar() {
            return environment.applicationDataRoot().resolve("versions")
                    .resolve(environment.version()).resolve("shaft-mcp.jar").toAbsolutePath().normalize();
        }

        Path javaExecutable() {
            return environment.javaExecutable();
        }

        Path workingDirectory() {
            return environment.workingDirectory();
        }

        Path codexConfig() {
            return Path.of(environment.environmentVariables().get("CODEX_HOME")).resolve("config.toml");
        }

        Path claudeCodeConfig() {
            return environment.userHome().resolve(".claude.json");
        }

        Path claudeDesktopConfig() {
            return environment.operatingSystem() == ShaftMcpInstaller.OperatingSystem.WINDOWS
                    ? Path.of(environment.environmentVariables().get("APPDATA"))
                            .resolve("Claude").resolve("claude_desktop_config.json")
                    : environment.userHome().resolve("Library").resolve("Application Support")
                            .resolve("Claude").resolve("claude_desktop_config.json");
        }

        Path copilotConfig() {
            return Path.of(environment.environmentVariables().get("COPILOT_HOME")).resolve("mcp-config.json");
        }

        Path vsCodeConfig() {
            return switch (environment.operatingSystem()) {
                case WINDOWS -> Path.of(environment.environmentVariables().get("APPDATA"))
                        .resolve("Code").resolve("User").resolve("mcp.json");
                case MACOS -> environment.userHome().resolve("Library").resolve("Application Support")
                        .resolve("Code").resolve("User").resolve("mcp.json");
                case LINUX -> Path.of(environment.environmentVariables().get("XDG_CONFIG_HOME"))
                        .resolve("Code").resolve("User").resolve("mcp.json");
            };
        }
    }

    private static final class FakeCommandRunner implements ShaftMcpInstaller.CommandRunner {
        private final ShaftMcpInstaller.OperatingSystem operatingSystem;
        private final Map<String, String> variables;
        private final Path home;
        private final List<String> unavailableCommands = new ArrayList<>();
        private boolean failCodexAdd;
        private String mavenVersion = "3.9.12";
        private Path configuredJava;
        private Path configuredJar;

        private FakeCommandRunner(
                ShaftMcpInstaller.OperatingSystem operatingSystem,
                Map<String, String> variables,
                Path home) {
            this.operatingSystem = operatingSystem;
            this.variables = variables;
            this.home = home;
        }

        @Override
        public ShaftMcpInstaller.CommandResult run(List<String> command) {
            try {
                if ("mvn".equals(command.getFirst())) {
                    return success("Apache Maven " + mavenVersion);
                }
                if (command.size() == 2 && "--version".equals(command.get(1))) {
                    return unavailableCommands.contains(command.getFirst())
                            ? failure("not found")
                            : success(command.getFirst() + " test");
                }
                if (command.size() >= 4 && command.subList(0, 3).equals(List.of("codex", "mcp", "add"))) {
                    captureCommand(command);
                    Path config = Path.of(variables.get("CODEX_HOME")).resolve("config.toml");
                    Files.createDirectories(config.getParent());
                    Files.writeString(config, failCodexAdd
                            ? "damaged"
                            : "[mcp_servers.shaft-mcp]\ncommand = \"" + configuredJava + "\"\n");
                    return failCodexAdd ? failure("simulated failure") : success("");
                }
                if (command.equals(List.of("codex", "mcp", "get", "shaft-mcp", "--json"))) {
                    ObjectNode transport = JSON.createObjectNode();
                    transport.put("command", configuredJava.toString());
                    transport.putArray("args").add("-jar").add(configuredJar.toString());
                    ObjectNode result = JSON.createObjectNode();
                    result.set("transport", transport);
                    return success(JSON.writeValueAsString(result));
                }
                if (command.size() >= 5 && command.subList(0, 3).equals(List.of("claude", "mcp", "remove"))) {
                    writeClaude(null, null);
                    return success("");
                }
                if (command.size() >= 6 && command.subList(0, 3).equals(List.of("claude", "mcp", "add"))) {
                    captureCommand(command);
                    writeClaude(configuredJava, configuredJar);
                    return success("");
                }
                if (command.size() == 3 && command.subList(0, 2).equals(List.of("code", "--add-mcp"))) {
                    JsonNode entry = JSON.readTree(command.get(2));
                    Path config = vsCodeConfig();
                    ObjectNode root = JSON.createObjectNode();
                    ObjectNode stored = entry.deepCopy();
                    stored.remove("name");
                    root.putObject("servers").set("shaft-mcp", stored);
                    InstallerFileOperations.writeJsonAtomically(config, root);
                    return success("");
                }
                return failure("unexpected command: " + command);
            } catch (IOException exception) {
                return failure(exception.getMessage());
            }
        }

        private void captureCommand(List<String> command) {
            int separator = command.indexOf("--");
            configuredJava = Path.of(command.get(separator + 1)).toAbsolutePath().normalize();
            configuredJar = Path.of(command.get(separator + 3)).toAbsolutePath().normalize();
        }

        private void writeClaude(Path java, Path jar) throws IOException {
            ObjectNode root = JSON.createObjectNode();
            ObjectNode servers = root.putObject("mcpServers");
            if (java != null && jar != null) {
                ObjectNode entry = servers.putObject("shaft-mcp");
                entry.put("command", java.toString());
                entry.putArray("args").add("-jar").add(jar.toString());
            }
            InstallerFileOperations.writeJsonAtomically(home.resolve(".claude.json"), root);
        }

        private Path vsCodeConfig() {
            return switch (operatingSystem) {
                case WINDOWS -> Path.of(variables.get("APPDATA"))
                        .resolve("Code").resolve("User").resolve("mcp.json");
                case MACOS -> home.resolve("Library").resolve("Application Support")
                        .resolve("Code").resolve("User").resolve("mcp.json");
                case LINUX -> Path.of(variables.get("XDG_CONFIG_HOME"))
                        .resolve("Code").resolve("User").resolve("mcp.json");
            };
        }

        private static ShaftMcpInstaller.CommandResult success(String output) {
            return new ShaftMcpInstaller.CommandResult(0, output);
        }

        private static ShaftMcpInstaller.CommandResult failure(String output) {
            return new ShaftMcpInstaller.CommandResult(1, output);
        }
    }
}
