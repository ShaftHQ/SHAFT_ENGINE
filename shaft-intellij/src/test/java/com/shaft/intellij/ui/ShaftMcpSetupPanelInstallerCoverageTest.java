package com.shaft.intellij.ui;

import com.intellij.openapi.project.Project;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import javax.swing.SwingUtilities;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers {@link ShaftMcpSetupPanel} public installer helpers and headless construction without
 * reflection.
 */
class ShaftMcpSetupPanelInstallerCoverageTest {

    @BeforeAll
    static void forceHeadless() {
        System.setProperty("java.awt.headless", "true");
    }

    @Test
    void installerTargetNamesAndArgumentsCoverEveryKnownTarget() {
        List<String> names = ShaftMcpSetupPanel.installerTargetNames();
        assertFalse(names.isEmpty());
        assertAll(
                () -> assertEquals("claude", ShaftMcpSetupPanel.installerArgumentFor("CLAUDE_CODE")),
                () -> assertEquals("claude-desktop", ShaftMcpSetupPanel.installerArgumentFor("CLAUDE_DESKTOP")),
                () -> assertEquals("copilot", ShaftMcpSetupPanel.installerArgumentFor("COPILOT_CLI")),
                () -> assertEquals("copilot-intellij", ShaftMcpSetupPanel.installerArgumentFor("COPILOT_INTELLIJ")),
                () -> assertEquals("intellij-plugin", ShaftMcpSetupPanel.installerArgumentFor("INTELLIJ_PLUGIN")),
                () -> assertEquals("grok", ShaftMcpSetupPanel.installerArgumentFor("GROK")),
                () -> assertEquals("antigravity", ShaftMcpSetupPanel.installerArgumentFor("ANTIGRAVITY")),
                () -> assertEquals("codex", ShaftMcpSetupPanel.installerArgumentFor("CODEX")),
                () -> assertEquals("", ShaftMcpSetupPanel.installerArgumentFor("")),
                () -> assertEquals("", ShaftMcpSetupPanel.installerArgumentFor(null)),
                () -> assertEquals("codex", ShaftMcpSetupPanel.installerArgumentFor("UNKNOWN_TARGET")));
        for (String name : names) {
            String argument = ShaftMcpSetupPanel.installerArgumentFor(name);
            assertFalse(argument.isBlank(), name);
            String command = ShaftMcpSetupPanel.installerCommandFor(argument);
            assertFalse(command.isBlank(), name);
            assertTrue(command.contains("install-shaft-agentic-tools") || command.contains("powershell"), name);
            String bodyNoCli = ShaftMcpSetupPanel.installerScriptBody(argument, false);
            String bodyWithCli = ShaftMcpSetupPanel.installerScriptBody(argument, true);
            assertAll(
                    () -> assertTrue(bodyNoCli.contains("--install-shaft-skills"), name),
                    () -> assertFalse(bodyNoCli.contains("--install-shaft-cli"), name),
                    () -> assertTrue(bodyWithCli.contains("--install-shaft-cli"), name));
        }
    }

    @Test
    void inferInstalledStdioCommandAndVersionReadApplicationDataLayout(@TempDir Path temp) throws Exception {
        Path appData = temp.resolve("appdata");
        Path bootstrap = temp.resolve("bootstrap");
        Files.createDirectories(appData);
        Files.createDirectories(bootstrap);
        assertEquals("", ShaftMcpSetupPanel.inferInstalledStdioCommand(appData, bootstrap));
        assertEquals("", ShaftMcpSetupPanel.installedShaftMcpVersion(appData));

        Path versionDir = appData.resolve("shaft-mcp").resolve("10.4.20261102");
        Files.createDirectories(versionDir);
        Path args = versionDir.resolve("shaft-mcp.args");
        Files.writeString(args, "\"/fake/java\"\n\"-jar\"\n\"/fake/shaft-mcp.jar\"\n");
        String inferred = ShaftMcpSetupPanel.inferInstalledStdioCommand(appData, bootstrap);
        assertNotNull(inferred);
        String version = ShaftMcpSetupPanel.installedShaftMcpVersion(appData);
        assertTrue(version.isBlank() || version.contains("10.4") || version.contains("2026"), version);
    }

    @Test
    void headlessConstructionDoesNotThrowForReadyAndBlankSettings() throws Exception {
        ShaftSettingsState.Settings ready = new ShaftSettingsState.Settings();
        ready.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        ready.mcpSetupComplete = true;
        ready.assistantFamily = "CODEX";

        ShaftSettingsState.Settings blank = new ShaftSettingsState.Settings();
        blank.assistantFamily = "GROK";

        Project project = fakeProject();
        SwingUtilities.invokeAndWait(() -> {
            ShaftMcpSetupPanel readyPanel = new ShaftMcpSetupPanel(
                    project, ready, () -> { }, (client, runtime) -> null, (client, runtime) -> null,
                    emptyKeyStore());
            ShaftMcpSetupPanel blankPanel = new ShaftMcpSetupPanel(
                    project, blank, () -> { }, (client, runtime) -> null, (client, runtime) -> null,
                    emptyKeyStore());
            assertNotNull(readyPanel);
            assertNotNull(blankPanel);
            readyPanel.dispose();
            blankPanel.dispose();
        });
    }

    private static Project fakeProject() {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, args) -> switch (method.getName()) {
                    case "equals" -> proxy == (args == null || args.length == 0 ? null : args[0]);
                    case "hashCode" -> System.identityHashCode(proxy);
                    case "getBasePath" -> System.getProperty("java.io.tmpdir");
                    case "getName", "getLocationHash" -> "coverage-mcp-setup";
                    case "getService" -> null;
                    default -> defaultProjectValue(method.getReturnType());
                });
    }

    private static Object defaultProjectValue(Class<?> returnType) {
        if (returnType == boolean.class) {
            return false;
        }
        if (returnType == int.class) {
            return 0;
        }
        if (returnType == long.class) {
            return 0L;
        }
        return null;
    }

    private static ShaftMcpSetupPanel.CloudKeyStore emptyKeyStore() {
        return new ShaftMcpSetupPanel.CloudKeyStore() {
            @Override
            public boolean hasKey(String keyName) {
                return false;
            }

            @Override
            public void saveKey(String keyName, char[] secret) {
                // no-op for headless coverage
            }
        };
    }
}
