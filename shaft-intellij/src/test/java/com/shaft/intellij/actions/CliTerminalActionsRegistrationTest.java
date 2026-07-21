package com.shaft.intellij.actions;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pins that the two new raw-CLI terminal actions (issue #3959) are registered in the
 * {@code Shaft.ToolsMenu} group next to {@code Shaft.OpenToolWindow}, mirroring {@code
 * ShaftIconAssetsTest#pluginDescriptorRegistersSvgIconsAndRestartRequirement}'s plain-text
 * descriptor assertions -- no live IDE bootstrap needed. Also pins that no new {@code
 * bundledPlugin(...)} Terminal dependency was introduced in {@code build.gradle.kts}: this
 * feature must stay on the same optional/reflection discipline {@code ShaftTerminalCommands}
 * already established (the #3917 spike's load-bearing constraint).
 */
class CliTerminalActionsRegistrationTest {
    @Test
    void pluginDescriptorRegistersBothCliTerminalActionsInTheToolsMenuGroup() throws IOException {
        String descriptor = Files.readString(Path.of("src/main/resources/META-INF/plugin.xml"));

        assertTrue(descriptor.contains("id=\"Shaft.ToolsMenu\""));
        assertTrue(descriptor.contains(
                "class=\"com.shaft.intellij.actions.OpenClaudeCodeTerminalAction\""));
        assertTrue(descriptor.contains(
                "class=\"com.shaft.intellij.actions.OpenCodexTerminalAction\""));
    }

    @Test
    void buildScriptDeclaresNoNewHardTerminalPluginDependency() throws IOException {
        String buildScript = Files.readString(Path.of("build.gradle.kts"));

        assertFalse(buildScript.contains("bundledPlugin(\"org.jetbrains.plugins.terminal\")"),
                "the Terminal plugin must stay optional/reflection-only, per the #3917 spike");
    }
}
