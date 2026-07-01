package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftIconAssetsTest {
    @Test
    void toolWindowIconsUseJetBrainsNewUiSvgVariants() throws IOException {
        Path icons = Path.of("src/main/resources/icons");

        assertAll(
                () -> assertSvgIcon(icons.resolve("shaftToolWindow.svg"), 16, "#6C707E"),
                () -> assertSvgIcon(icons.resolve("shaftToolWindow_dark.svg"), 16, "#CED0D6"),
                () -> assertSvgIcon(icons.resolve("shaftToolWindow@20x20.svg"), 20, "#6C707E"),
                () -> assertSvgIcon(icons.resolve("shaftToolWindow@20x20_dark.svg"), 20, "#CED0D6"),
                () -> assertFalse(Files.exists(icons.resolve("shaftToolWindow.png"))),
                () -> assertFalse(Files.exists(icons.resolve("shaftToolWindow_dark.png"))));
    }

    @Test
    void actionIconsUseJetBrainsStyleSvgPairs() throws IOException {
        Path actions = Path.of("src/main/resources/icons/actions");

        assertAll(
                actionIconAssertions(actions, "add"),
                actionIconAssertions(actions, "cancel"),
                actionIconAssertions(actions, "check"),
                actionIconAssertions(actions, "clear"),
                actionIconAssertions(actions, "code"),
                actionIconAssertions(actions, "copy"),
                actionIconAssertions(actions, "download"),
                actionIconAssertions(actions, "edit"),
                actionIconAssertions(actions, "github"),
                actionIconAssertions(actions, "help"),
                actionIconAssertions(actions, "rerun"),
                actionIconAssertions(actions, "reset"),
                actionIconAssertions(actions, "search"),
                actionIconAssertions(actions, "send"),
                actionIconAssertions(actions, "settings"),
                actionIconAssertions(actions, "view"));
    }

    @Test
    void pluginDescriptorRegistersSvgIconsAndRestartRequirement() throws IOException {
        String descriptor = Files.readString(Path.of("src/main/resources/META-INF/plugin.xml"));
        String javaDescriptor = Files.readString(Path.of(
                "src/main/resources/META-INF/io.github.shafthq.shaft-withJava.xml"));

        assertAll(
                () -> assertTrue(descriptor.contains("require-restart=\"true\"")),
                () -> assertTrue(descriptor.contains("icon=\"/icons/shaftToolWindow.svg\"")),
                () -> assertTrue(descriptor.contains("iconDark=\"/icons/shaftToolWindow_dark.svg\"")),
                () -> assertFalse(descriptor.contains("shaftToolWindow.png")),
                () -> assertTrue(descriptor.contains("optional=\"true\"")),
                () -> assertTrue(descriptor.contains("config-file=\"io.github.shafthq.shaft-withJava.xml\"")),
                () -> assertTrue(javaDescriptor.contains("Shaft.RecordJavaTarget")),
                () -> assertTrue(javaDescriptor.contains("EditorPopupMenu")),
                () -> assertTrue(javaDescriptor.contains("icon=\"/icons/shaftToolWindow.svg\"")),
                () -> assertTrue(javaDescriptor.contains("iconDark=\"/icons/shaftToolWindow_dark.svg\"")),
                () -> assertFalse(javaDescriptor.contains("shaftToolWindow.png")));
    }

    private static void assertSvgIcon(Path path, int size, String color) throws IOException {
        String svg = Files.readString(path);
        assertAll(
                () -> assertTrue(svg.contains("width=\"" + size + "\""), path.toString()),
                () -> assertTrue(svg.contains("height=\"" + size + "\""), path.toString()),
                () -> assertTrue(svg.contains("viewBox=\"0 0 " + size + " " + size + "\""), path.toString()),
                () -> assertTrue(svg.contains(color), path.toString()),
                () -> assertTrue(svg.contains("stroke-linecap=\"round\""), path.toString()),
                () -> assertTrue(svg.contains("stroke-linejoin=\"round\""), path.toString()));
    }

    private static org.junit.jupiter.api.function.Executable actionIconAssertions(Path actions, String name) {
        return () -> assertAll(
                () -> assertSvgIcon(actions.resolve(name + ".svg"), 16, "#6C707E"),
                () -> assertSvgIcon(actions.resolve(name + "_dark.svg"), 16, "#CED0D6"));
    }
}
