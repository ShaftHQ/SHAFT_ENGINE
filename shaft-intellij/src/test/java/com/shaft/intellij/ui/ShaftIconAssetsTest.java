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
    void composerActionIconsUseJetBrainsStyleSvgPairs() throws IOException {
        Path actions = Path.of("src/main/resources/icons/actions");

        assertAll(
                () -> assertSvgIcon(actions.resolve("help.svg"), 16, "#6C707E"),
                () -> assertSvgIcon(actions.resolve("help_dark.svg"), 16, "#CED0D6"),
                () -> assertSvgIcon(actions.resolve("send.svg"), 16, "#6C707E"),
                () -> assertSvgIcon(actions.resolve("send_dark.svg"), 16, "#CED0D6"));
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
}
