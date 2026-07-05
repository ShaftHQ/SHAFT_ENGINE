package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftIconAssetsTest {
    private static final String LIGHT_ACTION_COLOR = "#6C707E";
    private static final String DARK_ACTION_COLOR = "#CED0D6";

    @Test
    void toolWindowIconsUseOfficialShaftLogoSvgVariants() throws IOException {
        Path icons = Path.of("src/main/resources/icons");

        assertAll(
                () -> assertLogoIcon(icons.resolve("shaftToolWindow.svg"), 16,
                        Path.of("../shaft-engine/src/main/resources/images/shaft_standard.png")),
                () -> assertLogoIcon(icons.resolve("shaftToolWindow_dark.svg"), 16,
                        Path.of("../shaft-engine/src/main/resources/images/shaft_white.png")),
                () -> assertLogoIcon(icons.resolve("shaftToolWindow@20x20.svg"), 20,
                        Path.of("../shaft-engine/src/main/resources/images/shaft_standard.png")),
                () -> assertLogoIcon(icons.resolve("shaftToolWindow@20x20_dark.svg"), 20,
                        Path.of("../shaft-engine/src/main/resources/images/shaft_white.png")),
                () -> assertFalse(Files.exists(icons.resolve("shaftToolWindow.png"))),
                () -> assertFalse(Files.exists(icons.resolve("shaftToolWindow_dark.png"))));
    }

    @Test
    void marketplacePluginLogosUseOfficialShaftLogoSvgVariants() throws IOException {
        Path metaInf = Path.of("src/main/resources/META-INF");

        assertAll(
                () -> assertLogoIcon(metaInf.resolve("pluginIcon.svg"), 40,
                        Path.of("../shaft-engine/src/main/resources/images/shaft.png")),
                () -> assertLogoIcon(metaInf.resolve("pluginIcon_dark.svg"), 40,
                        Path.of("../shaft-engine/src/main/resources/images/shaft.png")),
                () -> assertFalse(Files.exists(metaInf.resolve("pluginIcon.png"))),
                () -> assertFalse(Files.exists(metaInf.resolve("pluginIcon_dark.png"))));
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

    private static void assertLogoIcon(Path svgPath, int size, Path canonicalPngPath) throws IOException {
        String svg = Files.readString(svgPath);
        byte[] canonicalPngBytes = Files.readAllBytes(canonicalPngPath);

        assertAll(
                () -> assertTrue(svg.contains("width=\"" + size + "\""), svgPath.toString()),
                () -> assertTrue(svg.contains("height=\"" + size + "\""), svgPath.toString()),
                () -> assertTrue(svg.contains("viewBox=\"0 0 " + size + " " + size + "\""), svgPath.toString()),
                () -> assertEmbeddedPngBytesMatchCanonical(svg, canonicalPngBytes, svgPath.toString()));
    }

    private static void assertEmbeddedPngBytesMatchCanonical(String svgContent, byte[] canonicalPngBytes,
                                                             String svgPath) {
        // Extract base64 data from SVG: href="data:image/png;base64,BASE64DATA"
        String base64Prefix = "data:image/png;base64,";
        int startIdx = svgContent.indexOf(base64Prefix);
        assertTrue(startIdx >= 0, "SVG does not contain embedded base64 PNG data: " + svgPath);

        startIdx += base64Prefix.length();
        int endIdx = svgContent.indexOf("\"", startIdx);
        assertTrue(endIdx > startIdx, "Malformed base64 data URI in SVG: " + svgPath);

        String embeddedBase64 = svgContent.substring(startIdx, endIdx);
        byte[] embeddedPngBytes = Base64.getDecoder().decode(embeddedBase64);

        assertArrayEquals(canonicalPngBytes, embeddedPngBytes,
                "Embedded PNG in " + svgPath + " does not match canonical source PNG bytes");
    }

    private static org.junit.jupiter.api.function.Executable actionIconAssertions(Path actions, String name) {
        return () -> assertAll(
                () -> assertSvgIcon(actions.resolve(name + ".svg"), 16, LIGHT_ACTION_COLOR),
                () -> assertSvgIcon(actions.resolve(name + "_dark.svg"), 16, DARK_ACTION_COLOR));
    }

}
