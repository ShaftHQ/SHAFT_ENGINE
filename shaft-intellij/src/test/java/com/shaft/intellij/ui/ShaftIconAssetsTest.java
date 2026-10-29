package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Locale;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Icon asset contracts for New UI (Carl F01 / W1.1): vector-only SVGs, New UI greys,
 * no PNG-in-SVG / data:image embeds, modest size budget.
 */
class ShaftIconAssetsTest {
    private static final String LIGHT_ACTION_COLOR = "#6C707E";
    private static final String DARK_ACTION_COLOR = "#CED0D6";
    /** DESIGN-SYSTEM §5.3 budget (~2 KB); leave headroom for comment headers. */
    private static final long MAX_ICON_SVG_BYTES = 4096L;

    @Test
    void toolWindowIconsAreOutlinedNewUiVectors() throws IOException {
        Path icons = Path.of("src/main/resources/icons");

        assertAll(
                () -> assertOutlinedIcon(icons.resolve("shaftToolWindow.svg"), 16, LIGHT_ACTION_COLOR),
                () -> assertOutlinedIcon(icons.resolve("shaftToolWindow_dark.svg"), 16, DARK_ACTION_COLOR),
                () -> assertOutlinedIcon(icons.resolve("shaftToolWindow@20x20.svg"), 20, LIGHT_ACTION_COLOR),
                () -> assertOutlinedIcon(icons.resolve("shaftToolWindow@20x20_dark.svg"), 20, DARK_ACTION_COLOR),
                () -> assertFalse(Files.exists(icons.resolve("shaftToolWindow.png"))),
                () -> assertFalse(Files.exists(icons.resolve("shaftToolWindow_dark.png"))));
    }

    @Test
    void marketplacePluginLogosAreOutlinedVectors() throws IOException {
        Path metaInf = Path.of("src/main/resources/META-INF");

        assertAll(
                () -> assertOutlinedIcon(metaInf.resolve("pluginIcon.svg"), 40, LIGHT_ACTION_COLOR),
                () -> assertOutlinedIcon(metaInf.resolve("pluginIcon_dark.svg"), 40, DARK_ACTION_COLOR),
                () -> assertFalse(Files.exists(metaInf.resolve("pluginIcon.png"))),
                () -> assertFalse(Files.exists(metaInf.resolve("pluginIcon_dark.png"))));
    }

    @Test
    void pluginOwnedActionIconsRemainVectorPairs() throws IOException {
        // W1.2: generic actions moved to AllIcons; only SHAFT-unique glyphs stay as plugin SVGs.
        Path actions = Path.of("src/main/resources/icons/actions");
        assertAll(
                actionIconAssertions(actions, "attach"),
                actionIconAssertions(actions, "github"));
    }

    @Test
    void pluginDescriptorRegistersIconMapperForNewUi() throws IOException {
        String descriptor = Files.readString(Path.of("src/main/resources/META-INF/plugin.xml"));
        String mappings = Files.readString(Path.of("src/main/resources/ShaftIconMappings.json"));
        Path expui = Path.of("src/main/resources/icons/expui/toolwindow/shaftToolWindow.svg");
        Path expuiDark = Path.of("src/main/resources/icons/expui/toolwindow/shaftToolWindow_dark.svg");

        assertAll(
                () -> assertTrue(descriptor.contains("iconMapper"), "iconMapper EP required (F02)"),
                () -> assertTrue(descriptor.contains("mappingFile=\"ShaftIconMappings.json\""),
                        "ShaftIconMappings.json must be registered"),
                () -> assertTrue(mappings.contains("shaftToolWindow.svg")),
                () -> assertTrue(mappings.contains("icons/shaftToolWindow.svg")),
                () -> assertOutlinedIcon(expui, 20, LIGHT_ACTION_COLOR),
                () -> assertOutlinedIcon(expuiDark, 20, DARK_ACTION_COLOR));
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

    @Test
    void allPluginSvgIconsBanPngInSvgAndStayUnderBudget() throws IOException {
        Path resources = Path.of("src/main/resources");
        List<Path> svgs;
        try (Stream<Path> stream = Files.walk(resources)) {
            svgs = stream
                    .filter(path -> path.getFileName().toString().toLowerCase(Locale.ROOT).endsWith(".svg"))
                    .sorted()
                    .toList();
        }
        assertFalse(svgs.isEmpty(), "expected SVG icons under plugin resources");
        assertAll(svgs.stream().map(ShaftIconAssetsTest::svgBudgetExecutable).toList());
    }

    private static Executable svgBudgetExecutable(Path path) {
        return () -> {
            assertNoRasterEmbed(path);
            assertTrue(Files.size(path) <= MAX_ICON_SVG_BYTES,
                    path + " exceeds " + MAX_ICON_SVG_BYTES + " bytes");
        };
    }

    private static void assertSvgIcon(Path path, int size, String color) throws IOException {
        String svg = Files.readString(path);
        assertAll(
                () -> assertTrue(svg.contains("width=\"" + size + "\""), path.toString()),
                () -> assertTrue(svg.contains("height=\"" + size + "\""), path.toString()),
                () -> assertTrue(svg.contains("viewBox=\"0 0 " + size + " " + size + "\""), path.toString()),
                () -> assertTrue(svg.contains(color), path.toString()),
                () -> assertTrue(svg.contains("stroke-linecap=\"round\""), path.toString()),
                () -> assertTrue(svg.contains("stroke-linejoin=\"round\""), path.toString()),
                () -> assertNoRasterEmbed(path));
    }

    private static void assertOutlinedIcon(Path svgPath, int size, String strokeColor) throws IOException {
        String svg = Files.readString(svgPath);
        assertAll(
                () -> assertTrue(Files.exists(svgPath), svgPath.toString()),
                () -> assertTrue(svg.contains("width=\"" + size + "\""), svgPath.toString()),
                () -> assertTrue(svg.contains("height=\"" + size + "\""), svgPath.toString()),
                () -> assertTrue(svg.contains("viewBox=\"0 0 " + size + " " + size + "\""), svgPath.toString()),
                () -> assertTrue(svg.contains(strokeColor), svgPath + " missing New UI stroke " + strokeColor),
                () -> assertTrue(svg.contains("stroke-linecap=\"round\""), svgPath.toString()),
                () -> assertTrue(svg.contains("stroke-linejoin=\"round\""), svgPath.toString()),
                () -> assertFalse(svg.toLowerCase(Locale.ROOT).contains("fill=\"#0060a8\""),
                        svgPath + " must not use brand-blue fill on strip/plugin icons"),
                () -> assertNoRasterEmbed(svgPath),
                () -> assertTrue(Files.size(svgPath) <= MAX_ICON_SVG_BYTES, svgPath + " over budget"));
    }

    private static void assertNoRasterEmbed(Path svgPath) throws IOException {
        String svg = Files.readString(svgPath).toLowerCase(Locale.ROOT);
        assertFalse(svg.contains("data:image"), "PNG-in-SVG / data:image banned: " + svgPath);
        assertFalse(svg.contains("<image"), "<image embed banned: " + svgPath);
        assertFalse(svg.contains("xlink:href=\"data:"), "data xlink:href banned: " + svgPath);
    }

    private static org.junit.jupiter.api.function.Executable actionIconAssertions(Path actions, String name) {
        return () -> assertAll(
                () -> assertSvgIcon(actions.resolve(name + ".svg"), 16, LIGHT_ACTION_COLOR),
                () -> assertSvgIcon(actions.resolve(name + "_dark.svg"), 16, DARK_ACTION_COLOR));
    }

}
