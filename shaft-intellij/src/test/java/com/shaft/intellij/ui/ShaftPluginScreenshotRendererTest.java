package com.shaft.intellij.ui;

import com.shaft.intellij.settings.ShaftSettingsState;
import com.intellij.openapi.project.Project;
import com.intellij.ui.JBColor;
import com.intellij.ui.components.JBTabbedPane;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;

import javax.imageio.ImageIO;
import javax.swing.AbstractButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.text.JTextComponent;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftPluginScreenshotRendererTest {
    private static final int WIDTH = 620;
    private static final int HEIGHT = 780;
    private static final String LIGHT_THEME = "com.intellij.ide.ui.laf.IntelliJLaf";
    private static final String DARK_THEME = "com.intellij.ide.ui.laf.darcula.DarculaLaf";
    private static final Color LIGHT_PANEL = new Color(0xF2F2F2);
    private static final Color LIGHT_FIELD = new Color(0xFFFFFF);
    private static final Color LIGHT_TEXT = new Color(0x1F2328);
    private static final Color LIGHT_BORDER = new Color(0xC9CCD1);
    private static final Color DARK_PANEL = new Color(0x3C3F41);
    private static final Color DARK_FIELD = new Color(0x45494A);
    private static final Color DARK_TEXT = new Color(0xDADADA);
    private static final Color DARK_BORDER = new Color(0x6B6F72);
    private static final Map<Class<?>, Object> PRIMITIVE_DEFAULTS = Map.of(
            boolean.class, false,
            byte.class, (byte) 0,
            short.class, (short) 0,
            int.class, 0,
            long.class, 0L,
            float.class, 0.0F,
            double.class, 0.0D,
            char.class, '\0');

    @Test
    void rendersFeatureCatalogScreenshotsWhenOutputDirectoryIsProvided() throws Exception {
        String outputDirectory = System.getProperty("shaft.intellij.screenshotDir", "").trim();
        Assumptions.assumeFalse(outputDirectory.isBlank(),
                "Set -Dshaft.intellij.screenshotDir=... to render screenshot evidence.");

        Path outputPath = Path.of(outputDirectory);
        Files.createDirectories(outputPath);

        Path assistantLightScreenshot = outputPath.resolve("intellij-plugin-assistant.png");
        Path assistantDarkScreenshot = outputPath.resolve("intellij-plugin-assistant-dark.png");
        Path recorderScreenshot = outputPath.resolve("intellij-plugin-recorder.png");
        Path inspectorScreenshot = outputPath.resolve("intellij-plugin-inspector.png");
        Path evidenceScreenshot = outputPath.resolve("intellij-plugin-evidence.png");
        Path projectsScreenshot = outputPath.resolve("intellij-plugin-projects.png");
        Path advancedToolsLightScreenshot = outputPath.resolve("intellij-plugin-advanced-tools.png");
        Path advancedToolsDarkScreenshot = outputPath.resolve("intellij-plugin-advanced-tools-dark.png");
        Path toolsLightScreenshot = outputPath.resolve("intellij-plugin-tools.png");
        Path toolsDarkScreenshot = outputPath.resolve("intellij-plugin-tools-dark.png");

        write(assistantLightScreenshot, renderToolWindow(0, "", LIGHT_THEME, false));
        write(assistantDarkScreenshot, renderToolWindow(0, "", DARK_THEME, true));
        write(recorderScreenshot, renderToolWindow(1, "", LIGHT_THEME, false));
        write(inspectorScreenshot, renderToolWindow(2, "", LIGHT_THEME, false));
        write(evidenceScreenshot, renderToolWindow(3, "", LIGHT_THEME, false));
        write(projectsScreenshot, renderToolWindow(4, "", LIGHT_THEME, false));
        write(advancedToolsLightScreenshot, renderToolWindow(5, "", LIGHT_THEME, false));
        write(advancedToolsDarkScreenshot, renderToolWindow(5, "", DARK_THEME, true));
        Files.copy(advancedToolsLightScreenshot, toolsLightScreenshot, StandardCopyOption.REPLACE_EXISTING);
        Files.copy(advancedToolsDarkScreenshot, toolsDarkScreenshot, StandardCopyOption.REPLACE_EXISTING);
        assertAll(
                () -> assertTrue(Files.size(assistantLightScreenshot) > 0, assistantLightScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantDarkScreenshot) > 0, assistantDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(recorderScreenshot) > 0, recorderScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(inspectorScreenshot) > 0, inspectorScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(evidenceScreenshot) > 0, evidenceScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(projectsScreenshot) > 0, projectsScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(advancedToolsLightScreenshot) > 0, advancedToolsLightScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(advancedToolsDarkScreenshot) > 0, advancedToolsDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(toolsLightScreenshot) > 0, toolsLightScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(toolsDarkScreenshot) > 0, toolsDarkScreenshot + " should be non-empty"),
                () -> assertDimensions(assistantLightScreenshot),
                () -> assertDimensions(assistantDarkScreenshot),
                () -> assertDimensions(recorderScreenshot),
                () -> assertDimensions(inspectorScreenshot),
                () -> assertDimensions(evidenceScreenshot),
                () -> assertDimensions(projectsScreenshot),
                () -> assertDimensions(advancedToolsLightScreenshot),
                () -> assertDimensions(advancedToolsDarkScreenshot),
                () -> assertDimensions(toolsLightScreenshot),
                () -> assertDimensions(toolsDarkScreenshot),
                () -> assertTrue(Files.mismatch(assistantLightScreenshot, assistantDarkScreenshot) >= 0,
                        "Assistant light and dark screenshots should differ"),
                () -> assertTrue(Files.mismatch(advancedToolsLightScreenshot, advancedToolsDarkScreenshot) >= 0,
                        "Advanced Tools light and dark screenshots should differ"));
    }

    private static BufferedImage renderToolWindow(int selectedTab, String toolsCategory, String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            JComponent component = toolWindow(selectedTab, toolsCategory);
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component));
        });
        return image.get();
    }

    private static JComponent toolWindow(int selectedTab, String toolsCategory) {
        Project project = screenshotProject();
        JBTabbedPane tabs = new JBTabbedPane();
        ShaftFeaturePanel recorder = new ShaftFeaturePanel(project, defaultSettings(),
                List.of(new ToolCategory("Recorder", ToolTemplates.recorder())));
        ShaftFeaturePanel inspector = new ShaftFeaturePanel(project, defaultSettings(),
                List.of(new ToolCategory("Inspector", ToolTemplates.inspector())));
        ShaftFeaturePanel evidence = new ShaftFeaturePanel(project, defaultSettings(),
                List.of(new ToolCategory("Evidence", Stream.concat(
                        ToolTemplates.doctor().stream(), ToolTemplates.healer().stream()).toList())));
        ShaftFeaturePanel projects = new ShaftFeaturePanel(project, defaultSettings(),
                List.of(new ToolCategory("Projects", ToolTemplates.projects())));
        ShaftFeaturePanel advancedTools = new ShaftFeaturePanel(project, defaultSettings(), ToolTemplates.categories());
        tabs.addTab("Assistant", new ShaftAssistantPanel(project, defaultSettings()));
        tabs.addTab("Recorder", recorder);
        tabs.addTab("Inspector", inspector);
        tabs.addTab("Evidence", evidence);
        tabs.addTab("Projects", projects);
        tabs.addTab("Advanced Tools", advancedTools);
        Component selected = tabs.getComponentAt(selectedTab);
        if (selected instanceof ShaftFeaturePanel featurePanel && !toolsCategory.isBlank()) {
            featurePanel.selectCategory(toolsCategory);
        }
        tabs.setSelectedIndex(selectedTab);
        return tabs;
    }

    private static Project screenshotProject() {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> switch (method.getName()) {
                    case "equals" -> proxy == arguments[0];
                    case "hashCode" -> System.identityHashCode(proxy);
                    case "toString" -> "SHAFT screenshot project";
                    case "getBasePath" -> "";
                    case "getName" -> "SHAFT";
                    default -> defaultValue(method.getReturnType());
                });
    }

    private static Object defaultValue(Class<?> returnType) {
        return PRIMITIVE_DEFAULTS.get(returnType);
    }

    private static ShaftSettingsState.Settings defaultSettings() {
        return new ShaftSettingsState.Settings();
    }

    private static void configureLookAndFeel(String lookAndFeelClassName, boolean dark) {
        try {
            JBColor.setDark(dark);
            UIManager.setLookAndFeel(lookAndFeelClassName);
            applyThemeDefaults(dark);
        } catch (Exception exception) {
            throw new IllegalStateException("Unable to configure Swing look and feel: " + lookAndFeelClassName, exception);
        }
    }

    private static void applyThemeDefaults(boolean dark) {
        Color panel = dark ? DARK_PANEL : LIGHT_PANEL;
        Color field = dark ? DARK_FIELD : LIGHT_FIELD;
        Color text = dark ? DARK_TEXT : LIGHT_TEXT;
        Color border = dark ? DARK_BORDER : LIGHT_BORDER;
        UIManager.put("Panel.background", panel);
        UIManager.put("TabbedPane.background", panel);
        UIManager.put("TabbedPane.foreground", text);
        UIManager.put("SplitPane.background", panel);
        UIManager.put("ScrollPane.background", panel);
        UIManager.put("Viewport.background", field);
        UIManager.put("Label.foreground", text);
        UIManager.put("Button.background", dark ? DARK_FIELD : new Color(0xE6E6E6));
        UIManager.put("Button.foreground", text);
        UIManager.put("ComboBox.background", field);
        UIManager.put("ComboBox.foreground", text);
        UIManager.put("TextArea.background", field);
        UIManager.put("TextArea.foreground", text);
        UIManager.put("TextArea.caretForeground", text);
        UIManager.put("TextField.background", field);
        UIManager.put("TextField.foreground", text);
        UIManager.put("TextField.caretForeground", text);
        UIManager.put("Component.borderColor", border);
    }

    private static void layout(Component component, boolean light) {
        applyComponentTheme(component, light);
        if (component instanceof Container container) {
            container.doLayout();
            for (Component child : container.getComponents()) {
                layout(child, light);
            }
        }
    }

    private static void applyComponentTheme(Component component, boolean light) {
        Color panel = light ? LIGHT_PANEL : DARK_PANEL;
        Color field = light ? LIGHT_FIELD : DARK_FIELD;
        Color text = light ? LIGHT_TEXT : DARK_TEXT;
        if (component instanceof AbstractButton button) {
            if (light) {
                button.setUI(new javax.swing.plaf.basic.BasicButtonUI());
            }
            button.setBackground(light ? new Color(0xE6E6E6) : DARK_FIELD);
            button.setForeground(text);
            button.setOpaque(true);
            button.setContentAreaFilled(true);
        } else if (component instanceof JComboBox<?> comboBox) {
            if (light) {
                comboBox.setUI(new javax.swing.plaf.basic.BasicComboBoxUI());
            }
            comboBox.setBackground(field);
            comboBox.setForeground(text);
        } else if (component instanceof JTextComponent textComponent) {
            textComponent.setBackground(field);
            textComponent.setForeground(text);
            textComponent.setCaretColor(text);
        } else if (component instanceof JLabel label) {
            label.setForeground(text);
        } else if (component instanceof JComponent jComponent) {
            jComponent.setBackground(panel);
        }
    }

    private static BufferedImage render(Component component) {
        BufferedImage image = new BufferedImage(WIDTH, HEIGHT, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = image.createGraphics();
        try {
            Color background = component.getBackground();
            if (background == null) {
                background = UIManager.getColor("Panel.background");
            }
            graphics.setColor(background == null ? Color.WHITE : background);
            graphics.fillRect(0, 0, WIDTH, HEIGHT);
            graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
                    RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
            component.printAll(graphics);
        } finally {
            graphics.dispose();
        }
        return image;
    }

    private static void write(Path outputPath, BufferedImage image) throws IOException {
        if (!ImageIO.write(image, "png", outputPath.toFile())) {
            throw new IOException("No PNG writer available for " + outputPath);
        }
    }

    private static void assertDimensions(Path imagePath) throws IOException {
        BufferedImage image = ImageIO.read(imagePath.toFile());
        assertAll(
                () -> assertTrue(image.getWidth() == WIDTH, imagePath + " width should be " + WIDTH),
                () -> assertTrue(image.getHeight() == HEIGHT, imagePath + " height should be " + HEIGHT));
    }
}
