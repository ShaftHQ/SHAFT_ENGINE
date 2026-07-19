package com.shaft.intellij.testrunner;

import com.intellij.ui.JBColor;
import com.shaft.intellij.ui.LookAndFeelIsolationExtension;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import javax.imageio.ImageIO;
import javax.swing.JComboBox;
import javax.swing.JComponent;
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
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftRunConfigurationOverridesPanelScreenshotTest {
    static {
        // Without an activated IconLoader this headless JVM paints placeholder glyphs instead of
        // the plugin's SVG action icons, which makes screenshot evidence unrepresentative.
        com.intellij.openapi.util.IconLoader.activate();
    }

    private static final String[] THEME_DEFAULT_KEYS = {
            "Panel.background", "TabbedPane.background", "TabbedPane.foreground", "SplitPane.background",
            "ScrollPane.background", "Viewport.background", "Label.foreground", "Button.background",
            "Button.foreground", "ComboBox.background", "ComboBox.foreground", "TextArea.background",
            "TextArea.foreground", "TextArea.caretForeground", "TextField.background", "TextField.foreground",
            "TextField.caretForeground", "Component.borderColor"
    };
    // Issue #3786: this class installs a real platform L&F (configureLookAndFeel below) exactly
    // like ShaftPluginScreenshotRendererTest but, unlike that class, never restored it -- the leaked
    // L&F (and the "TreeUI" -> DefaultTreeUI UIManager mapping that comes with it) stayed active for
    // the rest of the JVM and broke ShaftTestsPanelTest whenever the full suite ran with
    // -Dshaft.intellij.screenshotDir set. Reuses the same LookAndFeelIsolationExtension mechanism
    // ShaftPluginScreenshotRendererTest already proved sufficient (issue #3782 / PR #3781, #3787).
    @RegisterExtension
    static final LookAndFeelIsolationExtension LOOK_AND_FEEL_ISOLATION =
            new LookAndFeelIsolationExtension(THEME_DEFAULT_KEYS);

    private static final int WIDTH = 640;
    private static final int HEIGHT = 260;
    private static final String LIGHT_THEME = "com.intellij.ide.ui.laf.IntelliJLaf";
    private static final Color LIGHT_PANEL = new Color(0xF2F2F2);
    private static final Color LIGHT_FIELD = new Color(0xFFFFFF);
    private static final Color LIGHT_TEXT = new Color(0x1F2328);
    private static final Color LIGHT_BORDER = new Color(0xC9CCD1);
    private static final Color DARK_PANEL = new Color(0x3C3F41);
    private static final Color DARK_FIELD = new Color(0x45494A);
    private static final Color DARK_TEXT = new Color(0xDADADA);
    private static final Color DARK_BORDER = new Color(0x6B6F72);

    @Test
    void rendersRunConfigurationOverridesPanelScreenshotWhenOutputDirectoryIsProvided() throws Exception {
        String outputDirectory = System.getProperty("shaft.intellij.screenshotDir", "").trim();
        Assumptions.assumeFalse(outputDirectory.isBlank(),
                "Set -Dshaft.intellij.screenshotDir=... to render screenshot evidence.");

        Path outputPath = Path.of(outputDirectory);
        Files.createDirectories(outputPath);

        Path screenshotPath = outputPath.resolve("intellij-plugin-run-config-shaft-tab.png");

        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(LIGHT_THEME, false);
            ShaftRunConfigurationOverridesPanel component = new ShaftRunConfigurationOverridesPanel();

            // Populate with test state: enabled=true, browser="chrome", headless=true
            ShaftRunConfigurationOverrides state = new ShaftRunConfigurationOverrides();
            state.setEnabled(true);
            state.setBrowser("chrome");
            state.setHeadless(true);
            state.setExtraVmArgs("-DexecutionAddress=local");
            component.setState(state);

            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, true);
            image.set(render(component, WIDTH, HEIGHT));
        });

        write(screenshotPath, image.get());

        assertTrue(Files.size(screenshotPath) > 0, screenshotPath + " should be non-empty");
        assertDimensions(screenshotPath, WIDTH, HEIGHT);
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
        if (component.getClass().getName().contains("BasicTabbedPaneUI$TabContainer")) {
            return;
        }
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
        if (component instanceof javax.swing.AbstractButton button) {
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
        } else if (component instanceof javax.swing.JLabel label) {
            if (label.getForeground() instanceof javax.swing.plaf.ColorUIResource) {
                label.setForeground(text);
            }
        } else if (component instanceof JComponent jComponent) {
            jComponent.setBackground(panel);
        }
    }

    private static BufferedImage render(Component component, int width, int height) {
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = image.createGraphics();
        try {
            Color background = component.getBackground();
            if (background == null) {
                background = UIManager.getColor("Panel.background");
            }
            graphics.setColor(background == null ? Color.WHITE : background);
            graphics.fillRect(0, 0, width, height);
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

    private static void assertDimensions(Path imagePath, int width, int height) throws IOException {
        BufferedImage image = ImageIO.read(imagePath.toFile());
        assertTrue(image.getWidth() == width, imagePath + " width should be " + width);
        assertTrue(image.getHeight() == height, imagePath + " height should be " + height);
    }
}
