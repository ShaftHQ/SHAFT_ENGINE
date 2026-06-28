package com.shaft.intellij.ui;

import com.shaft.intellij.settings.ShaftSettingsState;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBTabbedPane;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;

import javax.imageio.ImageIO;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
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
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftPluginScreenshotRendererTest {
    private static final int WIDTH = 1200;
    private static final int HEIGHT = 780;
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
        Path assistantScreenshot = outputPath.resolve("intellij-plugin-assistant.png");
        Path toolsScreenshot = outputPath.resolve("intellij-plugin-tools.png");
        write(assistantScreenshot, renderToolWindow(0));
        write(toolsScreenshot, renderToolWindow(1));
        assertAll(
                () -> assertTrue(Files.size(assistantScreenshot) > 0, assistantScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(toolsScreenshot) > 0, toolsScreenshot + " should be non-empty"));
    }

    private static BufferedImage renderToolWindow(int selectedTab)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel();
            JComponent component = toolWindow(selectedTab);
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            component.doLayout();
            layout(component);
            image.set(render(component));
        });
        return image.get();
    }

    private static JComponent toolWindow(int selectedTab) {
        Project project = screenshotProject();
        JBTabbedPane tabs = new JBTabbedPane();
        tabs.addTab("Assistant", new ShaftAssistantPanel(project, defaultSettings()));
        tabs.addTab("Tools", new ShaftFeaturePanel(project));
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

    private static void configureLookAndFeel() {
        try {
            UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());
        } catch (Exception exception) {
            throw new IllegalStateException("Unable to configure Swing look and feel.", exception);
        }
    }

    private static void layout(Component component) {
        if (component instanceof Container container) {
            container.doLayout();
            for (Component child : container.getComponents()) {
                layout(child);
            }
        }
    }

    private static BufferedImage render(Component component) {
        BufferedImage image = new BufferedImage(WIDTH, HEIGHT, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = image.createGraphics();
        try {
            graphics.setColor(Color.WHITE);
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
}
