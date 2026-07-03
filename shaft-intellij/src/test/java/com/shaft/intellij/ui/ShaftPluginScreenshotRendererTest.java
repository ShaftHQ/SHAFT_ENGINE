package com.shaft.intellij.ui;

import com.intellij.openapi.project.Project;
import com.intellij.ui.JBColor;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsConfigurable;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;

import javax.imageio.ImageIO;
import javax.swing.AbstractButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.plaf.ColorUIResource;
import javax.swing.text.JTextComponent;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftPluginScreenshotRendererTest {
    private static final int WIDTH = 860;
    private static final int NARROW_WIDTH = 360;
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
    private static final String ASSISTANT_SHAFT_CODE_SAMPLE = """
            ```java
            public class SignInTest {
                private final SHAFT.GUI.WebDriver driver = new SHAFT.GUI.WebDriver();

                @Test
                void opensSignInPage() {
                    driver.browser().navigateToURL("https://example.com");
                    driver.element().click(SHAFT.GUI.Locator.clickableField("Sign in"));
                }
            }
            ```
            """.stripIndent().trim();
    private static final String DUCK_DUCK_GO_SHAFT_CODE_SAMPLE = """
            ```java
            public class DuckDuckGoSearchTest {
                private final SHAFT.GUI.WebDriver driver = new SHAFT.GUI.WebDriver();

                @Test
                void opensShaftEngineResult() {
                    driver.browser().navigateToURL("https://duckduckgo.com/");
                    driver.element().type(SHAFT.GUI.Locator.name("q"), "SHAFT Engine");
                    driver.element().keyPress(SHAFT.GUI.Locator.name("q"), "ENTER");
                    driver.element().click(SHAFT.GUI.Locator.xpath(
                            "(//article[@data-testid='result'])[1]//a[@data-testid='result-title-a']"));
                }
            }
            ```
            """.stripIndent().trim();
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
        Path assistantNarrowDarkScreenshot = outputPath.resolve("intellij-plugin-assistant-narrow-dark.png");
        Path assistantLiveDarkScreenshot = outputPath.resolve("intellij-plugin-assistant-live-output-dark.png");
        Path guidedScreenshot = outputPath.resolve("intellij-plugin-guided.png");
        Path recorderScreenshot = outputPath.resolve("intellij-plugin-recorder.png");
        Path inspectorScreenshot = outputPath.resolve("intellij-plugin-inspector.png");
        Path triageScreenshot = outputPath.resolve("intellij-plugin-triage.png");
        Path evidenceScreenshot = outputPath.resolve("intellij-plugin-evidence.png");
        Path projectsScreenshot = outputPath.resolve("intellij-plugin-projects.png");
        Path advancedToolsLightScreenshot = outputPath.resolve("intellij-plugin-advanced-tools.png");
        Path advancedToolsDarkScreenshot = outputPath.resolve("intellij-plugin-advanced-tools-dark.png");
        Path toolsLightScreenshot = outputPath.resolve("intellij-plugin-tools.png");
        Path toolsDarkScreenshot = outputPath.resolve("intellij-plugin-tools-dark.png");
        Path mcpSetupScreenshot = outputPath.resolve("intellij-plugin-mcp-setup.png");
        Path mcpSetupNarrowDarkScreenshot = outputPath.resolve("intellij-plugin-mcp-setup-narrow-dark.png");
        Path mcpSetupSuccessScreenshot = outputPath.resolve("intellij-plugin-mcp-setup-success.png");
        Path mcpSetupErrorScreenshot = outputPath.resolve("intellij-plugin-mcp-setup-error-dark.png");
        Path settingsScreenshot = outputPath.resolve("intellij-plugin-settings.png");
        Path settingsDarkScreenshot = outputPath.resolve("intellij-plugin-settings-dark.png");
        Path mcpGuideScreenshot = outputPath.resolve("intellij-plugin-mcp-guide.png");

        write(assistantLightScreenshot, renderToolWindow(0, "", LIGHT_THEME, false));
        write(assistantDarkScreenshot, renderToolWindow(0, "", DARK_THEME, true));
        write(assistantNarrowDarkScreenshot, renderToolWindow(0, "", DARK_THEME, true, NARROW_WIDTH, HEIGHT));
        write(assistantLiveDarkScreenshot, renderAssistantLiveOutput(DARK_THEME, true));
        write(guidedScreenshot, renderToolWindow(1, "", LIGHT_THEME, false));
        write(recorderScreenshot, renderToolWindow(2, "", LIGHT_THEME, false));
        write(inspectorScreenshot, renderToolWindow(3, "", LIGHT_THEME, false));
        write(triageScreenshot, renderToolWindow(4, "", LIGHT_THEME, false));
        write(evidenceScreenshot, renderToolWindow(5, "", LIGHT_THEME, false));
        write(projectsScreenshot, renderToolWindow(6, "", LIGHT_THEME, false));
        write(advancedToolsLightScreenshot, renderToolWindow(7, "", LIGHT_THEME, false));
        write(advancedToolsDarkScreenshot, renderToolWindow(7, "", DARK_THEME, true));
        Files.copy(advancedToolsLightScreenshot, toolsLightScreenshot, StandardCopyOption.REPLACE_EXISTING);
        Files.copy(advancedToolsDarkScreenshot, toolsDarkScreenshot, StandardCopyOption.REPLACE_EXISTING);
        write(mcpSetupScreenshot, renderSetup(LIGHT_THEME, false));
        write(mcpSetupNarrowDarkScreenshot, renderSetup(DARK_THEME, true, NARROW_WIDTH, HEIGHT));
        write(mcpSetupSuccessScreenshot, renderSetupSuccess(LIGHT_THEME, false));
        write(mcpSetupErrorScreenshot, renderSetupError(DARK_THEME, true));
        write(settingsScreenshot, renderSettings(LIGHT_THEME, false));
        write(settingsDarkScreenshot, renderSettings(DARK_THEME, true));
        write(mcpGuideScreenshot, renderToolWindow(7, "Guide", LIGHT_THEME, false));
        assertAll(
                () -> assertTrue(Files.size(assistantLightScreenshot) > 0, assistantLightScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantDarkScreenshot) > 0, assistantDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantNarrowDarkScreenshot) > 0, assistantNarrowDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantLiveDarkScreenshot) > 0, assistantLiveDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(guidedScreenshot) > 0, guidedScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(recorderScreenshot) > 0, recorderScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(inspectorScreenshot) > 0, inspectorScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(triageScreenshot) > 0, triageScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(evidenceScreenshot) > 0, evidenceScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(projectsScreenshot) > 0, projectsScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(advancedToolsLightScreenshot) > 0, advancedToolsLightScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(advancedToolsDarkScreenshot) > 0, advancedToolsDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(toolsLightScreenshot) > 0, toolsLightScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(toolsDarkScreenshot) > 0, toolsDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(mcpSetupScreenshot) > 0, mcpSetupScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(mcpSetupNarrowDarkScreenshot) > 0, mcpSetupNarrowDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(mcpSetupSuccessScreenshot) > 0, mcpSetupSuccessScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(mcpSetupErrorScreenshot) > 0, mcpSetupErrorScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(settingsScreenshot) > 0, settingsScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(settingsDarkScreenshot) > 0, settingsDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(mcpGuideScreenshot) > 0, mcpGuideScreenshot + " should be non-empty"),
                () -> assertDimensions(assistantLightScreenshot),
                () -> assertDimensions(assistantDarkScreenshot),
                () -> assertDimensions(assistantNarrowDarkScreenshot, NARROW_WIDTH, HEIGHT),
                () -> assertDimensions(assistantLiveDarkScreenshot),
                () -> assertDimensions(guidedScreenshot),
                () -> assertDimensions(recorderScreenshot),
                () -> assertDimensions(inspectorScreenshot),
                () -> assertDimensions(triageScreenshot),
                () -> assertDimensions(evidenceScreenshot),
                () -> assertDimensions(projectsScreenshot),
                () -> assertDimensions(advancedToolsLightScreenshot),
                () -> assertDimensions(advancedToolsDarkScreenshot),
                () -> assertDimensions(toolsLightScreenshot),
                () -> assertDimensions(toolsDarkScreenshot),
                () -> assertDimensions(mcpSetupScreenshot),
                () -> assertDimensions(mcpSetupNarrowDarkScreenshot, NARROW_WIDTH, HEIGHT),
                () -> assertDimensions(mcpSetupSuccessScreenshot),
                () -> assertDimensions(mcpSetupErrorScreenshot),
                () -> assertDimensions(settingsScreenshot),
                () -> assertDimensions(settingsDarkScreenshot),
                () -> assertDimensions(mcpGuideScreenshot),
                () -> assertTrue(Files.mismatch(assistantLightScreenshot, assistantDarkScreenshot) >= 0,
                        "Assistant light and dark screenshots should differ"),
                () -> assertTrue(Files.mismatch(settingsScreenshot, settingsDarkScreenshot) >= 0,
                        "Settings light and dark screenshots should differ"),
                () -> assertTrue(Files.mismatch(advancedToolsLightScreenshot, advancedToolsDarkScreenshot) >= 0,
                        "Advanced Tools light and dark screenshots should differ"),
                () -> assertTrue(ASSISTANT_SHAFT_CODE_SAMPLE.contains("SHAFT.GUI.WebDriver")),
                () -> assertTrue(ASSISTANT_SHAFT_CODE_SAMPLE.contains("driver.browser().navigateToURL")),
                () -> assertTrue(ASSISTANT_SHAFT_CODE_SAMPLE.contains("driver.element().click")),
                () -> assertFalse(ASSISTANT_SHAFT_CODE_SAMPLE.contains("driver.get(")),
                () -> assertFalse(ASSISTANT_SHAFT_CODE_SAMPLE.contains("driver.findElement(")),
                () -> assertTrue(DUCK_DUCK_GO_SHAFT_CODE_SAMPLE.contains("https://duckduckgo.com/")),
                () -> assertTrue(DUCK_DUCK_GO_SHAFT_CODE_SAMPLE.contains("result-title-a")),
                () -> assertFalse(DUCK_DUCK_GO_SHAFT_CODE_SAMPLE.contains("https://example.com")));
    }

    private static BufferedImage renderToolWindow(int selectedTab, String toolsCategory, String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        return renderToolWindow(selectedTab, toolsCategory, lookAndFeelClassName, dark, WIDTH, HEIGHT);
    }

    private static BufferedImage renderToolWindow(int selectedTab,
                                                  String toolsCategory,
                                                  String lookAndFeelClassName,
                                                  boolean dark,
                                                  int width,
                                                  int height)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            JComponent component = toolWindow(selectedTab, toolsCategory);
            component.setSize(new Dimension(width, height));
            component.setPreferredSize(new Dimension(width, height));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, width, height));
        });
        return image.get();
    }

    private static BufferedImage renderSettings(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            JComponent component = settingsPanel();
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, WIDTH, HEIGHT));
        });
        return image.get();
    }

    private static BufferedImage renderAssistantLiveOutput(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftAssistantChatState chatState = new ShaftAssistantChatState();
            chatState.append("user", """
                    /browser open https://example.com sign in
                    """.stripIndent().trim(), "");
            chatState.append("assistant", """
                    _Running local assistant..._

                    %s
                    """.formatted(ASSISTANT_SHAFT_CODE_SAMPLE).stripIndent().trim(), "");
            ShaftSettingsState.Settings settings = defaultSettings();
            settings.defaultAutobotMode = "AGENT";
            ShaftAssistantPanel component = new ShaftAssistantPanel(screenshotProject(), settings, chatState,
                    () -> {
                    });
            invokeSetRunning(component, true, "Thinking...");
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, WIDTH, HEIGHT));
            invokeSetRunning(component, false, "Try asking me to do something...");
        });
        return image.get();
    }

    private static BufferedImage renderSetup(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        return renderSetup(lookAndFeelClassName, dark, WIDTH, HEIGHT);
    }

    private static BufferedImage renderSetup(String lookAndFeelClassName, boolean dark, int width, int height)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            JComponent component = new ShaftToolWindowPanel(screenshotProject(), new ShaftSettingsState.Settings());
            component.setSize(new Dimension(width, height));
            component.setPreferredSize(new Dimension(width, height));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, width, height));
        });
        return image.get();
    }

    private static BufferedImage renderSetupSuccess(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
            settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
            ShaftMcpSetupPanel component = new ShaftMcpSetupPanel(screenshotProject(), settings,
                    () -> {
                    }, (client, runtime) -> ShaftMcpToolResult.success("Codex CLI executable is available on PATH."));
            invokeShowTestResult(component, ShaftMcpToolResult.success("""
                    Initialized SHAFT MCP 10.3.20260702
                    MCP workspace: C:\\Users\\demo\\IdeaProjects\\shop-tests
                    user.dir: C:\\Users\\demo\\AppData\\Local\\ShaftHQ\\shaft-mcp\\work
                    shaft.mcp.workspaceRoot: C:\\Users\\demo\\IdeaProjects\\shop-tests
                    SHAFT_MCP_WORKSPACE_ROOT: C:\\Users\\demo\\IdeaProjects\\shop-tests
                    """.stripIndent().trim()), null);
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, WIDTH, HEIGHT));
        });
        return image.get();
    }

    private static BufferedImage renderSetupError(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
            settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
            ShaftMcpSetupPanel component = new ShaftMcpSetupPanel(screenshotProject(), settings,
                    () -> {
                    });
            invokeShowTestResult(component, ShaftMcpToolResult.failure(
                    "Could not resolve artifact io.github.shafthq:shaft-mcp:jar:10.3.20260702"), null);
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, WIDTH, HEIGHT));
        });
        return image.get();
    }

    private static JComponent settingsPanel() {
        try {
            Class<?> credentialAccess = Class.forName(
                    "com.shaft.intellij.settings.ShaftSettingsConfigurable$CredentialAccess");
            Object credentials = Proxy.newProxyInstance(credentialAccess.getClassLoader(), new Class<?>[]{credentialAccess},
                    (proxy, method, arguments) -> switch (method.getName()) {
                        case "hasApiKey" -> false;
                        case "setApiKey" -> null;
                        default -> defaultValue(method.getReturnType());
                    });
            Constructor<ShaftSettingsConfigurable> constructor = ShaftSettingsConfigurable.class.getDeclaredConstructor(
                    ShaftSettingsState.Settings.class, credentialAccess);
            constructor.setAccessible(true);
            ShaftSettingsConfigurable configurable = constructor.newInstance(defaultSettings(), credentials);
            return (JComponent) configurable.createComponent();
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Unable to create settings screenshot panel", exception);
        }
    }

    private static void invokeSetRunning(ShaftAssistantPanel component, boolean running, String message) {
        component.setRunning(running, message);
    }

    private static void invokeShowTestResult(ShaftMcpSetupPanel component, ShaftMcpToolResult result, Throwable error) {
        try {
            Method method = ShaftMcpSetupPanel.class.getDeclaredMethod(
                    "showTestResult", ShaftMcpToolResult.class, Throwable.class);
            method.setAccessible(true);
            method.invoke(component, result, error);
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Unable to render setup probe result", exception);
        }
    }

    private static JComponent toolWindow(int selectedTab, String toolsCategory) {
        Project project = screenshotProject();
        ShaftSettingsState.Settings settings = defaultSettings();
        settings.advancedUiEnabled = true;
        ShaftAssistantChatState chatState = selectedTab == 0
                ? populatedAssistantChatState()
                : new ShaftAssistantChatState();
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(
                project, settings, AssistantLocalAgentRunner::readiness, chatState);
        JComboBox<ShaftToolWindowPanel.WorkflowView> selector = toolWindow.workflowSelector();
        ShaftToolWindowPanel.WorkflowView selectedView = selector.getItemAt(selectedTab);
        Component selected = selectedView.component();
        if (selected instanceof ShaftFeaturePanel featurePanel && !toolsCategory.isBlank()) {
            featurePanel.selectCategory(toolsCategory);
        }
        selector.setSelectedIndex(selectedTab);
        return toolWindow;
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

    private static ShaftAssistantChatState populatedAssistantChatState() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.append("user",
                "generate code that opens DuckDuckGo, searches SHAFT Engine, and opens the first result", "");
        chatState.append("assistant", """
                Confirmed target: https://duckduckgo.com/

                %s
                """.formatted(DUCK_DUCK_GO_SHAFT_CODE_SAMPLE).stripIndent().trim(), "");
        return chatState;
    }

    private static Object defaultValue(Class<?> returnType) {
        return PRIMITIVE_DEFAULTS.get(returnType);
    }

    private static ShaftSettingsState.Settings defaultSettings() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        settings.mcpSetupComplete = true;
        return settings;
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
            if (label.getForeground() instanceof ColorUIResource) {
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

    private static void assertDimensions(Path imagePath) throws IOException {
        assertDimensions(imagePath, WIDTH, HEIGHT);
    }

    private static void assertDimensions(Path imagePath, int width, int height) throws IOException {
        BufferedImage image = ImageIO.read(imagePath.toFile());
        assertAll(
                () -> assertTrue(image.getWidth() == width, imagePath + " width should be " + width),
                () -> assertTrue(image.getHeight() == height, imagePath + " height should be " + height));
    }
}
