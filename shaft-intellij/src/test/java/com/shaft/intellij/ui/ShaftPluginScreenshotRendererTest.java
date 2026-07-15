package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.intellij.openapi.project.Project;
import com.intellij.ui.JBColor;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsConfigurable;
import com.shaft.intellij.settings.ShaftSettingsState;
import com.shaft.intellij.testindex.ShaftTestIndex;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;

import javax.imageio.ImageIO;
import javax.swing.AbstractButton;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
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
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftPluginScreenshotRendererTest {
    static {
        // Without an activated IconLoader this headless JVM paints placeholder glyphs instead of
        // the plugin's SVG action icons, which makes screenshot evidence unrepresentative.
        com.intellij.openapi.util.IconLoader.activate();
    }

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
            import org.openqa.selenium.By;

            public class DuckDuckGoSearchTest {
                private final SHAFT.GUI.WebDriver driver = new SHAFT.GUI.WebDriver();

                @Test
                void opensShaftEngineResult() {
                    driver.browser().navigateToURL("https://duckduckgo.com/");
                    driver.element().type(SHAFT.GUI.Locator.name("q"), "SHAFT Engine");
                    driver.element().keyPress(SHAFT.GUI.Locator.name("q"), "ENTER");
                    driver.element().click(By.xpath(
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
        Path assistantEmptyScreenshot = outputPath.resolve("intellij-plugin-assistant-empty.png");
        Path assistantEmptyNarrowScreenshot = outputPath.resolve("intellij-plugin-assistant-empty-narrow.png");
        Path assistantDarkScreenshot = outputPath.resolve("intellij-plugin-assistant-dark.png");
        Path assistantNarrowDarkScreenshot = outputPath.resolve("intellij-plugin-assistant-narrow-dark.png");
        Path assistantLiveDarkScreenshot = outputPath.resolve("intellij-plugin-assistant-live-output-dark.png");
        Path assistantProgressMilestonesScreenshot = outputPath.resolve("intellij-plugin-assistant-progress-milestones.png");
        Path assistantFailureRecoveryCardScreenshot = outputPath.resolve("intellij-plugin-assistant-failure-recovery-card.png");
        Path assistantApprovalPromptScreenshot = outputPath.resolve("intellij-plugin-assistant-approval-prompt.png");
        Path assistantModelFallbackScreenshot = outputPath.resolve("intellij-plugin-assistant-model-fallback.png");
        Path assistantSlashCommandsScreenshot = outputPath.resolve("intellij-plugin-assistant-slash-commands.png");
        Path toolsHumanizedDoctorCardScreenshot = outputPath.resolve("intellij-plugin-tools-humanized-doctor-card.png");
        Path assistantDefaultModePrefillScreenshot = outputPath.resolve("intellij-plugin-assistant-default-mode-prefill.png");
        Path mcpSetupPostSetupScreenshot = outputPath.resolve("intellij-plugin-mcp-setup-post-setup.png");
        Path guidedScreenshot = outputPath.resolve("intellij-plugin-guided.png");
        Path recorderScreenshot = outputPath.resolve("intellij-plugin-recorder.png");
        Path inspectorScreenshot = outputPath.resolve("intellij-plugin-inspector.png");
        Path triageScreenshot = outputPath.resolve("intellij-plugin-triage.png");
        Path shaftTestsScreenshot = outputPath.resolve("intellij-plugin-shaft-tests.png");
        Path shaftTestsDarkScreenshot = outputPath.resolve("intellij-plugin-shaft-tests-dark.png");
        Path visualBaselinesScreenshot = outputPath.resolve("intellij-plugin-visual-baselines.png");
        Path evidenceScreenshot = outputPath.resolve("intellij-plugin-evidence.png");
        Path projectsScreenshot = outputPath.resolve("intellij-plugin-projects.png");
        Path advancedToolsLightScreenshot = outputPath.resolve("intellij-plugin-advanced-tools.png");
        Path advancedToolsDarkScreenshot = outputPath.resolve("intellij-plugin-advanced-tools-dark.png");
        Path toolsLightScreenshot = outputPath.resolve("intellij-plugin-tools.png");
        Path toolsDarkScreenshot = outputPath.resolve("intellij-plugin-tools-dark.png");
        Path mcpSetupScreenshot = outputPath.resolve("intellij-plugin-mcp-setup.png");
        Path mcpSetupGeminiScreenshot = outputPath.resolve("intellij-plugin-mcp-setup-gemini.png");
        Path mcpSetupNarrowDarkScreenshot = outputPath.resolve("intellij-plugin-mcp-setup-narrow-dark.png");
        Path mcpSetupSuccessScreenshot = outputPath.resolve("intellij-plugin-mcp-setup-success.png");
        Path mcpSetupErrorScreenshot = outputPath.resolve("intellij-plugin-mcp-setup-error-dark.png");
        Path mcpSetupOfflineScreenshot = outputPath.resolve("intellij-plugin-mcp-setup-offline.png");
        Path settingsScreenshot = outputPath.resolve("intellij-plugin-settings.png");
        Path settingsDarkScreenshot = outputPath.resolve("intellij-plugin-settings-dark.png");
        Path mcpGuideScreenshot = outputPath.resolve("intellij-plugin-mcp-guide.png");

        write(assistantLightScreenshot, renderToolWindow(0, "", LIGHT_THEME, false));
        write(assistantEmptyScreenshot, renderAssistantEmpty(LIGHT_THEME, false));
        write(assistantEmptyNarrowScreenshot, renderAssistantEmpty(DARK_THEME, true, NARROW_WIDTH, HEIGHT));
        write(assistantDarkScreenshot, renderToolWindow(0, "", DARK_THEME, true));
        write(assistantNarrowDarkScreenshot, renderToolWindow(0, "", DARK_THEME, true, NARROW_WIDTH, HEIGHT));
        write(assistantLiveDarkScreenshot, renderAssistantLiveOutput(DARK_THEME, true));
        write(assistantProgressMilestonesScreenshot, renderAssistantProgressMilestones(LIGHT_THEME, false));
        write(assistantFailureRecoveryCardScreenshot, renderAssistantFailureRecoveryCard(LIGHT_THEME, false));
        write(assistantApprovalPromptScreenshot, renderApprovalPrompt(LIGHT_THEME, false));
        write(assistantModelFallbackScreenshot, renderAssistantModelFallback(LIGHT_THEME, false));
        write(assistantSlashCommandsScreenshot, renderAssistantSlashCommands(LIGHT_THEME, false));
        write(toolsHumanizedDoctorCardScreenshot, renderToolsHumanizedDoctorCard(LIGHT_THEME, false));
        write(assistantDefaultModePrefillScreenshot, renderAssistantDefaultModePrefill(LIGHT_THEME, false));
        write(mcpSetupPostSetupScreenshot, renderPostSetupSettings(LIGHT_THEME, false));
        write(guidedScreenshot, renderToolWindow(1, "", LIGHT_THEME, false));
        write(recorderScreenshot, renderToolWindow(2, "", LIGHT_THEME, false));
        write(inspectorScreenshot, renderToolWindow(3, "", LIGHT_THEME, false));
        write(triageScreenshot, renderToolWindow(4, "", LIGHT_THEME, false));
        write(shaftTestsScreenshot, renderShaftTests(LIGHT_THEME, false));
        write(shaftTestsDarkScreenshot, renderShaftTests(DARK_THEME, true));
        write(visualBaselinesScreenshot, renderVisualBaselines(LIGHT_THEME, false));
        write(evidenceScreenshot, renderToolWindow(7, "", LIGHT_THEME, false));
        write(projectsScreenshot, renderToolWindow(8, "", LIGHT_THEME, false));
        write(advancedToolsLightScreenshot, renderToolWindow(9, "", LIGHT_THEME, false));
        write(advancedToolsDarkScreenshot, renderToolWindow(9, "", DARK_THEME, true));
        Files.copy(advancedToolsLightScreenshot, toolsLightScreenshot, StandardCopyOption.REPLACE_EXISTING);
        Files.copy(advancedToolsDarkScreenshot, toolsDarkScreenshot, StandardCopyOption.REPLACE_EXISTING);
        write(mcpSetupScreenshot, renderSetup(LIGHT_THEME, false));
        write(mcpSetupGeminiScreenshot, renderSetupGemini(LIGHT_THEME, false));
        write(mcpSetupNarrowDarkScreenshot, renderSetup(DARK_THEME, true, NARROW_WIDTH, HEIGHT));
        write(mcpSetupSuccessScreenshot, renderSetupSuccess(LIGHT_THEME, false));
        write(mcpSetupErrorScreenshot, renderSetupError(DARK_THEME, true));
        write(mcpSetupOfflineScreenshot, renderSetupMcpOffline(LIGHT_THEME, false));
        write(settingsScreenshot, renderSettings(LIGHT_THEME, false));
        write(settingsDarkScreenshot, renderSettings(DARK_THEME, true));
        write(mcpGuideScreenshot, renderToolWindow(9, "Guide", LIGHT_THEME, false));
        assertAll(
                () -> assertTrue(Files.size(assistantLightScreenshot) > 0, assistantLightScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantEmptyScreenshot) > 0, assistantEmptyScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantEmptyNarrowScreenshot) > 0, assistantEmptyNarrowScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantDarkScreenshot) > 0, assistantDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantNarrowDarkScreenshot) > 0, assistantNarrowDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantLiveDarkScreenshot) > 0, assistantLiveDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantProgressMilestonesScreenshot) > 0,
                        assistantProgressMilestonesScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantFailureRecoveryCardScreenshot) > 0,
                        assistantFailureRecoveryCardScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantApprovalPromptScreenshot) > 0, assistantApprovalPromptScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantModelFallbackScreenshot) > 0, assistantModelFallbackScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantSlashCommandsScreenshot) > 0, assistantSlashCommandsScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(toolsHumanizedDoctorCardScreenshot) > 0, toolsHumanizedDoctorCardScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantDefaultModePrefillScreenshot) > 0, assistantDefaultModePrefillScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(mcpSetupPostSetupScreenshot) > 0, mcpSetupPostSetupScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(guidedScreenshot) > 0, guidedScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(recorderScreenshot) > 0, recorderScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(inspectorScreenshot) > 0, inspectorScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(triageScreenshot) > 0, triageScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(shaftTestsScreenshot) > 0, shaftTestsScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(shaftTestsDarkScreenshot) > 0, shaftTestsDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(visualBaselinesScreenshot) > 0, visualBaselinesScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(evidenceScreenshot) > 0, evidenceScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(projectsScreenshot) > 0, projectsScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(advancedToolsLightScreenshot) > 0, advancedToolsLightScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(advancedToolsDarkScreenshot) > 0, advancedToolsDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(toolsLightScreenshot) > 0, toolsLightScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(toolsDarkScreenshot) > 0, toolsDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(mcpSetupScreenshot) > 0, mcpSetupScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(mcpSetupGeminiScreenshot) > 0, mcpSetupGeminiScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(mcpSetupNarrowDarkScreenshot) > 0, mcpSetupNarrowDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(mcpSetupSuccessScreenshot) > 0, mcpSetupSuccessScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(mcpSetupErrorScreenshot) > 0, mcpSetupErrorScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(mcpSetupOfflineScreenshot) > 0, mcpSetupOfflineScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(settingsScreenshot) > 0, settingsScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(settingsDarkScreenshot) > 0, settingsDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(mcpGuideScreenshot) > 0, mcpGuideScreenshot + " should be non-empty"),
                () -> assertDimensions(assistantLightScreenshot),
                () -> assertDimensions(assistantEmptyScreenshot),
                () -> assertDimensions(assistantEmptyNarrowScreenshot, NARROW_WIDTH, HEIGHT),
                () -> assertDimensions(assistantDarkScreenshot),
                () -> assertDimensions(assistantNarrowDarkScreenshot, NARROW_WIDTH, HEIGHT),
                () -> assertDimensions(assistantLiveDarkScreenshot),
                () -> assertDimensions(assistantProgressMilestonesScreenshot),
                () -> assertDimensions(assistantFailureRecoveryCardScreenshot),
                () -> assertDimensions(assistantApprovalPromptScreenshot),
                () -> assertDimensions(assistantModelFallbackScreenshot),
                () -> assertDimensions(toolsHumanizedDoctorCardScreenshot),
                () -> assertDimensions(assistantDefaultModePrefillScreenshot),
                () -> assertDimensions(mcpSetupPostSetupScreenshot),
                () -> assertDimensions(guidedScreenshot),
                () -> assertDimensions(recorderScreenshot),
                () -> assertDimensions(inspectorScreenshot),
                () -> assertDimensions(triageScreenshot),
                () -> assertDimensions(shaftTestsScreenshot),
                () -> assertDimensions(shaftTestsDarkScreenshot),
                () -> assertDimensions(visualBaselinesScreenshot),
                () -> assertDimensions(evidenceScreenshot),
                () -> assertDimensions(projectsScreenshot),
                () -> assertDimensions(advancedToolsLightScreenshot),
                () -> assertDimensions(advancedToolsDarkScreenshot),
                () -> assertDimensions(toolsLightScreenshot),
                () -> assertDimensions(toolsDarkScreenshot),
                () -> assertDimensions(mcpSetupScreenshot),
                () -> assertDimensions(mcpSetupGeminiScreenshot),
                () -> assertDimensions(mcpSetupNarrowDarkScreenshot, NARROW_WIDTH, HEIGHT),
                () -> assertDimensions(mcpSetupSuccessScreenshot),
                () -> assertDimensions(mcpSetupErrorScreenshot),
                () -> assertDimensions(mcpSetupOfflineScreenshot),
                () -> assertDimensions(settingsScreenshot),
                () -> assertDimensions(settingsDarkScreenshot),
                () -> assertDimensions(mcpGuideScreenshot),
                () -> assertTrue(Files.mismatch(assistantLightScreenshot, assistantDarkScreenshot) >= 0,
                        "Assistant light and dark screenshots should differ"),
                () -> assertTrue(Files.mismatch(shaftTestsScreenshot, shaftTestsDarkScreenshot) >= 0,
                        "SHAFT Tests light and dark screenshots should differ"),
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

    private static BufferedImage renderShaftTests(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftTestIndex testIndex = new ShaftTestIndex();
            long now = System.currentTimeMillis();
            testIndex.recordRun("com.example.SignInTest", 0, now - 120_000);
            testIndex.recordRun("com.example.CheckoutTest", 1, now - 60_000);
            testIndex.recordRun("com.example.SearchTest", 0, now);
            ShaftTestsPanel component = new ShaftTestsPanel(screenshotProject(), testIndex);
            selectFailRowIfPresent(component);
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, WIDTH, HEIGHT));
        });
        return image.get();
    }

    /** Selects the first FAIL row so the screenshot shows Doctor/Heal enabled, falling back to
     * row 0 when nothing failed. */
    private static void selectFailRowIfPresent(ShaftTestsPanel component) {
        DefaultListModel<ShaftTestIndex.TestRowState> model =
                (DefaultListModel<ShaftTestIndex.TestRowState>) component.rowListForTest().getModel();
        for (int i = 0; i < model.getSize(); i++) {
            if (ShaftTestsPanel.isFailRow(model.getElementAt(i))) {
                component.rowListForTest().setSelectedIndex(i);
                return;
            }
        }
        if (model.getSize() > 0) {
            component.rowListForTest().setSelectedIndex(0);
        }
    }

    private static BufferedImage renderVisualBaselines(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException, IOException {
        Path fixtureDirectory = createVisualBaselineFixture();
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            VisualBaselinesPanel component = new VisualBaselinesPanel(screenshotProject());
            component.directoryFieldForTest().setText(fixtureDirectory.toString());
            component.scanButtonForTest().doClick();
            if (component.rowListForTest().getModel().getSize() > 0) {
                component.rowListForTest().setSelectedIndex(0);
            }
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, WIDTH, HEIGHT));
        });
        return image.get();
    }

    /**
     * A fake baseline plus its {@code _diff.png} marker, matching the on-disk layout written by
     * {@code ImageProcessingActions#compareScreenshotAgainstBaselineByHash}, so the screenshot
     * evidence shows a populated triage row instead of an empty scan.
     */
    private static Path createVisualBaselineFixture() throws IOException {
        Path directory = Files.createTempDirectory("shaft-visual-baselines");
        BufferedImage baseline = new BufferedImage(48, 32, BufferedImage.TYPE_INT_RGB);
        Graphics2D baselineGraphics = baseline.createGraphics();
        baselineGraphics.setColor(new Color(0x2E7D32));
        baselineGraphics.fillRect(0, 0, 48, 32);
        baselineGraphics.dispose();
        BufferedImage diff = new BufferedImage(48, 32, BufferedImage.TYPE_INT_RGB);
        Graphics2D diffGraphics = diff.createGraphics();
        diffGraphics.setColor(new Color(0xC62828));
        diffGraphics.fillRect(0, 0, 48, 32);
        diffGraphics.dispose();
        ImageIO.write(baseline, "png", directory.resolve("signInHeader_chrome_windows.png").toFile());
        ImageIO.write(diff, "png", directory.resolve("signInHeader_chrome_windows_diff.png").toFile());
        return directory;
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

    private static BufferedImage renderAssistantEmpty(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        return renderAssistantEmpty(lookAndFeelClassName, dark, WIDTH, HEIGHT);
    }

    // A narrow width proves the first-run welcome bubble and empty-state chip row (issue #3540:
    // WrapLayout, not FlowLayout) report correct wrapped height and never clip against each other.
    private static BufferedImage renderAssistantEmpty(String lookAndFeelClassName, boolean dark, int width, int height)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            // Default (non-expert) settings on purpose: this shot documents the real first
            // contact — Assistant only, no workflow selector, an uncluttered empty chat.
            ShaftSettingsState.Settings settings = defaultSettings();
            JComponent component = new ShaftToolWindowPanel(
                    screenshotProject(), settings, AssistantLocalAgentRunner::readiness, new ShaftAssistantChatState());
            component.setSize(new Dimension(width, height));
            component.setPreferredSize(new Dimension(width, height));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, width, height));
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
            selectButton(component, "Verbose");
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

    /**
     * Renders the Assistant run timeline mid-{@code capture_generate_replay}, with several
     * streamed {@code notifications/progress} milestones already appended (issue #3546), so the
     * screenshot documents live-execution transparency instead of a static "Running" placeholder
     * that never changes. The milestone text mirrors what {@code CaptureGenerator#generate}
     * actually reports server-side (shaft-capture), for a realistic shot.
     *
     * <p>Driven through {@code addTimeline} directly — the same rendering path {@code
     * onToolProgress} feeds in production — rather than a real {@code dispatchApprovedTool} call,
     * because this harness's fake {@link #screenshotProject()} has no live
     * {@code ShaftMcpInvocationService} to dispatch through.
     */
    private static BufferedImage renderAssistantProgressMilestones(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftAssistantChatState chatState = new ShaftAssistantChatState();
            chatState.append("user", "/codegen recordings/demo-recording.json", "");
            ShaftSettingsState.Settings settings = defaultSettings();
            settings.defaultAutobotMode = "AGENT";
            ShaftAssistantPanel component = new ShaftAssistantPanel(screenshotProject(), settings, chatState,
                    () -> {
                    });
            invokeAddTimeline(component, "Tool selected: capture_generate_replay");
            invokeAddTimeline(component, "Running");
            invokeSetRunning(component, true, "Running: capture_generate_replay …");
            invokeAddTimeline(component, "Read capture session demo-recording.json");
            invokeAddTimeline(component, "Analyzed 12 captured event(s)");
            invokeAddTimeline(component, "Generated deterministic test source for DemoRecordingTest");
            invokeAddTimeline(component, "Compiled generated test: PASSED");
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

    private static void invokeAddTimeline(ShaftAssistantPanel component, String step) {
        try {
            Method method = ShaftAssistantPanel.class.getDeclaredMethod("addTimeline", String.class);
            method.setAccessible(true);
            method.invoke(component, step);
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Unable to render the progress-milestones timeline", exception);
        }
    }

    /**
     * Renders the Assistant transcript's failure-recovery card (issue #3547): the plain-language
     * root-cause card that {@code FailedRunDoctorNotifier} now renders automatically after a failed
     * test run (and that the notification's "Diagnose"/"Heal" buttons also produce directly, even
     * in default mode), instead of a bare "Failed" timeline entry or raw JSON. Seeded through the
     * same pure {@link ShaftAssistantPanel#toolCardMarkdown} formatting step {@code
     * runToolAndRenderCard} uses in production, then appended via {@link
     * ShaftAssistantPanel#simulateAppendForTest} -- this harness's fake {@link #screenshotProject()}
     * has no live {@code ShaftMcpInvocationService} to dispatch a real MCP round trip through, the
     * same reason {@link #renderAssistantProgressMilestones} drives {@code addTimeline} directly.
     */
    private static BufferedImage renderAssistantFailureRecoveryCard(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftAssistantChatState chatState = new ShaftAssistantChatState();
            ShaftSettingsState.Settings settings = defaultSettings();
            ShaftAssistantPanel component = new ShaftAssistantPanel(screenshotProject(), settings, chatState,
                    () -> {
                    });
            String cardMarkdown = ShaftAssistantPanel.toolCardMarkdown(
                    "doctor_analyze_failed_allure", ShaftMcpToolResult.success("""
                            {
                              "schemaVersion": "1.0",
                              "status": "DETERMINISTIC",
                              "bundleId": "bundle-456",
                              "primaryCause": "LOCATOR",
                              "confidence": "HIGH",
                              "summary": "The checkout submit button locator no longer matches the page after a redesign.",
                              "actions": [
                                {"title":"Update locator","action":"Replace the stale CSS selector with the new data-testid.",
                                 "status":"SUGGESTED"}
                              ],
                              "codeBlocks": [
                                {"title":"Locator fix","language":"java",
                                 "code":"driver.element().click(SHAFT.GUI.Locator.hasTestId(\\"checkout-submit\\"));",
                                 "copyPasteReady":true}
                              ],
                              "providerFallback": {"used":false,"reason":"AI advisory disabled by default."},
                              "bundlePath": "target/shaft-doctor/evidence-bundle.json",
                              "jsonReportPath": "target/shaft-doctor/doctor-report.json",
                              "markdownReportPath": "target/shaft-doctor/doctor-report.md",
                              "warnings": []
                            }
                            """), null);
            component.simulateAppendForTest("assistant", cardMarkdown, "");
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, WIDTH, HEIGHT));
        });
        return image.get();
    }

    private static BufferedImage renderApprovalPrompt(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftAssistantChatState chatState = new ShaftAssistantChatState();
            chatState.append("user", "/record-web https://example.com", "");
            ShaftSettingsState.Settings settings = defaultSettings();
            settings.defaultAutobotMode = "AGENT";
            ShaftAssistantPanel component = new ShaftAssistantPanel(screenshotProject(), settings, chatState,
                    () -> {
                    });
            selectButton(component, "Allow source edits");
            invokeStartMcpInvocation(component, AssistantCommand.Invocation.tool(
                    "capture_start", captureStartArguments()));
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, WIDTH, HEIGHT));
        });
        return image.get();
    }

    /**
     * Renders the Assistant local-model refresh control in its fallback state (issue #3551): when
     * the connected local CLI reports no models, {@code applyLocalModels} falls back to the curated
     * {@link AssistantModelCatalog} list and sets {@code localModelListIsFallback}, and the "Refresh
     * local agent models" button stays visible so the user can retry the live CLI listing. {@code
     * defaultSettings()} already normalizes to provider=LOCAL/runtime=CLI, so the panel starts in
     * the local-CLI configuration this control only appears in; the empty live list is then forced
     * through the same {@code applyLocalModels} seam production uses after a real CLI listing call
     * (via reflection, since it is private), rather than racing the panel's own async CLI probe
     * that already runs once at construction time.
     */
    private static BufferedImage renderAssistantModelFallback(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftAssistantChatState chatState = new ShaftAssistantChatState();
            ShaftAssistantPanel component = new ShaftAssistantPanel(screenshotProject(), defaultSettings(), chatState,
                    () -> {
                    });
            invokeApplyLocalModels(component, "CODEX", List.of());
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, WIDTH, HEIGHT));
        });
        return image.get();
    }

    private static void invokeApplyLocalModels(ShaftAssistantPanel component, String family, List<String> models) {
        try {
            Method method = ShaftAssistantPanel.class.getDeclaredMethod("applyLocalModels", String.class, List.class);
            method.setAccessible(true);
            method.invoke(component, family, models);
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Unable to apply the local-model fallback state", exception);
        }
    }

    /**
     * BEST-EFFORT representative shot of the "/" command popup (issue #3550). A live {@link
     * JPopupMenu} never paints when captured off-screen through {@link #render}: {@code
     * JPopupMenu#show} backs it with a heavyweight popup window this headless harness never
     * realizes, so the menu is built and populated exactly as {@code populateContextPopup} does --
     * one {@link JMenuItem} per {@link AssistantCommand.CommandHint} from {@code
     * AssistantCommand.commandHints(false)} (core-only, matching default non-Expert mode) -- and
     * rendered standalone, without ever calling {@code show}. The interactive filter/select
     * behaviour behind this popup is already covered by {@code ShaftPanelSetupTest}; this shot is
     * evidence of the command list's contents only.
     */
    private static BufferedImage renderAssistantSlashCommands(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            JPopupMenu popup = new JPopupMenu("Assistant context suggestions");
            popup.getAccessibleContext().setAccessibleName("Assistant context suggestions");
            for (AssistantCommand.CommandHint hint : AssistantCommand.commandHints(false)) {
                JMenuItem item = new JMenuItem("<html><b>" + hint.canonical() + "</b> — " + hint.summary()
                        + "<br><small>" + hint.example() + "</small></html>");
                item.getAccessibleContext().setAccessibleName("Insert " + hint.canonical());
                popup.add(item);
            }
            SwingUtilities.updateComponentTreeUI(popup);
            Dimension size = popup.getPreferredSize();
            popup.setSize(size);
            popup.doLayout();
            layout(popup, !dark);
            image.set(render(popup, Math.max(1, size.width), Math.max(1, size.height)));
        });
        return image.get();
    }

    /**
     * Renders the Tools panel's humanized doctor card (issue #3552): a {@code
     * doctor_analyze_failed_allour} result routed through {@link AssistantMarkdown} instead of raw
     * pretty-printed JSON, with the "View raw JSON" toggle button visible (still one click away).
     */
    private static BufferedImage renderToolsHumanizedDoctorCard(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftFeaturePanel component = new ShaftFeaturePanel(screenshotProject(), defaultSettings());
            invokeShowResult(component, "doctor_analyze_failed_allure", ShaftMcpToolResult.success("""
                    {
                      "schemaVersion": "1.0",
                      "status": "DETERMINISTIC",
                      "bundleId": "bundle-123",
                      "primaryCause": "LOCATOR",
                      "confidence": "HIGH",
                      "summary": "The sign-in button locator no longer matches the page after a redesign.",
                      "actions": [
                        {"title":"Update locator","action":"Replace the stale CSS selector with the new data-testid.",
                         "status":"SUGGESTED"}
                      ],
                      "codeBlocks": [
                        {"title":"Locator fix","language":"java",
                         "code":"driver.element().click(SHAFT.GUI.Locator.hasTestId(\\"sign-in-button\\"));",
                         "copyPasteReady":true}
                      ],
                      "providerFallback": {"used":false,"reason":"AI advisory disabled by default."},
                      "bundlePath": "target/shaft-doctor/evidence-bundle.json",
                      "jsonReportPath": "target/shaft-doctor/doctor-report.json",
                      "markdownReportPath": "target/shaft-doctor/doctor-report.md",
                      "warnings": []
                    }
                    """), null);
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, WIDTH, HEIGHT));
        });
        return image.get();
    }

    private static void invokeShowResult(
            ShaftFeaturePanel component, String toolName, ShaftMcpToolResult result, Throwable error) {
        try {
            Method method = ShaftFeaturePanel.class.getDeclaredMethod(
                    "showResult", String.class, ShaftMcpToolResult.class, Throwable.class);
            method.setAccessible(true);
            method.invoke(component, toolName, result, error);
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Unable to render the Tools panel result", exception);
        }
    }

    /**
     * Renders the SHAFT tool window in DEFAULT mode (advancedUiEnabled=false -- how most users run
     * it) right after a gate-audited entry point (issue #3552, e.g. {@code
     * ShaftToolWorkflowLauncher}, {@code RecordShaftFlowHereAction}, {@code RecordApiWebAction})
     * routed a plain-language request into the Assistant composer instead of silently no-opping or
     * dead-ending in a warning. The composer is filled but nothing has been sent -- the transcript
     * stays on the first-run welcome, proving this is a prefill, not an auto-send.
     */
    private static BufferedImage renderAssistantDefaultModePrefill(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftSettingsState.Settings settings = defaultSettings();
            ShaftToolWindowPanel component = new ShaftToolWindowPanel(screenshotProject(), settings);
            component.prefillAssistantPrompt("Diagnose my last failed test run");
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, WIDTH, HEIGHT));
        });
        return image.get();
    }

    private static BufferedImage renderPostSetupSettings(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
            settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
            settings.mcpSetupComplete = true;
            ShaftMcpSetupPanel component = new ShaftMcpSetupPanel(screenshotProject(), settings,
                    () -> {
                    }, (client, runtime) -> ShaftMcpToolResult.success("Codex CLI executable is available on PATH."));
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, WIDTH, HEIGHT));
        });
        return image.get();
    }

    private static JsonObject captureStartArguments() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("targetUrl", "https://example.com");
        arguments.addProperty("outputPath", "recordings/intellij-capture.json");
        return arguments;
    }

    private static void invokeStartMcpInvocation(ShaftAssistantPanel component, AssistantCommand.Invocation invocation) {
        try {
            Method method = ShaftAssistantPanel.class.getDeclaredMethod("startMcpInvocation", AssistantCommand.Invocation.class);
            method.setAccessible(true);
            method.invoke(component, invocation);
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Unable to render the approval prompt widget", exception);
        }
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
            ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(screenshotProject(), new ShaftSettingsState.Settings());
            toolWindow.setSize(new Dimension(width, height));
            toolWindow.setPreferredSize(new Dimension(width, height));
            SwingUtilities.updateComponentTreeUI(toolWindow);
            toolWindow.doLayout();
            layout(toolWindow, !dark);

            // Verify setup panel labels are not cropped and backgrounds are continuous
            verifySetupPanelRendering(toolWindow);

            image.set(render(toolWindow, width, height));
        });
        return image.get();
    }

    private static BufferedImage renderSetupGemini(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftMcpSetupPanel component = new ShaftMcpSetupPanel(screenshotProject(),
                    new ShaftSettingsState.Settings(),
                    () -> {
                    }, (client, runtime) -> ShaftMcpToolResult.success("Codex CLI executable is available on PATH."),
                    new ShaftMcpSetupPanel.CloudKeyStore() {
                        @Override
                        public boolean hasKey(String keyName) {
                            return false;
                        }

                        @Override
                        public void saveKey(String keyName, char[] secret) {
                            // Screenshot rendering never stores real keys.
                        }
                    });
            JComboBox<?> family = findComboByAccessibleName(component, "Assistant family");
            if (family != null) {
                family.setSelectedItem("GEMINI");
            }
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);

            // Verify setup panel labels are not cropped and backgrounds are continuous
            verifySetupPanelRendering(component);

            image.set(render(component, WIDTH, HEIGHT));
        });
        return image.get();
    }

    private static JComboBox<?> findComboByAccessibleName(java.awt.Component component, String accessibleName) {
        if (component instanceof JComboBox<?> combo
                && accessibleName.equals(combo.getAccessibleContext().getAccessibleName())) {
            return combo;
        }
        if (component instanceof java.awt.Container container) {
            for (java.awt.Component child : container.getComponents()) {
                JComboBox<?> found = findComboByAccessibleName(child, accessibleName);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
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
                    Initialized SHAFT MCP 10.3.20260714
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

            // Verify setup panel labels are not cropped and backgrounds are continuous
            verifySetupPanelRendering(component);

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
                    "Could not resolve artifact io.github.shafthq:shaft-mcp:jar:10.3.20260714"), null);
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);

            // Verify setup panel labels are not cropped and backgrounds are continuous
            verifySetupPanelRendering(component);

            image.set(render(component, WIDTH, HEIGHT));
        });
        return image.get();
    }

    /**
     * Renders the SHAFT MCP version wizard-step row in its Offline state (issue #3551): when the
     * "latest" half of a {@link ShaftMcpVersionCheck} can't be resolved (no network), the row shows
     * a neutral "Offline" badge with the currently-installed version and a "Press Check to retry."
     * callout -- never a red "Failed", and never blocking the rest of setup -- matching {@code
     * ShaftPanelSetupTest#setupPanelMcpVersionStepReflectsRealVersionCheck}. Reproduced the same way
     * that test does: swap in a fake {@code mcpVersionChecker} and click "Check SHAFT MCP version".
     */
    private static BufferedImage renderSetupMcpOffline(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
            settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
            ShaftMcpSetupPanel component = new ShaftMcpSetupPanel(screenshotProject(), settings,
                    () -> {
                    });
            setField(component, "mcpVersionChecker",
                    (java.util.function.Supplier<ShaftMcpVersionCheck.Result>) () -> new ShaftMcpVersionCheck.Result(
                            ShaftMcpVersionCheck.State.LATEST_UNKNOWN, "10.3.20260703", ""));
            clickAccessible(component, "Check SHAFT MCP version");
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);

            // Verify setup panel labels are not cropped and backgrounds are continuous
            verifySetupPanelRendering(component);

            image.set(render(component, WIDTH, HEIGHT));
        });
        return image.get();
    }

    private static void setField(Object target, String name, Object value) {
        try {
            Field field = target.getClass().getDeclaredField(name);
            field.setAccessible(true);
            field.set(target, value);
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Unable to set field " + name, exception);
        }
    }

    private static void clickAccessible(Component component, String accessibleName) {
        JButton button = findByAccessibleName(component, accessibleName, JButton.class);
        if (button == null) {
            throw new IllegalStateException("No button found with accessible name: " + accessibleName);
        }
        button.doClick();
    }

    private static <T extends JComponent> T findByAccessibleName(
            Component component, String accessibleName, Class<T> type) {
        if (type.isInstance(component)
                && accessibleName.equals(((JComponent) component).getAccessibleContext().getAccessibleName())) {
            return type.cast(component);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                T found = findByAccessibleName(child, accessibleName, type);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
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

    private static void selectButton(Component component, String text) {
        if (component instanceof AbstractButton button && text.equals(button.getText())) {
            button.setSelected(true);
            return;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                selectButton(child, text);
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

    private static void verifySetupPanelRendering(JComponent component) {
        // Find the setup panel within the component tree
        final JComponent[] setupPanel = {null};
        walkComponentsForVerification(component, comp -> {
            if (comp instanceof ShaftMcpSetupPanel) {
                setupPanel[0] = (JComponent) comp;
            }
        });

        if (setupPanel[0] != null) {
            verifySetupPanelLabelsAndBackground(setupPanel[0]);
        }
    }

    private static void verifySetupPanelLabelsAndBackground(JComponent setupPanel) {
        // Verify labels are not cropped by checking their preferred vs actual size.
        // Components that are hidden (e.g. runtimeStatus before verification, or the
        // "ready" chat row before setup completes) are intentionally zero-sized and
        // must not be treated as cropping regressions.
        final List<JLabel> labels = new ArrayList<>();
        walkComponentsForVerification(setupPanel, comp -> {
            if (comp instanceof JLabel lbl && lbl.getText() != null && !lbl.getText().isEmpty()
                    && isEffectivelyVisible(lbl)) {
                labels.add(lbl);
            }
        });

        // Check that labels render at their preferred size (no cropping)
        for (JLabel label : labels) {
            assertTrue(label.getSize().width >= label.getPreferredSize().width || label.getPreferredSize().width == 0,
                    "Label '" + label.getText() + "' should not be cropped horizontally. Size: " +
                    label.getSize().width + ", Preferred: " + label.getPreferredSize().width);
            assertTrue(label.getSize().height >= label.getPreferredSize().height || label.getPreferredSize().height == 0,
                    "Label '" + label.getText() + "' should not be cropped vertically. Size: " +
                    label.getSize().height + ", Preferred: " + label.getPreferredSize().height);
        }

        // Verify every step row's nested child panels are either non-opaque (so the
        // step's accent background shows through) or explicitly painted with that same
        // step's background color. Each wizard step (upgradeRow, chooseRow, installRow,
        // checkRow, chatRow) can carry a different accent color, so continuity must be
        // checked per-step-row rather than against one global background.
        final List<JPanel> stepRows = new ArrayList<>();
        for (Component child : setupPanel.getComponents()) {
            collectStepRows(child, stepRows);
        }

        for (JPanel stepRow : stepRows) {
            Color stepBackground = stepRow.getBackground();
            final List<JPanel> nestedPanels = new ArrayList<>();
            walkComponentsForVerification(stepRow, comp -> {
                if (comp instanceof JPanel pnl && comp != stepRow) {
                    nestedPanels.add(pnl);
                }
            });
            for (JPanel nested : nestedPanels) {
                boolean isNonOpaque = !nested.isOpaque();
                boolean matchesStepBackground = stepBackground != null
                        && stepBackground.equals(nested.getBackground());
                assertTrue(isNonOpaque || matchesStepBackground,
                        "Nested panel inside step row should be non-opaque or share the step's background color. "
                        + "Opaque: " + nested.isOpaque()
                        + ", panel background: " + nested.getBackground()
                        + ", step background: " + stepBackground);
            }
        }
    }

    /** A component is effectively invisible for cropping purposes if it or any ancestor up to the
     * setup panel has been explicitly hidden via {@link Component#setVisible(boolean)}. */
    private static boolean isEffectivelyVisible(Component component) {
        for (Component current = component; current != null; current = current.getParent()) {
            if (!current.isVisible()) {
                return false;
            }
        }
        return true;
    }

    /** Recursively collects the top-level wizard-step row panels (as produced by
     * {@code ShaftMcpSetupPanel#stepRow}) without descending into their own children,
     * so each row's background is only compared against its own nested panels. */
    private static void collectStepRows(Component component, List<JPanel> stepRows) {
        if (component instanceof JPanel panel && isWizardStepRow(panel)) {
            stepRows.add(panel);
            return;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                collectStepRows(child, stepRows);
            }
        }
    }

    /** Wizard step rows are opaque JPanels whose direct children include a step label and a state
     * label (see {@code ShaftMcpSetupPanel#stepRow}); plain layout containers are not opaque. */
    private static boolean isWizardStepRow(JPanel panel) {
        if (!panel.isOpaque()) {
            return false;
        }
        int labelCount = 0;
        for (Component child : panel.getComponents()) {
            if (child instanceof JLabel) {
                labelCount++;
            }
        }
        return labelCount >= 2;
    }

    private static void walkComponentsForVerification(Component component, Consumer<Component> visitor) {
        visitor.accept(component);
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                walkComponentsForVerification(child, visitor);
            }
        }
    }
}
