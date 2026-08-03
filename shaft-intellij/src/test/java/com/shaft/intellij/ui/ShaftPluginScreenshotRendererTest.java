package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.intellij.openapi.project.Project;
import com.intellij.ui.JBColor;
import com.intellij.ui.components.JBScrollPane;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsConfigurable;
import com.shaft.intellij.settings.ShaftSettingsState;
import com.shaft.intellij.testindex.ShaftTestDiscovery;
import com.shaft.intellij.testindex.ShaftTestIndex;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import javax.imageio.ImageIO;
import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollBar;
import javax.swing.JTextArea;
import javax.swing.JToggleButton;
import javax.swing.JViewport;
import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.plaf.ColorUIResource;
import javax.swing.text.BadLocationException;
import javax.swing.text.JTextComponent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Rectangle;
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
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
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
    // Every UIManager key applyThemeDefaults() overrides -- kept in sync with that method so the
    // regression test below can snapshot/restore each one (see its finally block).
    private static final String[] THEME_DEFAULT_KEYS = {
            "Panel.background", "TabbedPane.background", "TabbedPane.foreground", "SplitPane.background",
            "ScrollPane.background", "Viewport.background", "Label.foreground", "Button.background",
            "Button.foreground", "ComboBox.background", "ComboBox.foreground", "TextArea.background",
            "TextArea.foreground", "TextArea.caretForeground", "TextField.background", "TextField.foreground",
            "TextField.caretForeground", "Component.borderColor"
    };
    // Issue #3782: snapshots/restores the LookAndFeel instance, JBColor dark flag, and every
    // THEME_DEFAULT_KEYS UIManager key around each test in this class, on the EDT -- extracted from
    // (and behaviorally identical to) the inline finally block PR #3781 proved sufficient (5/5
    // consecutive green full-suite runs) for lightThemeCheckboxRendersVisibleGlyphAndReflectsSelectionState,
    // the only test in this class that installs a real platform L&F during an ordinary test run.
    @RegisterExtension
    static final LookAndFeelIsolationExtension LOOK_AND_FEEL_ISOLATION =
            new LookAndFeelIsolationExtension(THEME_DEFAULT_KEYS);
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
    private static final String WIKIPEDIA_SHAFT_CODE_SAMPLE = """
            ```java
            import org.openqa.selenium.By;

            public class WikipediaSearchTest {
                private final SHAFT.GUI.WebDriver driver = new SHAFT.GUI.WebDriver();

                @Test
                void opensSoftwareTestingResult() {
                    driver.browser().navigateToURL("https://en.wikipedia.org/wiki/Main_Page");
                    driver.element().type(By.id("searchInput"), "Software testing framework");
                    driver.element().keyPress(By.id("searchInput"), "ENTER");
                    driver.element().click(By.xpath(
                            "(//div[contains(@class,'mw-search-result-heading')])[1]//a"));
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
        Path assistantAttachmentsScreenshot = outputPath.resolve("intellij-plugin-assistant-attachments.png");
        Path assistantEmptyNarrowScreenshot = outputPath.resolve("intellij-plugin-assistant-empty-narrow.png");
        Path assistantExpandedSettingsNarrowScreenshot =
                outputPath.resolve("intellij-plugin-assistant-expanded-settings-narrow.png");
        Path assistantDarkScreenshot = outputPath.resolve("intellij-plugin-assistant-dark.png");
        Path assistantNarrowDarkScreenshot = outputPath.resolve("intellij-plugin-assistant-narrow-dark.png");
        Path assistantLiveDarkScreenshot = outputPath.resolve("intellij-plugin-assistant-live-output-dark.png");
        Path assistantActiveStatusNarrowScreenshot =
                outputPath.resolve("intellij-plugin-assistant-active-status-narrow.png");
        Path assistantProgressMilestonesScreenshot = outputPath.resolve("intellij-plugin-assistant-progress-milestones.png");
        Path assistantFailureRecoveryCardScreenshot = outputPath.resolve("intellij-plugin-assistant-failure-recovery-card.png");
        Path assistantToolResultRawOutputScreenshot = outputPath.resolve("intellij-plugin-assistant-tool-result-raw-output.png");
        Path assistantCancelledScreenshot = outputPath.resolve("intellij-plugin-assistant-cancelled.png");
        Path assistantKilledScreenshot = outputPath.resolve("intellij-plugin-assistant-killed.png");
        Path assistantCancelledPendingAnswerScreenshot =
                outputPath.resolve("intellij-plugin-assistant-cancelled-pending-answer.png");
        Path assistantApprovalPromptScreenshot = outputPath.resolve("intellij-plugin-assistant-approval-prompt.png");
        Path assistantModelUnavailableScreenshot = outputPath.resolve("intellij-plugin-assistant-model-unavailable.png");
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
        Path mcpSetupPrerequisitesRecheckBeforeScreenshot =
                outputPath.resolve("intellij-plugin-mcp-setup-prerequisites-recheck-before.png");
        Path mcpSetupPrerequisitesRecheckAfterScreenshot =
                outputPath.resolve("intellij-plugin-mcp-setup-prerequisites-recheck-after.png");
        Path settingsScreenshot = outputPath.resolve("intellij-plugin-settings.png");
        Path settingsDarkScreenshot = outputPath.resolve("intellij-plugin-settings-dark.png");
        Path mcpGuideScreenshot = outputPath.resolve("intellij-plugin-mcp-guide.png");

        write(assistantLightScreenshot, renderToolWindow(0, "", LIGHT_THEME, false));
        write(assistantEmptyScreenshot, renderAssistantEmpty(LIGHT_THEME, false));
        write(assistantAttachmentsScreenshot, renderAssistantWithAttachments(LIGHT_THEME, false));
        write(assistantEmptyNarrowScreenshot, renderAssistantEmpty(DARK_THEME, true, NARROW_WIDTH, HEIGHT));
        write(assistantExpandedSettingsNarrowScreenshot, renderAssistantExpandedSettingsNarrow(DARK_THEME, true));
        write(assistantDarkScreenshot, renderToolWindow(0, "", DARK_THEME, true));
        write(assistantNarrowDarkScreenshot, renderToolWindow(0, "", DARK_THEME, true, NARROW_WIDTH, HEIGHT));
        write(assistantLiveDarkScreenshot, renderAssistantLiveOutput(DARK_THEME, true));
        write(assistantActiveStatusNarrowScreenshot, renderAssistantActiveStatusNarrow(DARK_THEME, true));
        write(assistantProgressMilestonesScreenshot, renderAssistantProgressMilestones(LIGHT_THEME, false));
        write(assistantFailureRecoveryCardScreenshot, renderAssistantFailureRecoveryCard(LIGHT_THEME, false));
        write(assistantToolResultRawOutputScreenshot, renderAssistantToolResultRawOutput(LIGHT_THEME, false));
        write(assistantCancelledScreenshot, renderAssistantCancelled(LIGHT_THEME, false));
        write(assistantKilledScreenshot, renderAssistantKilled(LIGHT_THEME, false));
        write(assistantCancelledPendingAnswerScreenshot, renderAssistantCancelledPendingAnswer(LIGHT_THEME, false));
        write(assistantApprovalPromptScreenshot, renderApprovalPrompt(LIGHT_THEME, false));
        write(assistantModelUnavailableScreenshot, renderAssistantModelUnavailable(LIGHT_THEME, false));
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
        write(mcpSetupPrerequisitesRecheckBeforeScreenshot,
                renderSetupPrerequisitesRecheckBefore(LIGHT_THEME, false));
        write(mcpSetupPrerequisitesRecheckAfterScreenshot,
                renderSetupPrerequisitesRecheckAfter(LIGHT_THEME, false));
        write(settingsScreenshot, renderSettings(LIGHT_THEME, false));
        write(settingsDarkScreenshot, renderSettings(DARK_THEME, true));
        write(mcpGuideScreenshot, renderToolWindow(9, "Guide", LIGHT_THEME, false));
        assertAll(
                () -> assertTrue(Files.size(assistantLightScreenshot) > 0, assistantLightScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantEmptyScreenshot) > 0, assistantEmptyScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantAttachmentsScreenshot) > 0, assistantAttachmentsScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantEmptyNarrowScreenshot) > 0, assistantEmptyNarrowScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantExpandedSettingsNarrowScreenshot) > 0,
                        assistantExpandedSettingsNarrowScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantDarkScreenshot) > 0, assistantDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantNarrowDarkScreenshot) > 0, assistantNarrowDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantLiveDarkScreenshot) > 0, assistantLiveDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantActiveStatusNarrowScreenshot) > 0,
                        assistantActiveStatusNarrowScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantProgressMilestonesScreenshot) > 0,
                        assistantProgressMilestonesScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantFailureRecoveryCardScreenshot) > 0,
                        assistantFailureRecoveryCardScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantCancelledScreenshot) > 0, assistantCancelledScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantKilledScreenshot) > 0, assistantKilledScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantCancelledPendingAnswerScreenshot) > 0,
                        assistantCancelledPendingAnswerScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantApprovalPromptScreenshot) > 0, assistantApprovalPromptScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(assistantModelUnavailableScreenshot) > 0,
                        assistantModelUnavailableScreenshot + " should be non-empty"),
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
                () -> assertTrue(Files.size(mcpSetupPrerequisitesRecheckBeforeScreenshot) > 0,
                        mcpSetupPrerequisitesRecheckBeforeScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(mcpSetupPrerequisitesRecheckAfterScreenshot) > 0,
                        mcpSetupPrerequisitesRecheckAfterScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(settingsScreenshot) > 0, settingsScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(settingsDarkScreenshot) > 0, settingsDarkScreenshot + " should be non-empty"),
                () -> assertTrue(Files.size(mcpGuideScreenshot) > 0, mcpGuideScreenshot + " should be non-empty"),
                () -> assertDimensions(assistantLightScreenshot),
                () -> assertDimensions(assistantEmptyScreenshot),
                () -> assertDimensions(assistantAttachmentsScreenshot),
                () -> assertDimensions(assistantEmptyNarrowScreenshot, NARROW_WIDTH, HEIGHT),
                () -> assertDimensions(assistantExpandedSettingsNarrowScreenshot, NARROW_WIDTH, HEIGHT),
                () -> assertDimensions(assistantDarkScreenshot),
                () -> assertDimensions(assistantNarrowDarkScreenshot, NARROW_WIDTH, HEIGHT),
                () -> assertDimensions(assistantLiveDarkScreenshot),
                () -> assertDimensions(assistantActiveStatusNarrowScreenshot, NARROW_WIDTH, HEIGHT),
                () -> assertDimensions(assistantProgressMilestonesScreenshot),
                () -> assertDimensions(assistantFailureRecoveryCardScreenshot),
                () -> assertDimensions(assistantCancelledScreenshot),
                () -> assertDimensions(assistantKilledScreenshot),
                () -> assertDimensions(assistantApprovalPromptScreenshot),
                () -> assertDimensions(assistantModelUnavailableScreenshot),
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
                () -> assertDimensions(mcpSetupPrerequisitesRecheckBeforeScreenshot),
                () -> assertDimensions(mcpSetupPrerequisitesRecheckAfterScreenshot),
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
                () -> assertTrue(Files.mismatch(mcpSetupPrerequisitesRecheckBeforeScreenshot,
                                mcpSetupPrerequisitesRecheckAfterScreenshot) >= 0,
                        "Recheck collapsing the satisfied prerequisites row must visibly change the screenshot"),
                () -> assertTrue(ASSISTANT_SHAFT_CODE_SAMPLE.contains("SHAFT.GUI.WebDriver")),
                () -> assertTrue(ASSISTANT_SHAFT_CODE_SAMPLE.contains("driver.browser().navigateToURL")),
                () -> assertTrue(ASSISTANT_SHAFT_CODE_SAMPLE.contains("driver.element().click")),
                () -> assertFalse(ASSISTANT_SHAFT_CODE_SAMPLE.contains("driver.get(")),
                () -> assertFalse(ASSISTANT_SHAFT_CODE_SAMPLE.contains("driver.findElement(")),
                () -> assertTrue(WIKIPEDIA_SHAFT_CODE_SAMPLE.contains("https://en.wikipedia.org/")),
                () -> assertTrue(WIKIPEDIA_SHAFT_CODE_SAMPLE.contains("mw-search-result-heading")),
                () -> assertFalse(WIKIPEDIA_SHAFT_CODE_SAMPLE.contains("https://example.com")));
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
            // Fixed discovery result standing in for real PSI discovery, which this fake
            // screenshotProject() Proxy cannot back -- mirrors the recorded runs above so every
            // node in the screenshot's tree carries a decoration.
            List<ShaftTestDiscovery.DiscoveredTestClass> discoveredClasses = List.of(
                    new ShaftTestDiscovery.DiscoveredTestClass(
                            "com.example.SignInTest", "com.example", "SignInTest", List.of("testSignIn")),
                    new ShaftTestDiscovery.DiscoveredTestClass(
                            "com.example.CheckoutTest", "com.example", "CheckoutTest", List.of("testCheckout")),
                    new ShaftTestDiscovery.DiscoveredTestClass(
                            "com.example.SearchTest", "com.example", "SearchTest", List.of("testSearch")));
            ShaftTestsPanel component = new ShaftTestsPanel(
                    screenshotProject(), testIndex, () -> discoveredClasses);
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

    /** Selects the first FAIL class node so the screenshot shows Doctor/Heal enabled, falling back
     * to the first class node when nothing failed. */
    private static void selectFailRowIfPresent(ShaftTestsPanel component) {
        DefaultMutableTreeNode root = (DefaultMutableTreeNode) component.treeForTest().getModel().getRoot();
        DefaultMutableTreeNode fallback = null;
        Enumeration<TreeNode> packages = root.children();
        while (packages.hasMoreElements()) {
            DefaultMutableTreeNode packageNode = (DefaultMutableTreeNode) packages.nextElement();
            Enumeration<TreeNode> classes = packageNode.children();
            while (classes.hasMoreElements()) {
                DefaultMutableTreeNode classNode = (DefaultMutableTreeNode) classes.nextElement();
                if (fallback == null) {
                    fallback = classNode;
                }
                if (classNode.getUserObject() instanceof ShaftTestsPanel.TestTreeNode treeNode
                        && ShaftTestsPanel.isFailRow(treeNode.runState())) {
                    component.treeForTest().setSelectionPath(new TreePath(classNode.getPath()));
                    return;
                }
            }
        }
        if (fallback != null) {
            component.treeForTest().setSelectionPath(new TreePath(fallback.getPath()));
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

    /** Documents the narrow responsive layout after the compact Run settings disclosure is opened. */
    private static BufferedImage renderAssistantExpandedSettingsNarrow(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            JComponent component = new ShaftToolWindowPanel(
                    screenshotProject(), defaultSettings(), AssistantLocalAgentRunner::readiness,
                    new ShaftAssistantChatState());
            component.setSize(new Dimension(NARROW_WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(NARROW_WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            JToggleButton settings = findByAccessibleName(component, "Run settings", JToggleButton.class);
            assertNotNull(settings, "The narrow Assistant must expose Run settings");
            settings.doClick();
            component.revalidate();
            component.doLayout();
            layout(component, !dark);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, NARROW_WIDTH, HEIGHT));
        });
        return image.get();
    }

    /** The narrow empty state remains quiet: onboarding guidance lives in prefill suggestions only. */
    @Test
    void emptyAssistantHasNoFirstRunWelcomeAtNarrowDarkWidth() throws InterruptedException, InvocationTargetException {
        AtomicReference<JComponent> component = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(DARK_THEME, true);
            JComponent panel = new ShaftToolWindowPanel(
                    screenshotProject(), defaultSettings(), AssistantLocalAgentRunner::readiness,
                    new ShaftAssistantChatState());
            panel.setSize(new Dimension(NARROW_WIDTH, HEIGHT));
            layout(panel, false);
            component.set(panel);
        });

        assertNull(findByAccessibleName(component.get(), "Dismiss first run coach", JButton.class),
                "The quiet empty Assistant must not render a first-run welcome essay or dismissal control");
        assertNull(findByAccessibleName(component.get(), "Assistant welcome message content", JEditorPane.class));
    }

    /**
     * Issue #4191 (tracker #4160): PR #4184's {@link AssistantTranscriptView#widthCappedWidget}
     * javadoc claimed {@link ToolApprovalPromptPanel} has zero insets and is therefore unaffected by
     * that fix's insets-aware cap widening. Measuring the panel's real border
     * ({@code createEtchedBorder()} composed with {@code JBUI.Borders.empty(8)}) shows non-zero
     * insets of {@code (10,10,10,10)}, which does widen its outer-width cap by ~20px -- see the
     * corrected javadoc. No CI-run test exercised this panel's rendered width before this one: the
     * screenshot-renderer coverage that would show it ({@code assistantApprovalPromptScreenshot}) is
     * gated {@code assumeFalse} on {@code -Dshaft.intellij.screenshotDir}, which CI never sets. This
     * test carries no such gate and drives the exact same {@code NARROW_WIDTH} construction {@link
     * #emptyAssistantHasNoFirstRunWelcomeAtNarrowDarkWidth} uses, proving the extra ~20px does
     * not push the panel past the transcript viewport's visible (unscrolled-horizontally) width.
     */
    @Test
    void toolApprovalPromptFitsWithinTranscriptWidthAtNarrowToolWindowWidth()
            throws InterruptedException, InvocationTargetException {
        AtomicReference<ToolApprovalPromptPanel> approvalPanel = new AtomicReference<>();
        AtomicReference<JBScrollPane> transcriptScroll = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(DARK_THEME, true);
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
            component.setSize(new Dimension(NARROW_WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(NARROW_WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, false);

            approvalPanel.set(findByAccessibleName(
                    component, "Tool approval request for capture_start", ToolApprovalPromptPanel.class));
            transcriptScroll.set(findByAccessibleName(component, "Assistant transcript", JBScrollPane.class));
        });

        assertNotNull(approvalPanel.get(),
                "The tool approval prompt must render at a narrow dark tool window width");
        assertNotNull(transcriptScroll.get(),
                "The Assistant transcript scroll pane must be present at a narrow dark tool window width");

        JViewport viewport = transcriptScroll.get().getViewport();
        Rectangle boundsInView = SwingUtilities.convertRectangle(
                approvalPanel.get().getParent(), approvalPanel.get().getBounds(), viewport.getView());
        assertTrue(boundsInView.x + boundsInView.width <= viewport.getWidth(),
                "The tool approval prompt (real insets (10,10,10,10), widening widthCappedWidget's "
                        + "outer-width cap by ~20px) must not extend past the transcript viewport's "
                        + "visible width at NARROW_WIDTH -- panel right edge "
                        + (boundsInView.x + boundsInView.width) + "px, viewport width "
                        + viewport.getWidth() + "px.");
    }

    /**
     * Reproduces a visual clipping defect found while reviewing {@code
     * intellij-plugin-assistant-approval-prompt.png} evidence: at the ORDINARY (non-narrow) tool
     * window width this renderer already uses for {@link #renderApprovalPrompt}, the approval
     * prompt's plain-language summary ("This will run with targetUrl: ...") and its raw-JSON
     * arguments dump are both cut off mid-word with no ellipsis or scroll affordance -- the same
     * silent-clip failure mode as issue #4174's welcome-bubble paragraph crop, but in {@link
     * ToolApprovalPromptPanel} instead. {@code ToolApprovalPromptPanelTest#argumentsSummaryRendersFullJsonWithoutTruncation}
     * only asserts the underlying {@link JTextArea#getText()} model string is never
     * character-truncated; it never lays the panel out inside the real {@link ShaftAssistantPanel}
     * transcript and checks whether every rendered line is actually painted within the text area's
     * own allocated bounds, which is what this test drives instead, using the exact same {@code
     * captureStartArguments()} payload and {@code WIDTH}/{@code HEIGHT} {@link #renderApprovalPrompt}
     * already renders screenshot evidence for.
     */
    @Test
    void toolApprovalPromptArgumentsTextIsNotVerticallyClippedAtOrdinaryWidth()
            throws InterruptedException, InvocationTargetException {
        AtomicReference<JTextArea> plainLanguageArea = new AtomicReference<>();
        AtomicReference<JTextArea> argumentsArea = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(LIGHT_THEME, false);
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
            JToggleButton details = findByAccessibleName(component, "Show technical details", JToggleButton.class);
            assertNotNull(details);
            details.doClick();
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, true);

            plainLanguageArea.set(findByAccessibleName(
                    component, "Tool approval plain-language summary", JTextArea.class));
            argumentsArea.set(findByAccessibleName(component, "Tool approval arguments", JTextArea.class));
        });

        assertNotNull(plainLanguageArea.get(),
                "The tool approval prompt's plain-language summary must render at ordinary tool window width");
        assertNotNull(argumentsArea.get(),
                "The tool approval prompt's raw-JSON arguments summary must render at ordinary tool window width");
        assertTextAreaFullyPainted(plainLanguageArea.get(), "plain-language summary");
        assertTextAreaFullyPainted(argumentsArea.get(), "raw-JSON arguments summary");
    }

    /**
     * Asserts the last character of {@code area}'s full text model is painted within the text
     * area's own allocated height -- the same {@code modelToView2D}-based technique {@link
     * #firstRunWelcomeTrailingParagraphIsNotClippedAtNarrowWidth} already uses for a {@link
     * JEditorPane}, applied here to a {@link JTextArea}.
     */
    private static void assertTextAreaFullyPainted(JTextArea area, String label) {
        int length = area.getDocument().getLength();
        assertTrue(length > 0, "The " + label + " text area must contain rendered text");
        try {
            java.awt.geom.Rectangle2D bounds2D = area.modelToView2D(length - 1);
            assertNotNull(bounds2D, "The " + label + " text area's last character must resolve a caret rectangle");
            Rectangle bounds = bounds2D.getBounds();
            assertTrue(bounds.y + bounds.height <= area.getHeight(),
                    "The " + label + " text area's final characters must be painted inside its own bounds "
                            + "(height=" + area.getHeight() + "), not silently clipped with no ellipsis or "
                            + "scroll affordance -- last character bottom edge was "
                            + (bounds.y + bounds.height) + "px, full text was: " + area.getText());
        } catch (BadLocationException exception) {
            throw new IllegalStateException(exception);
        }
    }

    /**
     * Issue #4191 (tracker #4160): the same "zero insets, no-op" javadoc claim {@code
     * widthCappedWidget} previously made about {@link ToolApprovalPromptPanel} above also named
     * {@link AssistantQuestionOptionsPanel} -- its real border ({@code createEmptyBorder(4, 0, 0, 0)}
     * composed with {@code JBUI.Borders.empty(2)}) measures {@code (6,2,2,2)}, widening its cap by
     * ~4px. Renders the panel via the same package-private {@code showAssistantQuestionOptions} seam
     * production code uses when an assistant turn's markdown contains a detected {@link
     * AssistantQuestion}, at the same narrow tool-window width the welcome-bubble tests use, and
     * proves the extra ~4px does not push it past the transcript viewport's visible width.
     */
    @Test
    void assistantQuestionOptionsFitWithinTranscriptWidthAtNarrowToolWindowWidth()
            throws InterruptedException, InvocationTargetException {
        AtomicReference<AssistantQuestionOptionsPanel> optionsPanel = new AtomicReference<>();
        AtomicReference<JBScrollPane> transcriptScroll = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(DARK_THEME, true);
            ShaftAssistantChatState chatState = new ShaftAssistantChatState();
            ShaftSettingsState.Settings settings = defaultSettings();
            ShaftAssistantPanel component = new ShaftAssistantPanel(screenshotProject(), settings, chatState,
                    () -> {
                    });
            invokeShowAssistantQuestionOptions(component, new AssistantQuestion(
                    "Which browser should the recording target?",
                    List.of("Chromium", "Firefox", "WebKit")));
            component.setSize(new Dimension(NARROW_WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(NARROW_WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, false);

            optionsPanel.set(findByAccessibleName(
                    component, "Suggested answers", AssistantQuestionOptionsPanel.class));
            transcriptScroll.set(findByAccessibleName(component, "Assistant transcript", JBScrollPane.class));
        });

        assertNotNull(optionsPanel.get(),
                "The assistant question options panel must render at a narrow dark tool window width");
        assertNotNull(transcriptScroll.get(),
                "The Assistant transcript scroll pane must be present at a narrow dark tool window width");

        JViewport viewport = transcriptScroll.get().getViewport();
        Rectangle boundsInView = SwingUtilities.convertRectangle(
                optionsPanel.get().getParent(), optionsPanel.get().getBounds(), viewport.getView());
        assertTrue(boundsInView.x + boundsInView.width <= viewport.getWidth(),
                "The assistant question options panel (real insets (6,2,2,2), widening "
                        + "widthCappedWidget's outer-width cap by ~4px) must not extend past the "
                        + "transcript viewport's visible width at NARROW_WIDTH -- panel right edge "
                        + (boundsInView.x + boundsInView.width) + "px, viewport width "
                        + viewport.getWidth() + "px.");
    }

    private static void invokeShowAssistantQuestionOptions(ShaftAssistantPanel component, AssistantQuestion question) {
        try {
            Method method = ShaftAssistantPanel.class.getDeclaredMethod("showAssistantQuestionOptions", AssistantQuestion.class);
            method.setAccessible(true);
            method.invoke(component, question);
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Unable to render the assistant question options widget", exception);
        }
    }

    /**
     * Renders the composer with the attach affordances populated (issue #3727): the "Attach" toolbar
     * button beside Send, and a removable chip per attachment above the prompt -- a picked file, a
     * large file truncated with a visible note, and a picked image. Attaches through the same
     * package-private {@code attachFileAtPath}/{@code attachImageAtPath} seams {@code
     * ShaftPanelSetupTest} drives directly (real temp files on disk, not reflection), so this shot
     * documents the exact production code path a real "Attach file from disk…" pick would run.
     */
    private static BufferedImage renderAssistantWithAttachments(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException, IOException {
        Path fixtureDirectory = Files.createTempDirectory("shaft-assistant-attachments");
        Path smallFile = fixtureDirectory.resolve("SignInTest.java");
        Files.writeString(smallFile, ASSISTANT_SHAFT_CODE_SAMPLE);
        Path largeFile = fixtureDirectory.resolve("huge-log.txt");
        Files.writeString(largeFile, "x".repeat(20_000));
        Path image = fixtureDirectory.resolve("failure-screenshot.png");
        Files.write(image, new byte[]{(byte) 0x89, 'P', 'N', 'G'});

        AtomicReference<BufferedImage> screenshot = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftSettingsState.Settings settings = defaultSettings();
            ShaftAssistantPanel component = new ShaftAssistantPanel(screenshotProject(), settings,
                    new ShaftAssistantChatState(), () -> {
                    });
            component.attachFileAtPath(smallFile);
            component.attachFileAtPath(largeFile);
            component.attachImageAtPath(image);
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            screenshot.set(render(component, WIDTH, HEIGHT));
        });
        return screenshot.get();
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

    /** Documents the one-line active-run strip at narrow width, including its fixed Cancel edge. */
    private static BufferedImage renderAssistantActiveStatusNarrow(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftAssistantChatState chatState = new ShaftAssistantChatState();
            chatState.append("user", "/browser open https://example.com sign in", "");
            ShaftSettingsState.Settings settings = defaultSettings();
            settings.defaultAutobotMode = "AGENT";
            ShaftAssistantPanel component = new ShaftAssistantPanel(screenshotProject(), settings, chatState,
                    () -> {
                    });
            invokeSetRunning(component, true, "Running: browser open https://example.com sign in");
            component.setSize(new Dimension(NARROW_WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(NARROW_WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, NARROW_WIDTH, HEIGHT));
            invokeSetRunning(component, false, "Try asking me to do something...");
        });
        return image.get();
    }

    /**
     * Renders the Assistant transcript with several streamed {@code notifications/progress}
     * agent-milestone chat bubbles already appended (issue #3546), so the screenshot documents
     * live-execution transparency instead of a static "Running" placeholder that never changes.
     * These milestones used to render into a separately-scrollable "Run timeline" list; issue #3695
     * folded them into the transcript as their own chat bubbles instead, which is what this shot now
     * documents. The milestone text mirrors what {@code CaptureGenerator#generate} actually reports
     * server-side (shaft-capture), for a realistic shot.
     *
     * <p>Driven through {@code appendAgentMilestone} directly — the same rendering path {@code
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
            invokeAppendAgentMilestone(component, "Tool selected: capture_generate_replay");
            invokeAppendAgentMilestone(component, "Running");
            invokeSetRunning(component, true, "Running: capture_generate_replay …");
            invokeAppendAgentMilestone(component, "Read capture session demo-recording.json");
            invokeAppendAgentMilestone(component, "Analyzed 12 captured event(s)");
            invokeAppendAgentMilestone(component, "Generated deterministic test source for DemoRecordingTest");
            invokeAppendAgentMilestone(component, "Compiled generated test: PASSED");
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

    private static void invokeAppendAgentMilestone(ShaftAssistantPanel component, String step) {
        try {
            Method method = ShaftAssistantPanel.class.getDeclaredMethod("appendAgentMilestone", String.class);
            method.setAccessible(true);
            method.invoke(component, step);
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Unable to render the agent-milestone chat bubbles", exception);
        }
    }

    /**
     * Renders the Assistant transcript's failure-recovery card (issue #3547): the plain-language
     * root-cause card that {@code FailedRunDoctorNotifier} now renders automatically after a failed
     * test run (and that the notification's "Diagnose"/"Heal" buttons also produce directly, even
     * in default mode), instead of a bare "Failed" milestone bubble or raw JSON. Seeded through the
     * same pure {@link ShaftAssistantPanel#toolCardMarkdown} formatting step {@code
     * runToolAndRenderCard} uses in production, then appended via {@link
     * ShaftAssistantPanel#simulateAppendForTest} -- this harness's fake {@link #screenshotProject()}
     * has no live {@code ShaftMcpInvocationService} to dispatch a real MCP round trip through, the
     * same reason {@link #renderAssistantProgressMilestones} drives {@code appendAgentMilestone}
     * directly.
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

    /**
     * Renders a tool-result bubble with its "Show raw output" disclosure expanded (issue #3601 A5):
     * narrative-first tool results now carry the raw evidence behind a real per-message Swing
     * toggle instead of only a bulk "Copy full transcript" appendix. Seeded the same way {@link
     * #renderAssistantFailureRecoveryCard} is, but through the 3-arg {@link
     * ShaftAssistantPanel#simulateAppendForTest} with non-blank raw evidence so the disclosure
     * actually renders, then {@link #clickAccessible} drives the same toggle a user would click.
     */
    private static BufferedImage renderAssistantToolResultRawOutput(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftAssistantChatState chatState = new ShaftAssistantChatState();
            ShaftSettingsState.Settings settings = defaultSettings();
            ShaftAssistantPanel component = new ShaftAssistantPanel(screenshotProject(), settings, chatState,
                    () -> {
                    });
            String rawOutput = """
                    {
                      "schemaVersion": "1.0",
                      "status": "DETERMINISTIC",
                      "bundleId": "bundle-789",
                      "primaryCause": "LOCATOR",
                      "confidence": "HIGH",
                      "summary": "The checkout submit button locator no longer matches the page after a redesign."
                    }
                    """;
            String cardMarkdown = ShaftAssistantPanel.toolCardMarkdown(
                    "doctor_analyze_failed_allure", ShaftMcpToolResult.success(rawOutput), null);
            component.simulateAppendForTest("assistant", cardMarkdown, rawOutput);
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            clickAccessible(component, "Show raw output");
            layout(component, !dark);
            image.set(render(component, WIDTH, HEIGHT));
        });
        return image.get();
    }

    /**
     * Renders the terminal "Cancelled" chat bubble (tracker #4160 Area B audit / issue #4164: the
     * screenshot-evidence suite had zero rendered coverage of either terminal Cancel/Kill state).
     * Mirrors exactly how {@code ShaftAssistantPanel#showAgentCancelled} composes the bubble in
     * production: the already-streamed partial output wrapped in the same fenced-code-block format
     * {@code formatLocalAgentStreamingResponse} produces, followed by the terminal marker -- proving
     * a user-cancelled run stays legible instead of going blank.
     */
    private static BufferedImage renderAssistantCancelled(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        return renderAssistantTerminalState(lookAndFeelClassName, dark, "Cancelled");
    }

    /**
     * Same composition as {@link #renderAssistantCancelled}, for the user-initiated Kill
     * ({@code destroyForcibly()}) path -- distinct label, identical layout.
     */
    private static BufferedImage renderAssistantKilled(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        return renderAssistantTerminalState(lookAndFeelClassName, dark, "Killed");
    }

    /**
     * Issue #4210: same "Cancelled" composition as {@link #renderAssistantCancelled}, plus the
     * subtle pending-answer caption {@code ShaftAssistantPanel#PENDING_ANSWER_INDICATOR} appends
     * while the run's companion terminal-answer future is still unresolved -- visual evidence the
     * caption reads as a legible, non-intrusive aside rather than crowding the terminal marker.
     */
    private static BufferedImage renderAssistantCancelledPendingAnswer(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftAssistantChatState chatState = new ShaftAssistantChatState();
            chatState.append("user", "/codegen recordings/demo-recording.json", "");
            ShaftSettingsState.Settings settings = defaultSettings();
            ShaftAssistantPanel component = new ShaftAssistantPanel(screenshotProject(), settings, chatState,
                    () -> {
                    });
            String partialOutput = "Reading pom.xml...\nAnalyzing dependency tree for shaft-mcp...";
            String markdown = "```text\n" + partialOutput + "\n```\n\n_Cancelled._ (partial output above)"
                    + "\n\n_Recovering final answer..._";
            component.simulateAppendForTest("assistant", markdown, "");
            component.setSize(new Dimension(WIDTH, HEIGHT));
            component.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            SwingUtilities.updateComponentTreeUI(component);
            component.doLayout();
            layout(component, !dark);
            image.set(render(component, WIDTH, HEIGHT));
        });
        return image.get();
    }

    private static BufferedImage renderAssistantTerminalState(String lookAndFeelClassName, boolean dark, String label)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftAssistantChatState chatState = new ShaftAssistantChatState();
            chatState.append("user", "/codegen recordings/demo-recording.json", "");
            ShaftSettingsState.Settings settings = defaultSettings();
            ShaftAssistantPanel component = new ShaftAssistantPanel(screenshotProject(), settings, chatState,
                    () -> {
                    });
            String partialOutput = "Reading pom.xml...\nAnalyzing dependency tree for shaft-mcp...";
            String markdown = "```text\n" + partialOutput + "\n```\n\n_" + label + "._ (partial output above)";
            component.simulateAppendForTest("assistant", markdown, "");
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
     * Renders the Assistant local-model refresh control after no models are reported (issue #3551):
     * the editable selector stays empty and the "Refresh local agent models" button stays visible
     * so the user can retry the live CLI listing. {@code
     * defaultSettings()} already normalizes to provider=LOCAL/runtime=CLI, so the panel starts in
     * the local-CLI configuration this control only appears in; the empty live list is then forced
     * through the same {@code applyLocalModels} seam production uses after a real CLI listing call
     * (via reflection, since it is private), rather than racing the panel's own async CLI probe
     * that already runs once at construction time.
     */
    private static BufferedImage renderAssistantModelUnavailable(String lookAndFeelClassName, boolean dark)
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
            throw new IllegalStateException("Unable to apply the local-model unavailable state", exception);
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
            Project project = screenshotProject();
            ShaftFeaturePanel component = new ShaftFeaturePanel(project, defaultSettings());
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
                    """), null, project);
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
            ShaftFeaturePanel component, String toolName, ShaftMcpToolResult result, Throwable error,
            Project project) {
        try {
            Method method = ShaftFeaturePanel.class.getDeclaredMethod(
                    "showResult", String.class, ShaftMcpToolResult.class, Throwable.class, Project.class);
            method.setAccessible(true);
            method.invoke(component, toolName, result, error, project);
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
                    Initialized SHAFT MCP 10.3.20260801
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
                    "Could not resolve artifact io.github.shafthq:shaft-mcp:jar:10.3.20260801"), null);
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

    /**
     * Issue #4168 evidence, "before" half: a required prerequisite is still missing, so the
     * prerequisites row is legitimately expanded (established the same way every other legitimate
     * call site pairs {@code refreshPrerequisites()} with {@code updateActionState()} -- only the
     * Recheck button used not to, which is what the "after" screenshot below demonstrates the fix
     * for).
     */
    private static BufferedImage renderSetupPrerequisitesRecheckBefore(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
            settings.mcpCommand = "";
            ShaftMcpSetupPanel component = new ShaftMcpSetupPanel(screenshotProject(), settings,
                    () -> {
                    });
            setField(component, "prerequisitesDetector", missingPythonPrerequisiteDetector());
            invokeRefreshPrerequisitesAndUpdateActionState(component);
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
     * Issue #4168 evidence, "after" half: the user installs Python 3 outside the IDE, then clicks
     * the real "Recheck" button. With the fix, that click alone -- not some later unrelated
     * interaction -- collapses the now-satisfied prerequisites row.
     */
    private static BufferedImage renderSetupPrerequisitesRecheckAfter(String lookAndFeelClassName, boolean dark)
            throws InterruptedException, InvocationTargetException {
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
            settings.mcpCommand = "";
            ShaftMcpSetupPanel component = new ShaftMcpSetupPanel(screenshotProject(), settings,
                    () -> {
                    });
            setField(component, "prerequisitesDetector", missingPythonPrerequisiteDetector());
            invokeRefreshPrerequisitesAndUpdateActionState(component);
            setField(component, "prerequisitesDetector",
                    (java.util.function.Function<String, List<SetupPrerequisites.Prerequisite>>) family ->
                            List.of(new SetupPrerequisites.Prerequisite("Python 3", true, true, "")));
            clickAccessible(component, "Recheck prerequisites");
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

    private static java.util.function.Function<String, List<SetupPrerequisites.Prerequisite>>
            missingPythonPrerequisiteDetector() {
        return family -> List.of(new SetupPrerequisites.Prerequisite(
                "Python 3", false, true, "winget install -e --id Python.Python.3.12"));
    }

    private static void invokeRefreshPrerequisitesAndUpdateActionState(ShaftMcpSetupPanel component) {
        try {
            Method refresh = ShaftMcpSetupPanel.class.getDeclaredMethod("refreshPrerequisites");
            refresh.setAccessible(true);
            refresh.invoke(component);
            Method updateActionState = ShaftMcpSetupPanel.class.getDeclaredMethod("updateActionState", boolean.class);
            updateActionState.setAccessible(true);
            updateActionState.invoke(component, false);
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Unable to refresh prerequisites for screenshot setup", exception);
        }
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
                        case "hasApiKeyAsync" -> CompletableFuture.completedFuture(false);
                        case "setApiKeyAsync" -> CompletableFuture.completedFuture(null);
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
                "generate code that opens Wikipedia, searches for software testing, and opens the first result", "");
        chatState.append("assistant", """
                Confirmed target: https://en.wikipedia.org/wiki/Main_Page

                %s
                """.formatted(WIKIPEDIA_SHAFT_CODE_SAMPLE).stripIndent().trim(), "");
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
            // JCheckBox/JRadioButton (javax.swing.JToggleButton) paint their selection glyph
            // through the L&F's own CheckBoxUI/RadioButtonUI, which reads the button's
            // getDefaultIcon() fallback -- a control BasicButtonUI does not provide (it is meant
            // for plain JButtons, whose icon comes from an explicit setIcon() call instead).
            // Forcing BasicButtonUI onto a toggle control here left it with no icon at all under
            // the light theme: no check glyph, and a flat button-background band in its place,
            // identical regardless of selection state (issue #3777). Only real push buttons need
            // the BasicButtonUI swap (IntelliJLaf's own light ButtonUI does not paint a visible
            // background in this headless harness); toggle controls keep their L&F UI intact.
            if (light && !(button instanceof javax.swing.JToggleButton)) {
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

        // The setup journey is a progressive stepper, not a stack of equal-weight cards. Only the
        // current task may carry a filled surface; collapsed summaries stay left-aligned, borderless
        // and keyboard-inspectable through the panel's existing row bindings.
        final List<JPanel> allStepRows = new ArrayList<>();
        walkComponentsForVerification(setupPanel, comp -> {
            if (comp instanceof JPanel panel
                    && panel.getClientProperty("shaft.stepRow.action") instanceof JComponent) {
                allStepRows.add(panel);
            }
        });
        int expandedRows = 0;
        int readyRows = 0;
        for (JPanel stepRow : allStepRows) {
            JComponent action = (JComponent) stepRow.getClientProperty("shaft.stepRow.action");
            boolean readySummary = stepRow.getComponentCount() > 0
                    && stepRow.getComponent(0) instanceof JLabel label
                    && "Ready".equals(label.getText());
            if (readySummary) {
                if (isEffectivelyVisible(action)) {
                    readyRows++;
                    assertTrue(stepRow.isVisible(),
                            "The separate Ready / Start chatting state must only surface with its row");
                }
            } else if (action.isVisible()) {
                expandedRows++;
            } else {
                assertFalse(stepRow.isOpaque(),
                        "Collapsed setup steps must not render as full-width cards: " + stepRow.getName());
                assertTrue(stepRow.getBorder().getBorderInsets(stepRow).top <= 4,
                        "Collapsed setup steps need compact vertical rhythm");
                assertTrue(stepRow.getComponent(0) instanceof JLabel summary && summary.getX() <= 16,
                        "Collapsed setup summaries must remain left-aligned, not centered");
            }
        }
        assertTrue(expandedRows <= 1, "Only the current setup task should expand by default");
        assertTrue(readyRows <= 1, "Ready / Start chatting must remain one separate success state");

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

    /**
     * Regression test for issue #3777: under the light {@code IntelliJLaf}, every {@code JCheckBox}
     * on every screenshot-rendered surface painted with no visible check glyph and a flat grey band
     * where the control should be, while the identically-constructed dark-theme checkbox rendered
     * correctly. Renders a bare, textless checkbox (so its whole bounds are exactly the glyph/icon
     * area) through the same {@link #configureLookAndFeel}/{@link #layout}/{@link #render} pipeline
     * every other screenshot in this class uses, in both selected and unselected states, and asserts
     * the glyph area is not a single flat color and that selection state is visually distinguishable
     * -- the two symptoms called out in the issue.
     *
     * <p>{@code rendersFeatureCatalogScreenshotsWhenOutputDirectoryIsProvided} only runs its body
     * (including every {@code configureLookAndFeel} call) when {@code -Dshaft.intellij.screenshotDir}
     * is set -- which none of this module's CI or local {@code gradlew test} invocations do -- so in
     * practice this test, and {@link #firstRunWelcomeDismissButtonIsReachableAtNarrowDarkWidth}
     * (issue #4163), are the only code in the whole module that installs a real platform L&F
     * (IntelliJLaf/DarculaLaf) during an ordinary test run. Other test classes (e.g.
     * {@code ShaftTestsPanelTest}, {@code ToolApprovalPromptPanelTest}) build real
     * {@code com.intellij.ui.treeStructure.Tree}/button components with no L&F of their own and
     * implicitly depend on the JVM never having had one installed: IntelliJ's {@code DefaultTreeUI}
     * only becomes the active {@code TreeUI} delegate (with assertions that do not tolerate a L&F
     * swap mid-suite) once a platform L&F has been set at least once, and {@code Button.background}
     * et al. (see {@link #applyThemeDefaults}) are {@code UIManager.put} overrides that, by Swing
     * design, persist across L&F switches instead of resetting with them. So {@link
     * #LOOK_AND_FEEL_ISOLATION} captures and restores both the exact pre-test {@link LookAndFeel}
     * instance/{@link JBColor} dark flag *and* every {@link #applyThemeDefaults} key's prior value,
     * around this and every other test in this class, to leave the JVM exactly as it found it.
     */
    @Test
    void lightThemeCheckboxRendersVisibleGlyphAndReflectsSelectionState()
            throws InterruptedException, InvocationTargetException {
        BufferedImage lightUnselected = renderCheckboxGlyph(LIGHT_THEME, false, false);
        BufferedImage lightSelected = renderCheckboxGlyph(LIGHT_THEME, false, true);
        BufferedImage darkUnselected = renderCheckboxGlyph(DARK_THEME, true, false);
        BufferedImage darkSelected = renderCheckboxGlyph(DARK_THEME, true, true);

        assertAll(
                () -> assertFalse(isUniformImage(lightUnselected),
                        "Light-theme unselected checkbox should paint a glyph, not a flat uniform block"),
                () -> assertFalse(isUniformImage(lightSelected),
                        "Light-theme selected checkbox should paint a glyph, not a flat uniform block"),
                () -> assertTrue(imagesDiffer(lightUnselected, lightSelected),
                        "Light-theme selected and unselected checkboxes should render differently"),
                () -> assertFalse(isUniformImage(darkUnselected),
                        "Dark-theme unselected checkbox should paint a glyph, not a flat uniform block"),
                () -> assertFalse(isUniformImage(darkSelected),
                        "Dark-theme selected checkbox should paint a glyph, not a flat uniform block"),
                () -> assertTrue(imagesDiffer(darkUnselected, darkSelected),
                        "Dark-theme selected and unselected checkboxes should render differently"));
    }

    private static BufferedImage renderCheckboxGlyph(String lookAndFeelClassName, boolean dark, boolean selected)
            throws InterruptedException, InvocationTargetException {
        int size = 24;
        AtomicReference<BufferedImage> image = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            configureLookAndFeel(lookAndFeelClassName, dark);
            JCheckBox checkBox = new JCheckBox();
            checkBox.setSelected(selected);
            checkBox.setSize(new Dimension(size, size));
            checkBox.setPreferredSize(new Dimension(size, size));
            SwingUtilities.updateComponentTreeUI(checkBox);
            checkBox.doLayout();
            layout(checkBox, !dark);
            image.set(render(checkBox, size, size));
        });
        return image.get();
    }

    private static boolean isUniformImage(BufferedImage image) {
        int first = image.getRGB(0, 0);
        for (int y = 0; y < image.getHeight(); y++) {
            for (int x = 0; x < image.getWidth(); x++) {
                if (image.getRGB(x, y) != first) {
                    return false;
                }
            }
        }
        return true;
    }

    private static boolean imagesDiffer(BufferedImage a, BufferedImage b) {
        for (int y = 0; y < a.getHeight(); y++) {
            for (int x = 0; x < a.getWidth(); x++) {
                if (a.getRGB(x, y) != b.getRGB(x, y)) {
                    return true;
                }
            }
        }
        return false;
    }
}
