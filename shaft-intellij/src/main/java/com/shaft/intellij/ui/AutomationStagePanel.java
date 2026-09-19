package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBTabbedPane;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.Font;

/**
 * Automation stage canvas (issue #5957): live record is the default product surface, not an
 * expert tab. The primary tab hosts {@link GuidedWorkflowPanel} (start/pause/stop/clear) so
 * recording works with {@code advancedUiEnabled} false. Expert raw MCP and secondary surfaces
 * stay available as extra tabs or under More.
 */
final class AutomationStagePanel extends JPanel {
    static final String ACCESSIBLE_NAME = "SHAFT automation";
    static final String LIVE_RECORD_TAB = "Live record";
    static final String LOCATOR_PICKER_TAB = "Locator picker";

    private final GuidedWorkflowPanel guided;
    private final LocatorPlaygroundPanel locatorPlayground;
    private final JBTabbedPane surfaces;
    private final JBLabel packStrip;
    private boolean readyPackApplied;

    AutomationStagePanel(
            @Nullable Project project,
            @NotNull GuidedWorkflowPanel.ToolPrefill prefill,
            @NotNull ShaftSettingsState.Settings settings) {
        super(new BorderLayout(0, JBUI.scale(8)));
        setBorder(JBUI.Borders.empty(8));
        getAccessibleContext().setAccessibleName(ACCESSIBLE_NAME);
        getAccessibleContext().setAccessibleDescription(
                "Live record start, pause, stop, and clear for web, Playwright, mobile, and API");

        JBLabel title = new JBLabel("Automation");
        title.setFont(title.getFont().deriveFont(Font.BOLD));
        JBLabel hint = new JBLabel(
                "<html>Live record is the default. Start a session, interact, then stop and "
                        + "generate SHAFT fluent Java. Expert raw MCP stays under More.</html>");
        hint.setAllowAutoWrapping(true);
        packStrip = new JBLabel("Ready pack: —");
        packStrip.getAccessibleContext().setAccessibleName("Automation ready pack summary");

        JPanel header = new JPanel(new BorderLayout(0, JBUI.scale(4)));
        header.setOpaque(false);
        header.add(title, BorderLayout.NORTH);
        header.add(hint, BorderLayout.CENTER);
        header.add(packStrip, BorderLayout.SOUTH);

        guided = new GuidedWorkflowPanel(project, prefill, settings);
        locatorPlayground = new LocatorPlaygroundPanel(project);
        surfaces = new JBTabbedPane();
        surfaces.getAccessibleContext().setAccessibleName("SHAFT automation surfaces");
        surfaces.addTab(LIVE_RECORD_TAB, ShaftIcons.SEND, guided);
        surfaces.addTab(LOCATOR_PICKER_TAB, ShaftIcons.SEARCH, locatorPlayground);

        add(header, BorderLayout.NORTH);
        add(surfaces, BorderLayout.CENTER);
    }

    GuidedWorkflowPanel guidedWorkflowPanel() {
        return guided;
    }

    LocatorPlaygroundPanel locatorPlaygroundPanel() {
        return locatorPlayground;
    }

    void showLocatorPicker() {
        for (int index = 0; index < surfaces.getTabCount(); index++) {
            if (LOCATOR_PICKER_TAB.equals(surfaces.getTitleAt(index))) {
                surfaces.setSelectedIndex(index);
                return;
            }
        }
    }

    JBTabbedPane surfaces() {
        return surfaces;
    }

    void showLiveRecord() {
        surfaces.setSelectedIndex(0);
    }

    /**
     * Applies Design handoff Ready-pack URL/intent into the live recorder (issue #5957 FR-003).
     *
     * @param url optional target URL from {@code automationPrefill.url}
     * @param intent optional session goal / intent from {@code automationPrefill.intent}
     */
    String readyPackUrl() {
        return readyPackApplied ? guided.targetUrlField().getText().trim() : "";
    }

    String readyPackIntent() {
        return readyPackApplied ? guided.intentField().getText().trim() : "";
    }

    void applyReadyPackPrefill(@Nullable String url, @Nullable String intent) {
        guided.applyReadyPackPrefill(url, intent);
        readyPackApplied = (url != null && !url.isBlank()) || (intent != null && !intent.isBlank());
        showLiveRecord();
        String urlText = url == null || url.isBlank() ? "—" : url.trim();
        String intentText = intent == null || intent.isBlank() ? "—" : intent.trim();
        packStrip.setText("Ready pack: " + urlText + "  ·  " + intentText);
    }

    /**
     * Parses a {@code design_handoff} JSON payload and prefills URL/intent when present.
     *
     * @param json handoff tool output
     */
    void applyHandoffPrefillJson(@Nullable String json) {
        if (json == null || json.isBlank()) {
            return;
        }
        try {
            JsonObject root = JsonParser.parseString(json).getAsJsonObject();
            JsonObject prefill = root.has("automationPrefill") && root.get("automationPrefill").isJsonObject()
                    ? root.getAsJsonObject("automationPrefill")
                    : root;
            String url = text(prefill, "url");
            if (url.isBlank()) {
                url = text(root, "optionalUrl");
            }
            String intent = text(prefill, "intent");
            if (!url.isBlank() || !intent.isBlank()) {
                applyReadyPackPrefill(url, intent);
            }
        } catch (RuntimeException ignored) {
            // keep prior strip
        }
    }

    private static String text(JsonObject object, String key) {
        if (object == null || !object.has(key) || object.get(key).isJsonNull()) {
            return "";
        }
        try {
            return object.get(key).getAsString().trim();
        } catch (RuntimeException ignored) {
            return "";
        }
    }
}
