package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.jetbrains.annotations.Nullable;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;

/**
 * Reporting canvas: persist and suggest confirmed Doctor defect labels (issue #5971 / S3-05).
 *
 * <p>Clients MCP {@code doctor_cause_label} and CLI {@code shaft doctor cause-label}. Local
 * gitignored {@code .shaft/doctor-confirmed-labels.json}; no cloud ML. Evidence paths are never
 * stored.
 */
final class ReportingLabelsPanel extends JPanel {
    static final String ACCESSIBLE_NAME = "SHAFT reporting confirmed defect labels";
    static final String TAB_TITLE = "Labels";

    private static final String[] CAUSES = {
            "PRODUCT", "TEST", "ENVIRONMENT", "LOCATOR", "TIMING"
    };

    private final @Nullable Project project;
    private final JBTextField signatureField;
    private final JComboBox<String> causeBox;
    private final JBLabel status;
    private final JButton suggest;
    private final JButton confirm;
    private boolean busy;

    ReportingLabelsPanel() {
        this(null);
    }

    ReportingLabelsPanel(@Nullable Project project) {
        super(new BorderLayout(0, JBUI.scale(8)));
        this.project = project;
        setBorder(JBUI.Borders.empty(8));
        getAccessibleContext().setAccessibleName(ACCESSIBLE_NAME);
        getAccessibleContext().setAccessibleDescription(
                "Confirm or suggest Doctor defect labels by signature");

        JBLabel title = new JBLabel("Confirmed defect labels");
        title.setFont(title.getFont().deriveFont(Font.BOLD));
        JBLabel hint = new JBLabel(
                "<html>When you confirm <b>PRODUCT / TEST / ENVIRONMENT / LOCATOR / TIMING</b> for a "
                        + "Doctor signature, SHAFT persists it in the local gitignored store "
                        + "<code>.shaft/doctor-confirmed-labels.json</code>. The next matching failure "
                        + "suggests that label. Override replaces the stored type. No cloud ML; "
                        + "evidence paths are never stored.</html>");
        hint.setAllowAutoWrapping(true);

        signatureField = new JBTextField("");
        signatureField.getAccessibleContext().setAccessibleName("Doctor signature");
        causeBox = new JComboBox<>(CAUSES);
        causeBox.getAccessibleContext().setAccessibleName("Cause category");

        JPanel fields = new JPanel(new GridLayout(0, 1, 0, JBUI.scale(4)));
        fields.setOpaque(false);
        fields.add(labeled("Signature", signatureField));
        fields.add(labeled("Cause", causeBox));

        suggest = new JButton("Suggest");
        suggest.getAccessibleContext().setAccessibleName("Suggest confirmed defect label");
        suggest.addActionListener(event -> invoke("suggest"));
        confirm = new JButton("Confirm");
        confirm.getAccessibleContext().setAccessibleName("Confirm defect label");
        confirm.addActionListener(event -> invoke("confirm"));
        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, JBUI.scale(6), 0));
        actions.setOpaque(false);
        actions.add(suggest);
        actions.add(confirm);

        status = new JBLabel("No label lookup yet.");
        status.getAccessibleContext().setAccessibleName("Labels status");

        JPanel north = new JPanel(new BorderLayout(0, JBUI.scale(4)));
        north.setOpaque(false);
        north.add(title, BorderLayout.NORTH);
        north.add(hint, BorderLayout.CENTER);
        JPanel controls = new JPanel(new BorderLayout(0, JBUI.scale(4)));
        controls.setOpaque(false);
        controls.add(fields, BorderLayout.CENTER);
        controls.add(actions, BorderLayout.SOUTH);
        north.add(controls, BorderLayout.SOUTH);

        add(north, BorderLayout.NORTH);
        add(status, BorderLayout.SOUTH);
        updateEnabled();
    }

    JButton suggestButton() {
        return suggest;
    }

    JButton confirmButton() {
        return confirm;
    }

    JBLabel statusLabel() {
        return status;
    }

    JBTextField signatureField() {
        return signatureField;
    }

    void applyResultJson(String raw) {
        if (raw == null || raw.isBlank()) {
            status.setText("No label result.");
            return;
        }
        try {
            JsonElement parsed = JsonParser.parseString(raw);
            if (!parsed.isJsonObject()) {
                status.setText(truncate(raw));
                return;
            }
            JsonObject root = parsed.getAsJsonObject();
            String action = text(root, "action");
            String message = text(root, "message");
            String alias = text(root, "displayAlias");
            String cause = text(root, "causeCategory");
            boolean matched = root.has("matched") && root.get("matched").getAsBoolean();
            boolean replaced = root.has("replaced") && root.get("replaced").getAsBoolean();
            StringBuilder builder = new StringBuilder();
            if (!action.isBlank()) {
                builder.append(action).append(": ");
            }
            builder.append(message.isBlank() ? truncate(raw) : message);
            if (matched && !alias.isBlank()) {
                builder.append(" → ").append(alias);
            } else if (matched && !cause.isBlank()) {
                builder.append(" → ").append(cause);
            }
            if (replaced) {
                builder.append(" (replaced prior label)");
            }
            status.setText(builder.toString());
        } catch (RuntimeException exception) {
            status.setText(truncate(raw));
        }
    }

    private void invoke(String action) {
        if (project == null) {
            applyResultJson("{\"action\":\"" + action
                    + "\",\"status\":\"error\",\"message\":\"MCP is not available in this context.\"}");
            return;
        }
        busy = true;
        updateEnabled();
        JsonObject arguments = new JsonObject();
        arguments.addProperty("action", action);
        arguments.addProperty("signature", signatureField.getText().trim());
        if ("confirm".equals(action)) {
            Object selected = causeBox.getSelectedItem();
            arguments.addProperty("causeCategory", selected == null ? "" : selected.toString());
        }
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool("doctor_cause_label", arguments)
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    busy = false;
                    updateEnabled();
                    applyToolResult(result, error);
                }));
    }

    private void applyToolResult(@Nullable ShaftMcpToolResult result, @Nullable Throwable error) {
        if (error != null) {
            applyResultJson(error.getMessage() == null ? error.toString() : error.getMessage());
            return;
        }
        if (result == null || !result.success()) {
            String output = result == null || result.output() == null ? "" : result.output();
            applyResultJson(output.isBlank()
                    ? "{\"status\":\"error\",\"message\":\"doctor_cause_label failed.\"}"
                    : output);
            return;
        }
        applyResultJson(result.output() == null ? "" : result.output());
    }

    private void updateEnabled() {
        boolean enabled = project != null && !busy;
        suggest.setEnabled(enabled);
        confirm.setEnabled(enabled);
    }

    private static JPanel labeled(String label, java.awt.Component field) {
        JPanel row = new JPanel(new BorderLayout(JBUI.scale(6), 0));
        row.setOpaque(false);
        JBLabel caption = new JBLabel(label);
        caption.setPreferredSize(new Dimension(JBUI.scale(72), caption.getPreferredSize().height));
        row.add(caption, BorderLayout.WEST);
        row.add(field, BorderLayout.CENTER);
        return row;
    }

    private static String text(JsonObject object, String field) {
        return object.has(field) && !object.get(field).isJsonNull()
                ? object.get(field).getAsString()
                : "";
    }

    private static String truncate(String raw) {
        return raw.length() > 240 ? raw.substring(0, 240) + "…" : raw;
    }
}
