package com.shaft.intellij.ui;

import com.shaft.intellij.settings.ShaftSettingsState;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.FlowLayout;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.Consumer;

/**
 * Shared "am I ready?" summary (issue #3500 O4/A4): one component answering MCP state, workspace
 * state, last verified agent-lane state, and live recording activity with the same words wherever
 * it is embedded (tool-window header, setup ready row). Hosts wire MCP rechecks themselves; this
 * component only renders honest, already-verified state — it never probes on its own.
 */
public final class ShaftReadinessSummary extends JPanel {
    /** MCP chip states mirrored from the host's live checks. */
    public enum McpState { UNKNOWN, CHECKING, VERIFIED, FAILED }

    private final JLabel mcpChip = new JLabel();
    private final JLabel workspaceChip = new JLabel();
    private final JLabel agentChip = new JLabel();
    private final JLabel recordingChip = new JLabel();
    private final Consumer<Boolean> recordingListener;

    /**
     * Builds the summary strip.
     *
     * @param settings persisted plugin settings (agent-lane readiness)
     * @param workspaceRoot project base path, or {@code null} when unavailable
     */
    public ShaftReadinessSummary(ShaftSettingsState.Settings settings, Path workspaceRoot) {
        super(new FlowLayout(FlowLayout.LEFT, 8, 0));
        setOpaque(false);
        mcpChip.getAccessibleContext().setAccessibleName("SHAFT MCP health");
        workspaceChip.getAccessibleContext().setAccessibleName("SHAFT workspace state");
        agentChip.getAccessibleContext().setAccessibleName("SHAFT agent lane state");
        recordingChip.getAccessibleContext().setAccessibleName("SHAFT recording activity");
        add(mcpChip);
        add(workspaceChip);
        add(agentChip);
        add(recordingChip);
        applyMcpState(settings != null && settings.mcpReady() ? McpState.VERIFIED : McpState.UNKNOWN, "");
        applyWorkspace(workspaceRoot);
        applyAgentLane(settings != null && settings.agentLaneReady);
        applyRecording(ShaftRecordingActivity.active());
        recordingListener = active -> com.intellij.openapi.application.ApplicationManager.getApplication()
                .invokeLater(() -> applyRecording(active));
        ShaftRecordingActivity.listen(recordingListener);
    }

    /**
     * Detaches the recording-activity listener; call when the host is disposed.
     */
    public void dispose() {
        ShaftRecordingActivity.unlisten(recordingListener);
    }

    /**
     * Mirrors the host's MCP check state.
     *
     * @param state live check state
     * @param detail optional failure detail for the tooltip
     */
    public void applyMcpState(McpState state, String detail) {
        mcpChip.setText(switch (state) {
            case VERIFIED -> "MCP: verified";
            case FAILED -> "MCP: failed";
            case CHECKING -> "MCP: checking...";
            default -> "MCP: not checked";
        });
        mcpChip.setForeground(switch (state) {
            case VERIFIED -> ShaftStatusPresentation.success();
            case FAILED -> ShaftStatusPresentation.error();
            default -> ShaftStatusPresentation.pending();
        });
        mcpChip.setToolTipText(switch (state) {
            case VERIFIED -> "SHAFT MCP passed its last connection check. Click Recheck to verify again.";
            case FAILED -> "SHAFT MCP check failed" + (detail == null || detail.isBlank() ? "" : ": " + detail)
                    + ". Recheck, or reopen setup from the Assistant's Configure action.";
            case CHECKING -> "A live SHAFT MCP connection check is running.";
            default -> "SHAFT MCP has not been live-checked in this window yet. Click Recheck to verify.";
        });
    }

    /**
     * Reflects whether the project workspace root is usable.
     *
     * @param workspaceRoot project base path, or {@code null}
     */
    public void applyWorkspace(Path workspaceRoot) {
        boolean ok = workspaceRoot != null && Files.isDirectory(workspaceRoot);
        workspaceChip.setText(ok ? "Workspace: OK" : "Workspace: unavailable");
        workspaceChip.setForeground(ok ? ShaftStatusPresentation.success() : ShaftStatusPresentation.error());
        workspaceChip.setToolTipText(ok
                ? "Recordings, generated tests, and evidence are written under this project root."
                : "No usable project root was found; recordings and codegen need one.");
    }

    /**
     * Reflects the last verified agent-lane state (never invents readiness).
     *
     * @param agentReady last real agent readiness check outcome
     */
    public void applyAgentLane(boolean agentReady) {
        agentChip.setText(agentReady ? "Agent: ready" : "Agent: optional");
        agentChip.setForeground(agentReady
                ? ShaftStatusPresentation.success()
                : ShaftStatusPresentation.pending());
        agentChip.setToolTipText(agentReady
                ? "The optional agent lane passed its last setup check; chat can execute flows."
                : "No agent verified yet — recorder, codegen, doctor, and healer work without one. "
                    + "Connect an agent from setup to add chat execution.");
    }

    private void applyRecording(boolean active) {
        recordingChip.setVisible(active);
        recordingChip.setText(active ? "Recording active" : "");
        recordingChip.setForeground(ShaftStatusPresentation.error());
        recordingChip.setToolTipText("A SHAFT recording session is running; stop it from the browser "
                + "overlay or the Guided panel - both save the session.");
        revalidate();
        repaint();
    }

    /**
     * Access for hosts that place the MCP chip's recheck affordance next to this strip.
     *
     * @return the MCP chip component
     */
    public JComponent mcpChipComponent() {
        return mcpChip;
    }
}
