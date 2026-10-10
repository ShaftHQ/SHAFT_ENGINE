package com.shaft.intellij.ui.firstrun;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationInfo;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.MessageDialogBuilder;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.AssistantAgentRoute;
import com.shaft.intellij.settings.ShaftPluginResetService;
import com.shaft.intellij.settings.ShaftSettingsState;
import com.shaft.intellij.ui.ShaftIcons;
import com.shaft.intellij.ui.ShaftMcpSetupPanel;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JSlider;
import javax.swing.JTextArea;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.LayoutManager;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;

/**
 * Five-step first-run wizard. The plugin shows the installer command and does not run it.
 */
public final class FirstRunWizardPanel extends JPanel implements Disposable {
    private final Project project;
    private final ShaftSettingsState.Settings settings;
    private final Runnable onComplete;
    private final InstallProbe probe;
    private final PrerequisitePlan.Snapshot prerequisites;
    private final JLabel stepper = WizardUi.label("", WizardMessages.get("wizard.stepper"));
    private final JLabel heading = WizardUi.label("", WizardMessages.get("wizard.heading"));
    private final JLabel skipNotice = WizardUi.label(WizardMessages.get("wizard.skip.notice"), WizardMessages.get("wizard.skip.name"));
    private final JButton back;
    private final JButton skip;
    private final JButton primary;
    private final JPanel[] cards = new JPanel[5];
    private final JPanel deck = new JPanel(new StepDeckLayout());
    private final JPanel alternatives = new JPanel(new BorderLayout(0, 6));
    private final JComboBox<AssistantAgentRoute> agentCombo = new JComboBox<>(AssistantAgentRoute.values());
    private final JPasswordField apiKey = WizardUi.password(WizardMessages.get("wizard.apiKey"));
    private final JLabel recommended = WizardUi.label("", WizardMessages.get("wizard.agent.recommended"));
    private final JComboBox<String> targetCombo = WizardUi.combo(WizardMessages.get("wizard.target"));
    private final JTextArea commandArea = WizardUi.wrappingText(WizardMessages.get("wizard.command"));
    private final JLabel checkState = WizardUi.label(WizardMessages.get("wizard.state.waiting"), WizardMessages.get("wizard.check.state"));
    private final JLabel cause = WizardUi.label(WizardMessages.get("wizard.failure.cause"), WizardMessages.get("wizard.failure.cause.name"));
    private final JButton report;
    private final JLabel explain = WizardUi.label(WizardMessages.get("wizard.report.explain"), WizardMessages.get("wizard.report.explain"));
    private final JButton allow;
    private final JButton notNow;
    private final JPanel advancedBody = new JPanel(new BorderLayout(0, 6));
    private final JButton advancedToggle;
    private final JSlider ease = WizardUi.slider(WizardMessages.get("wizard.ux.ease"));
    private final JSlider usefulness = WizardUi.slider(WizardMessages.get("wizard.ux.useful"));
    private final JButton continueAnyway;
    private AssistantAgentRoute selected = AssistantAgentRoute.CODEX_CLI;
    private Consumer<String> copySink = text -> { };
    private TerminalOpener terminalOpener = (tab, command) -> false;
    private SecretStore secretStore = (name, value) -> { };
    private BooleanSupplier confirmReset = this::platformConfirm;
    private int step = 1;
    private boolean verified;
    private boolean userChecked;
    private boolean needsAttention;
    private boolean acknowledged;
    private boolean reportDismissed;

    public FirstRunWizardPanel(Project project, ShaftSettingsState.Settings settings, Runnable onComplete,
                               InstallProbe probe, PrerequisiteProbe prerequisites) {
        super(new BorderLayout(0, 8));
        this.project = project;
        this.settings = settings;
        this.onComplete = onComplete == null ? () -> { } : onComplete;
        this.probe = probe == null ? (client, runtime) -> ShaftMcpToolResult.failure("") : probe;
        PrerequisitePlan.Snapshot snapshot = prerequisites == null ? null : prerequisites.read();
        this.prerequisites = snapshot == null ? new PrerequisitePlan.Snapshot(false, "", false) : snapshot;
        WizardUi.name(this, WizardMessages.get("wizard.region"));
        heading.setFocusable(true);
        WizardUi.describe(stepper, progressWords());
        back = WizardUi.button(WizardMessages.get("wizard.back"), WizardMessages.get("wizard.back"), this::onBack);
        skip = WizardUi.button(WizardMessages.get("wizard.skip"), WizardMessages.get("wizard.skip"), this::onSkip);
        primary = WizardUi.button(WizardMessages.get("wizard.continue"), WizardMessages.get("wizard.primary"), this::onPrimary);
        primary.setDefaultCapable(true);
        continueAnyway = WizardUi.button(WizardMessages.get("wizard.continueAnyway"),
                WizardMessages.get("wizard.continueAnyway"), this::acknowledgeGaps);
        report = WizardUi.button(WizardMessages.get("wizard.report"), WizardMessages.get("wizard.report"), this::onReport);
        allow = WizardUi.button(WizardMessages.get("wizard.allow"), WizardMessages.get("wizard.allow"), this::onAllow);
        notNow = WizardUi.button(WizardMessages.get("wizard.notNow"), WizardMessages.get("wizard.notNow"), this::onNotNow);
        advancedToggle = WizardUi.button(WizardMessages.get("wizard.advanced"), WizardMessages.get("wizard.advanced"), this::toggleAdvanced);
        JButton docs = WizardUi.button(WizardMessages.get("wizard.docs"), WizardMessages.get("wizard.docs"), this::openDocs);
        WizardUi.describe(docs, WizardMessages.get("wizard.docs.url"));
        buildCards();
        buildChrome(docs);
        AssistantAgentRoute saved = AssistantAgentRoute.fromSettings(settings);
        selectAgent(saved == null ? AssistantAgentRoute.CODEX_CLI : saved);
        showStep(initialStep());
    }

    @FunctionalInterface
    public interface InstallProbe {
        ShaftMcpToolResult check(String client, String runtime);
    }

    @FunctionalInterface
    public interface PrerequisiteProbe {
        PrerequisitePlan.Snapshot read();
    }

    @FunctionalInterface
    public interface TerminalOpener {
        boolean open(String tab, String command);
    }

    @FunctionalInterface
    public interface SecretStore {
        void save(String name, char[] value);
    }

    public JButton primary() {
        return primary;
    }

    public int step() {
        return step;
    }

    public String stepTitle() {
        return WizardMessages.get("wizard.step." + step);
    }

    public String installerTarget() {
        Object item = targetCombo.getSelectedItem();
        if (item != null) {
            return item.toString();
        }
        var names = ShaftMcpSetupPanel.installerTargetNames();
        return names.get(names.size() - 1);
    }

    public boolean verified() {
        return verified;
    }

    public boolean userChecked() {
        return userChecked;
    }

    public AssistantAgentRoute selectedAgent() {
        return selected;
    }

    public void selectAgent(AssistantAgentRoute route) {
        selected = route == null ? AssistantAgentRoute.CODEX_CLI : route;
        if (agentCombo.getSelectedItem() != selected) {
            agentCombo.setSelectedItem(selected);
        }
        apiKey.setVisible(selected.gemini());
        recommended.setText(whyText());
    }

    public boolean alternativesVisible() {
        return alternatives.isVisible();
    }

    public JComponent preferredFocusComponent() {
        return heading;
    }

    public void setCopySink(Consumer<String> copySink) {
        this.copySink = copySink == null ? text -> { } : copySink;
    }

    public void setTerminalOpener(TerminalOpener terminalOpener) {
        this.terminalOpener = terminalOpener == null ? (tab, command) -> false : terminalOpener;
    }

    public void setSecretStore(SecretStore secretStore) {
        this.secretStore = secretStore == null ? (name, value) -> { } : secretStore;
    }

    void setConfirmReset(BooleanSupplier confirmReset) {
        this.confirmReset = confirmReset == null ? () -> false : confirmReset;
    }

    private boolean disposed;

    @Override
    public void dispose() {
        disposed = true;
    }

    private void buildChrome(JButton docs) {
        JPanel header = new JPanel(new BorderLayout(0, 4));
        header.add(stepper, BorderLayout.NORTH);
        header.add(heading, BorderLayout.CENTER);
        JPanel secondary = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
        secondary.add(back);
        secondary.add(skip);
        secondary.add(docs);
        JPanel notices = new JPanel(new BorderLayout(0, 4));
        skipNotice.setVisible(false);
        notices.add(skipNotice, BorderLayout.NORTH);
        notices.add(advancedToggle, BorderLayout.WEST);
        notices.add(advancedBody, BorderLayout.CENTER);
        notices.add(secondary, BorderLayout.SOUTH);
        JPanel south = new JPanel(new BorderLayout(0, 6));
        south.add(notices, BorderLayout.NORTH);
        south.add(primary, BorderLayout.SOUTH);
        add(header, BorderLayout.NORTH);
        add(deck, BorderLayout.CENTER);
        add(south, BorderLayout.SOUTH);
    }

    private void buildCards() {
        for (int index = 0; index < cards.length; index++) {
            cards[index] = new JPanel(new BorderLayout(0, 8));
            WizardUi.name(cards[index], WizardMessages.format("wizard.step.card", index + 1));
            deck.add(cards[index]);
        }
        cards[0].add(WizardUi.label(ProjectFacts.describe(project, settings), WizardMessages.get("wizard.facts.name")),
                BorderLayout.NORTH);
        cards[1].add(prerequisitesPanel(), BorderLayout.NORTH);
        cards[2].add(agentPanel(), BorderLayout.NORTH);
        cards[3].add(installPanel(), BorderLayout.NORTH);
        cards[4].add(successPanel(), BorderLayout.NORTH);
        fillTargets();
        commandArea.setText(builtCommand());
    }

    private JPanel prerequisitesPanel() {
        JPanel panel = new JPanel(new BorderLayout(0, 6));
        JLabel ready = WizardUi.iconLabel(ShaftIcons.CHECK, prerequisites.readyLine(), WizardMessages.get("wizard.ready.name"));
        ready.setVisible(!prerequisites.readyNames().isEmpty());
        JPanel missing = new JPanel(new BorderLayout(0, 4));
        JPanel rows = new JPanel(new BorderLayout());
        Component previous = null;
        for (String name : prerequisites.missingNames()) {
            JPanel row = missingRow(name);
            if (previous == null) {
                rows.add(row, BorderLayout.NORTH);
            } else {
                JPanel next = new JPanel(new BorderLayout(0, 4));
                next.add(previous, BorderLayout.NORTH);
                next.add(row, BorderLayout.CENTER);
                previous = next;
                continue;
            }
            previous = row;
        }
        if (previous != null) {
            missing.add(previous, BorderLayout.NORTH);
        }
        missing.add(continueAnyway, BorderLayout.SOUTH);
        continueAnyway.setVisible(!prerequisites.allPresent());
        panel.add(ready, BorderLayout.NORTH);
        panel.add(missing, BorderLayout.CENTER);
        return panel;
    }

    private JPanel missingRow(String toolName) {
        JPanel row = new JPanel(new BorderLayout(8, 0));
        JLabel label = WizardUi.iconLabel(ShaftIcons.CANCEL,
                WizardMessages.format("wizard.missing", toolName),
                WizardMessages.format("wizard.missing.name", toolName));
        JButton copy = WizardUi.button(WizardMessages.get("wizard.copy"),
                WizardMessages.format("wizard.prereq.copy", toolName),
                () -> copyText(PrerequisitePlan.installCommand(toolName), false));
        row.add(label, BorderLayout.CENTER);
        row.add(copy, BorderLayout.EAST);
        return row;
    }

    private JPanel agentPanel() {
        JPanel panel = new JPanel(new BorderLayout(0, 6));
        JButton different = WizardUi.button(WizardMessages.get("wizard.differentAgent"),
                WizardMessages.get("wizard.differentAgent"), () -> alternatives.setVisible(true));
        WizardUi.name(alternatives, WizardMessages.get("wizard.alternatives"));
        alternatives.setVisible(false);
        agentCombo.addActionListener(event -> {
            if (agentCombo.getSelectedItem() instanceof AssistantAgentRoute route) {
                selectAgent(route);
            }
        });
        alternatives.add(agentCombo, BorderLayout.NORTH);
        alternatives.add(apiKey, BorderLayout.SOUTH);
        apiKey.setVisible(false);
        panel.add(recommended, BorderLayout.NORTH);
        panel.add(different, BorderLayout.WEST);
        panel.add(alternatives, BorderLayout.SOUTH);
        return panel;
    }

    private JPanel installPanel() {
        JPanel panel = new JPanel(new BorderLayout(0, 6));
        JLabel disclaimer = WizardUi.label(WizardMessages.get("wizard.install.disclaimer"),
                WizardMessages.get("wizard.install.disclaimer"));
        JButton copy = WizardUi.button(WizardMessages.get("wizard.copy"), WizardMessages.get("wizard.copy"), this::copyInstaller);
        JPanel copyRow = new JPanel(new BorderLayout(8, 0));
        copyRow.add(commandArea, BorderLayout.CENTER);
        copyRow.add(copy, BorderLayout.EAST);
        cause.setVisible(false);
        report.setVisible(false);
        explain.setVisible(false);
        allow.setVisible(false);
        notNow.setVisible(false);
        JPanel failure = new JPanel(new BorderLayout(0, 4));
        WizardUi.name(failure, WizardMessages.get("wizard.failure.region"));
        failure.add(cause, BorderLayout.NORTH);
        JPanel reportRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
        reportRow.add(report);
        reportRow.add(allow);
        reportRow.add(notNow);
        failure.add(explain, BorderLayout.CENTER);
        failure.add(reportRow, BorderLayout.SOUTH);
        JPanel body = new JPanel(new BorderLayout(0, 6));
        body.add(disclaimer, BorderLayout.NORTH);
        body.add(copyRow, BorderLayout.CENTER);
        body.add(checkState, BorderLayout.SOUTH);
        panel.add(body, BorderLayout.NORTH);
        panel.add(failure, BorderLayout.SOUTH);
        return panel;
    }

    private JPanel successPanel() {
        JPanel panel = new JPanel(new BorderLayout(0, 6));
        JButton record = WizardUi.button(WizardMessages.get("wizard.record"), WizardMessages.get("wizard.record"), this::finish);
        JPanel feedback = new JPanel(new BorderLayout(0, 4));
        WizardUi.name(feedback, WizardMessages.get("wizard.ux.name"));
        feedback.add(WizardUi.label(WizardMessages.get("wizard.ux.ease"), WizardMessages.get("wizard.ux.ease")), BorderLayout.NORTH);
        JPanel sliders = new JPanel(new BorderLayout(0, 4));
        sliders.add(ease, BorderLayout.NORTH);
        sliders.add(WizardUi.label(WizardMessages.get("wizard.ux.useful"), WizardMessages.get("wizard.ux.useful")), BorderLayout.CENTER);
        sliders.add(usefulness, BorderLayout.SOUTH);
        feedback.add(sliders, BorderLayout.CENTER);
        JCheckBoxRow skipFeedback = new JCheckBoxRow();
        feedback.add(skipFeedback.box, BorderLayout.SOUTH);
        JLabel average = WizardUi.label(averageText(), WizardMessages.get("wizard.ux.average"));
        panel.add(record, BorderLayout.NORTH);
        panel.add(feedback, BorderLayout.CENTER);
        panel.add(average, BorderLayout.SOUTH);
        return panel;
    }

    private void fillTargets() {
        for (String name : ShaftMcpSetupPanel.installerTargetNames()) {
            targetCombo.addItem(name);
        }
        var names = ShaftMcpSetupPanel.installerTargetNames();
        targetCombo.setSelectedItem(names.get(names.size() - 1));
        JButton reset = WizardUi.button(WizardMessages.get("wizard.reset"), WizardMessages.get("wizard.reset"), this::confirmAndReset);
        advancedBody.add(targetCombo, BorderLayout.NORTH);
        advancedBody.add(reset, BorderLayout.SOUTH);
        advancedBody.setVisible(false);
        targetCombo.addActionListener(event -> onTargetChanged());
    }

    private void showStep(int next) {
        step = Math.min(5, Math.max(1, next));
        for (int index = 0; index < cards.length; index++) {
            cards[index].setVisible(index + 1 == step);
        }
        settings.firstRunWizardStep = step;
        refreshChrome();
        heading.requestFocusInWindow();
        deck.revalidate();
        deck.repaint();
    }

    private void refreshChrome() {
        heading.setText(stepTitle());
        stepper.setText(WizardMessages.format("wizard.stepper.line", step, 5, stepTitle()));
        WizardUi.describe(stepper, progressWords());
        back.setVisible(step > 1);
        advancedToggle.setVisible(step == 3 || step == 4);
        if (step != 3 && step != 4) {
            advancedBody.setVisible(false);
        }
        primary.setText(primaryLabel());
        primary.setEnabled(primaryEnabled());
    }

    private String primaryLabel() {
        if (step == 3) {
            return WizardMessages.get("wizard.useAgent");
        }
        if (step == 5) {
            return WizardMessages.get("wizard.openAssistant");
        }
        if (step != 4) {
            return WizardMessages.get("wizard.continue");
        }
        if (verified) {
            return WizardMessages.get("wizard.continue");
        }
        if (needsAttention) {
            return WizardMessages.get("wizard.failure.recovery");
        }
        return WizardMessages.get("wizard.check");
    }

    private boolean primaryEnabled() {
        if (step == 2 && !prerequisites.allPresent() && !acknowledged) {
            return false;
        }
        return true;
    }

    private void onPrimary() {
        if (!primary.isEnabled()) {
            return;
        }
        if (step == 3) {
            commitAgent();
        }
        if (step == 4 && !verified) {
            runCheck();
            return;
        }
        if (step == 5) {
            finish();
            return;
        }
        showStep(step + 1);
    }

    private void onBack() {
        if (step > 1) {
            showStep(step - 1);
        }
    }

    private void onSkip() {
        skipNotice.setVisible(true);
    }

    private void acknowledgeGaps() {
        acknowledged = true;
        refreshChrome();
    }

    private void runCheck() {
        userChecked = true;
        needsAttention = false;
        verified = false;
        checkState.setText(WizardMessages.get("wizard.state.checking"));
        AssistantAgentRoute route = selectedAgent();
        ShaftMcpToolResult result;
        try {
            result = probe.check(route.client(), route.runtime());
        } catch (RuntimeException exception) {
            result = ShaftMcpToolResult.failure("");
        }
        boolean passed = result != null && result.success();
        verified = passed;
        needsAttention = !passed;
        userChecked = true;
        checkState.setText(WizardMessages.get(passed ? "wizard.state.verified" : "wizard.state.attention"));
        cause.setVisible(!passed);
        report.setVisible(!passed);
        if (passed) {
            hideReportPrompt();
        }
        refreshChrome();
    }

    private void commitAgent() {
        if (selected != null) {
            selected.applyTo(settings);
        }
        if (selected != null && selected.gemini()) {
            char[] secret = apiKey.getPassword();
            if (secret != null && secret.length > 0) {
                secretStore.save("GEMINI_API_KEY", secret);
            }
        }
    }

    private void finish() {
        storeFeedback();
        settings.firstRunWizardCompleted = true;
        onComplete.run();
    }

    private void storeFeedback() {
        if (!(cards[4].getComponentCount() > 0)) {
            return;
        }
        if (feedbackSkipped() || ease.getValue() < 1 || usefulness.getValue() < 1) {
            return;
        }
        settings.uxLiteEase = ease.getValue();
        settings.uxLiteUsefulness = usefulness.getValue();
        settings.uxLiteResponses = Math.max(1, settings.uxLiteResponses + 1);
    }

    private boolean feedbackSkipped() {
        return findCheckSelected(cards[4]);
    }

    private void copyInstaller() {
        copyText(commandArea.getText(), true);
    }

    private void copyText(String command, boolean openTerminal) {
        if (command == null || command.isBlank()) {
            return;
        }
        copySink.accept(command);
        if (openTerminal) {
            terminalOpener.open(WizardMessages.get("wizard.command"), command);
        }
    }

    private void onTargetChanged() {
        commandArea.setText(builtCommand());
        verified = false;
        userChecked = false;
        needsAttention = false;
        checkState.setText(WizardMessages.get("wizard.state.waiting"));
        cause.setVisible(false);
        report.setVisible(false);
        hideReportPrompt();
        refreshChrome();
    }

    private String builtCommand() {
        return ShaftMcpSetupPanel.installerCommandFor(ShaftMcpSetupPanel.installerArgumentFor(installerTarget()));
    }

    private void toggleAdvanced() {
        advancedBody.setVisible(!advancedBody.isVisible());
    }

    private void confirmAndReset() {
        if (!confirmReset.getAsBoolean()) {
            return;
        }
        try {
            ShaftPluginResetService.getInstance().resetEverything();
        } catch (Throwable ignored) {
            // Headless tests and a missing application service leave settings untouched.
        }
    }

    private boolean platformConfirm() {
        try {
            if (ApplicationManager.getApplication() == null) {
                return false;
            }
            return MessageDialogBuilder.yesNo(WizardMessages.get("wizard.reset.title"),
                    WizardMessages.get("wizard.reset.confirm")).ask(project);
        } catch (Throwable ignored) {
            return false;
        }
    }

    private void onReport() {
        if (settings.reportSetupFailures) {
            fileReport();
            return;
        }
        if (reportDismissed) {
            return;
        }
        explain.setVisible(true);
        allow.setVisible(true);
        notNow.setVisible(true);
    }

    private void onAllow() {
        settings.reportSetupFailures = true;
        hideReportPrompt();
        fileReport();
    }

    private void onNotNow() {
        reportDismissed = true;
        hideReportPrompt();
    }

    private void hideReportPrompt() {
        explain.setVisible(false);
        allow.setVisible(false);
        notNow.setVisible(false);
    }

    private void fileReport() {
        String stepId = "install";
        IllegalStateException failure = new IllegalStateException(WizardMessages.get("wizard.failure.cause"));
        String frame = "com.shaft.intellij.ui.firstrun.FirstRunWizardPanel.runCheck";
        String raw = WizardMessages.format("wizard.report.body", pluginVersion(), ideBuild(),
                System.getProperty("os.name", ""), stepId, failure.getClass().getSimpleName(), frame,
                WizardMessages.get("wizard.failure.cause"));
        String body = SetupFailureReport.redact(raw);
        SetupFailureReport.fileDefault(settings.reportSetupFailures, SetupFailureReport.fingerprint(failure, stepId),
                WizardMessages.get("wizard.failure.cause"), body);
    }

    private void openDocs() {
        try {
            if (ApplicationManager.getApplication() == null) {
                return;
            }
            com.intellij.ide.BrowserUtil.browse(WizardMessages.get("wizard.docs.url"));
        } catch (Throwable ignored) {
            // Docs stay a link target when no IDE application is running.
        }
    }

    private String whyText() {
        AssistantAgentRoute saved = AssistantAgentRoute.fromSettings(settings);
        if (saved != null && saved == selected) {
            return WizardMessages.get("wizard.agent.why.saved");
        }
        if (selected != null && selected.gemini()) {
            return WizardMessages.get("wizard.agent.why.gemini");
        }
        if (selected == AssistantAgentRoute.CODEX_CLI) {
            return WizardMessages.get("wizard.agent.why.codex");
        }
        String name = selected == null ? "" : selected.displayName();
        return WizardMessages.format("wizard.agent.why.other", name);
    }

    private String averageText() {
        if (settings.uxLiteResponses < 1 || settings.uxLiteEase < 1 || settings.uxLiteUsefulness < 1) {
            return WizardMessages.get("wizard.ux.none");
        }
        int average = (((settings.uxLiteEase - 1) * 25) + ((settings.uxLiteUsefulness - 1) * 25)) / 2;
        return WizardMessages.format("wizard.ux.average", average);
    }

    private String progressWords() {
        return WizardMessages.get("wizard.progress.done") + ". "
                + WizardMessages.get("wizard.progress.current") + ". "
                + WizardMessages.get("wizard.progress.next");
    }

    private int initialStep() {
        int saved = settings.firstRunWizardStep;
        if (saved < 1 || saved > 5) {
            return 1;
        }
        return saved;
    }

    private String pluginVersion() {
        return WizardMessages.get("shaft.plugin.version");
    }

    private static String ideBuild() {
        try {
            return ApplicationInfo.getInstance().getBuild().asString();
        } catch (Throwable ignored) {
            return WizardMessages.get("wizard.report.unknown");
        }
    }

    private static boolean findCheckSelected(Component component) {
        if (component instanceof javax.swing.JCheckBox box && box.isSelected()) {
            return true;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                if (findCheckSelected(child)) {
                    return true;
                }
            }
        }
        return false;
    }

    /** Skip-feedback checkbox kept as a field so finish can read it without scanning. */
    private final class JCheckBoxRow {
        private final javax.swing.JCheckBox box =
                WizardUi.check(WizardMessages.get("wizard.ux.skip"), WizardMessages.get("wizard.ux.skip"));
    }

    private static final class StepDeckLayout implements LayoutManager {
        @Override
        public void addLayoutComponent(String name, Component component) {
        }

        @Override
        public void removeLayoutComponent(Component component) {
        }

        @Override
        public Dimension preferredLayoutSize(Container parent) {
            return new Dimension(320, 240);
        }

        @Override
        public Dimension minimumLayoutSize(Container parent) {
            return new Dimension(0, 0);
        }

        @Override
        public void layoutContainer(Container parent) {
            for (Component child : parent.getComponents()) {
                child.setBounds(0, 0, parent.getWidth(), parent.getHeight());
            }
        }
    }
}
