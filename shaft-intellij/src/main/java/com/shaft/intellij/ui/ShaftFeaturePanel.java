package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.ide.CopyPasteManager;
import com.intellij.openapi.options.ShowSettingsUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.McpInvocationError;
import com.shaft.intellij.mcp.RecoveryActions;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpProgress;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JSplitPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.datatransfer.StringSelection;
import java.util.LinkedHashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Locale;
import java.util.concurrent.CancellationException;
import java.util.stream.Collectors;

/**
 * MCP tools panel with editable JSON arguments.
 */
final class ShaftFeaturePanel extends JPanel {
    private static final String REFRESH_TOOLS_TOOLTIP = "Refresh tools";
    private static final String REFRESHING_TOOLTIP = "Refreshing...";

    // Package-private for ShaftFeaturePanelCatalogTest (read the merged catalog without reflection).
    List<ToolCategory> categories;
    private final boolean catalogRefreshEnabled;
    private final JBTextField search;
    private final JComboBox<ToolCategory> categorySelector;
    private final JComboBox<String> contextSelector;
    private final JComboBox<ToolTemplate> toolSelector;
    private final JLabel templateDescription;
    private final JBTextArea argumentsArea;
    private final JBTextArea outputArea;
    private final AssistantTranscriptView outputCard;
    private final JPanel outputCards;
    private final JButton toggleRawOutputButton;
    private boolean showingRawOutput;
    private final JButton runButton;
    private final JButton cancelButton;
    private final JButton restoreDefaultsButton;
    private final JButton copyOutputButton;
    private final JButton refreshCatalogButton;
    private final JButton recoveryButton;
    private final JProgressBar progress;
    private final JLabel status;
    private final ShaftSettingsState.Settings settings;
    private final Map<String, String> argumentDrafts = new LinkedHashMap<>();
    private ToolTemplate activeTemplate;
    // Package-private for ShaftFeaturePanelCatalogTest (simulate an in-flight user run without reflection).
    ShaftMcpInvocation currentInvocation;
    private boolean updatingTools;

    ShaftFeaturePanel(Project project) {
        this(project, ShaftSettingsState.getInstance().getState());
    }

    ShaftFeaturePanel(Project project, @NotNull ShaftSettingsState.Settings settings) {
        this(project, settings, ToolTemplates.categories(), true);
    }

    ShaftFeaturePanel(Project project, @NotNull ShaftSettingsState.Settings settings, @NotNull List<ToolCategory> categories) {
        this(project, settings, categories, false);
    }

    ShaftFeaturePanel(Project project,
                      @NotNull ShaftSettingsState.Settings settings,
                      @NotNull List<ToolCategory> categories,
                      boolean catalogRefreshEnabled) {
        super(new BorderLayout(6, 6));
        this.categories = List.copyOf(categories);
        this.catalogRefreshEnabled = catalogRefreshEnabled;
        this.settings = settings;
        setBorder(JBUI.Borders.empty(8));
        search = new JBTextField();
        search.setColumns(8);
        search.getEmptyText().setText("Search tools");
        search.getAccessibleContext().setAccessibleName("Search SHAFT tools");
        categorySelector = new JComboBox<>(this.categories.toArray(ToolCategory[]::new));
        categorySelector.setPrototypeDisplayValue(new ToolCategory("Advanced Tools", List.of()));
        categorySelector.getAccessibleContext().setAccessibleName("SHAFT tool category");
        contextSelector = new JComboBox<>();
        contextSelector.getAccessibleContext().setAccessibleName("SHAFT tool context");
        toolSelector = new JComboBox<>();
        toolSelector.setPrototypeDisplayValue(new ToolTemplate("Generate Playwright replay", "", "{}"));
        toolSelector.getAccessibleContext().setAccessibleName("SHAFT tool");
        templateDescription = new JLabel(" ");
        argumentsArea = new JBTextArea(16, 32);
        argumentsArea.getAccessibleContext().setAccessibleName("SHAFT tool arguments");
        argumentsArea.setLineWrap(true);
        argumentsArea.setWrapStyleWord(true);
        outputArea = new JBTextArea(10, 32);
        outputArea.getAccessibleContext().setAccessibleName("SHAFT tool output (raw JSON)");
        outputArea.setEditable(false);
        outputArea.setLineWrap(true);
        outputArea.setWrapStyleWord(true);
        // Humanized card is the default view (issue #3552); outputArea keeps holding the untouched
        // raw text so copyOutputButton and the "View raw JSON" toggle always show the exact bytes.
        outputCard = new AssistantTranscriptView(project);
        outputCard.getAccessibleContext().setAccessibleName("SHAFT tool output");
        outputCards = new JPanel(new java.awt.CardLayout());
        outputCards.add(outputCard, "card");
        outputCards.add(new JBScrollPane(outputArea), "raw");
        toggleRawOutputButton = new JButton();
        ShaftIconButtons.apply(toggleRawOutputButton, "View raw JSON", "Toggle raw SHAFT tool output",
                ShaftIcons.CODE);
        toggleRawOutputButton.setEnabled(false);
        toggleRawOutputButton.addActionListener(event -> toggleRawOutput());
        status = new JLabel("Ready");
        progress = new JProgressBar();
        progress.setIndeterminate(true);
        progress.setVisible(false);

        runButton = new JButton("Run");
        runButton.getAccessibleContext().setAccessibleName("Run SHAFT tool");
        ShaftIconButtons.apply(runButton, ShaftIcons.SEND);
        runButton.addActionListener(event -> run(project));
        cancelButton = new JButton("Cancel");
        cancelButton.getAccessibleContext().setAccessibleName("Cancel SHAFT tool");
        ShaftIconButtons.apply(cancelButton, ShaftIcons.CANCEL);
        cancelButton.setEnabled(false);
        cancelButton.addActionListener(event -> cancelCurrent());
        restoreDefaultsButton = new JButton("Restore defaults");
        restoreDefaultsButton.getAccessibleContext().setAccessibleName("Restore default SHAFT tool arguments");
        ShaftIconButtons.apply(restoreDefaultsButton, ShaftIcons.RESET);
        restoreDefaultsButton.addActionListener(event -> restoreDefaults());
        copyOutputButton = new JButton("Copy output");
        copyOutputButton.getAccessibleContext().setAccessibleName("Copy SHAFT tool output");
        ShaftIconButtons.apply(copyOutputButton, ShaftIcons.COPY);
        copyOutputButton.setEnabled(false);
        copyOutputButton.addActionListener(event -> copyOutput());
        refreshCatalogButton = new JButton("Refresh tools");
        refreshCatalogButton.getAccessibleContext().setAccessibleName("Refresh SHAFT MCP tool catalog");
        ShaftIconButtons.apply(refreshCatalogButton, ShaftIcons.RERUN);
        refreshCatalogButton.setVisible(catalogRefreshEnabled);
        refreshCatalogButton.addActionListener(event -> refreshCatalog(project));
        recoveryButton = new JButton();
        recoveryButton.getAccessibleContext().setAccessibleName("SHAFT tool recovery action");
        // Not run through ShaftIconButtons.apply(): that fixes a button to an icon-only 32x32 slot,
        // but this button's whole point is to show which recovery action applies ("Retry" / "Restart
        // MCP server" / "View logs" from showRecovery()) -- an icon alone can't convey that (#3626).
        recoveryButton.setIcon(ShaftIcons.RERUN);
        recoveryButton.setVisible(false);
        search.getDocument().addDocumentListener(new SimpleDocumentListener(this::refreshTools));
        argumentsArea.getDocument().addDocumentListener(new SimpleDocumentListener(this::validateArguments));
        categorySelector.addActionListener(event -> {
            if (!updatingTools) {
                refreshTools();
            }
        });
        contextSelector.addActionListener(event -> {
            if (!updatingTools) {
                refreshTools();
            }
        });
        toolSelector.addActionListener(event -> {
            if (!updatingTools) {
                loadSelectedTemplate();
            }
        });
        updateContextOptions();
        refreshTools();

        JPanel selectors = new JPanel(new GridBagLayout());
        JLabel searchLabel = label("Search", 'S', search);
        JLabel categoryLabel = label("Category", 'C', categorySelector);
        JLabel toolLabel = label("Tool", 'T', toolSelector);
        JLabel contextLabel = label("Context", 'O', contextSelector);
        addSelectorComponent(selectors, searchLabel, 0, 0.0, false);
        addSelectorComponent(selectors, search, 1, 0.0, false);
        addSelectorComponent(selectors, categoryLabel, 2, 0.0, false);
        addSelectorComponent(selectors, categorySelector, 3, 0.0, false);
        addSelectorComponent(selectors, contextLabel, 4, 0.0, false);
        addSelectorComponent(selectors, contextSelector, 5, 0.0, false);
        addSelectorComponent(selectors, toolLabel, 6, 0.0, false);
        addSelectorComponent(selectors, toolSelector, 7, 1.0, true);

        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        actions.add(runButton);
        actions.add(cancelButton);
        actions.add(restoreDefaultsButton);
        actions.add(copyOutputButton);
        actions.add(refreshCatalogButton);
        actions.add(recoveryButton);
        actions.add(progress);
        actions.add(status);

        JPanel header = new JPanel(new BorderLayout(4, 4));
        header.add(selectors, BorderLayout.NORTH);
        header.add(actions, BorderLayout.CENTER);

        JPanel north = new JPanel(new BorderLayout(4, 4));
        north.add(header, BorderLayout.NORTH);
        north.add(templateDescription, BorderLayout.CENTER);
        north.add(setupNotice(project, settings), BorderLayout.SOUTH);

        JPanel center = new JPanel(new BorderLayout(4, 4));
        JLabel argumentsLabel = label("Arguments", 'A', argumentsArea);
        center.add(argumentsLabel, BorderLayout.NORTH);
        center.add(new JBScrollPane(argumentsArea), BorderLayout.CENTER);

        JPanel output = new JPanel(new BorderLayout(4, 4));
        JLabel outputLabel = label("Output", 'O', outputArea);
        JPanel outputHeader = new JPanel(new BorderLayout());
        outputHeader.add(outputLabel, BorderLayout.WEST);
        outputHeader.add(toggleRawOutputButton, BorderLayout.EAST);
        output.add(outputHeader, BorderLayout.NORTH);
        output.add(outputCards, BorderLayout.CENTER);

        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, center, output);
        splitPane.setResizeWeight(0.66);
        splitPane.setBorder(JBUI.Borders.empty());
        add(north, BorderLayout.NORTH);
        add(splitPane, BorderLayout.CENTER);

        autoPopulateCatalog(project);
    }

    /**
     * Warms up the tool catalog in the background on first panel use, instead of leaving only the
     * curated fallback list until the user clicks "Refresh tools". Silent by design: this reads
     * through {@link ShaftMcpInvocationService#startListTools()}'s cache (so it costs nothing once a
     * catalog was already fetched this session) and never touches button/status state, so a failure
     * here (unconfigured MCP, a test double project with no wired service, a disconnected server)
     * leaves the panel exactly as it was before this call — the curated categories still work and
     * the user can always hit "Refresh tools" for an explicit, force-refreshed attempt.
     */
    private void autoPopulateCatalog(Project project) {
        if (!catalogRefreshEnabled || project == null || !mcpConfigured() || currentInvocation != null) {
            return;
        }
        try {
            ShaftMcpInvocation invocation = ShaftMcpInvocationService.getInstance(project).startListTools();
            invocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                    () -> applyAutoPopulatedCatalog(result, error)));
        } catch (RuntimeException ignored) {
            // Best-effort warm-up only; an unavailable service must never break panel construction.
        }
    }

    // Package-private for ShaftFeaturePanelCatalogTest, which drives the warm-up callback directly.
    void applyAutoPopulatedCatalog(ShaftMcpToolResult result, Throwable error) {
        if (currentInvocation != null || error != null || result == null || !result.success()) {
            // Either a real user-triggered run/refresh is already in flight (do not clobber it), or
            // the background warm-up itself failed/was cancelled: keep the curated fallback catalog.
            return;
        }
        categories = ToolTemplates.categories(result.output());
        reloadCategories();
    }

    JComponent preferredFocusComponent() {
        return argumentsArea;
    }

    JComboBox<ToolCategory> categorySelector() {
        return categorySelector;
    }

    private void run(Project project) {
        ToolTemplate template = selectedTemplate();
        if (template == null) {
            return;
        }
        if (!mcpConfigured()) {
            status.setText("Configure MCP");
            setOutput(template.toolName(), "Configure SHAFT MCP in Settings before running Tools requests.");
            return;
        }
        if (!projectAvailable(project, template.toolName())) {
            return;
        }
        if (template.confirmationRequired()
                && Messages.showOkCancelDialog(
                this,
                "Run " + template.label() + "?\n\nReview the arguments before continuing.",
                "Confirm SHAFT Tool",
                "Run",
                "Cancel",
                null) != Messages.OK) {
            status.setText("Cancelled");
            return;
        }
        JsonObject arguments;
        try {
            arguments = JsonParser.parseString(argumentsArea.getText().isBlank() ? "{}" : argumentsArea.getText())
                    .getAsJsonObject();
        } catch (RuntimeException exception) {
            status.setText("Invalid JSON");
            JsonText.JsonErrorLocation location = JsonText.findErrorLocation(argumentsArea.getText());
            String message = location != null ? "Invalid JSON at " + location : "Invalid JSON";
            setOutput(template.toolName(), message);
            if (location != null) {
                selectJsonErrorLocation(location);
            }
            return;
        }
        setRunning(true, "Running: " + template.toolName() + " …");
        clearOutput();
        currentInvocation = ShaftMcpInvocationService.getInstance(project).startTool(
                template.toolName(), arguments, this::onToolProgress);
        currentInvocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                () -> showResult(template.toolName(), result, error, project)));
    }

    /**
     * Reflects a streamed {@code notifications/progress} milestone in the status line while a tool
     * runs, so a long call (for example {@code capture_generate_replay}) shows what it is doing
     * instead of a static "Running: <tool> …" the whole time (issue #3546). The server streams
     * progress best-effort for opted-in tools only; every other tool call never invokes this.
     * Called from the MCP client's background thread, so the UI update is marshaled to the EDT.
     */
    private void onToolProgress(ShaftMcpProgress progress) {
        String message = progress.message();
        if (message == null || message.isBlank()) {
            return;
        }
        ApplicationManager.getApplication().invokeLater(() -> status.setText(message));
    }

    private void refreshCatalog(Project project) {
        if (!catalogRefreshEnabled) {
            return;
        }
        if (currentInvocation != null) {
            return;
        }
        if (!mcpConfigured()) {
            status.setText("Configure MCP");
            setOutput("", "Configure SHAFT MCP in Settings before refreshing the tool catalog.");
            return;
        }
        if (!projectAvailable(project, "")) {
            return;
        }
        setRunning(true, "Refreshing tools...");
        refreshCatalogButton.setToolTipText(REFRESHING_TOOLTIP);
        clearOutput();
        // The button is an explicit user request for fresh data, so it must bypass the cache
        // unlike the silent auto-populate warm-up (which serves the cache when present).
        currentInvocation = ShaftMcpInvocationService.getInstance(project).startListTools(true);
        currentInvocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                () -> showCatalogResult(result, error, project)));
    }

    private void showResult(String toolName, ShaftMcpToolResult result, Throwable error, Project project) {
        if (error instanceof CancellationException) {
            setRunning(false, "Cancelled");
            setOutput(toolName, "Cancelled.");
            recoveryButton.setVisible(false);
            return;
        }
        setRunning(false, error == null && result != null && result.success() ? "Finished" : "Failed");
        McpInvocationError category = null;
        if (error != null) {
            category = McpInvocationError.categorize(error);
            StringBuilder sb = new StringBuilder();
            sb.append(McpInvocationError.detail(error, category));
            if (category.recoveryAction() != null) {
                sb.append("\n\nRecovery: ").append(category.recoveryAction());
            }
            setOutput(toolName, sb.toString());
        } else if (result == null) {
            setOutput(toolName, "No result returned.");
        } else if (result.success()) {
            setOutput(toolName, JsonText.prettyOrOriginal(result.output()));
        } else {
            category = result.errorCategory();
            setOutput(toolName, formatErrorOutput(result));
        }
        showRecovery(category, project, () -> run(project));
    }

    private void showCatalogResult(ShaftMcpToolResult result, Throwable error, Project project) {
        refreshCatalogButton.setToolTipText(REFRESH_TOOLS_TOOLTIP);
        if (error instanceof CancellationException) {
            setRunning(false, "Cancelled");
            setOutput("", "Cancelled.");
            recoveryButton.setVisible(false);
            return;
        }
        boolean success = error == null && result != null && result.success();
        McpInvocationError category = null;
        // setRunning(false, ...) clears currentInvocation, which reloadCategories() -> refreshTools()
        // -> loadSelectedTemplate() -> validateArguments() reads to decide whether to reset the status
        // label back to "Ready". Run that reload first so the SUCCESS_ICON/ERROR_ICON completion
        // message set below is the last write to the status label, not clobbered by it.
        setRunning(false, null);
        if (success) {
            categories = ToolTemplates.categories(result.output());
            reloadCategories();
            setOutput("", JsonText.prettyOrOriginal(result.output()));
        } else if (error != null) {
            category = McpInvocationError.categorize(error);
            StringBuilder sb = new StringBuilder();
            sb.append(McpInvocationError.detail(error, category));
            if (category.recoveryAction() != null) {
                sb.append("\n\nRecovery: ").append(category.recoveryAction());
            }
            setOutput("", sb.toString());
        } else if (result == null) {
            setOutput("", "No result returned.");
        } else {
            category = result.errorCategory();
            setOutput("", formatErrorOutput(result));
        }
        status.setText(success
                ? ShaftStatusPresentation.SUCCESS_ICON + " Tools refreshed"
                : ShaftStatusPresentation.ERROR_ICON + " Failed");
        showRecovery(category, project, () -> refreshCatalog(project));
    }

    private void showRecovery(McpInvocationError category, Project project, Runnable retryAction) {
        if (category == null) {
            recoveryButton.setVisible(false);
            return;
        }
        RecoveryActions.Kind kind = RecoveryActions.forCategory(category);
        recoveryButton.setText(switch (kind) {
            case RETRY -> "Retry";
            case RESTART -> "Restart MCP server";
            case VIEW_LOGS -> "View logs";
        });
        for (var listener : recoveryButton.getActionListeners()) {
            recoveryButton.removeActionListener(listener);
        }
        recoveryButton.addActionListener(event -> {
            switch (kind) {
                case RETRY -> retryAction.run();
                case RESTART -> {
                    ShaftMcpInvocationService.getInstance(project).restartConnection();
                    status.setText("MCP connection reset");
                }
                case VIEW_LOGS -> RecoveryActions.activateEventLog(project);
                default -> throw new IllegalStateException("Unexpected recovery kind: " + kind);
            }
        });
        recoveryButton.setVisible(true);
    }

    private void reloadCategories() {
        Object selected = categorySelector.getSelectedItem();
        String selectedLabel = selected instanceof ToolCategory category ? category.label() : "";
        updatingTools = true;
        categorySelector.removeAllItems();
        for (ToolCategory category : categories) {
            categorySelector.addItem(category);
            if (category.label().equals(selectedLabel)) {
                categorySelector.setSelectedItem(category);
            }
        }
        if (categorySelector.getSelectedItem() == null && categorySelector.getItemCount() > 0) {
            categorySelector.setSelectedIndex(0);
        }
        updateContextOptions();
        updatingTools = false;
        refreshTools();
    }

    private void refreshTools() {
        saveActiveDraft();
        String query = search.getText().trim().toLowerCase();
        ToolCategory category = (ToolCategory) categorySelector.getSelectedItem();
        String context = normalizeContext(String.valueOf(contextSelector.getSelectedItem()));
        updatingTools = true;
        toolSelector.removeAllItems();
        if (category == null && query.isBlank()) {
            updatingTools = false;
            activeTemplate = null;
            argumentsArea.setText("");
            return;
        }
        matchingTemplates(category, query, context).forEach(toolSelector::addItem);
        updatingTools = false;
        loadSelectedTemplate();
    }

    private void updateContextOptions() {
        boolean wasUpdatingTools = updatingTools;
        updatingTools = true;
        String selected = normalizeContext(String.valueOf(contextSelector.getSelectedItem()));
        LinkedHashSet<String> contexts = new LinkedHashSet<>();
        contexts.add("all");
        for (ToolCategory category : categories) {
            for (ToolTemplate template : category.templates()) {
                contexts.addAll(template.contextTypes());
            }
        }
        if (contexts.isEmpty()) {
            contexts.add("all");
        }
        contextSelector.removeAllItems();
        List<String> sorted = contexts.stream()
                .distinct()
                .sorted()
                .collect(Collectors.toList());
        for (String value : sorted) {
            contextSelector.addItem(value);
        }
        contextSelector.setSelectedItem(sorted.contains(selected) ? selected : "all");
        contextSelector.setEnabled(contextSelector.getItemCount() > 1);
        updatingTools = wasUpdatingTools;
    }

    private void loadSelectedTemplate() {
        saveActiveDraft();
        ToolTemplate selected = selectedTemplate();
        activeTemplate = selected;
        if (selected != null) {
            argumentsArea.setText(argumentDrafts.getOrDefault(draftKey(selected), selected.arguments()));
            templateDescription.setText(description(selected));
            templateDescription.setEnabled(true);
        } else {
            argumentsArea.setText("");
            templateDescription.setText("No tool template matches the current filter.");
            templateDescription.setEnabled(false);
        }
        validateArguments();
    }

    private void saveActiveDraft() {
        if (activeTemplate != null) {
            argumentDrafts.put(draftKey(activeTemplate), argumentsArea.getText());
        }
    }

    private void setRunning(boolean running, String message) {
        if (running) {
            recoveryButton.setVisible(false);
        }
        runButton.setEnabled(!running);
        cancelButton.setEnabled(running);
        restoreDefaultsButton.setEnabled(!running);
        refreshCatalogButton.setEnabled(!running);
        search.setEnabled(!running);
        categorySelector.setEnabled(!running);
        toolSelector.setEnabled(!running);
        argumentsArea.setEnabled(!running);
        progress.setVisible(running);
        if (message != null) {
            status.setText(message);
        }
        if (!running) {
            currentInvocation = null;
        }
    }

    private ToolTemplate selectedTemplate() {
        return (ToolTemplate) toolSelector.getSelectedItem();
    }

    private static JLabel label(String text, char mnemonic, JComponent target) {
        JLabel label = new JLabel(text);
        label.setDisplayedMnemonic(mnemonic);
        label.setLabelFor(target);
        return label;
    }

    private static void addSelectorComponent(JPanel panel,
                                             JComponent component,
                                             int gridX,
                                             double weightX,
                                             boolean fillHorizontal) {
        GridBagConstraints constraints = new GridBagConstraints();
        constraints.gridx = gridX;
        constraints.gridy = 0;
        constraints.weightx = weightX;
        constraints.anchor = GridBagConstraints.WEST;
        constraints.fill = fillHorizontal ? GridBagConstraints.HORIZONTAL : GridBagConstraints.NONE;
        constraints.insets = new Insets(0, 0, 0, gridX == 7 ? 0 : 6);
        panel.add(component, constraints);
    }

    private boolean mcpConfigured() {
        return settings.mcpReady();
    }

    private boolean projectAvailable(Project project, String toolName) {
        if (project != null) {
            return true;
        }
        status.setText("Open project");
        setOutput(toolName, "Open an IntelliJ project before running SHAFT MCP tools.");
        return false;
    }

    /**
     * Renders a tool result as a humanized card (issue #3552), reusing the same
     * {@link AssistantMarkdown} the Assistant chat renders with so a Tools-panel run and an
     * Assistant-routed run of the same tool read identically. {@code outputArea} keeps holding the
     * exact, untouched {@code rawText} so {@link #copyOutputButton} and the raw-JSON toggle always
     * expose the real bytes, one click away from the card.
     */
    private void setOutput(String toolName, String rawText) {
        outputArea.setText(rawText);
        outputCard.setMarkdown(AssistantMarkdown.fromMcpOutput(toolName, rawText));
        showRawOutput(false);
        boolean hasOutput = !rawText.isBlank();
        copyOutputButton.setEnabled(hasOutput);
        toggleRawOutputButton.setEnabled(hasOutput);
    }

    private void clearOutput() {
        setOutput("", "");
    }

    private void toggleRawOutput() {
        showRawOutput(!showingRawOutput);
    }

    private void showRawOutput(boolean raw) {
        showingRawOutput = raw;
        ((java.awt.CardLayout) outputCards.getLayout()).show(outputCards, raw ? "raw" : "card");
        // Icon-only button (issue #3538 icon-only-and-symmetric convention): the state flips in the
        // tooltip, never in visible text.
        toggleRawOutputButton.setToolTipText(raw ? "View summarized output" : "View raw JSON");
    }

    private static JPanel setupNotice(Project project, ShaftSettingsState.Settings settings) {
        JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
        panel.add(new JLabel("Configure SHAFT MCP to run Assistant and Tools."));
        JButton openSettings = new JButton("Open Settings");
        openSettings.getAccessibleContext().setAccessibleName("Open SHAFT settings");
        ShaftIconButtons.apply(openSettings, ShaftIcons.SETTINGS);
        openSettings.addActionListener(event -> {
            if (project != null) {
                ShowSettingsUtil.getInstance().showSettingsDialog(project, "SHAFT");
            }
        });
        panel.add(openSettings);
        panel.setVisible(!settings.mcpReady());
        return panel;
    }

    private static String draftKey(ToolTemplate template) {
        return template.toolName() + "\n" + template.label();
    }

    boolean prefillTool(String toolName, JsonObject arguments) {
        saveActiveDraft();
        search.setText("");
        for (ToolCategory category : categories) {
            for (ToolTemplate template : category.templates()) {
                if (template.toolName().equals(toolName)) {
                    categorySelector.setSelectedItem(category);
                    toolSelector.setSelectedItem(template);
                    activeTemplate = template;
                    argumentsArea.setText(JsonText.prettyOrOriginal(arguments.toString()));
                    argumentDrafts.put(draftKey(template), argumentsArea.getText());
                    templateDescription.setText(description(template));
                    validateArguments();
                    return true;
                }
            }
        }
        return false;
    }

    void selectCategory(String label) {
        search.setText("");
        for (ToolCategory category : categories) {
            if (category.label().equals(label)) {
                categorySelector.setSelectedItem(category);
                return;
            }
        }
    }

    private void restoreDefaults() {
        ToolTemplate template = selectedTemplate();
        if (template != null) {
            argumentDrafts.remove(draftKey(template));
            argumentsArea.setText(template.arguments());
            validateArguments();
        }
    }

    private void validateArguments() {
        String error = JsonText.validateObject(argumentsArea.getText());
        if (error.isBlank()) {
            if (currentInvocation == null) {
                status.setText("Ready");
            }
            runButton.setEnabled(currentInvocation == null);
        } else {
            status.setText("Invalid JSON");
            runButton.setEnabled(false);
        }
    }

    private void cancelCurrent() {
        if (currentInvocation != null) {
            currentInvocation.cancel();
            status.setText("Cancelling...");
        }
    }

    private void copyOutput() {
        if (!outputArea.getText().isBlank()) {
            CopyPasteManager.getInstance().setContents(new StringSelection(outputArea.getText()));
            status.setText("Copied output");
        }
    }

    private List<ToolTemplate> matchingTemplates(ToolCategory category, String query) {
        return matchingTemplates(category, query, "all");
    }

    private List<ToolTemplate> matchingTemplates(ToolCategory category, String query, String context) {
        return categories.stream()
                .filter(candidate -> category == null || candidate.equals(category))
                .flatMap(candidate -> candidate.templates().stream())
                .filter(template -> (query.isBlank() || matches(template, query)) && matches(context, template))
                .toList();
    }

    private static boolean matches(String context, ToolTemplate template) {
        return "all".equals(context) || template.contextTypes().stream().anyMatch(context::equals);
    }

    private static String normalizeContext(String value) {
        return value == null || value.isBlank() ? "all" : value.trim().toLowerCase(Locale.ROOT).replace(' ', '-');
    }

    private static boolean matches(ToolTemplate template, String query) {
        return template.label().toLowerCase().contains(query)
                || template.toolName().toLowerCase().contains(query)
                || template.description().toLowerCase().contains(query);
    }

    private static String description(ToolTemplate template) {
        return template.description().isBlank() ? template.toolName() : template.description();
    }

    private static String formatErrorOutput(ShaftMcpToolResult result) {
        if (result.errorCategory() != null) {
            StringBuilder sb = new StringBuilder();
            sb.append(result.output());
            if (result.recoveryAction() != null) {
                sb.append("\n\nRecovery: ").append(result.recoveryAction());
            }
            return sb.toString();
        }
        return result.output();
    }

    private void selectJsonErrorLocation(JsonText.JsonErrorLocation location) {
        String text = argumentsArea.getText();
        int targetLine = location.line - 1;
        int currentLine = 0;
        int lineStart = 0;
        for (int i = 0; i < text.length(); i++) {
            if (currentLine == targetLine) {
                int lineEnd = text.indexOf('\n', i);
                if (lineEnd == -1) {
                    lineEnd = text.length();
                }
                int selectStart = Math.min(lineStart + location.column, lineEnd);
                argumentsArea.setCaretPosition(selectStart);
                argumentsArea.moveCaretPosition(Math.min(selectStart + 1, lineEnd));
                break;
            }
            if (text.charAt(i) == '\n') {
                currentLine++;
                lineStart = i + 1;
            }
        }
    }

    private record SimpleDocumentListener(Runnable callback) implements DocumentListener {
        @Override
        public void insertUpdate(DocumentEvent event) {
            callback.run();
        }

        @Override
        public void removeUpdate(DocumentEvent event) {
            callback.run();
        }

        @Override
        public void changedUpdate(DocumentEvent event) {
            callback.run();
        }
    }
}
