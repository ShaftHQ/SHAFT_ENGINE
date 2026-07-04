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
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
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
import java.util.ArrayList;
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
    private List<ToolCategory> categories;
    private final boolean catalogRefreshEnabled;
    private final JBTextField search;
    private final JComboBox<ToolCategory> categorySelector;
    private final JComboBox<String> contextSelector;
    private final JComboBox<ToolTemplate> toolSelector;
    private final JLabel templateDescription;
    private final JBTextArea argumentsArea;
    private final JBTextArea outputArea;
    private final JButton runButton;
    private final JButton cancelButton;
    private final JButton restoreDefaultsButton;
    private final JButton copyOutputButton;
    private final JButton refreshCatalogButton;
    private final JProgressBar progress;
    private final JLabel status;
    private final ShaftSettingsState.Settings settings;
    private final Map<String, String> argumentDrafts = new LinkedHashMap<>();
    private ToolTemplate activeTemplate;
    private ShaftMcpInvocation currentInvocation;
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
        outputArea.getAccessibleContext().setAccessibleName("SHAFT tool output");
        outputArea.setEditable(false);
        outputArea.setLineWrap(true);
        outputArea.setWrapStyleWord(true);
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
        output.add(outputLabel, BorderLayout.NORTH);
        output.add(new JBScrollPane(outputArea), BorderLayout.CENTER);

        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, center, output);
        splitPane.setResizeWeight(0.66);
        splitPane.setBorder(JBUI.Borders.empty());
        add(north, BorderLayout.NORTH);
        add(splitPane, BorderLayout.CENTER);
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
            outputArea.setText("Configure SHAFT MCP in Settings before running Tools requests.");
            copyOutputButton.setEnabled(true);
            return;
        }
        if (!projectAvailable(project)) {
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
            outputArea.setText("Invalid JSON: " + exception.getMessage());
            copyOutputButton.setEnabled(true);
            return;
        }
        setRunning(true, "Running " + template.toolName() + "...");
        outputArea.setText("");
        copyOutputButton.setEnabled(false);
        currentInvocation = ShaftMcpInvocationService.getInstance(project).startTool(template.toolName(), arguments);
        currentInvocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                () -> showResult(result, error)));
    }

    private void refreshCatalog(Project project) {
        if (!catalogRefreshEnabled) {
            return;
        }
        if (!mcpConfigured()) {
            status.setText("Configure MCP");
            outputArea.setText("Configure SHAFT MCP in Settings before refreshing the tool catalog.");
            copyOutputButton.setEnabled(true);
            return;
        }
        if (!projectAvailable(project)) {
            return;
        }
        setRunning(true, "Refreshing tools...");
        outputArea.setText("");
        copyOutputButton.setEnabled(false);
        currentInvocation = ShaftMcpInvocationService.getInstance(project).startListTools();
        currentInvocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                () -> showCatalogResult(result, error)));
    }

    private void showResult(ShaftMcpToolResult result, Throwable error) {
        if (error instanceof CancellationException) {
            setRunning(false, "Cancelled");
            outputArea.setText("Cancelled.");
            copyOutputButton.setEnabled(true);
            return;
        }
        setRunning(false, error == null && result != null && result.success() ? "Finished" : "Failed");
        if (error != null) {
            outputArea.setText(error.getMessage());
        } else if (result == null) {
            outputArea.setText("No result returned.");
        } else {
            outputArea.setText(JsonText.prettyOrOriginal(result.output()));
        }
        copyOutputButton.setEnabled(!outputArea.getText().isBlank());
    }

    private void showCatalogResult(ShaftMcpToolResult result, Throwable error) {
        if (error instanceof CancellationException) {
            setRunning(false, "Cancelled");
            outputArea.setText("Cancelled.");
            copyOutputButton.setEnabled(true);
            return;
        }
        boolean success = error == null && result != null && result.success();
        setRunning(false, success ? "Tools refreshed" : "Failed");
        if (success) {
            categories = ToolTemplates.categories(result.output());
            reloadCategories();
            outputArea.setText(JsonText.prettyOrOriginal(result.output()));
        } else if (error != null) {
            outputArea.setText(error.getMessage());
        } else {
            outputArea.setText(result == null ? "No result returned." : result.output());
        }
        copyOutputButton.setEnabled(!outputArea.getText().isBlank());
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
        } else {
            argumentsArea.setText("");
            templateDescription.setText("No tool template matches the current filter.");
        }
        validateArguments();
    }

    private void saveActiveDraft() {
        if (activeTemplate != null) {
            argumentDrafts.put(draftKey(activeTemplate), argumentsArea.getText());
        }
    }

    private void setRunning(boolean running, String message) {
        runButton.setEnabled(!running);
        cancelButton.setEnabled(running);
        restoreDefaultsButton.setEnabled(!running);
        refreshCatalogButton.setEnabled(!running);
        search.setEnabled(!running);
        categorySelector.setEnabled(!running);
        toolSelector.setEnabled(!running);
        argumentsArea.setEnabled(!running);
        progress.setVisible(running);
        status.setText(message);
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

    private boolean projectAvailable(Project project) {
        if (project != null) {
            return true;
        }
        status.setText("Open project");
        outputArea.setText("Open an IntelliJ project before running SHAFT MCP tools.");
        copyOutputButton.setEnabled(true);
        return false;
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
