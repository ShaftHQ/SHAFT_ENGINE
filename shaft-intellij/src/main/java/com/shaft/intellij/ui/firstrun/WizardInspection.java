package com.shaft.intellij.ui.firstrun;

import com.shaft.intellij.settings.ShaftSettingsState;
import com.shaft.intellij.ui.ShaftToolWindowPanel;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import java.awt.Component;
import java.awt.Container;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/** Ten yes/no probes. {@link FirstRunUxScore} counts them. */
public final class WizardInspection {
    private final boolean[] probes;

    private WizardInspection(boolean[] probes) {
        this.probes = probes;
    }

    public boolean[] probes() {
        return probes.clone();
    }

    public static WizardInspection failing() {
        return of();
    }

    public static WizardInspection of(boolean... values) {
        boolean[] probes = new boolean[10];
        if (values != null) {
            for (int index = 0; index < values.length && index < probes.length; index++) {
                probes[index] = values[index];
            }
        }
        return new WizardInspection(probes);
    }

    public static WizardInspection capture(FirstRunWizardPanel wizard) {
        boolean[] probes = new boolean[10];
        probes[0] = stepperProbe(wizard);
        probes[1] = bundleSentences(wizard);
        probes[2] = navigationProbe(wizard);
        probes[3] = oneDefaultButton(wizard);
        probes[4] = checkProbe(wizard);
        probes[5] = factsAndAgent(wizard);
        probes[6] = completedSkipsWizard(wizard);
        probes[7] = currentStepOnly(wizard);
        probes[8] = failureRecovery(wizard);
        probes[9] = docsLink(wizard);
        return new WizardInspection(probes);
    }

    private static boolean stepperProbe(FirstRunWizardPanel wizard) {
        JLabel stepper = label(wizard, WizardMessages.get("wizard.stepper"));
        if (stepper == null || stepper.getAccessibleContext() == null) {
            return false;
        }
        String text = stepper.getText() == null ? "" : stepper.getText();
        String description = stepper.getAccessibleContext().getAccessibleDescription();
        description = description == null ? "" : description;
        boolean indexed = text.contains(WizardMessages.format("wizard.stepper.line", wizard.step(), 5, wizard.stepTitle()));
        return indexed
                && description.contains(WizardMessages.get("wizard.progress.done"))
                && description.contains(WizardMessages.get("wizard.progress.current"))
                && description.contains(WizardMessages.get("wizard.progress.next"));
    }

    private static boolean bundleSentences(FirstRunWizardPanel wizard) {
        if (sourceHasUiLiterals()) {
            return false;
        }
        return !containsExit(wizard.primary().getText()) && !containsExit(labelText(wizard, "wizard.failure.cause.name"));
    }

    private static boolean navigationProbe(FirstRunWizardPanel wizard) {
        boolean controls = button(wizard, WizardMessages.get("wizard.back")) != null
                && button(wizard, WizardMessages.get("wizard.skip")) != null;
        String assistant = read(source("src/main/java/com/shaft/intellij/ui/ShaftAssistantPanel.java"));
        String activity = read(source("src/main/java/com/shaft/intellij/settings/ShaftFirstRunNoticeActivity.java"));
        boolean reopen = assistant.contains("Open SHAFT MCP setup");
        boolean startup = activity.contains("executeOnPooledThread")
                && !activity.contains("Dialog" + "Wrapper")
                && !activity.contains("FirstRun" + "WizardPanel");
        return controls && reopen && startup;
    }

    private static boolean oneDefaultButton(FirstRunWizardPanel wizard) {
        List<JButton> defaults = new ArrayList<>();
        walkDefaults(wizard, wizard, defaults);
        return defaults.size() == 1;
    }

    private static boolean checkProbe(FirstRunWizardPanel wizard) {
        boolean gated = !wizard.verified() || wizard.userChecked();
        return gated && installerNotExecuted();
    }

    private static boolean factsAndAgent(FirstRunWizardPanel wizard) {
        String facts = labelText(wizard, "wizard.facts.name");
        return factsContainsJdk(facts) && !facts.isBlank() && wizard.selectedAgent() != null;
    }

    private static boolean completedSkipsWizard(FirstRunWizardPanel wizard) {
        ShaftSettingsState.Settings completed = new ShaftSettingsState.Settings();
        completed.firstRunWizardCompleted = true;
        return !ShaftToolWindowPanel.opensWizard(completed, false) && !wizard.alternativesVisible();
    }

    private static boolean currentStepOnly(FirstRunWizardPanel wizard) {
        boolean cards = true;
        for (int index = 1; index <= 5; index++) {
            JComponent card = named(wizard, WizardMessages.format("wizard.step.card", index), JComponent.class);
            if (card == null || card.isVisible() != (index == wizard.step())) {
                cards = false;
            }
        }
        JLabel ready = label(wizard, WizardMessages.get("wizard.ready.name"));
        String readyPrefix = WizardMessages.get("wizard.ready").split("\\{", 2)[0];
        boolean oneLine = ready != null && ready.getText() != null && ready.getText().startsWith(readyPrefix)
                && !ready.getText().contains("\n");
        return cards && oneLine && !visibleMissing(wizard);
    }

    private static boolean failureRecovery(FirstRunWizardPanel wizard) {
        JLabel cause = label(wizard, WizardMessages.get("wizard.failure.cause.name"));
        return cause != null && shown(cause, wizard)
                && WizardMessages.get("wizard.failure.cause").equals(cause.getText())
                && WizardMessages.get("wizard.failure.recovery").equals(wizard.primary().getText());
    }

    private static boolean docsLink(FirstRunWizardPanel wizard) {
        String url = WizardMessages.get("wizard.docs.url");
        return described(wizard, wizard, url);
    }

    private static boolean factsContainsJdk(String facts) {
        String marker = WizardMessages.get("wizard.facts.jdk");
        int placeholder = marker.indexOf('{');
        String prefix = placeholder < 0 ? marker : marker.substring(0, placeholder);
        return facts.contains(prefix.trim());
    }

    private static boolean visibleMissing(Component component) {
        if (component.getAccessibleContext() != null) {
            String name = component.getAccessibleContext().getAccessibleName();
            String prefix = WizardMessages.get("wizard.missing.name").split("\\{", 2)[0];
            if (name != null && name.startsWith(prefix) && component.isVisible()) {
                return true;
            }
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                if (visibleMissing(child)) {
                    return true;
                }
            }
        }
        return false;
    }

    private static boolean described(Component component, Component root, String url) {
        if (shown(component, root) && component.getAccessibleContext() != null) {
            String description = component.getAccessibleContext().getAccessibleDescription();
            if (description != null && description.contains(url)) {
                return true;
            }
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                if (described(child, root, url)) {
                    return true;
                }
            }
        }
        return false;
    }

    private static void walkDefaults(Component component, Component root, List<JButton> found) {
        if (component instanceof JButton button && button.isDefaultCapable() && shown(button, root)) {
            found.add(button);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                walkDefaults(child, root, found);
            }
        }
    }

    private static boolean shown(Component component, Component root) {
        for (Component current = component; current != null; current = current.getParent()) {
            if (!current.isVisible()) {
                return false;
            }
            if (Objects.equals(current, root)) {
                return true;
            }
        }
        return false;
    }

    private static boolean containsExit(String text) {
        return text != null && text.toLowerCase(java.util.Locale.ROOT).contains("exit");
    }

    private static String labelText(Component root, String key) {
        JLabel label = label(root, WizardMessages.get(key));
        return label == null || label.getText() == null ? "" : label.getText();
    }

    private static JLabel label(Component root, String name) {
        return named(root, name, JLabel.class);
    }

    private static JButton button(Component root, String name) {
        return named(root, name, JButton.class);
    }

    private static <T extends Component> T named(Component component, String name, Class<T> type) {
        if (type.isInstance(component) && component.getAccessibleContext() != null
                && name.equals(component.getAccessibleContext().getAccessibleName())) {
            return type.cast(component);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                T found = named(child, name, type);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static boolean sourceHasUiLiterals() {
        String setText = "set" + "Text" + "(" + '"';
        String accessible = "set" + "Accessible" + "Name" + "(" + '"';
        String tip = "set" + "Tool" + "Tip" + "Text" + "(" + '"';
        String description = "set" + "Accessible" + "Description" + "(" + '"';
        String button = "new " + "JButton" + "(" + '"';
        String label = "new " + "JLabel" + "(" + '"';
        String jbLabel = "new " + "JBLabel" + "(" + '"';
        String source = packageSource();
        return source.contains(setText) || source.contains(accessible) || source.contains(tip)
                || source.contains(description) || source.contains(button) || source.contains(label)
                || source.contains(jbLabel);
    }

    private static boolean installerNotExecuted() {
        String process = "Process" + "Builder";
        String runtime = "Runtime.get" + "Runtime";
        String commandLine = "General" + "Command" + "Line";
        String script = "install-shaft-" + "agentic-tools";
        String curl = "curl -f" + "L";
        String web = "Invoke-" + "Web" + "Request";
        Path dir = packageDir();
        if (dir == null) {
            return false;
        }
        try (var paths = Files.list(dir)) {
            for (Path path : paths.filter(path -> path.toString().endsWith(".java")).toList()) {
                String text = Files.readString(path);
                if (text.contains(script) || text.contains(curl) || text.contains(web)) {
                    return false;
                }
                String name = path.getFileName().toString();
                boolean report = "SetupFailureReport.java".equals(name);
                if (!report && (text.contains(process) || text.contains(runtime) || text.contains(commandLine))) {
                    return false;
                }
            }
            return true;
        } catch (IOException exception) {
            return false;
        }
    }

    private static String packageSource() {
        Path dir = packageDir();
        if (dir == null) {
            return "";
        }
        StringBuilder source = new StringBuilder();
        try (var paths = Files.list(dir)) {
            for (Path path : paths.filter(path -> path.toString().endsWith(".java")).toList()) {
                source.append(Files.readString(path));
            }
        } catch (IOException exception) {
            return "";
        }
        return source.toString();
    }

    private static Path packageDir() {
        Path module = Path.of("src/main/java/com/shaft/intellij/ui/firstrun");
        if (Files.isDirectory(module)) {
            return module;
        }
        Path repo = Path.of("shaft-intellij/src/main/java/com/shaft/intellij/ui/firstrun");
        return Files.isDirectory(repo) ? repo : null;
    }

    private static Path source(String relative) {
        Path module = Path.of(relative);
        if (Files.isRegularFile(module)) {
            return module;
        }
        return Path.of("shaft-intellij").resolve(relative);
    }

    private static String read(Path path) {
        try {
            return Files.readString(path);
        } catch (IOException exception) {
            return "";
        }
    }
}
