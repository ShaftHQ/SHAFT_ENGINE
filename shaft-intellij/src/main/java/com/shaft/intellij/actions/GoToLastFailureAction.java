package com.shaft.intellij.actions;

import com.intellij.openapi.actionSystem.ActionUpdateThread;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.JavaPsiFacade;
import com.intellij.psi.PsiClass;
import com.intellij.psi.search.GlobalSearchScope;
import com.shaft.intellij.notifications.ShaftNotifier;
import com.shaft.intellij.testindex.LastRunResults;
import org.jetbrains.annotations.NotNull;

import java.nio.file.Path;
import java.util.List;

/**
 * Opens the failing line of a test from the newest allure-results (issue #6423). One failure
 * navigates directly; several show a chooser.
 */
public final class GoToLastFailureAction extends AnAction implements DumbAware {
    static final String NO_FAILURES = "The last run has no failed tests in allure-results.";

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        if (project == null || project.getBasePath() == null) {
            return;
        }
        List<LastRunResults.Result> failures = LastRunResults.failures(Path.of(project.getBasePath()));
        if (failures.isEmpty()) {
            ShaftNotifier.info(project, "Go to last failure", NO_FAILURES);
        } else if (failures.size() == 1) {
            open(project, failures.get(0));
        } else {
            JBPopupFactory.getInstance().createPopupChooserBuilder(failures)
                    .setTitle("SHAFT: Failed Tests in the Last Run")
                    .setRenderer(new com.intellij.ui.SimpleListCellRenderer<LastRunResults.Result>() {
                        @Override
                        public void customize(@NotNull javax.swing.JList<? extends LastRunResults.Result> list,
                                              LastRunResults.Result value, int index, boolean selected, boolean focused) {
                            setText(entryText(value));
                        }
                    })
                    .setItemChosenCallback(result -> open(project, result))
                    .createPopup().showCenteredInCurrentWindow(project);
        }
    }

    @Override
    public @NotNull ActionUpdateThread getActionUpdateThread() {
        return ActionUpdateThread.BGT;
    }

    /** List text for one failure, for example {@code LoginTest#signIn (line 42)}. */
    static String entryText(LastRunResults.Result result) {
        String name = result.fullName();
        int dot = name.lastIndexOf('.', name.lastIndexOf('.') - 1);
        String shortName = (dot < 0 ? name : name.substring(dot + 1)).replaceFirst("\\.(?=[^.]*$)", "#");
        return result.frame() == null ? shortName : shortName + " (line " + result.frame().line() + ")";
    }

    private static void open(Project project, LastRunResults.Result result) {
        LastRunResults.Frame frame = result.frame();
        String className = frame != null ? frame.className()
                : result.fullName().substring(0, Math.max(0, result.fullName().lastIndexOf('.')));
        VirtualFile file = ReadAction.compute(() -> {
            PsiClass psiClass = JavaPsiFacade.getInstance(project)
                    .findClass(className.replace('$', '.'), GlobalSearchScope.projectScope(project));
            return psiClass == null || psiClass.getContainingFile() == null
                    ? null : psiClass.getContainingFile().getVirtualFile();
        });
        if (file == null) {
            ShaftNotifier.warn(project, "Go to last failure", "Source for " + className + " is not in this project.");
            return;
        }
        new OpenFileDescriptor(project, file, frame == null ? 0 : Math.max(0, frame.line() - 1), 0).navigate(true);
    }
}
