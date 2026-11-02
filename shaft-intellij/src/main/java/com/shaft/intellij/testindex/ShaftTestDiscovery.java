package com.shaft.intellij.testindex;

import com.intellij.ide.highlighter.JavaFileType;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiAnnotation;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiJavaFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.shaft.intellij.testrunner.ShaftTestMethodAnnotations;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * Project-wide, PSI-based discovery of every SHAFT-runnable {@code @Test} class -- independent of
 * whether it has ever been run (that's {@link ShaftTestIndex}'s job; the two are combined by
 * {@code ShaftTestsPanel}'s package/class/method tree).
 * <p>
 * Scans project sources only ({@link GlobalSearchScope#projectScope}, not libraries) for
 * {@code .java} files, and keeps a class only when {@link ShaftTestMethodAnnotations#isShaftRunnableTestMethod}
 * accepts at least one of its methods -- the exact same annotation-FQN detection logic the
 * run-configuration producers use (see {@code ShaftTestNgRunConfigurationProducer}), so this tree
 * never disagrees with what a gutter icon would offer to run.
 * <p>
 * <b>v1 scope:</b> a synchronous, uncached project scan, run in a read action; each "Refresh" click
 * re-scans from scratch. Fine for the tab's manual-refresh usage today -- a future iteration could
 * cache against PSI/VFS change events if this proves slow on very large projects.
 */
public final class ShaftTestDiscovery {
    private ShaftTestDiscovery() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * One discovered SHAFT test class: plain data, no PSI/Swing coupling, so it is independently
     * unit-testable and directly usable as {@code ShaftTestsPanel}'s tree source.
     *
     * @param qualifiedName fully-qualified class name
     * @param packageName   containing package, or {@code ""} for the default package
     * @param simpleName    simple class name
     * @param methodNames   names of the class's runnable {@code @Test} methods, in declaration order
     */
    public record DiscoveredTestClass(String qualifiedName, String packageName, String simpleName,
                                       List<String> methodNames) {
    }

    /**
     * Discovers every SHAFT-runnable test class in {@code project}'s own sources.
     *
     * @param project the project to scan, or {@code null} for an immediate empty result -- mirrors
     *                {@link com.shaft.intellij.project.ShaftProjectDetector#isShaftProject}'s
     *                null-safety convention, since {@code ShaftTestsPanel} is constructed with a
     *                {@code null} project in unit tests
     * @return discovered classes, unordered; also empty (rather than throwing) when no platform
     *         {@code Application} is running -- {@code ShaftToolWindowPanel}'s plain-JUnit tests
     *         build a real, lightweight {@code Project} outside a full platform test harness, where
     *         {@link ApplicationManager#getApplication()} returns {@code null}
     */
    public static List<DiscoveredTestClass> discover(@Nullable Project project) {
        if (project == null || ApplicationManager.getApplication() == null) {
            return List.of();
        }
        return ApplicationManager.getApplication().runReadAction(
                (Computable<List<DiscoveredTestClass>>) () -> scan(project));
    }

    private static List<DiscoveredTestClass> scan(Project project) {
        GlobalSearchScope scope = GlobalSearchScope.projectScope(project);
        PsiManager psiManager = PsiManager.getInstance(project);
        List<DiscoveredTestClass> discovered = new ArrayList<>();
        for (VirtualFile file : FileTypeIndex.getFiles(JavaFileType.INSTANCE, scope)) {
            if (!(psiManager.findFile(file) instanceof PsiJavaFile javaFile)) {
                continue;
            }
            for (PsiClass psiClass : javaFile.getClasses()) {
                collectIfRunnable(psiClass, javaFile.getPackageName(), discovered);
            }
        }
        return discovered;
    }

    private static void collectIfRunnable(PsiClass psiClass, String packageName, List<DiscoveredTestClass> out) {
        String qualifiedName = psiClass.getQualifiedName();
        String simpleName = psiClass.getName();
        if (qualifiedName == null || simpleName == null) {
            return;
        }
        List<String> methodNames = new ArrayList<>();
        for (PsiMethod method : psiClass.getMethods()) {
            if (ShaftTestMethodAnnotations.isShaftRunnableTestMethod(annotationQualifiedNames(method))) {
                methodNames.add(method.getName());
            }
        }
        if (!methodNames.isEmpty()) {
            out.add(new DiscoveredTestClass(qualifiedName, packageName, simpleName, methodNames));
        }
    }

    private static List<String> annotationQualifiedNames(PsiMethod method) {
        List<String> qualifiedNames = new ArrayList<>();
        for (PsiAnnotation annotation : method.getModifierList().getAnnotations()) {
            String qualifiedName = annotation.getQualifiedName();
            if (qualifiedName != null) {
                qualifiedNames.add(qualifiedName);
            }
        }
        return qualifiedNames;
    }
}
