package com.shaft.intellij.testindex;

import com.intellij.execution.RunManager;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.junit.JUnitConfiguration;
import com.intellij.execution.junit.JUnitConfigurationType;
import com.intellij.execution.runners.ExecutionUtil;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.JavaPsiFacade;
import com.intellij.psi.PsiAnnotation;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.PsiShortNamesCache;
import com.shaft.intellij.testrunner.ShaftTestMethodAnnotations;
import com.theoryinpractice.testng.configuration.TestNGConfiguration;
import com.theoryinpractice.testng.configuration.TestNGConfigurationType;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

/**
 * Resolves a {@link ShaftTestIndex.TestRowState#testId()} (a run-configuration display name --
 * see that record's javadoc) back into something runnable or navigable, for the "SHAFT Tests"
 * tool-window tab ({@code ShaftTestsPanel}).
 * <p>
 * <b>Run:</b> if a run configuration with that exact name still exists, rerun it (same mechanism
 * as {@link ShaftTestWatchService}'s {@code rerunLastTest()}). Otherwise, a fresh one is created
 * from a resolved {@link PsiClass}, mirroring {@code ShaftTestNgRunConfigurationProducer} /
 * {@code ShaftJUnitRunConfigurationProducer}'s class-configuration setup.
 * <p>
 * <b>v1 short-name ambiguity:</b> when {@code testId} is not a fully-qualified name (no {@code .}),
 * class resolution falls back to {@link PsiShortNamesCache#getClassesByName}, which can return
 * multiple same-named classes from different packages; the first match IntelliJ returns is used
 * with no further disambiguation. This mirrors {@link ShaftTestIndex}'s own "v1 granularity, not
 * per-{@code @Test}-method" tradeoff (see that class's javadoc) -- a future iteration could
 * disambiguate using the containing module/package recorded at run time.
 */
public final class ShaftRunConfigurationResolver {
    private ShaftRunConfigurationResolver() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Looks up an existing run configuration by its exact display name.
     *
     * @param project project to search
     * @param name    run-configuration display name to match
     * @return the first matching settings, or empty if none match
     */
    public static Optional<RunnerAndConfigurationSettings> findByName(Project project, String name) {
        return findByName(RunManager.getInstance(project).getAllSettings(), name);
    }

    /**
     * Pure core of {@link #findByName(Project, String)}, kept separate from the {@code RunManager}
     * lookup so it is directly unit testable against a list of fakes.
     *
     * @param allSettings run configurations to search
     * @param name        run-configuration display name to match
     * @return the first matching settings, or empty if none match
     */
    static Optional<RunnerAndConfigurationSettings> findByName(
            List<RunnerAndConfigurationSettings> allSettings, String name) {
        return allSettings.stream()
                .filter(settings -> settings.getName().equals(name))
                .findFirst();
    }

    /**
     * Runs {@code testId}: reruns its existing run configuration if one is still registered under
     * that exact name, otherwise resolves a {@link PsiClass} for it and creates a fresh TestNG or
     * JUnit class configuration (see class javadoc for the short-name ambiguity this can hit).
     * A no-op if neither an existing configuration nor a resolvable, runnable class is found.
     *
     * @param project project the test belongs to
     * @param testId  run-configuration display name (or fully-qualified/short class name) to run
     */
    public static void run(Project project, String testId) {
        Optional<RunnerAndConfigurationSettings> existing = findByName(project, testId);
        if (existing.isPresent()) {
            runSettings(project, existing.get());
            return;
        }
        resolvePsiClass(project, testId).ifPresent(psiClass -> createAndRun(project, testId, psiClass));
    }

    /**
     * Navigates the editor to the class resolved for {@code testId}. A no-op if the class (or its
     * containing file) cannot be resolved.
     *
     * @param project project the test belongs to
     * @param testId  fully-qualified or short class name to navigate to
     */
    public static void navigate(Project project, String testId) {
        resolvePsiClass(project, testId).ifPresent(psiClass -> {
            VirtualFile file = psiClass.getContainingFile() == null
                    ? null : psiClass.getContainingFile().getVirtualFile();
            if (file != null) {
                new OpenFileDescriptor(project, file, psiClass.getTextOffset()).navigate(true);
            }
        });
    }

    private static void createAndRun(Project project, String testId, PsiClass psiClass) {
        Optional<FrameworkKind> kind = detectFrameworkKind(extractMethodAnnotationNames(psiClass));
        if (kind.isEmpty()) {
            // No recognized @Test method on the resolved class: nothing runnable to create, per
            // the same "decline silently" contract as the run-configuration producers.
            return;
        }
        RunManager runManager = RunManager.getInstance(project);
        ConfigurationFactory factory = configurationFactory(kind.get());
        RunnerAndConfigurationSettings settings = runManager.createConfiguration(testId, factory);
        beClassConfiguration(kind.get(), settings, psiClass);
        runManager.addConfiguration(settings);
        runSettings(project, settings);
    }

    private static ConfigurationFactory configurationFactory(FrameworkKind kind) {
        return kind == FrameworkKind.TESTNG
                ? (ConfigurationFactory) TestNGConfigurationType.getInstance()
                : JUnitConfigurationType.getInstance().getFactory();
    }

    private static void beClassConfiguration(
            FrameworkKind kind, RunnerAndConfigurationSettings settings, PsiClass psiClass) {
        if (kind == FrameworkKind.TESTNG) {
            ((TestNGConfiguration) settings.getConfiguration()).beClassConfiguration(psiClass);
        } else {
            ((JUnitConfiguration) settings.getConfiguration()).beClassConfiguration(psiClass);
        }
    }

    private static void runSettings(Project project, RunnerAndConfigurationSettings settings) {
        ApplicationManager.getApplication().invokeLater(() -> {
            if (!project.isDisposed()) {
                ExecutionUtil.runConfiguration(settings, DefaultRunExecutor.getRunExecutorInstance());
            }
        });
    }

    /**
     * Resolves a {@link PsiClass} for {@code testId}: a fully-qualified lookup when the id contains
     * a {@code .}, otherwise a short-name cache lookup (see class javadoc for the ambiguity this can
     * hit).
     *
     * @param project project to search
     * @param testId  fully-qualified or short class name
     * @return the resolved class, or empty if nothing matches
     */
    static Optional<PsiClass> resolvePsiClass(Project project, String testId) {
        if (testId == null || testId.isBlank()) {
            return Optional.empty();
        }
        GlobalSearchScope scope = GlobalSearchScope.allScope(project);
        if (looksFullyQualified(testId)) {
            return Optional.ofNullable(JavaPsiFacade.getInstance(project).findClass(testId, scope));
        }
        PsiClass[] matches = PsiShortNamesCache.getInstance(project).getClassesByName(testId, scope);
        return matches.length > 0 ? Optional.of(matches[0]) : Optional.empty();
    }

    /**
     * Returns whether {@code testId} looks like a fully-qualified class name (contains a {@code .}),
     * as opposed to a bare short class name.
     *
     * @param testId run-configuration/class id to inspect
     * @return {@code true} when {@code testId} is non-null and contains a {@code .}
     */
    static boolean looksFullyQualified(@Nullable String testId) {
        return testId != null && testId.indexOf('.') >= 0;
    }

    /**
     * Recognized SHAFT-runnable test frameworks, mirroring {@link ShaftTestMethodAnnotations}'s
     * annotation constants.
     */
    enum FrameworkKind {
        TESTNG, JUNIT
    }

    /**
     * Decides which framework a class's methods belong to, given each method's annotation
     * fully-qualified names. TestNG takes priority when a class mixes annotations (should not
     * normally happen). Pure predicate over plain FQN lists -- mirrors
     * {@link ShaftTestMethodAnnotations#hasRunnableTestMethod} so it is directly unit testable
     * without a real {@link PsiClass}.
     *
     * @param methodsAnnotationQualifiedNames one entry per method, each the annotation FQNs on that
     *                                        method (entries or the outer value may be {@code null})
     * @return the detected framework kind, or empty when no recognized {@code @Test} annotation is
     *         present
     */
    static Optional<FrameworkKind> detectFrameworkKind(
            Collection<? extends Collection<String>> methodsAnnotationQualifiedNames) {
        if (methodsAnnotationQualifiedNames == null || methodsAnnotationQualifiedNames.isEmpty()) {
            return Optional.empty();
        }
        boolean hasTestNg = methodsAnnotationQualifiedNames.stream()
                .anyMatch(names -> names != null && names.contains(ShaftTestMethodAnnotations.TESTNG_TEST));
        if (hasTestNg) {
            return Optional.of(FrameworkKind.TESTNG);
        }
        boolean hasJUnit = methodsAnnotationQualifiedNames.stream()
                .anyMatch(names -> names != null && (names.contains(ShaftTestMethodAnnotations.JUNIT4_TEST)
                        || names.contains(ShaftTestMethodAnnotations.JUNIT5_TEST)));
        return hasJUnit ? Optional.of(FrameworkKind.JUNIT) : Optional.empty();
    }

    private static List<List<String>> extractMethodAnnotationNames(PsiClass psiClass) {
        List<List<String>> methodsAnnotationQualifiedNames = new ArrayList<>();
        for (PsiMethod method : psiClass.getMethods()) {
            List<String> qualifiedNames = new ArrayList<>();
            for (PsiAnnotation annotation : method.getModifierList().getAnnotations()) {
                String qualifiedName = annotation.getQualifiedName();
                if (qualifiedName != null) {
                    qualifiedNames.add(qualifiedName);
                }
            }
            methodsAnnotationQualifiedNames.add(qualifiedNames);
        }
        return methodsAnnotationQualifiedNames;
    }
}
