package com.shaft.intellij.testindex;

import com.intellij.execution.RunnerAndConfigurationSettings;
import com.shaft.intellij.testrunner.ShaftTestMethodAnnotations;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Proxy;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers {@link ShaftRunConfigurationResolver}'s pure/testable logic: the fully-qualified-name
 * heuristic, the {@code RunManager}-free core of {@code findByName}, and the annotation-based
 * TestNG/JUnit framework detection.
 * <p>
 * The platform-glue methods ({@code run}, {@code navigate}, and PSI/{@code RunManager} resolution
 * against a real {@link com.intellij.openapi.project.Project}) are not covered here: this module
 * has no {@code BasePlatformTestCase} fixture (see {@code PickLocatorAtCaretActionTest}'s javadoc
 * and the run-configuration producers, which are likewise untested at the PSI level) and adding
 * one is a build-dependency change outside this class's scope.
 */
class ShaftRunConfigurationResolverTest {

    // ------------------------------------------------------------------
    // looksFullyQualified
    // ------------------------------------------------------------------

    @Test
    void looksFullyQualifiedIsTrueForADottedName() {
        assertTrue(ShaftRunConfigurationResolver.looksFullyQualified("com.example.CheckoutTest"));
    }

    @Test
    void looksFullyQualifiedIsFalseForAShortName() {
        assertFalse(ShaftRunConfigurationResolver.looksFullyQualified("CheckoutTest"));
    }

    @Test
    void looksFullyQualifiedIsFalseForNull() {
        assertFalse(ShaftRunConfigurationResolver.looksFullyQualified(null));
    }

    // ------------------------------------------------------------------
    // findByName (RunManager-free core)
    // ------------------------------------------------------------------

    @Test
    void findByNameReturnsTheSettingsWithAMatchingName() {
        List<RunnerAndConfigurationSettings> allSettings =
                List.of(fakeSettings("SignInTest"), fakeSettings("CheckoutTest"));

        Optional<RunnerAndConfigurationSettings> found =
                ShaftRunConfigurationResolver.findByName(allSettings, "CheckoutTest");

        assertTrue(found.isPresent());
        assertEquals("CheckoutTest", found.get().getName());
    }

    @Test
    void findByNameReturnsEmptyWhenNoNameMatches() {
        List<RunnerAndConfigurationSettings> allSettings = List.of(fakeSettings("SignInTest"));

        assertTrue(ShaftRunConfigurationResolver.findByName(allSettings, "CheckoutTest").isEmpty());
    }

    @Test
    void findByNameReturnsEmptyForAnEmptyList() {
        assertTrue(ShaftRunConfigurationResolver.findByName(List.of(), "CheckoutTest").isEmpty());
    }

    // ------------------------------------------------------------------
    // detectFrameworkKind
    // ------------------------------------------------------------------

    @Test
    void detectFrameworkKindReturnsTestNgForATestNgAnnotatedFixtureClass() {
        // "Fixture class" here is a list of per-method annotation FQNs, mirroring the same
        // PSI-free style ShaftTestMethodAnnotationsTest uses for the producers' own annotation
        // detection -- see this class's javadoc for why real PsiClass fixtures are out of reach.
        List<List<String>> methods = List.of(List.of(ShaftTestMethodAnnotations.TESTNG_TEST));

        Optional<ShaftRunConfigurationResolver.FrameworkKind> kind =
                ShaftRunConfigurationResolver.detectFrameworkKind(methods);

        assertEquals(Optional.of(ShaftRunConfigurationResolver.FrameworkKind.TESTNG), kind);
    }

    @Test
    void detectFrameworkKindReturnsJUnitForAJUnit4AnnotatedFixtureClass() {
        List<List<String>> methods = List.of(List.of(ShaftTestMethodAnnotations.JUNIT4_TEST));

        assertEquals(Optional.of(ShaftRunConfigurationResolver.FrameworkKind.JUNIT),
                ShaftRunConfigurationResolver.detectFrameworkKind(methods));
    }

    @Test
    void detectFrameworkKindReturnsJUnitForAJUnit5AnnotatedFixtureClass() {
        List<List<String>> methods = List.of(List.of(ShaftTestMethodAnnotations.JUNIT5_TEST));

        assertEquals(Optional.of(ShaftRunConfigurationResolver.FrameworkKind.JUNIT),
                ShaftRunConfigurationResolver.detectFrameworkKind(methods));
    }

    @Test
    void detectFrameworkKindPrefersTestNgWhenAClassMixesAnnotations() {
        List<List<String>> methods = List.of(
                List.of(ShaftTestMethodAnnotations.JUNIT5_TEST),
                List.of(ShaftTestMethodAnnotations.TESTNG_TEST));

        assertEquals(Optional.of(ShaftRunConfigurationResolver.FrameworkKind.TESTNG),
                ShaftRunConfigurationResolver.detectFrameworkKind(methods));
    }

    @Test
    void detectFrameworkKindReturnsEmptyWhenNoMethodHasARecognizedTestAnnotation() {
        List<List<String>> methods = List.of(List.of("java.lang.Override"));

        assertTrue(ShaftRunConfigurationResolver.detectFrameworkKind(methods).isEmpty());
    }

    @Test
    void detectFrameworkKindReturnsEmptyForNullOrEmptyInput() {
        assertTrue(ShaftRunConfigurationResolver.detectFrameworkKind(null).isEmpty());
        assertTrue(ShaftRunConfigurationResolver.detectFrameworkKind(List.of()).isEmpty());
    }

    // ------------------------------------------------------------------
    // resolvePsiClass -- only its null/blank short-circuit is reachable without a real Project
    // ------------------------------------------------------------------

    @Test
    void resolvePsiClassReturnsEmptyForANullTestIdWithoutTouchingTheProject() {
        // The null/blank guard returns before the project is ever dereferenced, so a null project
        // is safe here; every other branch needs a real Project/PSI index (see class javadoc).
        assertTrue(ShaftRunConfigurationResolver.resolvePsiClass(null, null).isEmpty());
    }

    @Test
    void resolvePsiClassReturnsEmptyForABlankTestIdWithoutTouchingTheProject() {
        assertTrue(ShaftRunConfigurationResolver.resolvePsiClass(null, "   ").isEmpty());
    }

    // ------------------------------------------------------------------
    // Fixture
    // ------------------------------------------------------------------

    /**
     * A minimal {@link RunnerAndConfigurationSettings} fake: {@code getName()} returns the given
     * name, every other member throws. Built with a dynamic proxy rather than a hand-implemented
     * class because the interface declares ~25 abstract members and {@code findByName} only ever
     * calls {@code getName()}.
     */
    private static RunnerAndConfigurationSettings fakeSettings(String name) {
        return (RunnerAndConfigurationSettings) Proxy.newProxyInstance(
                ShaftRunConfigurationResolverTest.class.getClassLoader(),
                new Class<?>[]{RunnerAndConfigurationSettings.class},
                (proxy, method, args) -> {
                    if ("getName".equals(method.getName())) {
                        return name;
                    }
                    throw new UnsupportedOperationException(
                            "fakeSettings only implements getName(), not " + method.getName());
                });
    }
}
