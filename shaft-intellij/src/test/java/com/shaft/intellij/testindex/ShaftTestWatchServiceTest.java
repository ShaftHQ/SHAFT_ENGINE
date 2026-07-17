package com.shaft.intellij.testindex;

import com.intellij.execution.configurations.RunConfiguration;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Proxy;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers {@link ShaftTestWatchService}'s directly-testable pure logic: the {@code src/test/} path
 * heuristic ({@code isTestSourceChange}), and the #3641 changed-file-vs-last-run-scope correlation
 * ({@code correlate}, {@code simpleName}, {@code extractTargetClassName}).
 * <p>
 * The PSI-facing wiring around that correlation ({@code resolveChangedClassFqn}, which needs a real
 * {@code VirtualFile}/{@code PsiJavaFile}) and the notification/rerun glue in {@code rerunLastTest}
 * (which needs a live {@code NotificationGroupManager}/{@code Application}) are not covered here:
 * like {@code ShaftRunConfigurationResolverTest}'s own disclaimer, this module has no
 * {@code BasePlatformTestCase} fixture and adding one is a build-dependency change outside this
 * class's scope. {@code extractTargetClassName}'s unrecognized-configuration-type branch is
 * reachable with a plain dynamic proxy (below) since it never invokes a method on the configuration
 * -- only its two {@code instanceof} checks -- but the real {@code TestNGConfiguration}/
 * {@code JUnitConfiguration} branches need real platform objects and are not reachable this way.
 */
class ShaftTestWatchServiceTest {
    private static final String PROJECT_ROOT = "C:/dev/shop-tests";

    // ------------------------------------------------------------------
    // isTestSourceChange (unchanged by #3641; still covered here)
    // ------------------------------------------------------------------

    @Test
    void recognizesTestSourceFileUnderProject() {
        assertTrue(ShaftTestWatchService.isTestSourceChange(
                "C:/dev/shop-tests/src/test/java/SignInTest.java", PROJECT_ROOT));
    }

    @Test
    void normalizesWindowsBackslashesBeforeMatching() {
        assertTrue(ShaftTestWatchService.isTestSourceChange(
                "C:\\dev\\shop-tests\\src\\test\\java\\SignInTest.java", "C:\\dev\\shop-tests"));
    }

    @Test
    void ignoresMainSourceFiles() {
        assertFalse(ShaftTestWatchService.isTestSourceChange(
                "C:/dev/shop-tests/src/main/java/App.java", PROJECT_ROOT));
    }

    @Test
    void ignoresFilesOutsideTheProject() {
        assertFalse(ShaftTestWatchService.isTestSourceChange(
                "C:/other/src/test/java/SignInTest.java", PROJECT_ROOT));
    }

    @Test
    void returnsFalseForNullInputs() {
        assertFalse(ShaftTestWatchService.isTestSourceChange(null, PROJECT_ROOT));
        assertFalse(ShaftTestWatchService.isTestSourceChange(
                "C:/dev/shop-tests/src/test/java/SignInTest.java", null));
    }

    @Test
    void ignoresTargetOrBuildOutputEvenIfPathContainsTestSubstring() {
        assertFalse(ShaftTestWatchService.isTestSourceChange(
                "C:/dev/shop-tests/target/test-classes/SignInTest.class", PROJECT_ROOT));
    }

    // ------------------------------------------------------------------
    // simpleName
    // ------------------------------------------------------------------

    @Test
    void simpleNameStripsThePackagePrefix() {
        assertEquals("SignInTest", ShaftTestWatchService.simpleName("com.example.SignInTest"));
    }

    @Test
    void simpleNameReturnsTheWholeStringWhenThereIsNoPackage() {
        assertEquals("SignInTest", ShaftTestWatchService.simpleName("SignInTest"));
    }

    // ------------------------------------------------------------------
    // correlate -- the #3641 changed-file-vs-last-run-scope decision
    // ------------------------------------------------------------------

    @Test
    void correlateMatchesOnAnExactFqn() {
        // (a) changed file matches the last run's scope exactly: proceed with the rerun.
        assertEquals(ShaftTestWatchService.CorrelationResult.MATCH,
                ShaftTestWatchService.correlate("com.example.SignInTest", "com.example.SignInTest"));
    }

    @Test
    void correlateMatchesOnSimpleNameWhenFqnsDifferButSimpleNamesMatch() {
        // Mirrors ShaftRunConfigurationResolver's own short-name-ambiguity precedent: an FQN mismatch
        // still counts as a match when the simple names agree.
        assertEquals(ShaftTestWatchService.CorrelationResult.MATCH,
                ShaftTestWatchService.correlate("com.example.a.SignInTest", "com.example.b.SignInTest"));
    }

    @Test
    void correlateMismatchesWhenSimpleNamesClearlyDiffer() {
        // (b) changed file does NOT match the last run's scope: do not rerun.
        assertEquals(ShaftTestWatchService.CorrelationResult.MISMATCH,
                ShaftTestWatchService.correlate("com.example.CheckoutTest", "com.example.SignInTest"));
    }

    @Test
    void correlateIsIndeterminateWhenTheChangedClassIsUnresolvable() {
        // (c) changed file is a non-class test resource (no resolvable PSI class): fall back to
        // today's unconditional-rerun behavior rather than blocking on it.
        assertEquals(ShaftTestWatchService.CorrelationResult.INDETERMINATE,
                ShaftTestWatchService.correlate(null, "com.example.SignInTest"));
    }

    @Test
    void correlateIsIndeterminateWhenTheChangedClassIsBlank() {
        assertEquals(ShaftTestWatchService.CorrelationResult.INDETERMINATE,
                ShaftTestWatchService.correlate("   ", "com.example.SignInTest"));
    }

    @Test
    void correlateIsIndeterminateWhenTheLastRunClassCannotBeDetermined() {
        // The last run's target class could not be introspected (unrecognized configuration type, or
        // an empty getMainClassName()): fall back rather than invent a new failure mode.
        assertEquals(ShaftTestWatchService.CorrelationResult.INDETERMINATE,
                ShaftTestWatchService.correlate("com.example.SignInTest", null));
        assertEquals(ShaftTestWatchService.CorrelationResult.INDETERMINATE,
                ShaftTestWatchService.correlate("com.example.SignInTest", ""));
    }

    // ------------------------------------------------------------------
    // extractTargetClassName -- only its unrecognized-configuration-type branch is reachable
    // without a real TestNGConfiguration/JUnitConfiguration (see class javadoc)
    // ------------------------------------------------------------------

    @Test
    void extractTargetClassNameReturnsNullForAnUnrecognizedConfigurationType() {
        RunConfiguration configuration = fakeUnrecognizedConfiguration();

        assertNull(ShaftTestWatchService.extractTargetClassName(configuration));
    }

    /**
     * A {@link RunConfiguration} that is neither a {@code TestNGConfiguration} nor a
     * {@code JUnitConfiguration}. {@code extractTargetClassName} only ever performs {@code instanceof}
     * checks against it, never invoking a method, so the proxy's handler is never reached.
     */
    private static RunConfiguration fakeUnrecognizedConfiguration() {
        return (RunConfiguration) Proxy.newProxyInstance(
                ShaftTestWatchServiceTest.class.getClassLoader(),
                new Class<?>[]{RunConfiguration.class},
                (proxy, method, args) -> {
                    throw new UnsupportedOperationException(
                            "fakeUnrecognizedConfiguration should never have a method invoked on it, "
                                    + "only instanceof-checked; got " + method.getName());
                });
    }
}
