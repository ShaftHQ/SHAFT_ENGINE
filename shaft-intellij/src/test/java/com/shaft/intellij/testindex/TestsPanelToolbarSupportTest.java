package com.shaft.intellij.testindex;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TestsPanelToolbarSupportTest {
    @TempDir
    Path temp;

    @Test
    void headlessRoundTripWritesCustomProperties() throws Exception {
        Path props = temp.resolve(TestsPanelToolbarSupport.CUSTOM_PROPERTIES_RELATIVE);
        Files.createDirectories(props.getParent());
        Files.writeString(props, "# keep\nother=1\n");

        assertFalse(TestsPanelToolbarSupport.isHeadless(temp));
        TestsPanelToolbarSupport.writeHeadless(temp, true);
        assertTrue(TestsPanelToolbarSupport.isHeadless(temp));
        assertTrue(Files.readString(props).contains("headlessExecution=true"));
        assertTrue(Files.readString(props).contains("other=1"));

        TestsPanelToolbarSupport.writeHeadless(temp, false);
        assertFalse(TestsPanelToolbarSupport.isHeadless(temp));
        assertTrue(TestsPanelToolbarSupport.showBrowserSelected(false));
        assertTrue(TestsPanelToolbarSupport.headlessFromShowBrowser(false));
    }

    @Test
    void runAllTargetsExpandsMethodsThenFallsBackToClass() {
        List<ShaftTestDiscovery.DiscoveredTestClass> discovered = List.of(
                new ShaftTestDiscovery.DiscoveredTestClass(
                        "com.example.SignInTest", "com.example", "SignInTest",
                        List.of("happy", "sad")),
                new ShaftTestDiscovery.DiscoveredTestClass(
                        "com.example.EmptyMethods", "com.example", "EmptyMethods", List.of()));

        List<TestsPanelToolbarSupport.RunTarget> targets =
                TestsPanelToolbarSupport.runAllTargets(discovered);

        assertEquals(List.of(
                new TestsPanelToolbarSupport.RunTarget("com.example.SignInTest", "happy"),
                new TestsPanelToolbarSupport.RunTarget("com.example.SignInTest", "sad"),
                new TestsPanelToolbarSupport.RunTarget("com.example.EmptyMethods", null)), targets);
    }

    @Test
    void rerunFailedTargetsParsesHashAndDottedMethodNames() {
        List<ShaftTestIndex.TestRowState> rows = List.of(
                new ShaftTestIndex.TestRowState(
                        "com.example.A#failing", ShaftTestIndex.Status.FAIL, 1L, 1),
                new ShaftTestIndex.TestRowState(
                        "com.example.B.method", ShaftTestIndex.Status.FAIL, 2L, 1),
                new ShaftTestIndex.TestRowState(
                        "com.example.C", ShaftTestIndex.Status.PASS, 3L, 0),
                new ShaftTestIndex.TestRowState(
                        "com.example.A#failing", ShaftTestIndex.Status.FAIL, 4L, 1));

        List<TestsPanelToolbarSupport.RunTarget> targets =
                TestsPanelToolbarSupport.rerunFailedTargets(rows);

        assertEquals(List.of(
                new TestsPanelToolbarSupport.RunTarget("com.example.A", "failing"),
                new TestsPanelToolbarSupport.RunTarget("com.example.B", "method")), targets);
    }

    @Test
    void listPropertyProfilesIncludesSubdirectoriesWithCustomProperties() throws Exception {
        Path propsDir = temp.resolve(TestsPanelToolbarSupport.PROPERTIES_DIR_RELATIVE);
        Files.createDirectories(propsDir.resolve("staging"));
        Files.writeString(propsDir.resolve("staging/custom.properties"), "x=1\n");
        Files.createDirectories(propsDir.resolve("empty"));

        List<String> profiles = TestsPanelToolbarSupport.listPropertyProfiles(temp);

        assertEquals(List.of(TestsPanelToolbarSupport.DEFAULT_PROFILE_LABEL, "staging"), profiles);
        assertEquals(
                propsDir.resolve("staging").toAbsolutePath().normalize().toString(),
                TestsPanelToolbarSupport.propertiesFolderPathForProfile(temp, "staging"));
        assertEquals("", TestsPanelToolbarSupport.propertiesFolderPathForProfile(
                temp, TestsPanelToolbarSupport.DEFAULT_PROFILE_LABEL));
    }

    @Test
    void browserRunMatrixPreservesCanonicalOrderAndEmptyMeansInherit() {
        assertEquals(List.of(""), TestsPanelToolbarSupport.browserRunMatrix(Set.of()));
        Set<String> selected = new LinkedHashSet<>();
        selected.add("firefox");
        selected.add("chrome");
        assertEquals(List.of("chrome", "firefox"),
                TestsPanelToolbarSupport.browserRunMatrix(selected));
    }

    @Test
    void shouldAutoOpenTraceRequiresOptInFailureAndTestRun() {
        assertAll(
                () -> assertFalse(TestsPanelToolbarSupport.shouldAutoOpenTrace(
                        false, 1, false, true)),
                () -> assertFalse(TestsPanelToolbarSupport.shouldAutoOpenTrace(
                        true, 0, false, true)),
                () -> assertFalse(TestsPanelToolbarSupport.shouldAutoOpenTrace(
                        true, 1, true, true)),
                () -> assertFalse(TestsPanelToolbarSupport.shouldAutoOpenTrace(
                        true, 1, false, false)),
                () -> assertTrue(TestsPanelToolbarSupport.shouldAutoOpenTrace(
                        true, 1, false, true)));
    }
}
