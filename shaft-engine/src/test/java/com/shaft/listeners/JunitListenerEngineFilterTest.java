package com.shaft.listeners;

import org.junit.platform.engine.TestExecutionResult;
import org.junit.platform.engine.UniqueId;
import org.junit.platform.engine.reporting.FileEntry;
import org.junit.platform.engine.reporting.ReportEntry;
import org.junit.platform.launcher.TestExecutionListener;
import org.junit.platform.launcher.TestIdentifier;
import org.junit.platform.launcher.TestPlan;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Set;

public class JunitListenerEngineFilterTest {
    private static final String TESTNG_METHOD = "[engine:testng]/[class:Foo]/[method:bar()]";
    private static final String NESTED_TESTNG_CLASS = "[engine:junit-platform-suite]/[suite:S]/[engine:testng]/[class:Foo]";
    private static final String JUPITER_METHOD = "[engine:junit-jupiter]/[class:Foo]/[method:bar()]";

    @Test
    public void isTestNgEngineFollowsTheLastEngineSegment() {
        Assert.assertTrue(JunitListener.isTestNgEngine(identifier(TESTNG_METHOD, true)));
        Assert.assertTrue(JunitListener.isTestNgEngine(identifier(NESTED_TESTNG_CLASS, false)));
        Assert.assertFalse(JunitListener.isTestNgEngine(identifier(JUPITER_METHOD, true)));
    }

    @Test
    public void hasNonTestNgTestsRejectsTestNgOnlyPlans() {
        TestIdentifier testNgTest = identifier(TESTNG_METHOD, true);
        TestPlan testNgOnly = plan(identifier("[engine:testng]", false), testNgTest);
        Assert.assertFalse(JunitListener.hasNonTestNgTests(testNgOnly));

        TestIdentifier jupiterTest = identifier(JUPITER_METHOD, true);
        TestPlan jupiterOnly = plan(identifier("[engine:junit-jupiter]", false), jupiterTest);
        Assert.assertTrue(JunitListener.hasNonTestNgTests(jupiterOnly));

        TestPlan mixed = Mockito.mock(TestPlan.class);
        TestIdentifier testNgRoot = identifier("[engine:testng]", false);
        TestIdentifier jupiterRoot = identifier("[engine:junit-jupiter]", false);
        Mockito.when(mixed.getRoots()).thenReturn(Set.of(testNgRoot, jupiterRoot));
        Mockito.when(mixed.getChildren(jupiterRoot)).thenReturn(Set.of(jupiterTest));
        Mockito.when(mixed.getChildren(jupiterTest)).thenReturn(Set.of());
        Assert.assertTrue(JunitListener.hasNonTestNgTests(mixed));
    }

    @Test
    public void hasNonTestNgTestsRejectsEmptyJupiterRootAndSuiteWrappedTestNg() {
        TestIdentifier emptyRoot = identifier("[engine:junit-jupiter]", false);
        TestPlan empty = Mockito.mock(TestPlan.class);
        Mockito.when(empty.getRoots()).thenReturn(Set.of(emptyRoot));
        Mockito.when(empty.getChildren(emptyRoot)).thenReturn(Set.of());
        Assert.assertFalse(JunitListener.hasNonTestNgTests(empty));

        TestIdentifier suiteRoot = identifier("[engine:junit-platform-suite]", false);
        TestIdentifier suiteNode = identifier("[engine:junit-platform-suite]/[suite:S]", false);
        TestIdentifier testNgEngine = identifier("[engine:junit-platform-suite]/[suite:S]/[engine:testng]", false);
        TestPlan wrapped = Mockito.mock(TestPlan.class);
        Mockito.when(wrapped.getRoots()).thenReturn(Set.of(suiteRoot));
        Mockito.when(wrapped.getChildren(suiteRoot)).thenReturn(Set.of(suiteNode));
        Mockito.when(wrapped.getChildren(suiteNode)).thenReturn(Set.of(testNgEngine));
        Mockito.when(wrapped.getChildren(testNgEngine)).thenReturn(Set.of());
        Assert.assertFalse(JunitListener.hasNonTestNgTests(wrapped));
    }

    @Test
    public void hasNonTestNgTestsAcceptsParameterizedLeafContainer() {
        TestPlan dynamic = jupiterContainerPlan();
        Assert.assertTrue(JunitListener.hasNonTestNgTests(dynamic));
    }

    @Test
    public void deactivatesAllureJunitPlatformUsesClassNameGlobs() {
        Assert.assertFalse(JunitListener.deactivatesAllureJunitPlatform(null));
        Assert.assertFalse(JunitListener.deactivatesAllureJunitPlatform(""));
        Assert.assertFalse(JunitListener.deactivatesAllureJunitPlatform("com.example.Foo*"));
        Assert.assertTrue(JunitListener.deactivatesAllureJunitPlatform("*"));
        Assert.assertTrue(JunitListener.deactivatesAllureJunitPlatform("io.qameta.allure.*"));
        Assert.assertTrue(JunitListener.deactivatesAllureJunitPlatform(
                "io.qameta.allure.junitplatform.AllureJunitPlatform"));
        Assert.assertTrue(JunitListener.deactivatesAllureJunitPlatform(
                "com.foo.Bar, io.qameta.allure.junitplatform.AllureJunitPlatform"));
    }

    @Test
    public void filteredAllureListenerDropsTestNgAndForwardsJupiter() {
        TestIdentifier testNgTest = identifier(TESTNG_METHOD, true);
        TestIdentifier nestedTestNg = identifier(NESTED_TESTNG_CLASS, false);
        TestIdentifier jupiterTest = identifier(JUPITER_METHOD, true);
        TestExecutionResult result = TestExecutionResult.successful();
        ReportEntry entry = ReportEntry.from("key", "value");
        FileEntry fileEntry = Mockito.mock(FileEntry.class);

        TestExecutionListener testNgDelegate = Mockito.mock(TestExecutionListener.class);
        TestExecutionListener testNgFiltered = JunitListener.createFilteredAllureListener(testNgDelegate);
        TestPlan testNgOnly = plan(identifier("[engine:testng]", false), testNgTest);
        testNgFiltered.testPlanExecutionStarted(testNgOnly);
        exercise(testNgFiltered, testNgTest, result, entry, fileEntry);
        exercise(testNgFiltered, nestedTestNg, result, entry, fileEntry);
        testNgFiltered.testPlanExecutionFinished(testNgOnly);
        Mockito.verifyNoInteractions(testNgDelegate);

        TestExecutionListener mixedDelegate = Mockito.mock(TestExecutionListener.class);
        TestExecutionListener mixedFiltered = JunitListener.createFilteredAllureListener(mixedDelegate);
        TestPlan mixed = Mockito.mock(TestPlan.class);
        TestIdentifier testNgRoot = identifier("[engine:testng]", false);
        TestIdentifier jupiterRoot = identifier("[engine:junit-jupiter]", false);
        Mockito.when(mixed.getRoots()).thenReturn(Set.of(testNgRoot, jupiterRoot));
        Mockito.when(mixed.getChildren(jupiterRoot)).thenReturn(Set.of(jupiterTest));
        Mockito.when(mixed.getChildren(jupiterTest)).thenReturn(Set.of());
        mixedFiltered.testPlanExecutionStarted(mixed);
        exercise(mixedFiltered, testNgTest, result, entry, fileEntry);
        exercise(mixedFiltered, nestedTestNg, result, entry, fileEntry);
        exercise(mixedFiltered, jupiterTest, result, entry, fileEntry);
        mixedFiltered.testPlanExecutionFinished(mixed);

        Mockito.verify(mixedDelegate).testPlanExecutionStarted(mixed);
        Mockito.verify(mixedDelegate).dynamicTestRegistered(jupiterTest);
        Mockito.verify(mixedDelegate).executionSkipped(jupiterTest, "skipped");
        Mockito.verify(mixedDelegate).executionStarted(jupiterTest);
        Mockito.verify(mixedDelegate).executionFinished(jupiterTest, result);
        Mockito.verify(mixedDelegate).reportingEntryPublished(jupiterTest, entry);
        Mockito.verify(mixedDelegate).fileEntryPublished(jupiterTest, fileEntry);
        Mockito.verify(mixedDelegate).testPlanExecutionFinished(mixed);
        Mockito.verify(mixedDelegate, Mockito.never()).dynamicTestRegistered(testNgTest);
        Mockito.verify(mixedDelegate, Mockito.never()).executionSkipped(testNgTest, "skipped");
        Mockito.verify(mixedDelegate, Mockito.never()).executionStarted(testNgTest);
        Mockito.verify(mixedDelegate, Mockito.never()).executionFinished(testNgTest, result);
        Mockito.verify(mixedDelegate, Mockito.never()).reportingEntryPublished(testNgTest, entry);
        Mockito.verify(mixedDelegate, Mockito.never()).fileEntryPublished(testNgTest, fileEntry);
        Mockito.verify(mixedDelegate, Mockito.never()).dynamicTestRegistered(nestedTestNg);
        Mockito.verify(mixedDelegate, Mockito.never()).executionStarted(nestedTestNg);
        Mockito.verifyNoMoreInteractions(mixedDelegate);
    }

    @Test
    public void filteredAllureListenerFinishesInactivePlansAndForwardsContainerOnlyDynamics() {
        TestExecutionListener delegate = Mockito.mock(TestExecutionListener.class);
        TestExecutionListener filtered = JunitListener.createFilteredAllureListener(delegate);
        TestPlan inactive = plan(identifier("[engine:testng]", false), identifier(TESTNG_METHOD, true));
        filtered.testPlanExecutionFinished(inactive);
        filtered.testPlanExecutionStarted(inactive);
        filtered.testPlanExecutionFinished(inactive);
        Mockito.verify(delegate, Mockito.never()).testPlanExecutionStarted(inactive);
        Mockito.verify(delegate, Mockito.never()).testPlanExecutionFinished(inactive);

        TestIdentifier template = identifier(
                "[engine:junit-jupiter]/[class:Foo]/[test-template:bar(java.lang.String)]", false);
        TestPlan dynamic = jupiterContainerPlan();
        filtered.testPlanExecutionStarted(dynamic);
        filtered.dynamicTestRegistered(template);
        Mockito.verify(delegate).testPlanExecutionStarted(dynamic);
        Mockito.verify(delegate).dynamicTestRegistered(template);
    }

    private static TestPlan jupiterContainerPlan() {
        TestIdentifier engine = identifier("[engine:junit-jupiter]", false);
        TestIdentifier testClass = identifier("[engine:junit-jupiter]/[class:Foo]", false);
        TestIdentifier template = identifier(
                "[engine:junit-jupiter]/[class:Foo]/[test-template:bar(java.lang.String)]", false);
        TestPlan dynamic = Mockito.mock(TestPlan.class);
        Mockito.when(dynamic.getRoots()).thenReturn(Set.of(engine));
        Mockito.when(dynamic.getChildren(engine)).thenReturn(Set.of(testClass));
        Mockito.when(dynamic.getChildren(testClass)).thenReturn(Set.of(template));
        Mockito.when(dynamic.getChildren(template)).thenReturn(Set.of());
        return dynamic;
    }

    private static void exercise(TestExecutionListener listener, TestIdentifier testIdentifier,
                                 TestExecutionResult result, ReportEntry entry, FileEntry fileEntry) {
        listener.dynamicTestRegistered(testIdentifier);
        listener.executionSkipped(testIdentifier, "skipped");
        listener.executionStarted(testIdentifier);
        listener.executionFinished(testIdentifier, result);
        listener.reportingEntryPublished(testIdentifier, entry);
        listener.fileEntryPublished(testIdentifier, fileEntry);
    }

    private static TestPlan plan(TestIdentifier root, TestIdentifier descendant) {
        TestPlan testPlan = Mockito.mock(TestPlan.class);
        Mockito.when(testPlan.getRoots()).thenReturn(Set.of(root));
        Mockito.when(testPlan.getChildren(root)).thenReturn(Set.of(descendant));
        Mockito.when(testPlan.getChildren(descendant)).thenReturn(Set.of());
        return testPlan;
    }

    private static TestIdentifier identifier(String uniqueId, boolean test) {
        TestIdentifier testIdentifier = Mockito.mock(TestIdentifier.class);
        Mockito.when(testIdentifier.getUniqueIdObject()).thenReturn(UniqueId.parse(uniqueId));
        Mockito.when(testIdentifier.getUniqueId()).thenReturn(uniqueId);
        Mockito.when(testIdentifier.isTest()).thenReturn(test);
        return testIdentifier;
    }
}
