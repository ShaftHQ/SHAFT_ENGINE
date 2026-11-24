package com.shaft.listeners;

import org.junit.platform.engine.TestExecutionResult;
import org.junit.platform.engine.UniqueId;
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
        Mockito.when(mixed.getDescendants(jupiterRoot)).thenReturn(Set.of(jupiterTest));
        Assert.assertTrue(JunitListener.hasNonTestNgTests(mixed));
    }

    @Test
    public void filteredAllureListenerDropsTestNgAndForwardsJupiter() {
        TestIdentifier testNgTest = identifier(TESTNG_METHOD, true);
        TestIdentifier nestedTestNg = identifier(NESTED_TESTNG_CLASS, false);
        TestIdentifier jupiterTest = identifier(JUPITER_METHOD, true);
        TestExecutionResult result = TestExecutionResult.successful();
        ReportEntry entry = ReportEntry.from("key", "value");

        TestExecutionListener testNgDelegate = Mockito.mock(TestExecutionListener.class);
        TestExecutionListener testNgFiltered = JunitListener.createFilteredAllureListener(testNgDelegate);
        TestPlan testNgOnly = plan(identifier("[engine:testng]", false), testNgTest);
        testNgFiltered.testPlanExecutionStarted(testNgOnly);
        exercise(testNgFiltered, testNgTest, result, entry);
        exercise(testNgFiltered, nestedTestNg, result, entry);
        testNgFiltered.testPlanExecutionFinished(testNgOnly);
        Mockito.verifyNoInteractions(testNgDelegate);

        TestExecutionListener mixedDelegate = Mockito.mock(TestExecutionListener.class);
        TestExecutionListener mixedFiltered = JunitListener.createFilteredAllureListener(mixedDelegate);
        TestPlan mixed = Mockito.mock(TestPlan.class);
        TestIdentifier testNgRoot = identifier("[engine:testng]", false);
        TestIdentifier jupiterRoot = identifier("[engine:junit-jupiter]", false);
        Mockito.when(mixed.getRoots()).thenReturn(Set.of(testNgRoot, jupiterRoot));
        Mockito.when(mixed.getDescendants(jupiterRoot)).thenReturn(Set.of(jupiterTest));
        mixedFiltered.testPlanExecutionStarted(mixed);
        exercise(mixedFiltered, testNgTest, result, entry);
        exercise(mixedFiltered, nestedTestNg, result, entry);
        exercise(mixedFiltered, jupiterTest, result, entry);
        mixedFiltered.testPlanExecutionFinished(mixed);

        Mockito.verify(mixedDelegate).testPlanExecutionStarted(mixed);
        Mockito.verify(mixedDelegate).dynamicTestRegistered(jupiterTest);
        Mockito.verify(mixedDelegate).executionSkipped(jupiterTest, "skipped");
        Mockito.verify(mixedDelegate).executionStarted(jupiterTest);
        Mockito.verify(mixedDelegate).executionFinished(jupiterTest, result);
        Mockito.verify(mixedDelegate).reportingEntryPublished(jupiterTest, entry);
        Mockito.verify(mixedDelegate).fileEntryPublished(jupiterTest, null);
        Mockito.verify(mixedDelegate).testPlanExecutionFinished(mixed);
        Mockito.verify(mixedDelegate, Mockito.never()).dynamicTestRegistered(testNgTest);
        Mockito.verify(mixedDelegate, Mockito.never()).executionSkipped(testNgTest, "skipped");
        Mockito.verify(mixedDelegate, Mockito.never()).executionStarted(testNgTest);
        Mockito.verify(mixedDelegate, Mockito.never()).executionFinished(testNgTest, result);
        Mockito.verify(mixedDelegate, Mockito.never()).reportingEntryPublished(testNgTest, entry);
        Mockito.verify(mixedDelegate, Mockito.never()).fileEntryPublished(testNgTest, null);
        Mockito.verify(mixedDelegate, Mockito.never()).dynamicTestRegistered(nestedTestNg);
        Mockito.verify(mixedDelegate, Mockito.never()).executionStarted(nestedTestNg);
        Mockito.verifyNoMoreInteractions(mixedDelegate);
    }

    private static void exercise(TestExecutionListener listener, TestIdentifier testIdentifier,
                                 TestExecutionResult result, ReportEntry entry) {
        listener.dynamicTestRegistered(testIdentifier);
        listener.executionSkipped(testIdentifier, "skipped");
        listener.executionStarted(testIdentifier);
        listener.executionFinished(testIdentifier, result);
        listener.reportingEntryPublished(testIdentifier, entry);
        listener.fileEntryPublished(testIdentifier, null);
    }

    private static TestPlan plan(TestIdentifier root, TestIdentifier descendant) {
        TestPlan testPlan = Mockito.mock(TestPlan.class);
        Mockito.when(testPlan.getRoots()).thenReturn(Set.of(root));
        Mockito.when(testPlan.getDescendants(root)).thenReturn(Set.of(descendant));
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
