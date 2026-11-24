package com.shaft.listeners;

import com.shaft.listeners.internal.ExecutionCountsTracker;
import com.shaft.listeners.internal.ExecutionLifecycleHelper;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.internal.ValidationsHelper;

import org.junit.platform.engine.TestExecutionResult;
import org.junit.platform.engine.UniqueId;
import org.junit.platform.engine.reporting.FileEntry;
import org.junit.platform.engine.reporting.ReportEntry;
import org.junit.platform.launcher.TestExecutionListener;
import org.junit.platform.launcher.TestIdentifier;
import org.junit.platform.launcher.TestPlan;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public class JunitListenerEngineFilterTest {
    private static final String ALLURE_LISTENER_DEACTIVATE = "junit.platform.execution.listeners.deactivate";
    private static final String ALLURE_JUNIT_PLATFORM_LISTENER = "io.qameta.allure.junitplatform.AllureJunitPlatform";
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
        Assert.assertFalse(JunitListener.deactivatesAllureJunitPlatform("*AllureJunitPlatform*"));
        Assert.assertFalse(JunitListener.deactivatesAllureJunitPlatform(
                "io.qameta.allure.junitplatform.AllureJunitPlatform*"));
        Assert.assertFalse(JunitListener.deactivatesAllureJunitPlatform(
                "io.qameta.allure.junitplatform*.AllureJunitPlatform"));
        Assert.assertTrue(JunitListener.deactivatesAllureJunitPlatform("*"));
        Assert.assertTrue(JunitListener.deactivatesAllureJunitPlatform("io.qameta.allure.*"));
        Assert.assertTrue(JunitListener.deactivatesAllureJunitPlatform("*AllureJunitPlatform"));
        Assert.assertTrue(JunitListener.deactivatesAllureJunitPlatform(
                "io.qameta.allure.junitplatform.AllureJunitPlatform"));
        Assert.assertTrue(JunitListener.deactivatesAllureJunitPlatform(
                "com.foo.Bar, io.qameta.allure.junitplatform.AllureJunitPlatform"));
        Assert.assertFalse(JunitListener.deactivatesAllureJunitPlatform(","));
        Assert.assertFalse(JunitListener.deactivatesAllureJunitPlatform("com.example.Foo, ,com.example.Bar"));
        Assert.assertFalse(JunitListener.deactivatesAllureJunitPlatform(
                "io.qameta.allure.junitplatform.AllureJunitPlatform("));
    }

    @Test
    public void dotInDeactivatePatternMatchesInnerClassSeparator() {
        Assert.assertTrue(JunitListener.matchesDeactivatePattern(
                "io.qameta.allure.junitplatform.AllureJunitPlatform",
                "io.qameta.allure.junitplatform$AllureJunitPlatform"));
        Assert.assertFalse(JunitListener.matchesDeactivatePattern("",
                "io.qameta.allure.junitplatform.AllureJunitPlatform"));
    }

    @Test
    public void hasNonTestNgTestsAcceptsTestRootWithoutEngineSegment() {
        TestIdentifier testRoot = identifier("[class:Foo]", true);
        Assert.assertFalse(JunitListener.isTestNgEngine(testRoot));
        TestPlan plan = Mockito.mock(TestPlan.class);
        Mockito.when(plan.getRoots()).thenReturn(Set.of(testRoot));
        Assert.assertTrue(JunitListener.hasNonTestNgTests(plan));
    }

    @Test
    public void publishAllureDeactivationPropertyCoversBlankAppendAndActiveProperty() {
        String original = System.getProperty(ALLURE_LISTENER_DEACTIVATE);
        ClassLoader originalLoader = Thread.currentThread().getContextClassLoader();
        try {
            System.setProperty(ALLURE_LISTENER_DEACTIVATE, ALLURE_JUNIT_PLATFORM_LISTENER);
            JunitListener.publishAllureDeactivationProperty();
            Assert.assertEquals(System.getProperty(ALLURE_LISTENER_DEACTIVATE), ALLURE_JUNIT_PLATFORM_LISTENER);
            Assert.assertTrue(JunitListener.isAllureJunitPlatformDeactivated());

            System.setProperty(ALLURE_LISTENER_DEACTIVATE, "com.example.Other");
            JunitListener.publishAllureDeactivationProperty();
            Assert.assertEquals(System.getProperty(ALLURE_LISTENER_DEACTIVATE),
                    "com.example.Other," + ALLURE_JUNIT_PLATFORM_LISTENER);

            System.setProperty(ALLURE_LISTENER_DEACTIVATE, "   ");
            JunitListener.publishAllureDeactivationProperty();
            Assert.assertEquals(System.getProperty(ALLURE_LISTENER_DEACTIVATE), ALLURE_JUNIT_PLATFORM_LISTENER);

            System.clearProperty(ALLURE_LISTENER_DEACTIVATE);
            Assert.assertTrue(JunitListener.isAllureJunitPlatformDeactivated());
            JunitListener.publishAllureDeactivationProperty();
            Assert.assertEquals(System.getProperty(ALLURE_LISTENER_DEACTIVATE), ALLURE_JUNIT_PLATFORM_LISTENER);

            Thread.currentThread().setContextClassLoader(new EmptyResourceLoader());
            Assert.assertTrue(JunitListener.classpathDeactivatesAllureJunitPlatform());
        } finally {
            Thread.currentThread().setContextClassLoader(originalLoader);
            restoreDeactivateProperty(original);
        }
    }

    @Test
    public void resourcesDeactivateAllureRejectsNullEmptyAndUnreadableLoaders() {
        Assert.assertFalse(JunitListener.resourcesDeactivateAllure(null));
        Assert.assertFalse(JunitListener.resourcesDeactivateAllure(new EmptyResourceLoader()));
        Assert.assertFalse(JunitListener.resourcesDeactivateAllure(new BrokenResourceLoader()));
    }

    @Test
    public void optionalAllureListenerStaysEmptyWhenClasspathDoesNotDeactivate() throws Exception {
        List<Path> propertyFiles = junitPlatformPropertyFiles();
        Assert.assertFalse(propertyFiles.isEmpty());
        List<byte[]> originals = new ArrayList<>();
        String original = System.getProperty(ALLURE_LISTENER_DEACTIVATE);
        try {
            for (Path propertyFile : propertyFiles) {
                originals.add(Files.readAllBytes(propertyFile));
                Files.writeString(propertyFile, "junit.jupiter.extensions.autodetection.enabled=true\n");
            }
            System.setProperty(ALLURE_LISTENER_DEACTIVATE, "com.example.Other");
            JunitListener.publishAllureDeactivationProperty();
            Assert.assertEquals(System.getProperty(ALLURE_LISTENER_DEACTIVATE), "com.example.Other");

            System.clearProperty(ALLURE_LISTENER_DEACTIVATE);
            Assert.assertFalse(JunitListener.isAllureJunitPlatformDeactivated());
            Assert.assertEquals(JunitListener.createOptionalFilteredAllureListener(), Optional.empty());
        } finally {
            for (int index = 0; index < originals.size(); index++) {
                Files.write(propertyFiles.get(index), originals.get(index));
            }
            restoreDeactivateProperty(original);
        }
    }

    @Test
    public void shaftListenerSkipsTestNgOnlyPlansAndForwardsReportingEvents() {
        TestExecutionListener delegate = Mockito.mock(TestExecutionListener.class);
        TestExecutionListener shaft = newShaftListener(delegate);
        TestIdentifier testNgTest = identifier(TESTNG_METHOD, true);
        TestIdentifier jupiterContainer = identifier("[engine:junit-jupiter]/[class:Foo]", false);
        TestPlan testNgOnly = plan(identifier("[engine:testng]", false), testNgTest);
        TestExecutionResult result = TestExecutionResult.successful();
        ReportEntry entry = ReportEntry.from("key", "value");
        FileEntry fileEntry = Mockito.mock(FileEntry.class);

        shaft.testPlanExecutionStarted(testNgOnly);
        shaft.dynamicTestRegistered(testNgTest);
        shaft.executionSkipped(testNgTest, "skipped");
        shaft.executionStarted(testNgTest);
        shaft.executionFinished(testNgTest, result);
        shaft.reportingEntryPublished(testNgTest, entry);
        shaft.fileEntryPublished(testNgTest, fileEntry);
        shaft.testPlanExecutionFinished(testNgOnly);

        setActive(shaft, true);
        shaft.executionSkipped(testNgTest, "skipped");
        shaft.executionSkipped(jupiterContainer, "skipped");
        shaft.executionFinished(jupiterContainer, result);

        Mockito.verify(delegate).testPlanExecutionStarted(testNgOnly);
        Mockito.verify(delegate).dynamicTestRegistered(testNgTest);
        Mockito.verify(delegate, Mockito.times(2)).executionSkipped(testNgTest, "skipped");
        Mockito.verify(delegate).executionSkipped(jupiterContainer, "skipped");
        Mockito.verify(delegate).executionStarted(testNgTest);
        Mockito.verify(delegate).executionFinished(testNgTest, result);
        Mockito.verify(delegate).executionFinished(jupiterContainer, result);
        Mockito.verify(delegate).reportingEntryPublished(testNgTest, entry);
        Mockito.verify(delegate).fileEntryPublished(testNgTest, fileEntry);
        Mockito.verify(delegate).testPlanExecutionFinished(testNgOnly);
        Mockito.verifyNoMoreInteractions(delegate);
    }

    @Test
    public void shaftListenerRecordsVerificationFailureForSuccessfulTest() {
        TestExecutionListener delegate = Mockito.mock(TestExecutionListener.class);
        TestExecutionListener shaft = newShaftListener(delegate);
        TestIdentifier jupiterTest = reportingIdentifier(JUPITER_METHOD);
        try (OutcomeSnapshot ignored = OutcomeSnapshot.capture()) {
            setActive(shaft, true);
            ValidationsHelper.recordVerificationFailure("soft failures");
            try (MockedStatic<ExecutionLifecycleHelper> lifecycle = Mockito.mockStatic(ExecutionLifecycleHelper.class);
                 MockedStatic<ReportManagerHelper> reports = Mockito.mockStatic(ReportManagerHelper.class)) {
                shaft.executionFinished(jupiterTest, TestExecutionResult.successful());
                reports.verify(() -> ReportManagerHelper.setDiscreteLogging(Mockito.anyBoolean()));
                lifecycle.verify(() -> ExecutionLifecycleHelper.attachTestArtifacts(Mockito.any(TestExecutionInfo.class)));
            }
            Mockito.verify(delegate).executionFinished(jupiterTest, TestExecutionResult.successful());
            Assert.assertFalse(JunitListener.getIsLastFinishedTestOK());
        } finally {
            ValidationsHelper.resetVerificationStateAfterFailing();
        }
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

    private static TestIdentifier reportingIdentifier(String uniqueId) {
        TestIdentifier testIdentifier = identifier(uniqueId, true);
        Mockito.when(testIdentifier.getDisplayName()).thenReturn("bar()");
        Mockito.when(testIdentifier.getLegacyReportingName()).thenReturn("bar()");
        Mockito.when(testIdentifier.getSource()).thenReturn(Optional.empty());
        return testIdentifier;
    }

    private static TestExecutionListener newShaftListener(TestExecutionListener delegate) {
        return JunitListener.createShaftExecutionListenerForTest(Optional.of(delegate));
    }

    private static void setActive(TestExecutionListener shaft, boolean active) {
        JunitListener.setShaftListenerActiveForTest(shaft, active);
    }

    private static void restoreDeactivateProperty(String original) {
        if (original == null) {
            System.clearProperty(ALLURE_LISTENER_DEACTIVATE);
        } else {
            System.setProperty(ALLURE_LISTENER_DEACTIVATE, original);
        }
    }

    private static List<Path> junitPlatformPropertyFiles() throws IOException {
        Enumeration<URL> resources = JunitListener.class.getClassLoader().getResources("junit-platform.properties");
        List<Path> paths = new ArrayList<>();
        while (resources.hasMoreElements()) {
            URL resource = resources.nextElement();
            if (!"file".equals(resource.getProtocol())) {
                continue;
            }
            try {
                paths.add(Path.of(resource.toURI()));
            } catch (URISyntaxException exception) {
                throw new IOException("Unable to read junit-platform.properties.", exception);
            }
        }
        return paths;
    }

    private static final class EmptyResourceLoader extends ClassLoader {
        @Override
        public Enumeration<URL> getResources(String name) {
            return Collections.emptyEnumeration();
        }
    }

    private static final class BrokenResourceLoader extends ClassLoader {
        @Override
        public Enumeration<URL> getResources(String name) throws IOException {
            throw new IOException(name);
        }
    }

    private static final class OutcomeSnapshot implements AutoCloseable {
        private final Boolean lastFinishedOk;
        private final List<TestIdentifier> passed;
        private final List<TestIdentifier> failed;
        private final List<TestIdentifier> skipped;
        private final ExecutionCountsTracker countsTracker;

        private OutcomeSnapshot(Boolean lastFinishedOk, List<TestIdentifier> passed, List<TestIdentifier> failed,
                                List<TestIdentifier> skipped, ExecutionCountsTracker countsTracker) {
            this.lastFinishedOk = lastFinishedOk;
            this.passed = passed;
            this.failed = failed;
            this.skipped = skipped;
            this.countsTracker = countsTracker;
        }

        private static OutcomeSnapshot capture() {
            Boolean lastFinishedOk = JunitListener.isLastFinishedTestOK;
            List<TestIdentifier> passed = List.copyOf(JunitListener.passedTests);
            List<TestIdentifier> failed = List.copyOf(JunitListener.failedTests);
            List<TestIdentifier> skipped = List.copyOf(JunitListener.skippedTests);
            ExecutionCountsTracker previous = JunitListener.countsTracker;
            JunitListener.countsTracker = new ExecutionCountsTracker();
            return new OutcomeSnapshot(lastFinishedOk, passed, failed, skipped, previous);
        }

        @Override
        public void close() {
            replaceIdentifiers(JunitListener.passedTests, passed);
            replaceIdentifiers(JunitListener.failedTests, failed);
            replaceIdentifiers(JunitListener.skippedTests, skipped);
            JunitListener.countsTracker = countsTracker;
            JunitListener.isLastFinishedTestOK = lastFinishedOk;
        }
    }

    private static void replaceIdentifiers(List<TestIdentifier> live, List<TestIdentifier> values) {
        synchronized (live) {
            live.clear();
            live.addAll(values);
        }
    }
}
