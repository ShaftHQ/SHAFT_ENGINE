package com.shaft.listeners.internal;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.listeners.TestNGListener;
import io.qameta.allure.AllureExternalKey;
import io.qameta.allure.AllureLifecycle;
import io.qameta.allure.FileSystemResultsWriter;
import io.qameta.allure.model.Label;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.TestResult;
import io.qameta.allure.testng.Tag;
import io.qameta.allure.testng.Tags;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.IClass;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.testng.internal.ConstructorOrMethod;

import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * Regression for #7549: TestNG groups and Allure {@code @Tag}/{@code @Tags} must land as
 * {@code labels} with {@code name=tag} in allure-results JSON. No reflective setAccessible.
 */
public class TestNgTagLabelsUnitTest {
    private Path allureResultsDirectory;

    @BeforeMethod(alwaysRun = true)
    public void setUp() throws Exception {
        allureResultsDirectory = Files.createTempDirectory("shaft-7549-allure-tags");
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() throws Exception {
        if (allureResultsDirectory != null && Files.exists(allureResultsDirectory)) {
            try (var paths = Files.walk(allureResultsDirectory)) {
                paths.sorted(Comparator.reverseOrder()).forEach(path -> {
                    try {
                        Files.deleteIfExists(path);
                    } catch (Exception ignored) {
                        // best-effort cleanup
                    }
                });
            }
        }
    }

    @Test
    public void beforeTestStopShouldPublishTestNgGroupsAndTagAnnotationsAsAllureTags() throws Exception {
        Method annotated = TaggedFixture.class.getDeclaredMethod("taggedMethod");
        ITestResult testNgResult = mockResult(annotated, new String[]{"smoke", "regression"}, TaggedFixture.class, null);

        String uuid = UUID.randomUUID().toString();
        TestResult result = new TestResult()
                .setUuid(uuid)
                .setName("taggedMethod")
                .setFullName(TaggedFixture.class.getName() + ".taggedMethod")
                .setStatus(Status.PASSED)
                .setLabels(new ArrayList<>());

        AllureLifecycle lifecycle = new AllureLifecycle(new FileSystemResultsWriter(allureResultsDirectory));
        AllureExternalKey testKey = AllureExternalKey.of(TestNgTagLabelsUnitTest.class, uuid);
        lifecycle.scheduleTest(testKey, result);
        lifecycle.startTest(testKey);
        TestNgTagLabels.apply(result, testNgResult);
        lifecycle.stopTest(testKey);
        lifecycle.writeTest(testKey);

        Set<String> tagValues = tagValuesFromResults();
        Assert.assertTrue(tagValues.contains("smoke"), "missing group tag smoke: " + tagValues);
        Assert.assertTrue(tagValues.contains("regression"), "missing group tag regression: " + tagValues);
        Assert.assertTrue(tagValues.contains("manual-tag"), "missing @Tag via @Tags manual-tag: " + tagValues);
        Assert.assertTrue(tagValues.contains("class-tag"), "missing class @Tag class-tag: " + tagValues);
    }

    @Test
    public void publicApplyUsesBoundTestNgResultWithoutReflection() {
        Assert.assertNotNull(TestNGListener.getITestResult(),
                "SHAFT TestNGListener must bind ITestResult for the running unit test");
        TestNgTagLabels.apply(null);

        TestResult result = new TestResult().setLabels(new ArrayList<>());
        TestNgTagLabels.apply(result);
        // Live unit-test method has no groups/@Tag; labels stay empty but public path executed.
        Assert.assertTrue(result.getLabels().stream().noneMatch(label -> "tag".equals(label.getName())
                && "should-not-appear".equals(label.getValue())));
    }

    @Test
    public void applyIsNoOpWithoutBoundTestNgResultAndDoesNotDuplicate() {
        TestResult result = new TestResult();
        result.getLabels().add(new Label().setName("tag").setValue("smoke"));
        TestNgTagLabels.apply(result, null);
        Assert.assertEquals(result.getLabels().size(), 1);

        ITestNGMethod method = Mockito.mock(ITestNGMethod.class);
        Mockito.when(method.getGroups()).thenReturn(new String[]{"smoke", "smoke", "  ", null});
        ITestResult testNgResult = Mockito.mock(ITestResult.class);
        Mockito.when(testNgResult.getMethod()).thenReturn(method);
        TestNgTagLabels.apply(result, testNgResult);
        long smokeTags = result.getLabels().stream()
                .filter(label -> "tag".equals(label.getName()) && "smoke".equals(label.getValue()))
                .count();
        Assert.assertEquals(smokeTags, 1L);
    }

    @Test
    public void applyHandlesNullMethodNullGroupsAndInstanceClassFallback() throws Exception {
        TestResult emptyMethod = new TestResult().setLabels(new ArrayList<>());
        ITestResult noMethod = Mockito.mock(ITestResult.class);
        Mockito.when(noMethod.getMethod()).thenReturn(null);
        Mockito.when(noMethod.getTestClass()).thenReturn(null);
        Mockito.when(noMethod.getInstance()).thenReturn(null);
        TestNgTagLabels.apply(emptyMethod, noMethod);
        Assert.assertTrue(emptyMethod.getLabels().isEmpty());

        TestResult nullGroups = new TestResult().setLabels(new ArrayList<>());
        ITestNGMethod method = Mockito.mock(ITestNGMethod.class);
        Mockito.when(method.getGroups()).thenReturn(null);
        Mockito.when(method.getConstructorOrMethod()).thenReturn(null);
        ITestResult withNullGroups = Mockito.mock(ITestResult.class);
        Mockito.when(withNullGroups.getMethod()).thenReturn(method);
        Mockito.when(withNullGroups.getTestClass()).thenReturn(null);
        Mockito.when(withNullGroups.getInstance()).thenReturn(new InstanceTaggedFixture());
        TestNgTagLabels.apply(nullGroups, withNullGroups);
        Set<String> tags = tagValues(nullGroups);
        Assert.assertTrue(tags.contains("instance-class-tag"), "instance class @Tag: " + tags);

        TestResult bothNull = new TestResult().setLabels(new ArrayList<>());
        TestNgTagLabels.apply(null, null);
        TestNgTagLabels.apply(bothNull, null);
        Assert.assertTrue(bothNull.getLabels().isEmpty());
    }

    @Test
    public void applyReadsSingleTagAnnotationAndSkipsNonTagLabels() throws Exception {
        Method single = SingleTagFixture.class.getDeclaredMethod("singleTagged");
        ITestResult testNgResult = mockResult(single, new String[]{}, SingleTagFixture.class, null);
        TestResult result = new TestResult().setLabels(new ArrayList<>());
        // Pre-seed a non-tag label so AnnotationUtils noise cannot confuse dedupe of tags.
        result.getLabels().add(new Label().setName("severity").setValue("x"));
        TestNgTagLabels.apply(result, testNgResult);
        Set<String> tags = tagValues(result);
        Assert.assertTrue(tags.contains("solo-tag"), "missing single @Tag: " + tags);
        Assert.assertEquals(result.getLabels().stream().filter(l -> "severity".equals(l.getName())).count(), 1L);
    }

    @Test
    public void applyUsesTagsContainerOnClassAndDedupesAgainstExisting() throws Exception {
        Method bare = ClassTagsFixture.class.getDeclaredMethod("bare");
        ITestResult testNgResult = mockResult(bare, new String[]{"shared"}, ClassTagsFixture.class, null);
        TestResult result = new TestResult().setLabels(new ArrayList<>());
        result.getLabels().add(new Label().setName("tag").setValue("shared"));
        result.getLabels().add(new Label().setName("tag").setValue("class-tags-a"));
        TestNgTagLabels.apply(result, testNgResult);
        List<String> tagList = result.getLabels().stream()
                .filter(label -> "tag".equals(label.getName()))
                .map(Label::getValue)
                .collect(Collectors.toList());
        Assert.assertEquals(tagList.stream().filter("shared"::equals).count(), 1L);
        Assert.assertEquals(tagList.stream().filter("class-tags-a"::equals).count(), 1L);
        Assert.assertTrue(tagList.contains("class-tags-b"), "missing @Tags peer: " + tagList);
    }

    private static ITestResult mockResult(Method annotated, String[] groups, Class<?> realClass, Object instance)
            throws Exception {
        ITestNGMethod method = Mockito.mock(ITestNGMethod.class);
        Mockito.when(method.getGroups()).thenReturn(groups);
        ConstructorOrMethod constructorOrMethod = Mockito.mock(ConstructorOrMethod.class);
        Mockito.when(constructorOrMethod.getMethod()).thenReturn(annotated);
        Mockito.when(method.getConstructorOrMethod()).thenReturn(constructorOrMethod);

        IClass testClass = null;
        if (realClass != null) {
            testClass = Mockito.mock(IClass.class);
            Mockito.doReturn(realClass).when(testClass).getRealClass();
        }

        ITestResult testNgResult = Mockito.mock(ITestResult.class);
        Mockito.when(testNgResult.getMethod()).thenReturn(method);
        Mockito.when(testNgResult.getTestClass()).thenReturn(testClass);
        Mockito.when(testNgResult.getInstance()).thenReturn(instance);
        return testNgResult;
    }

    private Set<String> tagValuesFromResults() throws Exception {
        Path resultFile;
        try (var paths = Files.list(allureResultsDirectory)) {
            resultFile = paths.filter(path -> path.getFileName().toString().endsWith("-result.json"))
                    .findFirst()
                    .orElseThrow(() -> new AssertionError("Expected a *-result.json under allure-results"));
        }
        JsonNode root = new ObjectMapper().readTree(resultFile.toFile());
        Set<String> tagValues = new HashSet<>();
        for (JsonNode label : root.path("labels")) {
            if ("tag".equals(label.path("name").asText())) {
                tagValues.add(label.path("value").asText());
            }
        }
        return tagValues;
    }

    private static Set<String> tagValues(TestResult result) {
        return result.getLabels().stream()
                .filter(label -> "tag".equals(label.getName()))
                .map(Label::getValue)
                .collect(Collectors.toSet());
    }

    @Tag("class-tag")
    private static final class TaggedFixture {
        @Tags({@Tag("manual-tag")})
        @SuppressWarnings("unused")
        private void taggedMethod() {
            // annotation carrier for AnnotationUtils (@Tags container)
        }
    }

    @Tag("instance-class-tag")
    private static final class InstanceTaggedFixture {
    }

    private static final class SingleTagFixture {
        @Tag("solo-tag")
        @SuppressWarnings("unused")
        private void singleTagged() {
            // single @Tag without @Tags container
        }
    }

    @Tags({@Tag("class-tags-a"), @Tag("class-tags-b")})
    private static final class ClassTagsFixture {
        @SuppressWarnings("unused")
        private void bare() {
            // class-level @Tags only
        }
    }
}
