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

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

/**
 * Regression for #7549: TestNG groups and Allure {@code @Tag} must land as
 * {@code labels} with {@code name=tag} in allure-results JSON.
 */
public class TestNgTagLabelsUnitTest {
    private Path allureResultsDirectory;
    private Field iTestResultField;

    @BeforeMethod(alwaysRun = true)
    public void setUp() throws Exception {
        allureResultsDirectory = Files.createTempDirectory("shaft-7549-allure-tags");
        iTestResultField = TestNGListener.class.getDeclaredField("iTestResult");
        iTestResultField.setAccessible(true);
        iTestResultField.set(null, null);
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() throws Exception {
        iTestResultField.set(null, null);
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
        ITestNGMethod method = Mockito.mock(ITestNGMethod.class);
        Mockito.when(method.getGroups()).thenReturn(new String[]{"smoke", "regression"});
        ConstructorOrMethod constructorOrMethod = Mockito.mock(ConstructorOrMethod.class);
        Mockito.when(constructorOrMethod.getMethod()).thenReturn(annotated);
        Mockito.when(method.getConstructorOrMethod()).thenReturn(constructorOrMethod);

        IClass testClass = Mockito.mock(IClass.class);
        Mockito.doReturn(TaggedFixture.class).when(testClass).getRealClass();

        ITestResult testNgResult = Mockito.mock(ITestResult.class);
        Mockito.when(testNgResult.getMethod()).thenReturn(method);
        Mockito.when(testNgResult.getTestClass()).thenReturn(testClass);
        iTestResultField.set(null, testNgResult);

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
        TestNgTagLabels.apply(result);
        lifecycle.stopTest(testKey);
        lifecycle.writeTest(testKey);

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
        Assert.assertTrue(tagValues.contains("smoke"), "missing group tag smoke: " + tagValues);
        Assert.assertTrue(tagValues.contains("regression"), "missing group tag regression: " + tagValues);
        Assert.assertTrue(tagValues.contains("manual-tag"), "missing @Tag manual-tag: " + tagValues);
        Assert.assertTrue(tagValues.contains("class-tag"), "missing class @Tag class-tag: " + tagValues);
    }

    @Test
    public void applyIsNoOpWithoutBoundTestNgResultAndDoesNotDuplicate() {
        TestResult result = new TestResult();
        result.getLabels().add(new Label().setName("tag").setValue("smoke"));
        TestNgTagLabels.apply(result);
        Assert.assertEquals(result.getLabels().size(), 1);

        ITestNGMethod method = Mockito.mock(ITestNGMethod.class);
        Mockito.when(method.getGroups()).thenReturn(new String[]{"smoke", "smoke", "  "});
        ITestResult testNgResult = Mockito.mock(ITestResult.class);
        Mockito.when(testNgResult.getMethod()).thenReturn(method);
        TestNgTagLabels.apply(result, testNgResult);
        long smokeTags = result.getLabels().stream()
                .filter(label -> "tag".equals(label.getName()) && "smoke".equals(label.getValue()))
                .count();
        Assert.assertEquals(smokeTags, 1L);
    }

    @Tag("class-tag")
    private static final class TaggedFixture {
        @Tags({@Tag("manual-tag")})
        @SuppressWarnings("unused")
        private void taggedMethod() {
            // annotation carrier for AnnotationUtils
        }
    }
}
