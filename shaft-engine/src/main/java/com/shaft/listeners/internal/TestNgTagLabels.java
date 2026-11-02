package com.shaft.listeners.internal;

import com.shaft.listeners.TestNGListener;
import io.qameta.allure.model.Label;
import io.qameta.allure.model.TestResult;
import io.qameta.allure.util.AnnotationUtils;
import org.testng.IClass;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;

import java.lang.reflect.Method;
import java.util.List;

/**
 * Copies TestNG {@code groups} and Allure {@code @Tag}/{@code @Tags} onto the Allure result as
 * {@code labels} with {@code name=tag} so they appear and filter in the SHAFT Allure report (#7549).
 * Existing tag labels are kept; duplicates are skipped. No-op when no TestNG result is bound.
 */
public final class TestNgTagLabels {
    private static final String TAG_LABEL_NAME = "tag";

    private TestNgTagLabels() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Applies group and annotation tags from the current TestNG result onto {@code result}.
     *
     * @param result the Allure result about to be stopped
     */
    public static void apply(TestResult result) {
        if (result == null) {
            return;
        }
        ITestResult testNgResult = TestNGListener.getITestResult();
        if (testNgResult == null) {
            return;
        }
        apply(result, testNgResult);
    }

    /**
     * Applies group and annotation tags from {@code testNgResult} onto {@code result}.
     *
     * @param result       the Allure result about to be stopped
     * @param testNgResult the bound TestNG result
     */
    static void apply(TestResult result, ITestResult testNgResult) {
        if (result == null || testNgResult == null) {
            return;
        }
        ITestNGMethod method = testNgResult.getMethod();
        if (method != null) {
            String[] groups = method.getGroups();
            if (groups != null) {
                for (String group : groups) {
                    addTag(result, group);
                }
            }
        }
        Method javaMethod = javaMethod(method);
        if (javaMethod != null) {
            for (Label label : AnnotationUtils.getLabels(javaMethod)) {
                if (TAG_LABEL_NAME.equals(label.getName())) {
                    addTag(result, label.getValue());
                }
            }
        }
        Class<?> realClass = realClass(testNgResult);
        if (realClass != null) {
            for (Label label : AnnotationUtils.getLabels(realClass)) {
                if (TAG_LABEL_NAME.equals(label.getName())) {
                    addTag(result, label.getValue());
                }
            }
        }
    }

    private static void addTag(TestResult result, String value) {
        if (value == null) {
            return;
        }
        String trimmed = value.trim();
        if (trimmed.isEmpty()) {
            return;
        }
        List<Label> labels = result.getLabels();
        if (labels.stream().anyMatch(label -> TAG_LABEL_NAME.equals(label.getName())
                && trimmed.equals(label.getValue()))) {
            return;
        }
        labels.add(new Label().setName(TAG_LABEL_NAME).setValue(trimmed));
    }

    private static Method javaMethod(ITestNGMethod method) {
        if (method == null || method.getConstructorOrMethod() == null) {
            return null;
        }
        return method.getConstructorOrMethod().getMethod();
    }

    private static Class<?> realClass(ITestResult testNgResult) {
        IClass testClass = testNgResult.getTestClass();
        if (testClass != null && testClass.getRealClass() != null) {
            return testClass.getRealClass();
        }
        Object instance = testNgResult.getInstance();
        return instance == null ? null : instance.getClass();
    }
}
