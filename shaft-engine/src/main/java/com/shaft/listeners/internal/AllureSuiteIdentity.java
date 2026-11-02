package com.shaft.listeners.internal;

import io.qameta.allure.model.Parameter;
import io.qameta.allure.model.TestResult;
import io.qameta.allure.util.ResultsUtils;

import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Objects;

/**
 * Suite identity for Allure {@code historyId} and {@code testCaseId} before the report is built.
 * The same method on another platform or language must not collapse into a retry.
 * A repeat of the same platform and language keeps both ids.
 * A parameter that already carries platform or language is counted once.
 */
public final class AllureSuiteIdentity {
    private AllureSuiteIdentity() {
    }

    public static void apply(TestResult result, String platform, String language) {
        if (result == null) {
            return;
        }
        String resolvedPlatform = firstNonBlank(
                parameterValue(result, "platform", "targetPlatform"),
                normalize(platform));
        String resolvedLanguage = firstNonBlank(parameterValue(result, "language"), normalize(language));
        String locale = parameterValue(result, "locale");
        if (!locale.isBlank() && !containsToken(resolvedLanguage, locale)) {
            resolvedLanguage = resolvedLanguage.isBlank() ? locale : resolvedLanguage + "-" + locale;
        }
        if (resolvedPlatform.isBlank() && resolvedLanguage.isBlank()) {
            return;
        }
        String base = firstNonBlank(result.getFullName(), result.getName(), result.getTestCaseId(), "shaft-test");
        String testCaseId = ResultsUtils.md5(base + "\nplatform=" + resolvedPlatform + "\nlanguage=" + resolvedLanguage);
        StringBuilder historySource = new StringBuilder(testCaseId);
        List<Parameter> parameters = result.getParameters();
        if (parameters != null) {
            parameters.stream()
                    .filter(Objects::nonNull)
                    .filter(parameter -> !Boolean.TRUE.equals(parameter.getExcluded()))
                    .filter(parameter -> !identityName(parameter.getName()))
                    .sorted(Comparator
                            .comparing((Parameter parameter) -> Objects.toString(parameter.getName(), ""))
                            .thenComparing(parameter -> Objects.toString(parameter.getValue(), "")))
                    .forEachOrdered(parameter -> historySource
                            .append(Objects.toString(parameter.getName(), ""))
                            .append(Objects.toString(parameter.getValue(), "")));
        }
        result.setTestCaseId(testCaseId);
        result.setHistoryId(ResultsUtils.md5(historySource.toString()));
    }

    private static boolean identityName(String name) {
        if (name == null) {
            return false;
        }
        String normalized = name.trim().toLowerCase(Locale.ROOT);
        return normalized.equals("platform")
                || normalized.equals("targetplatform")
                || normalized.equals("language")
                || normalized.equals("locale");
    }

    private static String parameterValue(TestResult result, String... names) {
        List<Parameter> parameters = result.getParameters();
        if (parameters == null) {
            return "";
        }
        for (Parameter parameter : parameters) {
            if (parameter == null || Boolean.TRUE.equals(parameter.getExcluded()) || parameter.getName() == null) {
                continue;
            }
            for (String name : names) {
                if (parameter.getName().trim().equalsIgnoreCase(name)) {
                    return normalize(parameter.getValue());
                }
            }
        }
        return "";
    }

    private static boolean containsToken(String text, String token) {
        return text != null && token != null && !token.isBlank() && text.contains(token);
    }

    private static String firstNonBlank(String... values) {
        if (values == null) {
            return "";
        }
        for (String value : values) {
            if (value != null && !value.isBlank()) {
                return value.trim();
            }
        }
        return "";
    }

    private static String normalize(String value) {
        return value == null ? "" : value.trim();
    }
}
