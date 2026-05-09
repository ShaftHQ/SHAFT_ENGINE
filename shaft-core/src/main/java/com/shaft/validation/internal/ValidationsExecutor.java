package com.shaft.validation.internal;

import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.PathNotFoundException;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ProgressBarLogger;
import com.shaft.validation.ValidationEnums;
import io.qameta.allure.Step;
import io.restassured.response.Response;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

public class ValidationsExecutor {
    protected final StringBuilder reportMessageBuilder;
    private final ValidationEnums.ValidationCategory validationCategory;
    private final ValidationEnums.ValidationType validationType;
    private final String validationMethod;
    private final ThreadLocal<Object> driver = new ThreadLocal<>();
    private final ThreadLocal<Object> response = new ThreadLocal<>();
    @SuppressWarnings({"FieldCanBeLocal", "unused"})
    private String validationCategoryString;
    protected Object locator;
    private String customReportMessage = "";
    protected Object visualValidationEngine;
    protected String elementAttribute;
    protected String elementCssProperty;
    protected String browserAttribute;
    protected ValidationEnums.ValidationComparisonType validationComparisonType;
    protected Object expectedValue;
    protected boolean condition;
    protected Object actualValue;
    protected ValidationEnums.NumbersComparativeRelation numbersComparativeRelation;
    protected String fileAbsolutePath;
    protected Object restComparisonType;
    protected String jsonPath;
    protected String folderRelativePath;
    protected String fileName;

    // ── Constructors for shaft-core builder types ──────────────────────────────

    public ValidationsExecutor(NativeValidationsBuilder nativeValidationsBuilder) {
        this.validationCategory = nativeValidationsBuilder.validationCategory;
        this.driver.set(nativeValidationsBuilder.driver);
        this.locator = nativeValidationsBuilder.locator;
        this.validationType = nativeValidationsBuilder.validationType;
        this.validationMethod = nativeValidationsBuilder.validationMethod;
        this.elementAttribute = nativeValidationsBuilder.elementAttribute;
        this.validationComparisonType = nativeValidationsBuilder.validationComparisonType;
        this.expectedValue = nativeValidationsBuilder.expectedValue;
        this.actualValue = nativeValidationsBuilder.actualValue;
        this.elementCssProperty = nativeValidationsBuilder.elementCssProperty;
        this.browserAttribute = nativeValidationsBuilder.browserAttribute;
        this.response.set(nativeValidationsBuilder.response);
        this.jsonPath = nativeValidationsBuilder.jsonPath;
        this.folderRelativePath = nativeValidationsBuilder.folderRelativePath;
        this.fileName = nativeValidationsBuilder.fileName;
        this.reportMessageBuilder = nativeValidationsBuilder.reportMessageBuilder;
    }

    public ValidationsExecutor(ValidationsBuilder validationsBuilder) {
        this.validationCategory = validationsBuilder.validationCategory;
        this.validationType = validationsBuilder.validationType;
        this.validationMethod = validationsBuilder.validationMethod;
        this.condition = validationsBuilder.condition;
        this.actualValue = validationsBuilder.actualValue;
        this.reportMessageBuilder = validationsBuilder.reportMessageBuilder;
    }

    public ValidationsExecutor(NumberValidationsBuilder numberValidationsBuilder) {
        this.validationCategory = numberValidationsBuilder.validationCategory;
        this.validationType = numberValidationsBuilder.validationType;
        this.validationMethod = numberValidationsBuilder.validationMethod;
        this.expectedValue = numberValidationsBuilder.expectedValue;
        this.actualValue = numberValidationsBuilder.actualValue;
        this.numbersComparativeRelation = numberValidationsBuilder.numbersComparativeRelation;
        this.response.set(numberValidationsBuilder.response);
        this.jsonPath = numberValidationsBuilder.jsonPath;
        this.reportMessageBuilder = numberValidationsBuilder.reportMessageBuilder;
    }

    public ValidationsExecutor(FileValidationsBuilder fileValidationsBuilder) {
        this.validationCategory = fileValidationsBuilder.validationCategory;
        this.validationMethod = fileValidationsBuilder.validationMethod;
        this.validationType = fileValidationsBuilder.validationType;
        this.folderRelativePath = fileValidationsBuilder.folderRelativePath;
        this.fileName = fileValidationsBuilder.fileName;
        this.reportMessageBuilder = fileValidationsBuilder.reportMessageBuilder;
    }

    /**
     * Protected constructor for shaft-web subclasses (WebDriverValidationsExecutor) to init
     * the final fields; subclasses then set WebDriver-specific protected fields directly.
     */
    protected ValidationsExecutor(ValidationEnums.ValidationCategory validationCategory,
                                   ValidationEnums.ValidationType validationType,
                                   String validationMethod,
                                   StringBuilder reportMessageBuilder) {
        this.validationCategory = validationCategory;
        this.validationType = validationType;
        this.validationMethod = validationMethod;
        this.reportMessageBuilder = reportMessageBuilder;
    }

    /** Sets the WebDriver instance stored in the ThreadLocal (for subclasses). */
    protected final void setDriver(Object driverValue) {
        driver.set(driverValue);
    }

    /** Sets the response object stored in the ThreadLocal (for subclasses). */
    protected final void setResponse(Object responseValue) {
        response.set(responseValue);
    }

    // ── Public API ─────────────────────────────────────────────────────────────

    /**
     * Set a customized business-readable message that will appear in the execution report instead of the technical log message which will be nested under it
     *
     * @param customReportMessage the message that you would like to describe this validation in the execution report
     * @return the current ValidationsExecutor object so that you can call the "perform()" method and execute this validation
     */
    public ValidationsExecutor withCustomReportMessage(String customReportMessage) {
        this.customReportMessage = customReportMessage;
        return this;
    }

    /**
     * Execute this validation
     */
    public void perform() {
        ReportManager.log(customReportMessage);
    }

    public void internalPerform() {
        // Invoke JavaScriptWaitManager.waitForLazyLoading via reflection only when a driver is set
        if (driver.get() != null) {
            try {
                Class<?> jswm = Class.forName("com.shaft.gui.browser.internal.JavaScriptWaitManager");
                Method waitMethod = Arrays.stream(jswm.getMethods())
                        .filter(m -> m.getName().equals("waitForLazyLoading"))
                        .findFirst().orElse(null);
                if (waitMethod != null) waitMethod.invoke(null, driver.get());
            } catch (Exception ignored) {
                // shaft-web not on classpath — skip lazy-loading wait
            }
        }
        boolean generatedCustomReportMessage = false;
        if (customReportMessage.isBlank()) {
            customReportMessage = reportMessageBuilder.toString();
            generatedCustomReportMessage = true;
        }
        this.validationCategoryString = validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT) ? "Assert" : "Verify";
        ReportManager.logDiscrete(this.validationCategoryString + " that " + this.customReportMessage);
        String progressTaskName = this.validationCategoryString.equals("Assert") ? "Asserting..." : "Verifying...";
        try {
            try (ProgressBarLogger ignored = new ProgressBarLogger(progressTaskName)) {
                performValidation();
            }
        } finally {
            if (generatedCustomReportMessage) {
                customReportMessage = "";
            }
            driver.remove();
            response.remove();
        }
    }

    @Step(" {this.validationCategoryString} that {this.customReportMessage}")
    private void performValidation() {
        switch (validationMethod) {
            case "forceFail" -> invokeHelper("validateFail", validationCategory, customReportMessage);
            case "objectsAreEqual" ->
                    invokeHelper2("validateEquals", expectedValue, actualValue, validationComparisonType, validationType);
            case "conditionIsTrue" ->
                    invokeHelper("validateTrue", validationCategory, condition, validationType, customReportMessage);
            case "elementExists" ->
                    invokeHelper2("validateElementExists", driver.get(), locator, validationType);
            case "elementMatches" ->
                    invokeHelper2("validateElementMatches", driver.get(), locator, visualValidationEngine, validationType);
            case "elementAttributeEquals" ->
                    invokeHelper2("validateElementAttribute", driver.get(), locator, elementAttribute, String.valueOf(expectedValue), validationComparisonType, validationType);
            case "elementDomAttributeEquals" ->
                    invokeHelper2("validateElementDomAttribute", driver.get(), locator, elementAttribute, String.valueOf(expectedValue), validationComparisonType, validationType);
            case "elementDomPropertyEquals" ->
                    invokeHelper2("validateElementDomProperty", driver.get(), locator, elementAttribute, String.valueOf(expectedValue), validationComparisonType, validationType);
            case "elementCssPropertyEquals" ->
                    invokeHelper2("validateElementCSSProperty", driver.get(), locator, elementCssProperty, String.valueOf(expectedValue), validationComparisonType, validationType);
            case "browserAttributeEquals" ->
                    invokeHelper2("validateBrowserAttribute", driver.get(), browserAttribute, String.valueOf(expectedValue), validationComparisonType, validationType);
            case "comparativeRelationBetweenNumbers" ->
                    invokeHelper2("validateNumber", (Number) expectedValue, (Number) actualValue, numbersComparativeRelation, validationType);
            case "fileExists" ->
                    invokeHelper("validateFileExists", validationCategory, folderRelativePath, fileName, 5, validationType, customReportMessage);
            case "responseEqualsFileContent" ->
                    invokeHelper("validateJSONFileContent", validationCategory, response.get(), fileAbsolutePath, restComparisonType, "", validationType, customReportMessage);
            case "jsonPathValueEquals" ->
                    invokeHelper2("validateEquals", expectedValue, extractJsonValue(response.get(), jsonPath), validationComparisonType, validationType);
            case "jsonPathValueAsListEquals" -> {
                for (Object value : Objects.requireNonNull(extractJsonValueAsList(response.get(), jsonPath))) {
                    invokeHelper2("validateEquals", expectedValue, value.toString(), validationComparisonType, validationType);
                }
            }
            case "responseBody" ->
                    invokeHelper2("validateEquals", expectedValue, extractResponseBody(response.get()), validationComparisonType, validationType);
            case "responseTime" ->
                    invokeHelper2("validateNumber", (Number) expectedValue, extractResponseTime(response.get()), numbersComparativeRelation, validationType);
            case "checkResponseSchema" ->
                    invokeHelper("validateResponseFileSchema", validationCategory, response.get(), fileAbsolutePath, restComparisonType, "", validationType, customReportMessage);
            case "fileContent" -> {
                String fileContent = FileActions.getInstance(true).readFile(folderRelativePath, fileName);
                invokeHelper2("validateEquals", expectedValue, fileContent, validationComparisonType, validationType);
            }
            case "fileChecksum" -> {
                var fileChecksum = FileActions.getInstance(true).getFileChecksum(
                        newTerminalActions(), folderRelativePath, fileName);
                invokeHelper2("validateEquals", expectedValue, fileChecksum, validationComparisonType, validationType);
            }
            default -> {
            }
        }
    }

    // ── Response extraction helpers ────────────────────────────────────────────

    private static String extractResponseBody(Object response) {
        if (response instanceof Response r) return r.getBody().asString();
        return response != null ? response.toString() : "";
    }

    private static long extractResponseTime(Object response) {
        if (response instanceof Response r) return r.timeIn(TimeUnit.MILLISECONDS);
        return 0L;
    }

    private static String extractJsonValue(Object response, String jsonPath) {
        String json = toJsonString(response);
        try {
            if (jsonPath.contains("?")) {
                List<Object> list = JsonPath.read(json, jsonPath);
                return list.isEmpty() ? null : String.valueOf(list.getFirst());
            }
            return String.valueOf((Object) JsonPath.read(json, jsonPath));
        } catch (PathNotFoundException e) {
            return null;
        }
    }

    private static List<Object> extractJsonValueAsList(Object response, String jsonPath) {
        String json = toJsonString(response);
        try {
            return JsonPath.read(json, jsonPath);
        } catch (PathNotFoundException e) {
            return List.of();
        }
    }

    private static String toJsonString(Object response) {
        if (response instanceof Response r) return r.asPrettyString();
        if (response instanceof String s) return s;
        return response != null ? response.toString() : "{}";
    }

    // ── Reflection helpers for ValidationsHelper / ValidationsHelper2 ──────────

    private void invokeHelper(String methodName, Object... args) {
        try {
            Class<?> cls = Class.forName("com.shaft.validation.internal.ValidationsHelper");
            Object helper = cls.getDeclaredConstructor().newInstance();
            Method m = findMethodByName(cls, methodName, args.length);
            m.invoke(helper, args);
        } catch (ClassNotFoundException e) {
            throw new UnsupportedOperationException("ValidationsHelper not available — shaft-engine required for " + methodName, e);
        } catch (ReflectiveOperationException e) {
            Throwable cause = e.getCause();
            if (cause instanceof AssertionError ae) throw ae;
            throw new RuntimeException("Failed to invoke ValidationsHelper." + methodName, e);
        }
    }

    private void invokeHelper2(String methodName, Object... args) {
        try {
            Class<?> cls = Class.forName("com.shaft.validation.internal.ValidationsHelper2");
            Object helper = cls.getDeclaredConstructor(ValidationEnums.ValidationCategory.class).newInstance(validationCategory);
            Method m = findMethodByName(cls, methodName, args.length);
            m.invoke(helper, args);
        } catch (ClassNotFoundException e) {
            throw new UnsupportedOperationException("ValidationsHelper2 not available — shaft-engine required for " + methodName, e);
        } catch (ReflectiveOperationException e) {
            Throwable cause = e.getCause();
            if (cause instanceof AssertionError ae) throw ae;
            throw new RuntimeException("Failed to invoke ValidationsHelper2." + methodName, e);
        }
    }

    private static Method findMethodByName(Class<?> cls, String name, int paramCount) {
        return Arrays.stream(cls.getDeclaredMethods())
                .filter(m -> m.getName().equals(name) && m.getParameterCount() == paramCount)
                .findFirst()
                .orElseGet(() -> Arrays.stream(cls.getMethods())
                        .filter(m -> m.getName().equals(name) && m.getParameterCount() == paramCount)
                        .findFirst()
                        .orElseThrow(() -> new RuntimeException(
                                "Method " + name + "/" + paramCount + " not found in " + cls.getName())));
    }

    private static TerminalActions newTerminalActions() {
        return new TerminalActions();
    }
}
