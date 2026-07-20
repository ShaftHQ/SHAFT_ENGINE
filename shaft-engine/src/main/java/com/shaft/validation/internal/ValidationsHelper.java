package com.shaft.validation.internal;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.internal.BrowserActionsHelper;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.internal.Actions;
import com.shaft.gui.internal.aria.AriaSnapshotHelper;
import com.shaft.gui.internal.aria.MobileAccessibilityTreeConverter;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.internal.image.ScreenshotHelper;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.gui.internal.image.VisualProcessingProvider;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.AssertionEvidenceReporter;
import com.shaft.tools.io.internal.CheckpointCounter;
import com.shaft.tools.io.internal.CheckpointStatus;
import com.shaft.tools.io.internal.CheckpointType;
import com.shaft.tools.io.internal.ExecutionSummaryReport;
import com.shaft.tools.io.internal.FlakeProfiler;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.tools.io.internal.TraceEventRecorder;
import com.shaft.validation.ValidationEnums;
import io.qameta.allure.Allure;
import io.qameta.allure.model.Parameter;
import io.qameta.allure.model.Status;
import io.restassured.response.Response;
import org.apache.logging.log4j.Level;
import org.json.JSONException;
import org.json.JSONObject;
import org.openqa.selenium.*;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.remote.Browser;
import org.skyscreamer.jsonassert.JSONAssert;
import org.skyscreamer.jsonassert.JSONCompareMode;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

import static io.restassured.module.jsv.JsonSchemaValidator.matchesJsonSchema;

public class ValidationsHelper {
    private static final String VISUAL_COMPARISON_ATTACHMENT_NAME = "Visual Comparison";
    protected static final ThreadLocal<List<String>> verificationFailuresList = ThreadLocal.withInitial(ArrayList::new);
    protected static final ThreadLocal<AssertionError> verificationError = new ThreadLocal<>();
    private static final ThreadLocal<Boolean> profiledValidationSuccessful = new ThreadLocal<>();
    private final ValidationEnums.ValidationCategory validationCategory;
    private final String validationCategoryString;
    // captured at construction (the start of the validation flow) so the reported
    // outcome step reflects the real validation duration instead of zero
    private long validationStartTime = System.currentTimeMillis();

    ValidationsHelper(ValidationEnums.ValidationCategory validationCategory) {
        this.validationCategory = validationCategory;
        this.validationCategoryString = validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT) ? "Assert" : "Verify";
    }

    public static AssertionError getVerificationErrorToForceFail() {
        return verificationError.get();
    }

    public static void resetVerificationStateAfterFailing() {
        verificationFailuresList.remove();
        verificationError.remove();
    }

    /**
     * Writes an end-of-test Allure step summarizing every accumulated soft (verify) failure, in the
     * order they occurred, before the test is force-failed. This gives a single place to see all
     * verification failures instead of scanning the step list. No-op when there were no soft
     * failures. Must be called before {@link #resetVerificationStateAfterFailing()} clears the list.
     */
    public static void attachVerificationSummary() {
        String summary = verificationSummaryText();
        if (summary == null) {
            return;
        }
        ReportManagerHelper.writeStepToReport(summary, Level.ERROR, Status.FAILED,
                System.currentTimeMillis());
    }

    /**
     * Builds the numbered soft-verification summary text from the accumulated failures, or
     * {@code null} when there were none.
     *
     * @return the summary text, or {@code null} if there are no accumulated soft failures
     */
    static String verificationSummaryText() {
        List<String> failures = verificationFailuresList.get();
        if (failures == null || failures.isEmpty()) {
            return null;
        }
        StringBuilder summary = new StringBuilder("Soft verification summary: " + failures.size()
                + " failure(s) before the test was force-failed");
        int index = 0;
        for (String failure : failures) {
            summary.append(System.lineSeparator()).append("  ").append(++index).append(". ").append(failure);
        }
        return summary.toString();
    }

    /**
     * Records a soft verification failure so it can be reported at the end of the test.
     *
     * @param failureMessage the verification failure message to accumulate
     */
    public static void recordVerificationFailure(String failureMessage) {
        verificationFailuresList.get().add(failureMessage);
        verificationError.set(new AssertionError(String.join("\nAND ", verificationFailuresList.get())));
    }

    static void beginProfiledValidation() {
        profiledValidationSuccessful.set(true);
    }

    static boolean profiledValidationSuccessful() {
        return Boolean.TRUE.equals(profiledValidationSuccessful.get());
    }

    static void clearProfiledValidation() {
        profiledValidationSuccessful.remove();
    }

    private static void recordProfiledValidationState(boolean validationState) {
        Boolean currentState = profiledValidationSuccessful.get();
        if (currentState != null) {
            profiledValidationSuccessful.set(currentState && validationState);
        }
    }

    /**
     * Reports a validation outcome that was evaluated by a non-WebDriver backend while preserving
     * the same checkpoint, attachment, and failure-reporting semantics used by WebDriver validations.
     *
     * @param validationCategory the validation category
     * @param validationState    true when the validation passed
     * @param expected           the reported expected value
     * @param actual             the reported actual value
     * @param attachments        report attachments prepared by the backend
     */
    public static void reportValidationState(ValidationEnums.ValidationCategory validationCategory,
                                             boolean validationState,
                                             Object expected,
                                             Object actual,
                                             List<List<Object>> attachments) {
        reportValidationState(validationCategory, validationState, expected, actual, attachments, System.currentTimeMillis());
    }

    /**
     * Same as {@link #reportValidationState(ValidationEnums.ValidationCategory, boolean, Object, Object, List)}
     * with an explicit validation start timestamp so the reported outcome step carries the real
     * validation duration.
     *
     * @param validationCategory  the validation category
     * @param validationState     true when the validation passed
     * @param expected            the reported expected value
     * @param actual              the reported actual value
     * @param attachments         report attachments prepared by the backend
     * @param validationStartTime epoch milliseconds at which the validation started
     */
    public static void reportValidationState(ValidationEnums.ValidationCategory validationCategory,
                                             boolean validationState,
                                             Object expected,
                                             Object actual,
                                             List<List<Object>> attachments,
                                             long validationStartTime) {
        var validationsHelper = new ValidationsHelper(validationCategory);
        validationsHelper.validationStartTime = validationStartTime;
        validationsHelper.reportValidationState(validationState, expected, actual, null, null, attachments);
    }

    /**
     * Same as {@link #reportValidationState(ValidationEnums.ValidationCategory, boolean, Object, Object, List, long)}
     * for callers that already attached authoritative comparison evidence through a different channel
     * before invoking this method (e.g. a Playwright visual-diff image attached directly via
     * {@code Allure.addAttachment}). When {@code richEvidenceAlreadyAttached} is {@code true}, the
     * generic "Validation Test Data" Expected/Actual text attachments and the "Assertion evidence"
     * card are skipped, since they would otherwise redundantly restate a boolean/opaque comparison
     * result that carries no information beyond what the rich evidence already shows (issue #3804).
     *
     * @param validationCategory        the validation category
     * @param validationState           true when the validation passed
     * @param expected                  the reported expected value
     * @param actual                    the reported actual value
     * @param attachments               report attachments prepared by the backend
     * @param validationStartTime       epoch milliseconds at which the validation started
     * @param richEvidenceAlreadyAttached true to skip the generic supplementary evidence attachments
     */
    public static void reportValidationState(ValidationEnums.ValidationCategory validationCategory,
                                             boolean validationState,
                                             Object expected,
                                             Object actual,
                                             List<List<Object>> attachments,
                                             long validationStartTime,
                                             boolean richEvidenceAlreadyAttached) {
        var validationsHelper = new ValidationsHelper(validationCategory);
        validationsHelper.validationStartTime = validationStartTime;
        validationsHelper.reportValidationState(validationState, expected, actual, null, null, attachments,
                false, richEvidenceAlreadyAttached);
    }

    /**
     * Automatically formats an AssertionError by detecting the package from the stack trace.
     * This method extracts the package from the first stack trace element that is not from
     * framework packages (org.testng, java, com.shaft.validation.internal, etc.)
     *
     * @param error the AssertionError to format
     * @return formatted error message, or null if formatting fails
     */
    private static String formatAssertionErrorWithAutoDetectedPackage(AssertionError error) {
        if (error == null) {
            return null;
        }

        StackTraceElement[] stackTrace = error.getStackTrace();
        // Framework packages to skip when looking for test code
        String[] frameworkPackages = {"org.testng", "java.", "jdk.", "com.shaft.validation.internal",
                                      "com.shaft.validation", "com.shaft.tools", "com.shaft.driver", "com.shaft.gui"};

        // Generic top-level domains to avoid as fallback patterns
        String[] genericTlds = {"com", "org", "net", "java", "jdk"};

        // Find the first stack trace element that is from test code (not framework code)
        for (StackTraceElement element : stackTrace) {
            String className = element.getClassName();

            // Skip framework packages
            boolean isFrameworkPackage = false;
            for (String frameworkPkg : frameworkPackages) {
                if (className.startsWith(frameworkPkg)) {
                    isFrameworkPackage = true;
                    break;
                }
            }

            if (!isFrameworkPackage && element.getLineNumber() > 0) {
                // Extract package from class name (everything before the last dot)
                int lastDotIndex = className.lastIndexOf('.');
                if (lastDotIndex > 0) {
                    String packageName = className.substring(0, lastDotIndex);
                    // Try formatting with the detected package
                    String formatted = AssertionFailureFormatter.formatFailureWithStackTrace(error, packageName);
                    if (formatted != null) {
                        return formatted;
                    }
                    // If exact package doesn't work, try with common patterns (but avoid generic TLDs)
                    String firstPackageSegment = packageName.split("\\.")[0];
                    String[] commonPatterns = {"tests", "test"};
                    // Only add first segment if it's not a generic TLD
                    boolean isGenericTld = false;
                    for (String tld : genericTlds) {
                        if (firstPackageSegment.equals(tld)) {
                            isGenericTld = true;
                            break;
                        }
                    }
                    if (!isGenericTld) {
                        commonPatterns = new String[]{firstPackageSegment, "tests", "test"};
                    }
                    for (String pattern : commonPatterns) {
                        formatted = AssertionFailureFormatter.formatFailureWithStackTrace(error, pattern);
                        if (formatted != null) {
                            return formatted;
                        }
                    }
                }
            }
        }

        return null;
    }

    protected void validateFail(String customReportMessage) {
        String actual = customReportMessage == null || customReportMessage.isBlank() ? "Force fail." : customReportMessage;
        var parameters = new LinkedHashMap<String, String>();
        parameters.put("Expected value", "Validation should pass");
        parameters.put("Actual value", actual);
        updateAllureParameters(parameters);
        reportValidationState(false, "Validation should pass", actual, null, null, null);
    }

    protected void validateTrue(Boolean conditionalStatement, ValidationEnums.ValidationType validationType) {
        Boolean expected = validationType.getValue();
        boolean validationState = performValidation(expected, conditionalStatement,
                ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
        var parameters = new LinkedHashMap<>(setCommonParameters(expected, conditionalStatement,
                ValidationEnums.ValidationComparisonType.EQUALS.name()));
        updateAllureParameters(parameters);
        reportValidationState(validationState, expected, conditionalStatement, null, null, null);
    }

    protected void validateFileExists(String fileFolderName, String fileName, int numberOfRetries,
                                      ValidationEnums.ValidationType validationType) {
        boolean expected = validationType.getValue();
        boolean actual = FileActions.getInstance(true).doesFileExist(fileFolderName, fileName, numberOfRetries);
        String filePrefix = "File '";
        String[] expectedAttributeStates = {"' should exist, after up to '", "' should not exist, after up to '"};
        String numberOfRetriesPostfix = "' retries";
        String reportedExpectedValue = filePrefix + fileFolderName + fileName + expectedAttributeStates[expected ? 0 : 1]
                + numberOfRetries + numberOfRetriesPostfix;
        String reportedActualValue = actual ? "File exists" : "File does not exist";
        boolean validationState = performValidation(expected, actual,
                ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
        var parameters = new LinkedHashMap<>(setCommonParameters(reportedExpectedValue, reportedActualValue,
                ValidationEnums.ValidationComparisonType.EQUALS.name()));
        updateAllureParameters(parameters);
        reportValidationState(validationState, reportedExpectedValue, reportedActualValue, null, null, null);
    }

    protected void validateJSONFileContent(Response response, String referenceJsonFilePath,
                                           RestActions.ComparisonType comparisonType, String jsonPathToTargetArray,
                                           ValidationEnums.ValidationType validationType) {
        boolean expected = validationType.getValue();
        String reportedExpectedValue = reportedJsonExpectation(referenceJsonFilePath, jsonPathToTargetArray, expected);
        Boolean comparisonResult = RestActions.compareJSON(response, referenceJsonFilePath, comparisonType,
                jsonPathToTargetArray);

        List<List<Object>> attachments = new ArrayList<>();
        attachments.add(Arrays.asList("Validation Test Data", "Expected JSON Value",
                RestActions.parseBodyToJson(FileActions.getInstance(true).readFile(referenceJsonFilePath))));
        attachments.add(Arrays.asList("Validation Test Data", "Actual JSON Value",
                RestActions.parseBodyToJson(response)));

        boolean validationState = performValidation(expected, comparisonResult,
                ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
        var parameters = new LinkedHashMap<>(setCommonParameters(reportedExpectedValue, comparisonResult,
                comparisonType.name()));
        updateAllureParameters(parameters);
        reportValidationState(validationState, reportedExpectedValue, comparisonResult, null, null, attachments);
    }

    protected void validateResponseFileSchema(Response response, String referenceJsonFilePath,
                                              RestActions.ComparisonType comparisonType, String jsonPathToTargetArray,
                                              ValidationEnums.ValidationType validationType) {
        boolean expected = validationType.getValue();
        String reportedExpectedValue = reportedJsonExpectation(referenceJsonFilePath, jsonPathToTargetArray, expected);
        boolean comparisonResult;
        try {
            response.then().body(matchesJsonSchema(new File(referenceJsonFilePath)));
            comparisonResult = true;
        } catch (AssertionError assertionError) {
            comparisonResult = false;
        }

        List<List<Object>> attachments = new ArrayList<>();
        attachments.add(Arrays.asList("Validation Test Data", "Expected JSON Value",
                RestActions.parseBodyToJson(FileActions.getInstance(true).readFile(referenceJsonFilePath))));
        attachments.add(Arrays.asList("Validation Test Data", "Actual JSON Value",
                RestActions.parseBodyToJson(response)));

        boolean validationState = performValidation(expected, comparisonResult,
                ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
        var parameters = new LinkedHashMap<>(setCommonParameters(reportedExpectedValue, comparisonResult,
                comparisonType.name()));
        updateAllureParameters(parameters);
        reportValidationState(validationState, reportedExpectedValue, comparisonResult, null, null, attachments);
    }

    private String reportedJsonExpectation(String referenceJsonFilePath, String jsonPathToTargetArray, boolean expected) {
        StringBuilder reportedExpectedValue = new StringBuilder("Response data should ");
        if (!expected) {
            reportedExpectedValue.append("not ");
        }
        reportedExpectedValue.append("match the JSON File in this path '").append(referenceJsonFilePath).append("'");
        if (!jsonPathToTargetArray.isBlank()) {
            reportedExpectedValue.append(", with path to Target Array '").append(jsonPathToTargetArray).append("'");
        }
        return reportedExpectedValue.toString();
    }

    protected void validateEquals(Object expected, Object actual,
                                  ValidationEnums.ValidationComparisonType comparisonType, ValidationEnums.ValidationType validationType) {
        // read actual value based on desired attribute
        // Note: do not try/catch this block as the upstream failure will already be reported along with any needed attachments

        //reporting block
        String comparisonTypeStr = ValidationEnums.ValidationType.NEGATIVE.name().equals(validationType.name()) ? "not " + comparisonType.name() : comparisonType.name();
        var parameters = new LinkedHashMap<>(setCommonParameters(expected, actual, comparisonTypeStr));
        updateAllureParameters(parameters);
        //end of reporting block
        boolean validationState = performValidation(expected, actual, comparisonType, validationType);
        reportValidationState(validationState, expected, actual, null, null, null);
    }

    protected void validateJsonEqualsIgnoringOrder(Object expected, Object actual,
                                                   ValidationEnums.ValidationType validationType) {
        boolean positiveMatch = isJsonEqualIgnoringOrder(String.valueOf(expected), String.valueOf(actual));
        boolean validationState = validationType == ValidationEnums.ValidationType.POSITIVE ? positiveMatch : !positiveMatch;
        String comparisonTypeStr = validationType == ValidationEnums.ValidationType.NEGATIVE
                ? "not EQUALS_IGNORING_ORDER"
                : "EQUALS_IGNORING_ORDER";
        var parameters = new LinkedHashMap<>(setCommonParameters(expected, actual, comparisonTypeStr));
        updateAllureParameters(parameters);
        reportValidationState(validationState, expected, actual, null, null, null);
    }

    protected void validateNumber(Number expected, Number actual,
                                  ValidationEnums.NumbersComparativeRelation comparisonType, ValidationEnums.ValidationType validationType) {
        // read actual value based on desired attribute
        // Note: do not try/catch this block as the upstream failure will already be reported along with any needed attachments
        boolean validationState = performValidation(expected, actual, comparisonType, validationType);
        //reporting block
        String comparisonTypeStr = ValidationEnums.ValidationType.NEGATIVE.name().equals(validationType.name()) ? "not " + comparisonType.name() : comparisonType.name();
        var parameters = new LinkedHashMap<>(setCommonParameters(expected, actual, comparisonTypeStr));
        updateAllureParameters(parameters);
        reportValidationState(validationState, expected, actual, null, null, null);
    }

    protected void validateBrowserAttribute(WebDriver driver, String attribute,
                                            String expected, ValidationEnums.ValidationComparisonType comparisonType, ValidationEnums.ValidationType validationType) {
        // read actual value based on desired attribute
        // Note: do not try/catch this block as the upstream failure will already be reported along with any needed attachments
        AtomicReference<String> actual = new AtomicReference<>();
        AtomicReference<Boolean> validationState = new AtomicReference<>();

        try {
            new SynchronizationManager(driver).fluentWait(false).until(f -> {
                actual.set(switch (attribute.toLowerCase()) {
                    case "currenturl", "pageurl", "windowurl", "url" ->
                            new BrowserActions(driver, true).getCurrentURL();
                    case "pagesource", "windowsource", "source" -> new BrowserActions(driver, true).getPageSource();
                    case "title", "windowtitle", "pagetitle" ->
                            new BrowserActions(driver, true).getCurrentWindowTitle();
                    case "alerttext", "alert", "dialogtext" -> new BrowserActions(driver, true).getAlertText();
                    case "text", "pagetext", "windowtext" -> getPageText(driver);
                    case "textdirection", "pagedirection", "windowdirection" -> getPageTextDirection(driver);
                    case "textalignment", "pagealignment", "windowalignment" -> getPageTextAlignmentDirection(driver);
                    case "textorientation", "pageorientation", "windoworientation" -> getPageTextOrientationDirection(driver);
                    case "textdisplaystyle", "pagedisplaystyle", "windowdisplaystyle" -> getPageTextDisplayStyleDirection(driver);
                    case "windowhandle", "pagehndle", "handle" -> new BrowserActions(driver, true).getWindowHandle();
                    case "windowposition", "pageposition", "position" ->
                            new BrowserActions(driver, true).getWindowPosition();
                    case "windowsize", "pagesize", "size" -> new BrowserActions(driver, true).getWindowSize();
                    default -> "";
                });
                validationState.set(performValidation(expected, actual.get(), comparisonType, validationType));
                return validationState.get();
            });
        } catch (TimeoutException timeoutException) {
            //timeout was exhausted and the validation failed
        }
        //reporting block
        String comparisonTypeStr = ValidationEnums.ValidationType.NEGATIVE.name().equals(validationType.name()) ? "not " + comparisonType.name() : comparisonType.name();
        var parameters = new LinkedHashMap<String, String>();
        parameters.put("Attribute", attribute);
        parameters.putAll(setCommonParameters(expected, actual, comparisonTypeStr));
        updateAllureParameters(parameters);
        reportValidationState(validationState.get(), expected, actual, driver, null, null);
    }

    protected void validateElementDomProperty(WebDriver driver, By locator, String domProperty,
                                              String expected, ValidationEnums.ValidationComparisonType comparisonType, ValidationEnums.ValidationType validationType) {
        // read actual value based on desired attribute
        // Note: do not try/catch this block as the upstream failure will already be reported along with any needed attachments

        AtomicReference<String> actual = new AtomicReference<>();
        AtomicReference<Boolean> validationState = new AtomicReference<>();

        try {
            new SynchronizationManager(driver).fluentWait(false).until(f -> {
                actual.set(new SilentElementReader(driver).readDomProperty(locator, domProperty));
                validationState.set(performValidation(expected, actual.get(), comparisonType, validationType));
                return validationState.get();
            });
        } catch (TimeoutException timeoutException) {
            //timeout was exhausted and the validation failed
        }
        //reporting block
        String comparisonTypeStr = ValidationEnums.ValidationType.NEGATIVE.name().equals(validationType.name()) ? "not " + comparisonType.name() : comparisonType.name();
        var parameters = new LinkedHashMap<String, String>();
        parameters.put("Locator", String.valueOf(locator));
        parameters.put("DOM Property", domProperty);
        parameters.putAll(setCommonParameters(expected, actual, comparisonTypeStr));
        updateAllureParameters(parameters);
        reportValidationState(validationState.get(), expected, actual, driver, locator, null);
    }

    protected void validateElementAttribute(WebDriver driver, By locator, String attribute,
                                            String expected, ValidationEnums.ValidationComparisonType comparisonType, ValidationEnums.ValidationType validationType) {
        AtomicReference<String> actual = new AtomicReference<>();
        AtomicReference<Boolean> validationState = new AtomicReference<>();

        try {
            new SynchronizationManager(driver).fluentWait(false).until(f -> {
                SilentElementReader elementReader = new SilentElementReader(driver);
                actual.set(switch (attribute.toLowerCase()) {
                    case "text" -> elementReader.readText(locator);
                    case "texttrimmed", "trimmedtext" -> elementReader.readText(locator).trim();
                    case "selectedtext" -> elementReader.readSelectedText(locator);
                    default -> elementReader.readAttribute(locator, attribute);
                });
                validationState.set(performValidation(expected, actual.get(), comparisonType, validationType));
                return validationState.get();
            });
        } catch (TimeoutException timeoutException) {
            //timeout was exhausted and the validation failed
        }
        //reporting block
        String comparisonTypeStr = ValidationEnums.ValidationType.NEGATIVE.name().equals(validationType.name()) ? "not " + comparisonType.name() : comparisonType.name();
        var parameters = new LinkedHashMap<String, String>();
        parameters.put("Locator", String.valueOf(locator));
        parameters.put("Attribute", attribute);
        parameters.putAll(setCommonParameters(expected, actual, comparisonTypeStr));
        updateAllureParameters(parameters);
        reportValidationState(validationState.get(), expected, actual, driver, locator, null);
    }


    protected void validateElementDomAttribute(WebDriver driver, By locator, String attribute,
                                               String expected, ValidationEnums.ValidationComparisonType comparisonType, ValidationEnums.ValidationType validationType) {
        // read actual value based on desired attribute
        AtomicReference<String> actual = new AtomicReference<>();
        AtomicReference<Boolean> validationState = new AtomicReference<>();

        try {
            new SynchronizationManager(driver).fluentWait(false).until(f -> {
                SilentElementReader elementReader = new SilentElementReader(driver);
                actual.set(switch (attribute.toLowerCase()) {
                    case "text" -> elementReader.readText(locator);
                    case "texttrimmed", "trimmedtext" -> elementReader.readText(locator).trim();
                    case "selectedtext" -> elementReader.readSelectedText(locator);
                    case "textdirection" -> getElementTextDirection(driver, locator);
                    case "textalignment" -> getElementTextAlignmentDirection(driver, locator);
                    case "textorientation" -> getElementTextOrientationDirection(driver, locator);
                    case "textdisplaystyle" -> getElementTextDisplayStyleDirection(driver, locator);
                    default -> elementReader.readDomAttribute(locator, attribute);
                });
                validationState.set(performValidation(expected, actual.get(), comparisonType, validationType));
                return validationState.get();
            });
        } catch (TimeoutException timeoutException) {
            //timeout was exhausted and the validation failed
        }
        //reporting block
        String comparisonTypeStr = ValidationEnums.ValidationType.NEGATIVE.name().equals(validationType.name()) ? "not " + comparisonType.name() : comparisonType.name();
        var parameters = new LinkedHashMap<String, String>();
        parameters.put("Locator", String.valueOf(locator));
        parameters.put("DOM Attribute", attribute);
        parameters.putAll(setCommonParameters(expected, actual, comparisonTypeStr));
        updateAllureParameters(parameters);
        reportValidationState(validationState.get(), expected, actual, driver, locator, null);
    }

    protected void validateElementCSSProperty(WebDriver driver, By locator, String property,
                                              String expected, ValidationEnums.ValidationComparisonType comparisonType, ValidationEnums.ValidationType validationType) {
        // read actual value based on desired css property
        // Note: do not try/catch this block as the upstream failure will already be reported along with any needed attachments
        AtomicReference<String> actual = new AtomicReference<>();
        AtomicReference<Boolean> validationState = new AtomicReference<>();

        try {
            new SynchronizationManager(driver).fluentWait(false).until(f -> {
                actual.set(new SilentElementReader(driver).readCssValue(locator, property));
                validationState.set(performValidation(expected, actual.get(), comparisonType, validationType));
                return validationState.get();
            });
        } catch (TimeoutException timeoutException) {
            //timeout was exhausted and the validation failed
        }
        //reporting block
        String comparisonTypeStr = ValidationEnums.ValidationType.NEGATIVE.name().equals(validationType.name()) ? "not " + comparisonType.name() : comparisonType.name();
        var parameters = new LinkedHashMap<String, String>();
        parameters.put("Locator", String.valueOf(locator));
        parameters.put("CSS Property", property);
        parameters.putAll(setCommonParameters(expected, actual, comparisonTypeStr));
        updateAllureParameters(parameters);
        reportValidationState(validationState.get(), expected, actual, driver, locator, null);
    }

    protected void validateElementExists(WebDriver driver, By locator,
                                         ValidationEnums.ValidationType validationType) {
        // read actual value based on desired existing state
        // Note: do not try/catch this block as the upstream failure will already be reported along with any needed attachments
        AtomicBoolean expected = new AtomicBoolean(false);
        AtomicBoolean actual = new AtomicBoolean(false);
        AtomicBoolean validationState = new AtomicBoolean(false);
        AtomicInteger elementCount = new AtomicInteger();

        try {
            new SynchronizationManager(driver).fluentWait(false).until(f -> {
                elementCount.set(new ElementActions(driver, true).getElementsCount(locator));
                expected.set(validationType.getValue());
                actual.set(elementCount.get() > 0);
                // force validation type to be positive since the expected and actual values have been adjusted already
                validationState.set(performValidation(expected.get(), actual.get(), ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE));
                return validationState.get();
            });
        } catch (TimeoutException timeoutException) {
            //timeout was exhausted and the validation failed
        }
        //reporting block
        var parameters = new LinkedHashMap<String, String>();
        parameters.put("Locator", String.valueOf(locator));
        parameters.put("Should exist", String.valueOf(expected.get()));
        parameters.put("Actual value", String.valueOf(actual.get()));
        updateAllureParameters(parameters);

        // force take page screenshot, (rather than element highlighted screenshot)
        reportValidationState(validationState.get(), expected, actual, driver, elementCount.get() == 0 ? null : locator, null);
    }


    protected void validateElementMatches(WebDriver driver, By locator,
                                          ValidationEnums.VisualValidationEngine visualValidationEngine, ValidationEnums.ValidationType validationType) {
        // read actual value based on desired existing state
        // Note: do not try/catch this block as the upstream failure will already be reported along with any needed attachments
        AtomicBoolean expected = new AtomicBoolean(false);
        AtomicBoolean actual = new AtomicBoolean(false);
        AtomicBoolean validationState = new AtomicBoolean(false);
        AtomicInteger elementCount = new AtomicInteger();
        AtomicReference<ValidationEnums.VisualValidationEngine> internalVisualEngine = new AtomicReference<>(visualValidationEngine);
        List<List<Object>> attachments = new ArrayList<>();
        AtomicBoolean visualComparisonAttached = new AtomicBoolean(false);

        try {
            //https://github.com/assertthat/selenium-shutterbug/issues/105
            if (Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
                internalVisualEngine.set(ValidationEnums.VisualValidationEngine.EXACT_OPENCV);
            }

            // get reference image
            byte[] referenceImage = ImageProcessingActions.getReferenceImage(locator);
            if (referenceImage !=null && !Arrays.equals(new byte[0], referenceImage)) {
                ReportManagerHelper.logDiscrete("Reference image found.", Level.INFO);
            } else {
                ReportManagerHelper.logDiscrete("Reference image not found, attempting to capture new reference.", Level.INFO);
            }

            new SynchronizationManager(driver).fluentWait(true).until(f -> {
                // get actual screenshot
                byte[] elementScreenshot;
                Boolean actualResult;

                try {
                    elementScreenshot = driver.findElement(locator).getScreenshotAs(OutputType.BYTES);
                } catch (WebDriverException webDriverException) {
                    if (Objects.requireNonNull(webDriverException.getMessage()).contains("Cannot take screenshot with 0 width.")) {
                        throw new NoSuchElementException("Cannot take screenshot with 0 width.");
                    } else if (Objects.requireNonNull(webDriverException.getMessage()).contains("NS_ERROR_FAILURE")) {
                        return false; // force the wait block to try again in case of unknown error with firefox
                    } else {
                        throw webDriverException;
                    }
                }
                actualResult = ImageProcessingActions.compareAgainstBaseline(driver, locator, elementScreenshot, ImageProcessingActions.VisualValidationEngine.valueOf(visualValidationEngine.name()));

                // prepare content for allure attachment
                byte[] shutterbugDifferencesImage = new byte[0];

                // compare actual and reference screenshots
                boolean isDifferencesImageApplicable = visualValidationEngine.equals(ValidationEnums.VisualValidationEngine.EXACT_SHUTTERBUG) && !actualResult;
                if (isDifferencesImageApplicable) {
                    //if shutterbug and failed, get differences screenshot
                    shutterbugDifferencesImage = ImageProcessingActions.getShutterbugDifferencesImage(locator);
                }

                if (referenceImage != null) {
                    visualComparisonAttached.set(attachVisualComparison(referenceImage, elementScreenshot, shutterbugDifferencesImage));
                }

                // if found set value to 1, else set value to zero
                elementCount.set(actualResult ? 1 : 0);

                expected.set(validationType.getValue());
                actual.set(actualResult);
                // force validation type to be positive since the expected and actual values have been adjusted already
                validationState.set(performValidation(expected.get(), actual.get(), ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE));
                return validationState.get();
            });
        } catch (TimeoutException timeoutException) {
            //timeout was exhausted and the validation failed
        }
        //reporting block
        var parameters = new LinkedHashMap<String, String>();
        parameters.put("Locator", String.valueOf(locator));
        parameters.put("Should match", String.valueOf(expected.get()));
        parameters.put("Visual engine", internalVisualEngine.get().name());
        parameters.put("Actual value", String.valueOf(actual.get()));
        updateAllureParameters(parameters);
        // force take page screenshot, (rather than element highlighted screenshot)
        reportValidationState(validationState.get(), expected, actual, driver, elementCount.get() == 0 ? null : locator, attachments, visualComparisonAttached.get());
    }

    protected void validateMatchesScreenshot(WebDriver driver, By locator, boolean pageLevel, List<By> maskLocators,
                                             Integer maxDiffPixels, Double maxDiffPixelRatio, ValidationEnums.ValidationType validationType) {
        // read actual value based on desired existing state
        // Note: do not try/catch this block as the upstream failure will already be reported along with any needed attachments
        AtomicBoolean expected = new AtomicBoolean(false);
        AtomicBoolean actual = new AtomicBoolean(false);
        AtomicBoolean validationState = new AtomicBoolean(false);
        List<List<Object>> attachments = new ArrayList<>();
        AtomicBoolean visualComparisonAttached = new AtomicBoolean(false);
        // Hoisted out of the fluentWait lambda so the reporting block can render a domain-consistent
        // visual evidence card from the numeric diff metadata (issue #3532 E).
        AtomicLong diffPixelsRef = new AtomicLong(0);
        AtomicReference<Double> diffRatioRef = new AtomicReference<>(0.0);
        String pageBaselineKey = pageLevel ? "page_" + ReportManagerHelper.getCallingMethodFullName() : null;

        try {
            new SynchronizationManager(driver).fluentWait(true).until(f -> {
                byte[] actualScreenshot;
                if (pageLevel) {
                    try {
                        actualScreenshot = ScreenshotHelper.makeFullScreenshot(driver);
                    } catch (IOException ioException) {
                        ReportManagerHelper.logDiscrete(ioException);
                        return false; // force the wait block to try again
                    }
                } else {
                    try {
                        actualScreenshot = driver.findElement(locator).getScreenshotAs(OutputType.BYTES);
                    } catch (WebDriverException webDriverException) {
                        if (Objects.requireNonNull(webDriverException.getMessage()).contains("Cannot take screenshot with 0 width.")) {
                            throw new NoSuchElementException("Cannot take screenshot with 0 width.");
                        } else if (Objects.requireNonNull(webDriverException.getMessage()).contains("NS_ERROR_FAILURE")) {
                            return false; // force the wait block to try again in case of unknown error with firefox
                        } else {
                            throw webDriverException;
                        }
                    }
                }

                List<int[]> maskRects = resolveMaskRects(driver, maskLocators);

                byte[] baselineImage = pageLevel
                        ? ImageProcessingActions.getReferenceImage(pageBaselineKey)
                        : ImageProcessingActions.getReferenceImage(locator);

                VisualProcessingProvider.ScreenshotComparisonResult comparisonResult = pageLevel
                        ? ImageProcessingActions.compareScreenshotAgainstBaseline(pageBaselineKey, actualScreenshot, maskRects, maxDiffPixels, maxDiffPixelRatio)
                        : ImageProcessingActions.compareScreenshotAgainstBaseline(locator, actualScreenshot, maskRects, maxDiffPixels, maxDiffPixelRatio);

                if (baselineImage != null) {
                    visualComparisonAttached.set(attachVisualComparison(baselineImage, actualScreenshot, comparisonResult.diffImage()));
                }
                diffPixelsRef.set(comparisonResult.diffPixels());
                diffRatioRef.set(comparisonResult.diffRatio());

                expected.set(validationType.getValue());
                actual.set(comparisonResult.matched());
                // force validation type to be positive since the expected and actual values have been adjusted already
                validationState.set(performValidation(expected.get(), actual.get(), ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE));
                return validationState.get();
            });
        } catch (TimeoutException timeoutException) {
            //timeout was exhausted and the validation failed
        }
        //reporting block
        var parameters = new LinkedHashMap<String, String>();
        parameters.put(pageLevel ? "Page" : "Locator", pageLevel ? "current page" : String.valueOf(locator));
        parameters.put("Should match", String.valueOf(expected.get()));
        parameters.put("Actual value", String.valueOf(actual.get()));
        updateAllureParameters(parameters);
        // Prepend a domain-consistent visual evidence card (diff pixels/ratio vs budget + pointer to
        // the attached image diff) whenever an actual baseline comparison happened (#3532 E).
        if (visualComparisonAttached.get()) {
            String visualCard = AssertionEvidenceReporter.renderVisualCard(validationState.get(),
                    diffPixelsRef.get(), maxDiffPixels, diffRatioRef.get(), maxDiffPixelRatio);
            if (!visualCard.isBlank()) {
                attachments.add(0, Arrays.asList("Visual evidence", "visual-evidence.html", visualCard));
            }
        }
        // force take page screenshot, (rather than element highlighted screenshot)
        reportValidationState(validationState.get(), expected, actual, driver, pageLevel ? null : locator, attachments, visualComparisonAttached.get());
    }

    private static List<int[]> resolveMaskRects(WebDriver driver, List<By> maskLocators) {
        if (maskLocators == null || maskLocators.isEmpty()) {
            return List.of();
        }
        double scalingFactor = SHAFT.Properties.visuals.screenshotParamsScalingFactor();
        List<int[]> rects = new ArrayList<>();
        for (By mask : maskLocators) {
            try {
                var rect = driver.findElement(mask).getRect();
                rects.add(new int[]{
                        (int) Math.round(rect.getX() * scalingFactor),
                        (int) Math.round(rect.getY() * scalingFactor),
                        (int) Math.round(rect.getWidth() * scalingFactor),
                        (int) Math.round(rect.getHeight() * scalingFactor)
                });
            } catch (NoSuchElementException noSuchElementException) {
                ReportManagerHelper.logDiscrete("Mask locator \"" + mask + "\" did not match any element; skipping mask.", Level.DEBUG);
            }
        }
        return rects;
    }

    protected void validateElementAriaSnapshot(WebDriver driver, By locator, String snapshotFileName, ValidationEnums.ValidationType validationType) {
        AtomicBoolean expected = new AtomicBoolean(false);
        AtomicBoolean actual = new AtomicBoolean(false);
        AtomicBoolean validationState = new AtomicBoolean(false);
        AtomicReference<List<List<Object>>> attachmentsRef = new AtomicReference<>(new ArrayList<>());
        // Hoisted out of the fluentWait lambda so the reporting block can render a domain-consistent
        // accessibility evidence card from the aria YAML (issue #3532 E).
        AtomicReference<String> baselineYamlRef = new AtomicReference<>("");
        AtomicReference<String> actualYamlRef = new AtomicReference<>("");
        String baselinePath = resolveAriaSnapshotPath(snapshotFileName);

        try {
            new SynchronizationManager(driver).fluentWait(true).until(f -> {
                List<List<Object>> attachments = new ArrayList<>();
                String actualYaml = DriverFactoryHelper.isMobileNativeExecution()
                        ? MobileAccessibilityTreeConverter.captureAriaSnapshot(driver, locator)
                        : AriaSnapshotHelper.captureAriaSnapshot(driver, locator);
                boolean updateSnapshots = SHAFT.Properties.visuals.updateSnapshots();
                boolean baselineExists = FileActions.getInstance(true).doesFileExist(baselinePath);
                boolean matched;
                String baselineYaml;

                if (!baselineExists || updateSnapshots) {
                    ReportManager.logDiscrete("Passing the test and saving a reference aria snapshot baseline.");
                    FileActions.getInstance(true).writeToFile(baselinePath, actualYaml);
                    matched = true;
                    baselineYaml = actualYaml;
                } else {
                    baselineYaml = FileActions.getInstance(true).readFile(baselinePath);
                    var matchResult = AriaSnapshotHelper.match(baselineYaml, actualYaml);
                    matched = matchResult.matched();
                    if (!matched) {
                        attachments.add(List.of("Aria Snapshot Diff", "aria-snapshot-diff.txt", matchResult.diffMessage()));
                    }
                }
                attachments.add(List.of("Expected Aria Snapshot", snapshotFileName + ".yaml", baselineYaml));
                attachments.add(List.of("Actual Aria Snapshot", snapshotFileName + "_actual.yaml", actualYaml));
                baselineYamlRef.set(baselineYaml);
                actualYamlRef.set(actualYaml);
                attachmentsRef.set(attachments);

                expected.set(validationType.getValue());
                actual.set(matched);
                // force validation type to be positive since the expected and actual values have been adjusted already
                validationState.set(performValidation(expected.get(), actual.get(), ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE));
                return validationState.get();
            });
        } catch (TimeoutException timeoutException) {
            //timeout was exhausted and the validation failed
        }
        //reporting block
        var parameters = new LinkedHashMap<String, String>();
        parameters.put("Locator", String.valueOf(locator));
        parameters.put("Snapshot file", snapshotFileName);
        parameters.put("Should match", String.valueOf(expected.get()));
        parameters.put("Actual value", String.valueOf(actual.get()));
        updateAllureParameters(parameters);
        // Prepend a domain-consistent accessibility evidence card (summary + aria-YAML diff) so the
        // aria-snapshot outcome reads like an assertion card instead of a raw YAML dump (#3532 E).
        List<List<Object>> ariaAttachments = attachmentsRef.get();
        String accessibilityCard = AssertionEvidenceReporter.renderAccessibilityCard(
                validationState.get(), baselineYamlRef.get(), actualYamlRef.get());
        if (!accessibilityCard.isBlank()) {
            ariaAttachments.add(0, Arrays.asList("Accessibility evidence", "accessibility-evidence.html", accessibilityCard));
        }
        reportValidationState(validationState.get(), expected, actual, driver, locator, ariaAttachments);
    }

    private static String resolveAriaSnapshotPath(String snapshotFileName) {
        String fileName = snapshotFileName.endsWith(".yaml") || snapshotFileName.endsWith(".yml")
                ? snapshotFileName : snapshotFileName + ".yaml";
        return SHAFT.Properties.paths.ariaSnapshot() + fileName;
    }

    private static boolean attachVisualComparison(byte[] expectedImage, byte[] actualImage, byte[] differenceImage) {
        try {
            var content = new JSONObject()
                    .put("expected", "data:image/png;base64," + Base64.getEncoder().encodeToString(expectedImage))
                    .put("actual", "data:image/png;base64," + Base64.getEncoder().encodeToString(actualImage));
            if (differenceImage != null && differenceImage.length > 0) {
                content.put("diff", "data:image/png;base64," + Base64.getEncoder().encodeToString(differenceImage));
            }
            Allure.addAttachment(VISUAL_COMPARISON_ATTACHMENT_NAME, "application/vnd.allure.image.diff", content.toString());
            return true;
        } catch (JSONException jsonException) {
            ReportManagerHelper.logDiscrete(jsonException, Level.DEBUG);
            return false;
        }
    }

    private String getPageText(WebDriver driver) {
        // Fallback order keeps behavior stable across DOM variants:
        // body.innerText -> documentElement.innerText -> empty string.
        return executeJavascript(driver, "return (document.body && document.body.innerText) || document.documentElement.innerText || '';");
    }

    private String getPageTextDirection(WebDriver driver) {
        // Direction cascade:
        // documentElement dir attribute -> body dir attribute -> body computed direction
        // -> documentElement computed direction -> default ltr.
        return normalizeDirection(executeJavascript(driver,
                "const doc=document.documentElement; const body=document.body; const bodyStyle=body?window.getComputedStyle(body):null; " +
                        "const dir=(doc.getAttribute('dir')|| (body?body.getAttribute('dir'):'') || (bodyStyle?bodyStyle.direction:'') || getComputedStyle(doc).direction || 'ltr'); return dir;"));
    }

    private String getPageTextAlignmentDirection(WebDriver driver) {
        return normalizeDirection(executeJavascript(driver,
                "const body=document.body; const doc=document.documentElement; const style=body?window.getComputedStyle(body):window.getComputedStyle(doc); " +
                        "const align=(style.textAlign || '').toLowerCase(); if(align==='right'||align==='end'){return 'rtl';} if(align==='left'||align==='start'){return 'ltr';} " +
                        "const dir=(doc.getAttribute('dir')|| (body?body.getAttribute('dir'):'') || style.direction || 'ltr'); return dir;"));
    }

    private String getPageTextOrientationDirection(WebDriver driver) {
        return normalizeDirection(executeJavascript(driver,
                "const body=document.body; const doc=document.documentElement; const style=body?window.getComputedStyle(body):window.getComputedStyle(doc); " +
                        "const writingMode=(style.writingMode||'').toLowerCase(); if(writingMode.includes('rl')){return 'rtl';} if(writingMode.includes('lr')){return 'ltr';} " +
                        "const dir=(doc.getAttribute('dir')|| (body?body.getAttribute('dir'):'') || style.direction || 'ltr'); return dir;"));
    }

    private String getPageTextDisplayStyleDirection(WebDriver driver) {
        return normalizeDirection(executeJavascript(driver,
                "const body=document.body; const doc=document.documentElement; const style=body?window.getComputedStyle(body):window.getComputedStyle(doc); " +
                        "const dir=(doc.getAttribute('dir')|| (body?body.getAttribute('dir'):'') || style.direction || 'ltr'); return dir;"));
    }

    private String getElementTextDirection(WebDriver driver, By locator) {
        return normalizeDirection(executeJavascript(driver,
                "const el=arguments[0]; const style=window.getComputedStyle(el); const doc=document.documentElement; " +
                        "const dir=(el.getAttribute('dir')|| style.direction || doc.getAttribute('dir') || getComputedStyle(doc).direction || 'ltr'); return dir;",
                locator));
    }

    private String getElementTextAlignmentDirection(WebDriver driver, By locator) {
        return normalizeDirection(executeJavascript(driver,
                "const el=arguments[0]; const style=window.getComputedStyle(el); const align=(style.textAlign||'').toLowerCase(); " +
                        "if(align==='right'||align==='end'){return 'rtl';} if(align==='left'||align==='start'){return 'ltr';} " +
                        "const doc=document.documentElement; const dir=(el.getAttribute('dir')|| style.direction || doc.getAttribute('dir') || getComputedStyle(doc).direction || 'ltr'); return dir;",
                locator));
    }

    private String getElementTextOrientationDirection(WebDriver driver, By locator) {
        return normalizeDirection(executeJavascript(driver,
                "const el=arguments[0]; const style=window.getComputedStyle(el); const writingMode=(style.writingMode||'').toLowerCase(); " +
                        "if(writingMode.includes('rl')){return 'rtl';} if(writingMode.includes('lr')){return 'ltr';} " +
                        "const doc=document.documentElement; const dir=(el.getAttribute('dir')|| style.direction || doc.getAttribute('dir') || getComputedStyle(doc).direction || 'ltr'); return dir;",
                locator));
    }

    private String getElementTextDisplayStyleDirection(WebDriver driver, By locator) {
        return normalizeDirection(executeJavascript(driver,
                "const el=arguments[0]; const style=window.getComputedStyle(el); const doc=document.documentElement; " +
                        "const dir=(el.getAttribute('dir')|| style.direction || doc.getAttribute('dir') || getComputedStyle(doc).direction || 'ltr'); return dir;",
                locator));
    }

    private String executeJavascript(WebDriver driver, String script) {
        Object value = ((JavascriptExecutor) driver).executeScript(script);
        return value == null ? "" : String.valueOf(value);
    }

    private String executeJavascript(WebDriver driver, String script, By locator) {
        WebElement element = driver.findElement(locator);
        Object value = ((JavascriptExecutor) driver).executeScript(script, element);
        return value == null ? "" : String.valueOf(value);
    }

    private String normalizeDirection(String direction) {
        return "rtl".equalsIgnoreCase(direction) ? "rtl" : "ltr";
    }

    private LinkedHashMap<String, String> setCommonParameters(Object expected, Object actual, String comparisonType) {
        var commonParams = new LinkedHashMap<String, String>();
        commonParams.put("Expected value", String.valueOf(expected));
        commonParams.put("Comparison type", JavaHelper.convertToSentenceCase(comparisonType));
        commonParams.put("Actual value", String.valueOf(actual));
        return commonParams;
    }

    private static class SilentElementReader extends Actions {
        SilentElementReader(WebDriver driver) {
            super(driver, true);
        }

        String readAttribute(By locator, String attributeName) {
            return performAction(ActionType.GET_ATTRIBUTE, locator, attributeName);
        }

        String readDomAttribute(By locator, String attributeName) {
            return performAction(ActionType.GET_DOM_ATTRIBUTE, locator, attributeName);
        }

        String readDomProperty(By locator, String propertyName) {
            return performAction(ActionType.GET_DOM_PROPERTY, locator, propertyName);
        }

        String readText(By locator) {
            return performAction(ActionType.GET_TEXT, locator, null);
        }

        String readSelectedText(By locator) {
            return performAction(ActionType.GET_SELECTED_TEXT, locator, null);
        }

        String readCssValue(By locator, String propertyName) {
            return performAction(ActionType.GET_CSS_VALUE, locator, propertyName);
        }
    }

    // this method will accept a hashmap of String parameter names and values to be added to the current step in allure
    private void updateAllureParameters(LinkedHashMap<String, String> parameters) {
        //reporting block
        List<Parameter> params = new ArrayList<>();
        parameters.forEach((key, value) -> params.add(new Parameter().setName(key).setValue(String.valueOf(value)).setMode(Parameter.Mode.DEFAULT)));
        Allure.getLifecycle().updateStep(stepResult -> stepResult.setParameters(params));
    }

    private boolean performValidation(Object expected, Object actual,
                                      Object comparisonType, ValidationEnums.ValidationType validationType) {
        // compare actual and expected results
        int comparisonResult = 0;
        if (comparisonType instanceof ValidationEnums.ValidationComparisonType validationComparisonType) {
            // comparison integer is used for all string-based, null, boolean, and Object comparisons
            comparisonResult = JavaHelper.compareTwoObjects(expected, actual,
                    validationComparisonType.getValue(), validationType.getValue());
        } else if (comparisonType instanceof ValidationEnums.NumbersComparativeRelation numbersComparativeRelation) {
            // this means that it is a number-based comparison
            comparisonResult = JavaHelper.compareTwoObjects(expected, actual,
                    numbersComparativeRelation, validationType.getValue());
        }
        // set validation state based on comparison results
        boolean validationState;
        if (comparisonResult == 1) {
            validationState = ValidationEnums.ValidationState.PASSED.getValue();
        } else {
            validationState = ValidationEnums.ValidationState.FAILED.getValue();
        }
        return validationState;
    }

    private boolean isJsonEqualIgnoringOrder(String expected, String actual) {
        try {
            JSONAssert.assertEquals(expected, actual, JSONCompareMode.NON_EXTENSIBLE);
            return true;
        } catch (JSONException | AssertionError e) {
            return false;
        }
    }

    private void reportValidationState(boolean validationState, Object expected, Object actual, WebDriver driver, By locator, List<List<Object>> attachments) {
        reportValidationState(validationState, expected, actual, driver, locator, attachments, false);
    }

    private void reportValidationState(boolean validationState, Object expected, Object actual, WebDriver driver, By locator, List<List<Object>> attachments, boolean skipDefaultScreenshot) {
        reportValidationState(validationState, expected, actual, driver, locator, attachments, skipDefaultScreenshot, false);
    }

    private void reportValidationState(boolean validationState, Object expected, Object actual, WebDriver driver, By locator, List<List<Object>> attachments, boolean skipDefaultScreenshot, boolean richEvidenceAlreadyAttached) {
        TraceEventRecorder.Event traceEvent = TraceEventRecorder.start(
                "validation",
                this.validationCategoryString,
                locator,
                driver);
        recordProfiledValidationState(validationState);
        //initialize attachments object if no attachments were already prepared
        attachments = attachments == null ? new ArrayList<>() : attachments;

        prepareEvidenceAttachments(validationState, expected, actual, driver, locator, skipDefaultScreenshot,
                richEvidenceAlreadyAttached, attachments);
        attachEvidenceCardIfNeeded(richEvidenceAlreadyAttached, validationState, expected, actual, attachments);
        attachEvidenceWithProfiling(attachments);

        // determine checkpoint type
        CheckpointType checkpointType = determineCheckpointType();
        String checkpointMessage = this.validationCategoryString + ": expected \"" + expected + "\", actual \"" + actual + "\"";
        // handle reporting & failure based on validation category
        ReportManager.logDiscrete("Expected \"" + expected + "\", and actual \"" + actual + "\"", Level.DEBUG);
        routeValidationOutcome(validationState, expected, actual, locator, attachments, traceEvent, checkpointType, checkpointMessage);
    }

    /**
     * Prepares the reportable evidence attachments, mutating {@code attachments} in place. When a
     * WebDriver is present the driver-derived evidence (screenshot/page-snapshot) is always
     * considered; {@code richEvidenceAlreadyAttached} only gates the plain testData attachments
     * prepared below when no driver is available.
     */
    private void prepareEvidenceAttachments(boolean validationState, Object expected, Object actual, WebDriver driver,
                                            By locator, boolean skipDefaultScreenshot, boolean richEvidenceAlreadyAttached,
                                            List<List<Object>> attachments) {
        if (driver != null) {
            prepareWebDriverAttachments(driver, locator, validationState, skipDefaultScreenshot, attachments);
        } else if (!richEvidenceAlreadyAttached) {
            prepareTestDataAttachments(expected, actual, attachments);
        }
    }

    /**
     * Prepares the WebDriver-derived evidence: the default element-highlighted screenshot (unless
     * skipped, or attachments are already populated) plus the page snapshot/HTML capture.
     */
    private void prepareWebDriverAttachments(WebDriver driver, By locator, boolean validationState,
                                             boolean skipDefaultScreenshot, List<List<Object>> attachments) {
        // prepare screenshot with element highlighting
        if (!skipDefaultScreenshot && attachments.isEmpty())
            attachments.add(new ScreenshotManager().takeScreenshot(driver, locator, this.validationCategoryString, validationState));
        capturePageSourceSnapshotIfNeeded(driver, validationState, attachments);
    }

    // prepare page snapshot mhtml/html — property-gated (issue reported 2026-07-18): default
    // assertion evidence is a screenshot and nothing else, so "never" must suppress the
    // page-source attachment even on a failing validation. Previously the failure branch
    // (!validationState) bypassed the property entirely, attaching page source on every
    // failure regardless of the configured value.
    private static boolean shouldCapturePageSourceSnapshot(boolean validationState) {
        var whenToTakePageSourceSnapshot = SHAFT.Properties.visuals.whenToTakePageSourceSnapshot().toLowerCase();
        return switch (whenToTakePageSourceSnapshot) {
            case "never" -> false;
            case "always", "validationpointsonly" -> true;
            // "failuresonly" (and any legacy/unrecognized value) preserves the prior opt-in
            // behavior: capture on failure only.
            default -> !validationState;
        };
    }

    private void capturePageSourceSnapshotIfNeeded(WebDriver driver, boolean validationState, List<List<Object>> attachments) {
        if (!shouldCapturePageSourceSnapshot(validationState)) {
            return;
        }
        var logMessage = "";
        long profilerStart = FlakeProfiler.isEnabled() ? System.nanoTime() : 0L;
        var pageSnapshot = new BrowserActionsHelper(true).capturePageSnapshot(driver);
        if (profilerStart != 0L) {
            FlakeProfiler.recordEvidenceCapture("page snapshot", this.validationCategoryString,
                    TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - profilerStart));
        }
        if (pageSnapshot.startsWith("From: <Saved by Blink>")) {
            logMessage = "page snapshot";
        } else if (pageSnapshot.startsWith("<html")) {
            logMessage = "page HTML";
        }
        List<Object> pageSourceAttachment = Arrays.asList(this.validationCategoryString, logMessage, pageSnapshot);
        attachments.add(pageSourceAttachment);
    }

    // prepare testData attachments; always attach expected/actual so every checkpoint carries its evidence
    private void prepareTestDataAttachments(Object expected, Object actual, List<List<Object>> attachments) {
        List<Object> expectedValueAttachment = Arrays.asList("Validation Test Data", "Expected Value",
                String.valueOf(expected));
        List<Object> actualValueAttachment = Arrays.asList("Validation Test Data", "Actual Value",
                String.valueOf(actual));
        attachments.add(expectedValueAttachment);
        attachments.add(actualValueAttachment);
        ReportManager.logDiscrete("Expected and Actual values are attached.");
    }

    // Single primary assertion-evidence card (issue #3502 A+B): one HTML block carrying the
    // redacted expected/actual plus a diff, placed first so it headlines the Allure step. The
    // existing Expected/Actual text attachments stay (dual-write) so consumers that scrape
    // those names keep working. Skipped when the caller already attached authoritative rich
    // comparison evidence elsewhere (e.g. a Playwright visual-diff image) — issue #3804: the
    // card would otherwise redundantly restate a boolean/opaque result.
    private void attachEvidenceCardIfNeeded(boolean richEvidenceAlreadyAttached, boolean validationState,
                                            Object expected, Object actual, List<List<Object>> attachments) {
        if (richEvidenceAlreadyAttached) {
            return;
        }
        String evidenceCard = AssertionEvidenceReporter.renderCard(
                this.validationCategoryString, validationState, expected, actual);
        if (!evidenceCard.isBlank()) {
            attachments.add(0, Arrays.asList("Assertion evidence", "assertion-evidence.html", evidenceCard));
        }
    }

    // add attachments, profiling the report-attachment step when flake profiling is enabled
    private void attachEvidenceWithProfiling(List<List<Object>> attachments) {
        long profilerAttachmentStart = !attachments.isEmpty() && FlakeProfiler.isEnabled() ? System.nanoTime() : 0L;
        ReportManagerHelper.attach(attachments);
        if (profilerAttachmentStart != 0L) {
            FlakeProfiler.recordEvidenceCapture("report attachment", this.validationCategoryString,
                    TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - profilerAttachmentStart));
        }
    }

    private CheckpointType determineCheckpointType() {
        return this.validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT)
                ? CheckpointType.ASSERTION : CheckpointType.VERIFICATION;
    }

    private void routeValidationOutcome(boolean validationState, Object expected, Object actual, By locator,
                                        List<List<Object>> attachments, TraceEventRecorder.Event traceEvent,
                                        CheckpointType checkpointType, String checkpointMessage) {
        if (!validationState) {
            handleValidationFailure(expected, actual, locator, attachments, traceEvent, checkpointType, checkpointMessage);
        } else {
            handleValidationSuccess(expected, actual, locator, attachments, traceEvent, checkpointType, checkpointMessage);
        }
    }

    private void handleValidationFailure(Object expected, Object actual, By locator, List<List<Object>> attachments,
                                         TraceEventRecorder.Event traceEvent, CheckpointType checkpointType,
                                         String checkpointMessage) {
        String failureMessage = this.validationCategoryString.replace("erify", "erificat") + "ion failed; expected " + expected + ", but found " + actual;
        // Format failure message using CustomSoftAssert for enhanced stack trace reporting
        AssertionError assertionError = new AssertionError(failureMessage);
        // Automatically extract package pattern from stack trace
        String enhancedFailureMessage = formatAssertionErrorWithAutoDetectedPackage(assertionError);
        // Use enhanced message if available, otherwise fall back to original
        String finalFailureMessage = (enhancedFailureMessage != null) ? enhancedFailureMessage : failureMessage;

        CheckpointCounter.increment(checkpointType, checkpointMessage, CheckpointStatus.FAIL);
        TraceEventRecorder.finish(traceEvent, "failed",
                this.validationCategoryString.replace("erify", "erificat") + "ion failed",
                assertionError, traceMetadata(checkpointType, locator, expected, actual), summarizeAttachments(attachments));
        if (this.validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT)) {
            ExecutionSummaryReport.validationsIncrement(CheckpointStatus.FAIL);
            // single timed outcome step spanning the real validation duration, then fail hard
            ReportManagerHelper.writeStepToReport(finalFailureMessage, Level.ERROR, Status.FAILED, this.validationStartTime);
            throw new AssertionError(finalFailureMessage);
        } else {
            // soft assert: accumulate first so the running failure count is accurate, then
            // report the outcome step tagged with its running position, so soft failures are
            // easy to count at a glance and are visibly distinct from a hard assertion failure
            ValidationsHelper.recordVerificationFailure(failureMessage);
            ExecutionSummaryReport.validationsIncrement(CheckpointStatus.FAIL);
            int softFailureNumber = verificationFailuresList.get().size();
            ReportManagerHelper.writeStepToReport("Soft failure #" + softFailureNumber + " — " + finalFailureMessage,
                    Level.ERROR, Status.FAILED, this.validationStartTime);
        }
    }

    private void handleValidationSuccess(Object expected, Object actual, By locator, List<List<Object>> attachments,
                                         TraceEventRecorder.Event traceEvent, CheckpointType checkpointType,
                                         String checkpointMessage) {
        CheckpointCounter.increment(checkpointType, checkpointMessage, CheckpointStatus.PASS);
        ExecutionSummaryReport.validationsIncrement(CheckpointStatus.PASS);
        ReportManagerHelper.writeStepToReport(this.validationCategoryString.replace("erify", "erificat") + "ion passed",
                Level.INFO, Status.PASSED, this.validationStartTime);
        TraceEventRecorder.finish(traceEvent, "passed",
                this.validationCategoryString.replace("erify", "erificat") + "ion passed",
                null, traceMetadata(checkpointType, locator, expected, actual), summarizeAttachments(attachments));
    }

    private static Map<String, String> traceMetadata(CheckpointType checkpointType, By locator,
                                                     Object expected, Object actual) {
        Map<String, String> metadata = new LinkedHashMap<>();
        metadata.put("checkpointType", checkpointType.name().toLowerCase());
        metadata.put("expected", truncateForTrace(expected));
        metadata.put("actual", truncateForTrace(actual));
        if (locator != null) {
            metadata.put("locator", JavaHelper.formatLocatorToString(locator));
        }
        return metadata;
    }

    /**
     * Bounds expected/actual values embedded in trace metadata so page-sized comparisons cannot
     * bloat the trace bundle; the full values remain available as report attachments.
     */
    private static String truncateForTrace(Object value) {
        String text = String.valueOf(value);
        return text.length() <= 500 ? text : text.substring(0, 500) + "… [truncated]";
    }

    private static List<String> summarizeAttachments(List<List<Object>> attachments) {
        if (attachments == null || attachments.isEmpty()) {
            return List.of();
        }
        List<String> summaries = new ArrayList<>();
        for (List<Object> attachment : attachments) {
            if (attachment == null || attachment.isEmpty()) {
                continue;
            }
            String description = String.valueOf(attachment.getFirst());
            String name = attachment.size() > 1 ? String.valueOf(attachment.get(1)) : "";
            Object payload = attachment.size() > 2 ? attachment.get(2) : null;
            summaries.add(description + (name.isBlank() ? "" : " - " + name) + payloadSize(payload));
        }
        return summaries;
    }

    private static String payloadSize(Object payload) {
        if (payload instanceof byte[] bytes) {
            return " (" + bytes.length + " bytes)";
        }
        if (payload instanceof CharSequence text) {
            return " (" + text.length() + " chars)";
        }
        return "";
    }
}
