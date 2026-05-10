package com.shaft.validation.internal;

import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.CheckpointCounter;
import com.shaft.tools.io.internal.CheckpointStatus;
import com.shaft.tools.io.internal.CheckpointType;
import com.shaft.tools.io.internal.ExecutionSummaryReport;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.ValidationEnums;
import io.qameta.allure.Allure;
import io.qameta.allure.model.Parameter;
import org.apache.logging.log4j.Level;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * Primitive validation execution that does not depend on shaft-web (Selenium / WebDriver).
 *
 * <p>Lives in shaft-core so that lean consumers (shaft-api only, shaft-db only) can use
 * {@code Validations.assertThat().object/number(...).isEqualTo()/perform()} without needing
 * shaft-web on the classpath.
 *
 * <p>The web-specific extension {@code com.shaft.validation.internal.ValidationsHelper2} in
 * shaft-web extends this class and adds the element/browser validation methods.
 */
public class PrimitiveValidationsHelper {

    protected final ValidationEnums.ValidationCategory validationCategory;
    protected final String validationCategoryString;

    public PrimitiveValidationsHelper(ValidationEnums.ValidationCategory validationCategory) {
        this.validationCategory = validationCategory;
        this.validationCategoryString = validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT)
                ? "Assert" : "Verify";
    }

    public void validateEquals(Object expected, Object actual,
                                ValidationEnums.ValidationComparisonType comparisonType,
                                ValidationEnums.ValidationType validationType) {
        String comparisonTypeStr = ValidationEnums.ValidationType.NEGATIVE.name().equals(validationType.name())
                ? "not " + comparisonType.name() : comparisonType.name();
        var parameters = new LinkedHashMap<>(setCommonParameters(expected, actual, comparisonTypeStr));
        updateAllureParameters(parameters);
        boolean validationState = performValidation(expected, actual, comparisonType, validationType);
        reportPrimitiveValidationState(validationState, expected, actual, null);
    }

    public void validateNumber(Number expected, Number actual,
                                ValidationEnums.NumbersComparativeRelation comparisonType,
                                ValidationEnums.ValidationType validationType) {
        boolean validationState = performValidation(expected, actual, comparisonType, validationType);
        String comparisonTypeStr = ValidationEnums.ValidationType.NEGATIVE.name().equals(validationType.name())
                ? "not " + comparisonType.name() : comparisonType.name();
        var parameters = new LinkedHashMap<>(setCommonParameters(expected, actual, comparisonTypeStr));
        updateAllureParameters(parameters);
        reportPrimitiveValidationState(validationState, expected, actual, null);
    }

    // ── Shared utilities — moved from ValidationsHelper2 ──────────────────────────

    protected LinkedHashMap<String, String> setCommonParameters(Object expected, Object actual, String comparisonType) {
        var commonParams = new LinkedHashMap<String, String>();
        commonParams.put("Expected value", String.valueOf(expected));
        commonParams.put("Comparison type", JavaHelper.convertToSentenceCase(comparisonType));
        commonParams.put("Actual value", String.valueOf(actual));
        return commonParams;
    }

    protected void updateAllureParameters(LinkedHashMap<String, String> parameters) {
        List<Parameter> params = new ArrayList<>();
        parameters.forEach((key, value) -> params.add(new Parameter().setName(key).setValue(String.valueOf(value)).setMode(Parameter.Mode.DEFAULT)));
        Allure.getLifecycle().updateStep(stepResult -> stepResult.setParameters(params));
    }

    protected boolean performValidation(Object expected, Object actual,
                                         Object comparisonType, ValidationEnums.ValidationType validationType) {
        int comparisonResult = 0;
        if (comparisonType instanceof ValidationEnums.ValidationComparisonType validationComparisonType) {
            comparisonResult = JavaHelper.compareTwoObjects(expected, actual,
                    validationComparisonType.getValue(), validationType.getValue());
        } else if (comparisonType instanceof ValidationEnums.NumbersComparativeRelation numbersComparativeRelation) {
            comparisonResult = JavaHelper.compareTwoObjects(expected, actual,
                    numbersComparativeRelation, validationType.getValue());
        }
        return comparisonResult == 1
                ? ValidationEnums.ValidationState.PASSED.getValue()
                : ValidationEnums.ValidationState.FAILED.getValue();
    }

    /**
     * Reports the outcome of a primitive validation. Handles testData attachments, checkpoint
     * counters, and hard/soft assertion failure paths. Does NOT take a WebDriver — that is the
     * web subclass's responsibility ({@code ValidationsHelper2.reportWebValidationState}).
     */
    protected void reportPrimitiveValidationState(boolean validationState, Object expected, Object actual,
                                                   List<List<Object>> attachments) {
        attachments = attachments == null ? new ArrayList<>() : attachments;
        if (isExpectedOrActualValueLong(String.valueOf(expected), String.valueOf(actual))) {
            attachments.add(Arrays.asList("Validation Test Data", "Expected Value", expected));
            attachments.add(Arrays.asList("Validation Test Data", "Actual Value", actual));
            ReportManager.logDiscrete("Expected and Actual values are attached.");
        }
        ReportManagerHelper.attach(attachments);

        CheckpointType checkpointType = this.validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT)
                ? CheckpointType.ASSERTION : CheckpointType.VERIFICATION;
        String checkpointMessage = this.validationCategoryString + ": expected \"" + expected + "\", actual \"" + actual + "\"";
        ReportManager.logDiscrete("Expected \"" + expected + "\", and actual \"" + actual + "\"", Level.DEBUG);

        if (!validationState) {
            String failureMessage = this.validationCategoryString.replace("erify", "erificat")
                    + "ion failed; expected " + expected + ", but found " + actual;
            AssertionError assertionError = new AssertionError(failureMessage);
            String enhancedFailureMessage = formatAssertionErrorWithAutoDetectedPackage(assertionError);
            String finalFailureMessage = (enhancedFailureMessage != null) ? enhancedFailureMessage : failureMessage;

            CheckpointCounter.increment(checkpointType, checkpointMessage, CheckpointStatus.FAIL);
            if (this.validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT)) {
                ExecutionSummaryReport.validationsIncrement(CheckpointStatus.FAIL);
                Allure.getLifecycle().updateStep(stepResult -> FailureReporter.fail(finalFailureMessage));
            } else {
                appendSoftAssertFailure(failureMessage, finalFailureMessage);
            }
        } else {
            CheckpointCounter.increment(checkpointType, checkpointMessage, CheckpointStatus.PASS);
            ExecutionSummaryReport.validationsIncrement(CheckpointStatus.PASS);
            Allure.getLifecycle().updateStep(stepResult -> ReportManager.log(this.validationCategoryString.replace("erify", "erificat") + "ion passed"));
        }
    }

    /**
     * Appends a soft-assert failure to the shaft-web {@code ValidationsHelper} accumulator
     * (via reflection — same pattern shaft-core's {@code IssueReporter} uses). When shaft-web is
     * NOT on the classpath the accumulator does not exist; in that case we just log the
     * failure and move on, since the suite-end drain mechanism that consumes the list also
     * lives in shaft-web and would not run anyway.
     */
    private void appendSoftAssertFailure(String failureMessage, String finalFailureMessage) {
        try {
            Class<?> v1 = Class.forName("com.shaft.validation.internal.ValidationsHelper",
                    false, Thread.currentThread().getContextClassLoader());
            java.lang.reflect.Field listField = v1.getDeclaredField("verificationFailuresList");
            listField.setAccessible(true);
            ThreadLocal<?> listTl = (ThreadLocal<?>) listField.get(null);
            @SuppressWarnings("unchecked")
            List<String> list = (List<String>) listTl.get();
            list.add(failureMessage);
            java.lang.reflect.Field errField = v1.getDeclaredField("verificationError");
            errField.setAccessible(true);
            @SuppressWarnings("unchecked")
            ThreadLocal<Object> errTl = (ThreadLocal<Object>) errField.get(null);
            errTl.set(new AssertionError(String.join("\nAND ", list)));
        } catch (ClassNotFoundException ignored) {
            // lean scope — accumulator unavailable, just log the failure
            ReportManager.logDiscrete("Soft-assert failure (shaft-web absent — failure tracking limited): " + failureMessage);
        } catch (ReflectiveOperationException e) {
            ReportManagerHelper.logDiscrete(e);
        }
        ExecutionSummaryReport.validationsIncrement(CheckpointStatus.FAIL);
        Allure.getLifecycle().updateStep(stepResult -> ReportManager.log(finalFailureMessage));
    }

    /**
     * Pure string-length check inlined from the now-removed
     * {@code ValidationsHelper.isExpectedOrActualValueLong} in shaft-web,
     * so that shaft-core does not compile-depend on shaft-web.
     */
    private static boolean isExpectedOrActualValueLong(String expectedValue, String actualValue) {
        return (expectedValue != null && expectedValue.length() >= 500)
                || (actualValue != null && actualValue.length() >= 500);
    }

    /**
     * Walks the stack trace to find the first non-framework class, then uses
     * {@link CustomSoftAssert#formatFailureWithStackTrace(AssertionError, String)} to format
     * the assertion error message with a clickable trace anchored at the test code package.
     *
     * <p>{@code protected static} so {@link ValidationsHelper2} in shaft-web can reuse the
     * same formatter for the driver-aware reporting path.
     */
    protected static String formatAssertionErrorWithAutoDetectedPackage(AssertionError error) {
        if (error == null) return null;
        StackTraceElement[] stackTrace = error.getStackTrace();
        String[] frameworkPackages = {"org.testng", "java.", "jdk.", "com.shaft.validation.internal",
                "com.shaft.validation", "com.shaft.tools", "com.shaft.driver", "com.shaft.gui"};
        String[] genericTlds = {"com", "org", "net", "java", "jdk"};
        for (StackTraceElement element : stackTrace) {
            String className = element.getClassName();
            boolean isFrameworkPackage = false;
            for (String frameworkPkg : frameworkPackages) {
                if (className.startsWith(frameworkPkg)) {
                    isFrameworkPackage = true;
                    break;
                }
            }
            if (!isFrameworkPackage && element.getLineNumber() > 0) {
                int lastDotIndex = className.lastIndexOf('.');
                if (lastDotIndex > 0) {
                    String packageName = className.substring(0, lastDotIndex);
                    String formatted = CustomSoftAssert.formatFailureWithStackTrace(error, packageName);
                    if (formatted != null) return formatted;
                    String firstPackageSegment = packageName.split("\\.")[0];
                    String[] commonPatterns = {"tests", "test"};
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
                        formatted = CustomSoftAssert.formatFailureWithStackTrace(error, pattern);
                        if (formatted != null) return formatted;
                    }
                }
            }
        }
        return null;
    }
}
