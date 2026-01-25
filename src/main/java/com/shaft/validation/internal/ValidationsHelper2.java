package com.shaft.validation.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.internal.BrowserActionsHelper;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.internal.Actions;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.CheckpointStatus;
import com.shaft.tools.io.internal.ExecutionSummaryReport;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.constants.CustomSoftAssert;
import io.qameta.allure.Allure;
import io.qameta.allure.model.Parameter;
import org.apache.logging.log4j.Level;
import org.json.JSONException;
import org.json.JSONObject;
import org.openqa.selenium.*;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.remote.Browser;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public class ValidationsHelper2 {
    private final ValidationEnums.ValidationCategory validationCategory;
    private final String validationCategoryString;

    ValidationsHelper2(ValidationEnums.ValidationCategory validationCategory) {
        this.validationCategory = validationCategory;
        this.validationCategoryString = validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT) ? "Assert" : "Verify";
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
                                      "com.shaft.tools", "com.shaft.driver", "com.shaft.gui"};
        
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
                    String formatted = CustomSoftAssert.formatFailureWithStackTrace(error, packageName);
                    if (formatted != null) {
                        return formatted;
                    }
                    // If exact package doesn't work, try with common patterns
                    String[] commonPatterns = {packageName.split("\\.")[0], "tests", "test"};
                    for (String pattern : commonPatterns) {
                        formatted = CustomSoftAssert.formatFailureWithStackTrace(error, pattern);
                        if (formatted != null) {
                            return formatted;
                        }
                    }
                }
            }
        }
        
        return null;
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
                actual.set(new Actions(driver, true).get().domProperty(locator, domProperty));
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
                actual.set(switch (attribute.toLowerCase()) {
                    case "text" -> new Actions(driver, true).get().text(locator);
                    case "texttrimmed", "trimmedtext" -> new Actions(driver, true).get().text(locator).trim();
                    case "selectedtext" -> new Actions(driver, true).get().selectedText(locator);
                    default -> new Actions(driver, true).get().attribute(locator, attribute);
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
                actual.set(switch (attribute.toLowerCase()) {
                    case "text" -> new Actions(driver, true).get().text(locator);
                    case "texttrimmed", "trimmedtext" -> new Actions(driver, true).get().text(locator).trim();
                    case "selectedtext" -> new Actions(driver, true).get().selectedText(locator);
                    default -> new Actions(driver, true).get().domAttribute(locator, attribute);
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
                actual.set(new Actions(driver, true).get().cssValue(locator, property));
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

        try {
            //https://github.com/assertthat/selenium-shutterbug/issues/105
            if (Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
                internalVisualEngine.set(ValidationEnums.VisualValidationEngine.EXACT_OPENCV);
            }

            // get reference image
            byte[] referenceImage = ImageProcessingActions.getReferenceImage(locator);
            if (referenceImage !=null && !Arrays.equals(new byte[0], referenceImage)) {
                ReportManagerHelper.logDiscrete("Reference image found.", Level.INFO);
                List<Object> expectedValueAttachment = Arrays.asList("Validation Test Data", "Reference Screenshot",
                        referenceImage);
                attachments.add(expectedValueAttachment);
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

                List<Object> actualValueAttachment = Arrays.asList("Validation Test Data", "Actual Screenshot",
                        elementScreenshot);
                attachments.add(actualValueAttachment);

                // prepare content for allure attachment
                var content = new JSONObject();
                byte[] shutterbugDifferencesImage = new byte[0];

                // compare actual and reference screenshots
                boolean isDifferencesImageApplicable = visualValidationEngine.equals(ValidationEnums.VisualValidationEngine.EXACT_SHUTTERBUG) && !actualResult;
                if (isDifferencesImageApplicable) {
                    //if shutterbug and failed, get differences screenshot
                    shutterbugDifferencesImage = ImageProcessingActions.getShutterbugDifferencesImage(locator);
                    if (!Arrays.equals(new byte[0], shutterbugDifferencesImage)) {
                        List<Object> differencesAttachment = Arrays.asList("Validation Test Data", "Differences",
                                shutterbugDifferencesImage);
                        attachments.add(differencesAttachment);
                    }
                }

                if (referenceImage != null) {
                    try {
                        // prepare content for allure attachment
                        content.put("expected", "data:image/png;base64,"
                                + Base64.getEncoder().encodeToString(referenceImage))
                                .put("actual", "data:image/png;base64,"
                                + Base64.getEncoder().encodeToString(elementScreenshot));
                        if (isDifferencesImageApplicable && !Arrays.equals(new byte[0], shutterbugDifferencesImage))
                            content.put("diff", "data:image/png;base64,"
                                    + Base64.getEncoder().encodeToString(shutterbugDifferencesImage));

                        Allure.addAttachment("Screenshot diff", "application/vnd.allure.image.diff", content.toString());
                    } catch (JSONException jsonException) {
                        //failed to add differences image to allure attachment, ignore it
                    }
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
        reportValidationState(validationState.get(), expected, actual, driver, elementCount.get() == 0 ? null : locator, attachments);
    }

    private LinkedHashMap<String, String> setCommonParameters(Object expected, Object actual, String comparisonType) {
        var commonParams = new LinkedHashMap<String, String>();
        commonParams.put("Expected value", String.valueOf(expected));
        commonParams.put("Comparison type", JavaHelper.convertToSentenceCase(comparisonType));
        commonParams.put("Actual value", String.valueOf(actual));
        return commonParams;
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

    private void reportValidationState(boolean validationState, Object expected, Object actual, WebDriver driver, By locator, List<List<Object>> attachments) {
        //initialize attachments object if no attachments were already prepared
        attachments = attachments == null ? new ArrayList<>() : attachments;

        // prepare WebDriver attachments
        if (driver != null) {
            // prepare screenshot with element highlighting
            if (attachments.isEmpty())
                attachments.add(new ScreenshotManager().takeScreenshot(driver, locator, this.validationCategoryString, validationState));
            // prepare page snapshot mhtml/html
            var whenToTakePageSourceSnapshot = SHAFT.Properties.visuals.whenToTakePageSourceSnapshot().toLowerCase();
            if (!validationState
                    || Arrays.asList("always", "validationpointsonly").contains(whenToTakePageSourceSnapshot)) {
                var logMessage = "";
                var pageSnapshot = new BrowserActionsHelper(true).capturePageSnapshot(driver);
                if (pageSnapshot.startsWith("From: <Saved by Blink>")) {
                    logMessage = "page snapshot";
                } else if (pageSnapshot.startsWith("<html")) {
                    logMessage = "page HTML";
                }
                List<Object> pageSourceAttachment = Arrays.asList(this.validationCategoryString, logMessage, pageSnapshot);
                attachments.add(pageSourceAttachment);
            }
        } else {
            // prepare testData attachments
            boolean isExpectedOrActualValueLong = ValidationsHelper.isExpectedOrActualValueLong(String.valueOf(expected), String.valueOf(actual));
            if (isExpectedOrActualValueLong) {
                List<Object> expectedValueAttachment = Arrays.asList("Validation Test Data", "Expected Value",
                        expected);
                List<Object> actualValueAttachment = Arrays.asList("Validation Test Data", "Actual Value", actual);
                attachments.add(expectedValueAttachment);
                attachments.add(actualValueAttachment);
                ReportManager.logDiscrete("Expected and Actual values are attached.");
            }
        }
        // add attachments
        ReportManagerHelper.attach(attachments);
        // handle reporting & failure based on validation category
        ReportManager.logDiscrete("Expected \"" + expected + "\", and actual \"" + actual + "\"");
        if (!validationState) {
            String failureMessage = this.validationCategoryString.replace("erify", "erificat") + "ion failed; expected " + expected + ", but found " + actual;
            // Format failure message using CustomSoftAssert for enhanced stack trace reporting
            AssertionError assertionError = new AssertionError(failureMessage);
            // Automatically extract package pattern from stack trace
            String enhancedFailureMessage = formatAssertionErrorWithAutoDetectedPackage(assertionError);
            // Use enhanced message if available, otherwise fall back to original
            String finalFailureMessage = (enhancedFailureMessage != null) ? enhancedFailureMessage : failureMessage;
            
            if (this.validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT)) {
                ExecutionSummaryReport.validationsIncrement(CheckpointStatus.FAIL);
                Allure.getLifecycle().updateStep(stepResult -> FailureReporter.fail(finalFailureMessage));
            } else {
                // soft assert - use formatted message
                ValidationsHelper.verificationFailuresList.add(finalFailureMessage);
                ValidationsHelper.verificationError = new AssertionError(String.join("\nAND ", ValidationsHelper.verificationFailuresList));
                ExecutionSummaryReport.validationsIncrement(CheckpointStatus.FAIL);
                Allure.getLifecycle().updateStep(stepResult -> ReportManager.log(finalFailureMessage));
            }
        } else {
            ExecutionSummaryReport.validationsIncrement(CheckpointStatus.PASS);
            Allure.getLifecycle().updateStep(stepResult -> ReportManager.log(this.validationCategoryString.replace("erify", "erificat") + "ion passed"));
        }
    }
}
