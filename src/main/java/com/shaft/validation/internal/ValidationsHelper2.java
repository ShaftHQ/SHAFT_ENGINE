package com.shaft.validation.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.internal.BrowserActionsHelper;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.gui.element.internal.ElementInformation;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.ValidationEnums;
import io.qameta.allure.Allure;
import io.qameta.allure.model.Parameter;
import org.openqa.selenium.By;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
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

    protected void validateEquals(Object expected, Object actual,
                                  ValidationEnums.ValidationComparisonType type, ValidationEnums.ValidationType validation) {
        // read actual value based on desired attribute
        // Note: do not try/catch this block as the upstream failure will already be reported along with any needed attachments

        //reporting block
        List<Parameter> parameters = new ArrayList<>();
        parameters.add(new Parameter().setName("Expected value").setValue(String.valueOf(expected)).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Actual value").setValue(String.valueOf(actual)).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Comparison type").setValue(JavaHelper.convertToSentenceCase(String.valueOf(type))).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Validation").setValue(JavaHelper.convertToSentenceCase(String.valueOf(validation))).setMode(Parameter.Mode.DEFAULT));
        Allure.getLifecycle().updateStep(stepResult -> stepResult.setParameters(parameters));
        //end of reporting block
        boolean validationState = performValidation(expected, actual, type, validation);
        reportValidationState(validationState, expected, actual, null, null);
    }

    protected void validateNumber(Number expected, Number actual,
                                  ValidationEnums.NumbersComparativeRelation type, ValidationEnums.ValidationType validation) {
        // read actual value based on desired attribute
        // Note: do not try/catch this block as the upstream failure will already be reported along with any needed attachments
        boolean validationState = performValidation(expected, actual, type, validation);
        //reporting block
        List<Parameter> parameters = new ArrayList<>();
        parameters.add(new Parameter().setName("Expected value").setValue(String.valueOf(expected)).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Actual value").setValue(String.valueOf(actual)).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Comparison type").setValue(JavaHelper.convertToSentenceCase(String.valueOf(type))).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Validation").setValue(JavaHelper.convertToSentenceCase(String.valueOf(validation))).setMode(Parameter.Mode.DEFAULT));
        Allure.getLifecycle().updateStep(stepResult -> stepResult.setParameters(parameters));
        reportValidationState(validationState, expected, actual, null, null);
    }

    protected void validateBrowserAttribute(WebDriver driver, String attribute,
                                            String expected, ValidationEnums.ValidationComparisonType type, ValidationEnums.ValidationType validation) {
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
                validationState.set(performValidation(expected, actual.get(), type, validation));
                return validationState.get();
            });
        } catch (TimeoutException timeoutException) {
            //timeout was exhausted and the validation failed
        }
        //reporting block
        List<Parameter> parameters = new ArrayList<>();
        parameters.add(new Parameter().setName("Attribute").setValue(attribute).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Expected value").setValue(String.valueOf(expected)).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Actual value").setValue(String.valueOf(actual)).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Comparison type").setValue(JavaHelper.convertToSentenceCase(String.valueOf(type))).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Validation").setValue(JavaHelper.convertToSentenceCase(String.valueOf(validation))).setMode(Parameter.Mode.DEFAULT));
        Allure.getLifecycle().updateStep(stepResult -> stepResult.setParameters(parameters));
        reportValidationState(validationState.get(), expected, actual, driver, null);
    }

    protected void validateElementAttribute(WebDriver driver, By locator, String attribute,
                                            String expected, ValidationEnums.ValidationComparisonType type, ValidationEnums.ValidationType validation) {
        // read actual value based on desired attribute
        // Note: do not try/catch this block as the upstream failure will already be reported along with any needed attachments
        var elementInformation = ElementInformation.fromList(new ElementActionsHelper(true).identifyUniqueElementIgnoringVisibility(driver, locator));

        AtomicReference<String> actual = new AtomicReference<>();
        AtomicReference<Boolean> validationState = new AtomicReference<>();

        try {
            new SynchronizationManager(driver).fluentWait(false).until(f -> {
                actual.set(switch (attribute.toLowerCase()) {
                    case "text" -> new ElementActions(driver, true).getText(locator);
                    case "texttrimmed", "trimmedtext" -> new ElementActions(driver, true).getText(locator).trim();
                    case "tagname" -> elementInformation.getElementTag();
                    case "size" -> elementInformation.getFirstElement().getSize().toString();
                    case "selectedtext" -> new ElementActions(driver, true).getSelectedText(locator);
                    default -> new ElementActions(driver, true).getAttribute(locator, attribute);
                });
                validationState.set(performValidation(expected, actual.get(), type, validation));
                return validationState.get();
            });
        } catch (TimeoutException timeoutException) {
            //timeout was exhausted and the validation failed
        }
        //reporting block
        List<Parameter> parameters = new ArrayList<>();
        parameters.add(new Parameter().setName("Locator").setValue(String.valueOf(locator)).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Attribute").setValue(attribute).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Expected value").setValue(String.valueOf(expected)).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Actual value").setValue(String.valueOf(actual)).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Comparison type").setValue(JavaHelper.convertToSentenceCase(String.valueOf(type))).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Validation").setValue(JavaHelper.convertToSentenceCase(String.valueOf(validation))).setMode(Parameter.Mode.DEFAULT));
        Allure.getLifecycle().updateStep(stepResult -> stepResult.setParameters(parameters));
        reportValidationState(validationState.get(), expected, actual, driver, locator);
    }

    protected void validateElementCSSProperty(WebDriver driver, By locator, String property,
                                              String expected, ValidationEnums.ValidationComparisonType type, ValidationEnums.ValidationType validation) {
        // read actual value based on desired css property
        // Note: do not try/catch this block as the upstream failure will already be reported along with any needed attachments
        AtomicReference<String> actual = new AtomicReference<>();
        AtomicReference<Boolean> validationState = new AtomicReference<>();

        try {
            new SynchronizationManager(driver).fluentWait(false).until(f -> {
                actual.set(new ElementActions(driver, true).getCSSProperty(locator, property));
                validationState.set(performValidation(expected, actual.get(), type, validation));
                return validationState.get();
            });
        } catch (TimeoutException timeoutException) {
            //timeout was exhausted and the validation failed
        }
        //reporting block
        List<Parameter> parameters = new ArrayList<>();
        parameters.add(new Parameter().setName("Locator").setValue(String.valueOf(locator)).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("CSS Property").setValue(property).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Expected value").setValue(String.valueOf(expected)).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Actual value").setValue(String.valueOf(actual)).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Comparison type").setValue(JavaHelper.convertToSentenceCase(String.valueOf(type))).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Validation").setValue(JavaHelper.convertToSentenceCase(String.valueOf(validation))).setMode(Parameter.Mode.DEFAULT));
        Allure.getLifecycle().updateStep(stepResult -> stepResult.setParameters(parameters));
        reportValidationState(validationState.get(), expected, actual, driver, locator);
    }

    protected void validateElementExists(WebDriver driver, By locator,
                                         ValidationEnums.ValidationType validation) {
        // read actual value based on desired existing state
        // Note: do not try/catch this block as the upstream failure will already be reported along with any needed attachments
        AtomicBoolean expected = new AtomicBoolean(false);
        AtomicBoolean actual = new AtomicBoolean(false);
        AtomicBoolean validationState = new AtomicBoolean(false);
        AtomicInteger elementCount = new AtomicInteger();

        try {
            new SynchronizationManager(driver).fluentWait(false).until(f -> {
                elementCount.set(new ElementActions(driver, true).getElementsCount(locator));
                expected.set(validation.getValue());
                actual.set(elementCount.get() > 0);
                // force validation type to be positive since the expected and actual values have been adjusted already
                validationState.set(performValidation(expected.get(), actual.get(), ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE));
                return validationState.get();
            });
        } catch (TimeoutException timeoutException) {
            //timeout was exhausted and the validation failed
        }
        //reporting block
        List<Parameter> parameters = new ArrayList<>();
        parameters.add(new Parameter().setName("Locator").setValue(String.valueOf(locator)).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Should exist").setValue(String.valueOf(expected.get())).setMode(Parameter.Mode.DEFAULT));
        parameters.add(new Parameter().setName("Actual value").setValue(String.valueOf(actual.get())).setMode(Parameter.Mode.DEFAULT));
        Allure.getLifecycle().updateStep(stepResult -> stepResult.setParameters(parameters));
        // force take page screenshot, (rather than element highlighted screenshot)
        reportValidationState(validationState.get(), expected, actual, driver, elementCount.get() == 0 ? null : locator);
    }

    private boolean performValidation(Object expected, Object actual,
                                      Object type, ValidationEnums.ValidationType validation) {
        // compare actual and expected results
        int comparisonResult = 0;
        if (type instanceof ValidationEnums.ValidationComparisonType validationComparisonType) {
            // comparison integer is used for all string-based, null, boolean, and Object comparisons
            comparisonResult = JavaHelper.compareTwoObjects(expected, actual,
                    validationComparisonType.getValue(), validation.getValue());
        } else if (type instanceof ValidationEnums.NumbersComparativeRelation numbersComparativeRelation) {
            // this means that it is a number-based comparison
            comparisonResult = JavaHelper.compareTwoObjects(expected, actual,
                    numbersComparativeRelation, validation.getValue());
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

    private void reportValidationState(boolean validationState, Object expected, Object actual, WebDriver driver, By locator) {
        List<List<Object>> attachments = new ArrayList<>();
        // prepare WebDriver attachments
        if (driver != null) {
            // prepare screenshot with element highlighting
            attachments.add(new ScreenshotManager().takeScreenshot(driver, locator, this.validationCategoryString, validationState));
            // prepare page snapshot mhtml/html
            var whenToTakePageSourceSnapshot = SHAFT.Properties.visuals.whenToTakePageSourceSnapshot().toLowerCase();
            if (Boolean.FALSE.equals(validationState)
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
            if (Boolean.TRUE.equals(isExpectedOrActualValueLong)) {
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
            if (this.validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT)) {
                Allure.getLifecycle().updateStep(stepResult -> FailureReporter.fail(failureMessage));
            } else {
                // soft assert
                ValidationsHelper.verificationFailuresList.add(failureMessage);
                ValidationsHelper.verificationError = new AssertionError(String.join("\nAND ", ValidationsHelper.verificationFailuresList));
                Allure.getLifecycle().updateStep(stepResult -> ReportManager.log(failureMessage));
            }
        } else {
            Allure.getLifecycle().updateStep(stepResult -> ReportManager.log(this.validationCategoryString.replace("erify", "erificat") + "ion passed"));
        }
    }
}
