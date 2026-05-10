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
import org.json.JSONException;
import org.json.JSONObject;
import org.openqa.selenium.*;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.remote.Browser;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public class ValidationsHelper2 extends PrimitiveValidationsHelper {

    public ValidationsHelper2(ValidationEnums.ValidationCategory validationCategory) {
        super(validationCategory);
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
        reportWebValidationState(validationState.get(), expected, actual, driver, null, null);
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
        reportWebValidationState(validationState.get(), expected, actual, driver, locator, null);
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
        reportWebValidationState(validationState.get(), expected, actual, driver, locator, null);
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
                    case "textdirection" -> getElementTextDirection(driver, locator);
                    case "textalignment" -> getElementTextAlignmentDirection(driver, locator);
                    case "textorientation" -> getElementTextOrientationDirection(driver, locator);
                    case "textdisplaystyle" -> getElementTextDisplayStyleDirection(driver, locator);
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
        reportWebValidationState(validationState.get(), expected, actual, driver, locator, null);
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
        reportWebValidationState(validationState.get(), expected, actual, driver, locator, null);
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
        reportWebValidationState(validationState.get(), expected, actual, driver, elementCount.get() == 0 ? null : locator, null);
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
        reportWebValidationState(validationState.get(), expected, actual, driver, elementCount.get() == 0 ? null : locator, attachments);
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

    /**
     * Driver-aware reporting for web validation methods. The primitive (no-driver) path is
     * handled by {@code super.reportPrimitiveValidationState(...)} in shaft-core.
     */
    private void reportWebValidationState(boolean validationState, Object expected, Object actual,
                                           WebDriver driver, By locator, List<List<Object>> attachments) {
        if (driver == null) {
            // No driver provided — delegate to the primitive (no-driver) path in the base class.
            super.reportPrimitiveValidationState(validationState, expected, actual, attachments);
            return;
        }
        attachments = attachments == null ? new ArrayList<>() : attachments;
        if (attachments.isEmpty()) {
            attachments.add(new ScreenshotManager().takeScreenshot(driver, locator, this.validationCategoryString, validationState));
        }
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
            attachments.add(Arrays.asList(this.validationCategoryString, logMessage, pageSnapshot));
        }
        ReportManagerHelper.attach(attachments);
        CheckpointType checkpointType = this.validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT)
                ? CheckpointType.ASSERTION : CheckpointType.VERIFICATION;
        String checkpointMessage = this.validationCategoryString + ": expected \"" + expected + "\", actual \"" + actual + "\"";
        ReportManager.logDiscrete("Expected \"" + expected + "\", and actual \"" + actual + "\"", Level.DEBUG);
        if (!validationState) {
            String failureMessage = this.validationCategoryString.replace("erify", "erificat") + "ion failed; expected " + expected + ", but found " + actual;
            AssertionError assertionError = new AssertionError(failureMessage);
            String enhancedFailureMessage = formatAssertionErrorWithAutoDetectedPackage(assertionError);
            String finalFailureMessage = (enhancedFailureMessage != null) ? enhancedFailureMessage : failureMessage;

            CheckpointCounter.increment(checkpointType, checkpointMessage, CheckpointStatus.FAIL);
            if (this.validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT)) {
                ExecutionSummaryReport.validationsIncrement(CheckpointStatus.FAIL);
                Allure.getLifecycle().updateStep(stepResult -> FailureReporter.fail(finalFailureMessage));
            } else {
                // soft assert — append to the V1 failure accumulator (shared with primitive helper)
                ValidationsHelper.verificationFailuresList.get().add(failureMessage);
                ValidationsHelper.verificationError.set(new AssertionError(String.join("\nAND ", ValidationsHelper.verificationFailuresList.get())));
                ExecutionSummaryReport.validationsIncrement(CheckpointStatus.FAIL);
                Allure.getLifecycle().updateStep(stepResult -> ReportManager.log(finalFailureMessage));
            }
        } else {
            CheckpointCounter.increment(checkpointType, checkpointMessage, CheckpointStatus.PASS);
            ExecutionSummaryReport.validationsIncrement(CheckpointStatus.PASS);
            Allure.getLifecycle().updateStep(stepResult -> ReportManager.log(this.validationCategoryString.replace("erify", "erificat") + "ion passed"));
        }
    }
}
