package com.shaft.validation.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.internal.BrowserActionsHelper;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.ValidationEnums;
import io.qameta.allure.Step;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ValidationsHelper2 {
    private final ValidationEnums.ValidationCategory validationCategory;
    private final String validationCategoryString;

    ValidationsHelper2(ValidationEnums.ValidationCategory validationCategory) {
        this.validationCategory = validationCategory;
        this.validationCategoryString = validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT) ? "Assert" : "Verify";
    }

    @Step(" {this.validationCategoryString} that {customReportMessage}")
    protected void validateElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
                                            String expectedValue, ValidationEnums.ValidationComparisonType validationComparisonType, ValidationEnums.ValidationType validationType, String customReportMessage) {
        var isDiscrete = ReportManagerHelper.getDiscreteLogging();
        String actualValue = "";

        // convert internal steps to log entries to reduce report footprint
        ReportManagerHelper.setDiscreteLogging(true);

        // read actual value based on desired attribute
        // Note: do not try/catch this block as the upstream failure will already be reported along with any needed attachments
        actualValue = switch (elementAttribute.toLowerCase()) {
            case "text" -> new ElementActions(driver).getText(elementLocator);
            case "texttrimmed" -> new ElementActions(driver).getText(elementLocator).trim();
            case "tagname" ->
                    ((WebElement) ElementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator).get(1)).getTagName();
            case "size" ->
                    ((WebElement) ElementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator).get(1)).getSize().toString();
            case "selectedtext" -> new ElementActions(driver).getSelectedText(elementLocator);
            default -> new ElementActions(driver).getAttribute(elementLocator, elementAttribute);
        };

        // re-enable full reporting
        ReportManagerHelper.setDiscreteLogging(isDiscrete);

        // compare actual and expected results
        int comparisonResult = JavaHelper.compareTwoObjects(expectedValue, actualValue,
                validationComparisonType.getValue(), validationType.getValue());

        // get validation method name, replace validate with exact validation category (verify/assert)
        String validationMethodName = (new Throwable()).getStackTrace()[0].getMethodName().replace("validate", this.validationCategoryString);

        // set validation state based on comparison results
        boolean validationState;
        if (comparisonResult == 1) {
            validationState = ValidationEnums.ValidationState.PASSED.getValue();
        } else {
            validationState = ValidationEnums.ValidationState.FAILED.getValue();
        }

        List<List<Object>> attachments = new ArrayList<>();
        // prepare screenshot with element highlighting
        attachments.add(ScreenshotManager.takeScreenshot(driver, elementLocator, validationMethodName, validationState));

        // prepare page snapshot mhtml/html
        var whenToTakePageSourceSnapshot = SHAFT.Properties.visuals.whenToTakePageSourceSnapshot().toLowerCase();
        if (Boolean.FALSE.equals(validationState)
                || Arrays.asList("always", "validationpointsonly").contains(whenToTakePageSourceSnapshot)) {
            var logMessage = "";
            var pageSnapshot = BrowserActionsHelper.capturePageSnapshot(driver);
            if (pageSnapshot.startsWith("From: <Saved by Blink>")) {
                logMessage = "page snapshot";
            } else if (pageSnapshot.startsWith("<html")) {
                logMessage = "page HTML";
            }
            List<Object> pageSourceAttachment = Arrays.asList(validationMethodName, logMessage, pageSnapshot);
            attachments.add(pageSourceAttachment);
        }

        // add attachments
        ReportManagerHelper.attach(attachments);

        // handle failure based on validation category
        if (!validationState) {
            if (this.validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT)) {
                FailureReporter.fail(customReportMessage);
            } else {
                // soft assert
                ValidationsHelper.verificationFailuresList.add("Failed to " + this.validationCategoryString.toLowerCase() + " that " + customReportMessage);
                ValidationsHelper.verificationError = new AssertionError(String.join("\nAND ", ValidationsHelper.verificationFailuresList));
            }
        }
    }
}
