package com.shaft.validation.internal.executor;

import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.builder.Native;
import com.shaft.validation.internal.builder.Standalone;
import com.shaft.validation.internal.builder.ValidationsBuilder;
import com.shaft.validation.internal.builder.WebElement;
import io.qameta.allure.Step;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;


public class WebExecutor implements ValidationsExecutor {
    protected final StringBuilder reportMessageBuilder;
    private final ValidationEnums.ValidationCategory validationCategory;
    private final ValidationEnums.ValidationType validationType;
    private final String validationMethod;
    private final WebDriver driver;
    private final By locator;
    private String validationCategoryString;
    private String validationMethodString;
    private String customReportMessage = "";
    private ValidationEnums.VisualValidationEngine visualValidationEngine;
    private String elementAttribute;
    private String elementCssProperty;
    private String browserAttribute;
    private ValidationEnums.ValidationComparisonType validationComparisonType;
    private Object expectedValue;

    public WebExecutor(WebElement webElementValidationsBuilder) {
        this.validationCategory = webElementValidationsBuilder.getValidationCategory();
        this.validationType = webElementValidationsBuilder.getValidationType();
        this.validationMethod = webElementValidationsBuilder.getValidationMethod();

        this.driver = webElementValidationsBuilder.getDriver();
        this.locator = webElementValidationsBuilder.getLocator();

        this.visualValidationEngine = webElementValidationsBuilder.getVisualValidationEngine();

        this.reportMessageBuilder = webElementValidationsBuilder.getReportMessageBuilder();
    }

    public WebExecutor(Native nativeValidationsBuilder) {
        this.validationCategory = nativeValidationsBuilder.getValidationCategory();
        this.validationType = nativeValidationsBuilder.getValidationType();
        this.validationMethod = nativeValidationsBuilder.getValidationMethod();
        this.validationComparisonType = nativeValidationsBuilder.getValidationComparisonType();

        this.driver = nativeValidationsBuilder.getDriver();
        this.locator = nativeValidationsBuilder.getLocator();

        this.expectedValue = nativeValidationsBuilder.getExpectedValue();

        this.elementAttribute = nativeValidationsBuilder.getElementAttribute();
        this.elementCssProperty = nativeValidationsBuilder.getElementCssProperty();
        this.browserAttribute = nativeValidationsBuilder.getBrowserAttribute();

        this.reportMessageBuilder = nativeValidationsBuilder.getReportMessageBuilder();
    }

    public WebExecutor withCustomReportMessage(String customReportMessage) {
        this.customReportMessage = customReportMessage;
        return this;
    }

    public void perform() {
        JavaScriptWaitManager.waitForLazyLoading(driver);
        if (customReportMessage.isBlank()) {
            customReportMessage = reportMessageBuilder.toString();
        }
        validationCategoryString = validationCategory.equals(ValidationEnums.ValidationCategory.HARD_ASSERT) ? "Assert that" : "Verify that";
        validationMethodString = JavaHelper.convertToSentenceCase(validationMethod).toLowerCase();
        performValidation();
    }

    @Step(" {this.validationCategoryString} {this.validationMethodString}")
    private ValidationsBuilder performValidation() {
        switch (validationMethod) {
            case "elementExists" -> {
                Helper.validateElementExists(validationCategory, driver, locator, validationType, customReportMessage);
                return new Standalone(validationCategory).element(driver, locator);
            }
            case "elementMatches" -> {
                Helper.validateElementMatches(validationCategory, driver, locator, visualValidationEngine, validationType, customReportMessage);
                return new Standalone(validationCategory).element(driver, locator);
            }
            case "elementAttributeEquals" -> {
                Helper.validateElementAttribute(validationCategory, driver, locator, elementAttribute, String.valueOf(expectedValue), validationComparisonType, validationType, customReportMessage);
                return new Standalone(validationCategory).element(driver, locator);
            }
            case "elementCssPropertyEquals" -> {
                Helper.validateElementCSSProperty(driver, validationCategory, locator, elementCssProperty, String.valueOf(expectedValue), validationComparisonType, validationType, customReportMessage);
                return new Standalone(validationCategory).element(driver, locator);
            }
            case "browserAttributeEquals" -> {
                Helper.validateBrowserAttribute(validationCategory, driver, browserAttribute, String.valueOf(expectedValue), validationComparisonType, validationType, customReportMessage);
                return new Standalone(validationCategory).browser(driver);
            }
        }
        return new Standalone(validationCategory);
    }
}
