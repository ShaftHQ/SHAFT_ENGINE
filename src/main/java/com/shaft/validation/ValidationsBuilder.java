package com.shaft.validation;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class ValidationsBuilder {
    ValidationEnums.ValidationCategory validationCategory = ValidationEnums.ValidationCategory.HARD_ASSERT;
    String validationMethod = "";
    Object expectedValue = null;
    Object actualValue = null;
    String folderRelativePath = "";
    String fileName = "";
    boolean condition = true;

    ValidationsBuilder(ValidationEnums.ValidationCategory validationCategory) {
        this.validationCategory = validationCategory;
    }

    public ValidationsAttributesBuilder forceFail() {
        validationMethod = "forceFail";
        return new ValidationsAttributesBuilder(this);
    }

    public ValidationsAttributesBuilder objectsAreEqual(Object actualValue, Object expectedValue) {
        validationMethod = "objectsAreEqual";
        this.expectedValue = expectedValue;
        this.actualValue = actualValue;
        return new ValidationsAttributesBuilder(this);
    }

    public ValidationsAttributesBuilder conditionIsTrue(boolean condition) {
        validationMethod = "conditionIsTrue";
        this.condition = condition;
        return new ValidationsAttributesBuilder(this);
    }

    public ValidationsAttributesBuilder objectIsNull(Object actualValue) {
        validationMethod = "objectIsNull";
        this.actualValue = actualValue;
        return new ValidationsAttributesBuilder(this);
    }

    public ValidationsAttributesBuilder comparativeRelationBetweenNumbers(Number actualValue, Number expectedValue) {
        validationMethod = "comparativeRelationBetweenNumbers";
        this.expectedValue = expectedValue;
        this.actualValue = actualValue;
        return new ValidationsAttributesBuilder(this);
    }

    public ValidationsAttributesBuilder fileExists(String folderRelativePath, String fileName) {
        validationMethod = "fileExists";
        this.folderRelativePath = folderRelativePath;
        this.fileName = fileName;
        return new ValidationsAttributesBuilder(this);
    }

    public WebElementValidationsBuilder element(WebDriver driver, By locator) {
        return new WebElementValidationsBuilder(this, driver, locator);
    }

    public WebBrowserValidationsBuilder browser(WebDriver driver) {
        return new WebBrowserValidationsBuilder(this, driver);
    }

    public JsonValidationsBuilder json(Object response) {
        return new JsonValidationsBuilder(this, response);
    }
}
