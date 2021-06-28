package com.shaft.validation;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class ValidationsBuilder {
    ValidationEnums.ValidationCategory validationCategory;
    String validationMethod;
    ValidationEnums.ValidationType validationType;
    boolean condition;
    Object actualValue;

    public ValidationsBuilder(ValidationEnums.ValidationCategory validationCategory) {
        this.validationCategory = validationCategory;
    }

    public NativeValidationsBuilder object(Object actualValue) {
        this.validationMethod = "objectsAreEqual";
        this.actualValue = actualValue;
        return new NativeValidationsBuilder(this);
    }

    public NumberValidationsBuilder number(Number actualValue) {
        this.validationMethod = "comparativeRelationBetweenNumbers";
        this.actualValue = actualValue;
        return new NumberValidationsBuilder(this);
    }

    public WebDriverElementValidationsBuilder element(WebDriver driver, By locator) {
        return new WebDriverElementValidationsBuilder(validationCategory, driver, locator);
    }

    public WebDriverBrowserValidationsBuilder browser(WebDriver driver) {
        return new WebDriverBrowserValidationsBuilder(validationCategory, driver);
    }

    public RestValidationsBuilder response(Object response) {
        return new RestValidationsBuilder(validationCategory, response);
    }

    public FileValidationsBuilder file(String folderRelativePath, String fileName) {
        return new FileValidationsBuilder(validationCategory, folderRelativePath, fileName);
    }

    public ValidationsExecutor forceFail() {
        this.validationMethod = "forceFail";
        return new ValidationsExecutor(this);
    }
}
