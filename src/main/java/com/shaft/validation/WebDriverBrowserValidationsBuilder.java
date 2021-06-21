package com.shaft.validation;

import org.openqa.selenium.WebDriver;

public class WebDriverBrowserValidationsBuilder {
    ValidationEnums.ValidationCategory validationCategory;
    WebDriver driver;

    String validationMethod;
    String browserAttribute;

    public WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, WebDriver driver) {
        this.validationCategory = validationCategory;
        this.driver = driver;
    }

    public ValidationsComparisonTypeManager attribute(String browserAttribute) {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = browserAttribute;
        return new ValidationsComparisonTypeManager(this);
    }

    public ValidationsComparisonTypeManager attribute(ValidationEnums.BrowserAttribute browserAttribute) {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = browserAttribute.getValue();
        return new ValidationsComparisonTypeManager(this);
    }

}