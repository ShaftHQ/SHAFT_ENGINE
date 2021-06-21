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

    public NativeValidationsBuilder attribute(String browserAttribute) {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = browserAttribute;
        return new NativeValidationsBuilder(this);
    }

    public NativeValidationsBuilder attribute(ValidationEnums.BrowserAttribute browserAttribute) {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = browserAttribute.getValue();
        return new NativeValidationsBuilder(this);
    }

}