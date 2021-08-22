package com.shaft.validation;

import org.openqa.selenium.WebDriver;

public class WebDriverBrowserValidationsBuilder {
    protected ValidationEnums.ValidationCategory validationCategory;
    protected WebDriver driver;

    protected String validationMethod;
    protected String browserAttribute;

    public WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, WebDriver driver) {
        this.validationCategory = validationCategory;
        this.driver = driver;
    }

    /**
     * Use this to check against a certain browser attribute
     * @param browserAttribute the target browser attribute that will be checked against
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder attribute(String browserAttribute) {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = browserAttribute;
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check against a certain browser attribute
     * @param browserAttribute the target browser attribute that will be checked against
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder attribute(ValidationEnums.BrowserAttribute browserAttribute) {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = browserAttribute.getValue();
        return new NativeValidationsBuilder(this);
    }

}