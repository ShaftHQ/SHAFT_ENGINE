package com.shaft.validation;

import org.openqa.selenium.WebDriver;

public class WebDriverBrowserValidationsBuilder {
    protected ValidationEnums.ValidationCategory validationCategory;
    protected WebDriver driver;

    protected String validationMethod;
    protected String browserAttribute;

    protected StringBuilder reportMessageBuilder;

    public WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, WebDriver driver, StringBuilder reportMessageBuilder) {
        this.validationCategory = validationCategory;
        this.driver = driver;

        this.reportMessageBuilder = reportMessageBuilder;
    }

    /**
     * Use this to check against a certain browser attribute
     * @param browserAttribute the target browser attribute that will be checked against
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder attribute(String browserAttribute) {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = browserAttribute;
        reportMessageBuilder.append("attribute [").append(browserAttribute).append("] ");
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
        reportMessageBuilder.append("attribute [").append(browserAttribute).append("] ");
        return new NativeValidationsBuilder(this);
    }

}