package com.shaft.validation;

import org.openqa.selenium.WebDriver;

public class WebDriverBrowserValidationsBuilder {
    protected final ValidationEnums.ValidationCategory validationCategory;
    protected final WebDriver driver;

    protected String validationMethod;
    protected String browserAttribute;

    protected final StringBuilder reportMessageBuilder;

    public WebDriverBrowserValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, WebDriver driver, StringBuilder reportMessageBuilder) {
        this.validationCategory = validationCategory;
        this.driver = driver;

        this.reportMessageBuilder = reportMessageBuilder;
    }

    /**
     * Use this to check against a certain browser attribute
     *
     * @param browserAttribute the target browser attribute that will be checked against
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder attribute(String browserAttribute) {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = browserAttribute;
        reportMessageBuilder.append("attribute \"").append(browserAttribute).append("\" ");
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check against a certain browser attribute
     *
     * @param browserAttribute the target browser attribute that will be checked against
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    @Deprecated
    public NativeValidationsBuilder attribute(ValidationEnums.BrowserAttribute browserAttribute) {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = browserAttribute.getValue();
        reportMessageBuilder.append("attribute \"").append(browserAttribute).append("\" ");
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check against the current page URL
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder url() {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = ValidationEnums.BrowserAttribute.CURRENT_URL.getValue();
        reportMessageBuilder.append("URL ");
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check against the current page title
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder title() {
        this.validationMethod = "browserAttributeEquals";
        this.browserAttribute = ValidationEnums.BrowserAttribute.TITLE.getValue();
        reportMessageBuilder.append("title ");
        return new NativeValidationsBuilder(this);
    }

}