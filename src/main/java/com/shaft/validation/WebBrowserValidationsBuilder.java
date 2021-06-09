package com.shaft.validation;

import org.openqa.selenium.WebDriver;

public class WebBrowserValidationsBuilder {
    ValidationsBuilder validationsBuilder;
    WebDriver driver;
    String browserAttribute;

    public WebBrowserValidationsBuilder(ValidationsBuilder validationsBuilder, WebDriver driver) {
        this.validationsBuilder = validationsBuilder;
        this.driver = driver;
    }

    public ValidationsAttributesBuilder attributeEquals(ValidationEnums.BrowserAttribute browserAttribute, String expectedValue) {
        this.validationsBuilder.validationMethod = "browserAttributeEquals";
        this.validationsBuilder.expectedValue = expectedValue;
        this.browserAttribute = browserAttribute.getValue();
        return new ValidationsAttributesBuilder(this);
    }

    public ValidationsAttributesBuilder attributeEquals(String browserAttribute, String expectedValue) {
        this.validationsBuilder.validationMethod = "browserAttributeEquals";
        this.validationsBuilder.expectedValue = expectedValue;
        this.browserAttribute = browserAttribute;
        return new ValidationsAttributesBuilder(this);
    }

}
