package com.shaft.validation;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class WebElementValidationsBuilder {
    ValidationsBuilder validationsBuilder;
    WebDriver driver;
    By locator;
    String elementAttribute;
    String elementCssProperty;
    ValidationEnums.VisualValidationEngine visualValidationEngine = ValidationEnums.VisualValidationEngine.EXACT_OPENCV;

    public WebElementValidationsBuilder(ValidationsBuilder validationsBuilder, WebDriver driver, By locator) {
        this.validationsBuilder = validationsBuilder;
        this.driver = driver;
        this.locator = locator;
    }

    public ValidationsAttributesBuilder exists() {
        this.validationsBuilder.validationMethod = "elementExists";
        return new ValidationsAttributesBuilder(this);
    }

    public ValidationsAttributesBuilder matches() {
        this.validationsBuilder.validationMethod = "elementMatches";
        return new ValidationsAttributesBuilder(this);
    }

    public ValidationsAttributesBuilder matches(ValidationEnums.VisualValidationEngine visualValidationEngine) {
        this.validationsBuilder.validationMethod = "elementMatches";
        this.visualValidationEngine = visualValidationEngine;
        return new ValidationsAttributesBuilder(this);
    }

    public ValidationsAttributesBuilder attributeEquals(ValidationEnums.ElementAttribute elementAttribute, String expectedValue) {
        this.validationsBuilder.validationMethod = "elementAttributeEquals";
        this.validationsBuilder.expectedValue = expectedValue;
        this.elementAttribute = elementAttribute.getValue();
        return new ValidationsAttributesBuilder(this);
    }

    public ValidationsAttributesBuilder attributeEquals(String elementAttribute, String expectedValue) {
        this.validationsBuilder.validationMethod = "elementAttributeEquals";
        this.validationsBuilder.expectedValue = expectedValue;
        this.elementAttribute = elementAttribute;
        return new ValidationsAttributesBuilder(this);
    }

    public ValidationsAttributesBuilder cssPropertyEquals(String elementCssProperty, String expectedValue) {
        this.validationsBuilder.validationMethod = "elementCssPropertyEquals";
        this.validationsBuilder.expectedValue = expectedValue;
        this.elementCssProperty = elementCssProperty;
        return new ValidationsAttributesBuilder(this);
    }
}
