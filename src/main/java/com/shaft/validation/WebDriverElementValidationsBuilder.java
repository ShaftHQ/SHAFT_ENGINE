package com.shaft.validation;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class WebDriverElementValidationsBuilder {
    ValidationEnums.ValidationCategory validationCategory;
    WebDriver driver;
    By locator;

    ValidationEnums.ValidationType validationType;
    String validationMethod;
    ValidationEnums.VisualValidationEngine visualValidationEngine;
    String elementAttribute;
    String elementCssProperty;

    public WebDriverElementValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, WebDriver driver, By locator) {
        this.validationCategory = validationCategory;
        this.driver = driver;
        this.locator = locator;
    }

    public ValidationsExecutor exists() {
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        this.validationMethod = "elementExists";
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor doesNotExist() {
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        this.validationMethod = "elementExists";
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor matchesReferenceImage() {
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        this.validationMethod = "elementMatches";
        this.visualValidationEngine = ValidationEnums.VisualValidationEngine.EXACT_OPENCV;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor matchesReferenceImage(ValidationEnums.VisualValidationEngine visualValidationEngine) {
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        this.validationMethod = "elementMatches";
        this.visualValidationEngine = visualValidationEngine;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor doesNotMatchReferenceImage() {
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        this.validationMethod = "elementMatches";
        this.visualValidationEngine = ValidationEnums.VisualValidationEngine.EXACT_OPENCV;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor doesNotMatchReferenceImage(ValidationEnums.VisualValidationEngine visualValidationEngine) {
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        this.validationMethod = "elementMatches";
        this.visualValidationEngine = visualValidationEngine;
        return new ValidationsExecutor(this);
    }

    public ValidationsComparisonTypeManager attribute(String elementAttribute) {
        this.validationMethod = "elementAttributeEquals";
        this.elementAttribute = elementAttribute;
        return new ValidationsComparisonTypeManager(this);
    }

    public ValidationsComparisonTypeManager attribute(ValidationEnums.ElementAttribute elementAttribute) {
        this.validationMethod = "elementAttributeEquals";
        this.elementAttribute = elementAttribute.getValue();
        return new ValidationsComparisonTypeManager(this);
    }

    public ValidationsComparisonTypeManager cssProperty(String elementCssProperty) {
        this.validationMethod = "elementCssPropertyEquals";
        this.elementCssProperty = elementCssProperty;
        return new ValidationsComparisonTypeManager(this);
    }


}
