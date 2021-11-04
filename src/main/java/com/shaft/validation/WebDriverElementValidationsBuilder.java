package com.shaft.validation;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class WebDriverElementValidationsBuilder {
    protected ValidationEnums.ValidationCategory validationCategory;
    protected WebDriver driver;
    protected By locator;

    protected ValidationEnums.ValidationType validationType;
    protected String validationMethod;
    protected ValidationEnums.VisualValidationEngine visualValidationEngine;
    protected String elementAttribute;
    protected String elementCssProperty;

    protected StringBuilder reportMessageBuilder;

    public WebDriverElementValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, WebDriver driver, By locator, StringBuilder reportMessageBuilder) {
        this.validationCategory = validationCategory;
        this.driver = driver;
        this.locator = locator;

        this.reportMessageBuilder = reportMessageBuilder;
    }

    /**
     * Use this to check that the target element exists
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor exists() {
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        this.validationMethod = "elementExists";
        reportMessageBuilder.append("exists.");
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the target element does not exist
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotExist() {
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        this.validationMethod = "elementExists";
        reportMessageBuilder.append("does not exist.");
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the target element matches a reference image (using the Artificial Intelligence library OpenCV).
     * On the first test run this method will take a screenshot of the target element and the test will pass, and on following runs the element will be compared against that reference image.
     * The reference images are stored under src/test/resources/DynamicObjectRepository for later maintenance
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor matchesReferenceImage() {
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        this.validationMethod = "elementMatches";
        this.visualValidationEngine = ValidationEnums.VisualValidationEngine.EXACT_OPENCV;
        reportMessageBuilder.append("matches the reference image [").append(ValidationEnums.VisualValidationEngine.EXACT_OPENCV).append("].");
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the target element matches a reference image.
     * @param visualValidationEngine the selected visualValidationEngine that will be used to perform the image comparison
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor matchesReferenceImage(ValidationEnums.VisualValidationEngine visualValidationEngine) {
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        this.validationMethod = "elementMatches";
        this.visualValidationEngine = visualValidationEngine;
        reportMessageBuilder.append("matches the reference image [").append(visualValidationEngine).append("].");
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the target element does not match a reference image (using the Artificial Intelligence library OpenCV).
     * On the first test run this method will take a screenshot of the target element and the test will pass, and on following runs the element will be compared against that reference image.
     * The reference images are stored under src/test/resources/DynamicObjectRepository for later maintenance
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotMatchReferenceImage() {
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        this.validationMethod = "elementMatches";
        this.visualValidationEngine = ValidationEnums.VisualValidationEngine.EXACT_OPENCV;
        reportMessageBuilder.append("does not match the reference image [").append(ValidationEnums.VisualValidationEngine.EXACT_OPENCV).append("].");
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the target element does not match a reference image.
     * @param visualValidationEngine the selected visualValidationEngine that will be used to perform the image comparison
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotMatchReferenceImage(ValidationEnums.VisualValidationEngine visualValidationEngine) {
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        this.validationMethod = "elementMatches";
        this.visualValidationEngine = visualValidationEngine;
        reportMessageBuilder.append("does not match the reference image [").append(visualValidationEngine).append("].");
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check against a certain element attribute
     * @param elementAttribute the target element attribute that will be checked against
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder attribute(String elementAttribute) {
        this.validationMethod = "elementAttributeEquals";
        this.elementAttribute = elementAttribute;
        reportMessageBuilder.append("attribute [").append(elementAttribute).append("] ");
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check against a certain element attribute
     * @param elementAttribute the target element attribute that will be checked against
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder attribute(ValidationEnums.ElementAttribute elementAttribute) {
        this.validationMethod = "elementAttributeEquals";
        this.elementAttribute = elementAttribute.getValue();
        reportMessageBuilder.append("attribute [").append(elementAttribute).append("] ");
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check against the provided elements text attribute
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder text() {
        this.validationMethod = "elementAttributeEquals";
        this.elementAttribute = ValidationEnums.ElementAttribute.TEXT.getValue();
        reportMessageBuilder.append("text ");
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check against a certain element attribute
     * @param elementCssProperty the target element css property that will be checked against
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder cssProperty(String elementCssProperty) {
        this.validationMethod = "elementCssPropertyEquals";
        this.elementCssProperty = elementCssProperty;
        reportMessageBuilder.append("CSS property [").append(elementCssProperty).append("] ");
        return new NativeValidationsBuilder(this);
    }


}
