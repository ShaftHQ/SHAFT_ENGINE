package com.shaft.validation.internal;

import com.shaft.validation.ValidationEnums;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

@SuppressWarnings("unused")
public class WebDriverElementValidationsBuilder {
    protected final ValidationEnums.ValidationCategory validationCategory;
    protected final WebDriver driver;
    protected final By locator;
    protected final StringBuilder reportMessageBuilder;
    protected ValidationEnums.ValidationType validationType;
    protected String validationMethod;
    protected ValidationEnums.VisualValidationEngine visualValidationEngine;
    protected String elementAttribute;
    protected String elementCssProperty;

    public WebDriverElementValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, WebDriver driver, By locator, StringBuilder reportMessageBuilder) {
        this.validationCategory = validationCategory;
        this.driver = driver;
        this.locator = locator;

        this.reportMessageBuilder = reportMessageBuilder;
    }

    /**
     * Use this to check that the target element exists
     *
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor exists() {
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        this.validationMethod = "elementExists";
        reportMessageBuilder.append("exists.");
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check that the target element does not exist
     *
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotExist() {
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        this.validationMethod = "elementExists";
        reportMessageBuilder.append("does not exist.");
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check that the target element matches a reference image (using the Artificial Intelligence library SHUTTERBUG).
     * On the first test run this method will take a screenshot of the target element and the test will pass, and on following runs the element will be compared against that reference image.
     * The reference images are stored under src/test/resources/DynamicObjectRepository for later maintenance
     *
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor matchesReferenceImage() {
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        this.validationMethod = "elementMatches";
        this.visualValidationEngine = ValidationEnums.VisualValidationEngine.EXACT_SHUTTERBUG;
        reportMessageBuilder.append("matches the reference image \"").append(ValidationEnums.VisualValidationEngine.EXACT_SHUTTERBUG).append("\".");
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check that the target element matches a reference image.
     *
     * @param visualValidationEngine the selected visualValidationEngine that will be used to perform the image comparison
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor matchesReferenceImage(ValidationEnums.VisualValidationEngine visualValidationEngine) {
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        this.validationMethod = "elementMatches";
        this.visualValidationEngine = visualValidationEngine;
        reportMessageBuilder.append("matches the reference image \"").append(visualValidationEngine).append("\".");
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check that the target element does not match a reference image (using the Artificial Intelligence library OpenCV).
     * On the first test run this method will take a screenshot of the target element and the test will pass, and on following runs the element will be compared against that reference image.
     * The reference images are stored under src/test/resources/DynamicObjectRepository for later maintenance
     *
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotMatchReferenceImage() {
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        this.validationMethod = "elementMatches";
        this.visualValidationEngine = ValidationEnums.VisualValidationEngine.EXACT_OPENCV;
        reportMessageBuilder.append("does not match the reference image \"").append(ValidationEnums.VisualValidationEngine.EXACT_OPENCV).append("\".");
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check that the target element does not match a reference image.
     *
     * @param visualValidationEngine the selected visualValidationEngine that will be used to perform the image comparison
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotMatchReferenceImage(ValidationEnums.VisualValidationEngine visualValidationEngine) {
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        this.validationMethod = "elementMatches";
        this.visualValidationEngine = visualValidationEngine;
        reportMessageBuilder.append("does not match the reference image \"").append(visualValidationEngine).append("\".");
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check against a certain element attribute
     *
     * @param elementAttribute the target element attribute that will be checked against
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder attribute(String elementAttribute) {
        this.validationMethod = "elementAttributeEquals";
        this.elementAttribute = elementAttribute;
        reportMessageBuilder.append("attribute \"").append(elementAttribute).append("\" ");
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check against the provided elements selected attribute
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public ValidationsExecutor isSelected() {
        this.validationMethod = "elementAttributeEquals";
        this.elementAttribute = "selected";
        reportMessageBuilder.append("is selected; selected attribute ");
        return new NativeValidationsBuilder(this).isTrue();
    }

    /**
     * Use this to check against the provided elements checked attribute
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public ValidationsExecutor isChecked() {
        this.validationMethod = "elementAttributeEquals";
        this.elementAttribute = "checked";
        reportMessageBuilder.append("is checked; checked attribute ");
        return new NativeValidationsBuilder(this).isTrue();
    }

    /**
     * Use this to check against the provided elements hidden attribute
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public ValidationsExecutor isVisible() {
        this.validationMethod = "elementAttributeEquals";
        this.elementAttribute = "hidden";
        reportMessageBuilder.append("is visible; hidden attribute ");
        return new NativeValidationsBuilder(this).isEqualTo("null");
    }

    /**
     * Use this to check against the provided elements disabled attribute
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public ValidationsExecutor isEnabled() {
        this.validationMethod = "elementAttributeEquals";
        this.elementAttribute = "disabled";
        reportMessageBuilder.append("is enabled; disabled attribute ");
        return new NativeValidationsBuilder(this).isEqualTo("null");
    }

    /**
     * Use this to check against the provided elements selected attribute
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public ValidationsExecutor isNotSelected() {
        this.validationMethod = "elementAttributeEquals";
        this.elementAttribute = "selected";
        reportMessageBuilder.append("is not selected; selected attribute ");
        return new NativeValidationsBuilder(this).isEqualTo("null");
    }

    /**
     * Use this to check against the provided elements checked attribute
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public ValidationsExecutor isNotChecked() {
        this.validationMethod = "elementAttributeEquals";
        this.elementAttribute = "checked";
        reportMessageBuilder.append("is not checked; checked attribute ");
        return new NativeValidationsBuilder(this).isEqualTo("null");
    }

    /**
     * Use this to check against the provided elements hidden attribute
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public ValidationsExecutor isHidden() {
        this.validationMethod = "elementAttributeEquals";
        this.elementAttribute = "hidden";
        reportMessageBuilder.append("is hidden; hidden attribute ");
        return new NativeValidationsBuilder(this).isTrue();
    }

    /**
     * Use this to check against the provided elements disabled attribute
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public ValidationsExecutor isDisabled() {
        this.validationMethod = "elementAttributeEquals";
        this.elementAttribute = "disabled";
        reportMessageBuilder.append("is disabled; disabled attribute ");
        return new NativeValidationsBuilder(this).isTrue();
    }

    /**
     * Use this to check against the provided elements text attribute
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder text() {
        this.validationMethod = "elementAttributeEquals";
        this.elementAttribute = "text";
        reportMessageBuilder.append("text ");
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check against the provided elements text attribute after it's trimmed (all leading and trailing space removed)
     *
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder textTrimmed() {
        this.validationMethod = "elementAttributeEquals";
        this.elementAttribute = "textTrimmed";
        reportMessageBuilder.append("text trimmed ");
        return new NativeValidationsBuilder(this);
    }

    /**
     * Use this to check against a certain element attribute
     *
     * @param elementCssProperty the target element css property that will be checked against
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder cssProperty(String elementCssProperty) {
        this.validationMethod = "elementCssPropertyEquals";
        this.elementCssProperty = elementCssProperty;
        reportMessageBuilder.append("CSS property \"").append(elementCssProperty).append("\" ");
        return new NativeValidationsBuilder(this);
    }


}
