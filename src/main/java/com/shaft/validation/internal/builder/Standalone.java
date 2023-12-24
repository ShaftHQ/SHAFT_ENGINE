package com.shaft.validation.internal.builder;

import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.executor.GenericExecutor;
import lombok.Getter;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import static com.shaft.gui.element.internal.ElementActionsHelper.formatLocatorToString;

@SuppressWarnings("unused")
@Getter
public class Standalone implements ValidationsBuilder {
    protected final ValidationEnums.ValidationCategory validationCategory;
    protected String validationMethod;
    protected ValidationEnums.ValidationType validationType;
    protected Object actualValue;

    protected final StringBuilder reportMessageBuilder = new StringBuilder();

    public Standalone(ValidationEnums.ValidationCategory validationCategory) {
        this.validationCategory = validationCategory;
    }

    /**
     * Build a native validation to check against the target object
     *
     * @param actualValue the actual object that will be compared against
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public Native object(Object actualValue) {
        this.validationMethod = "objectsAreEqual";
        this.actualValue = actualValue;
        reportMessageBuilder.append("\"").append(actualValue).append("\" ");
        return new Native(this);
    }

    /**
     * Build a number validation to check against the target number
     *
     * @param actualValue the actual number that will be compared against
     * @return a NumberValidationsBuilder object to continue building your validation
     */
    public Number number(java.lang.Number actualValue) {
        this.validationMethod = "comparativeRelationBetweenNumbers";
        this.actualValue = actualValue;
        reportMessageBuilder.append("\"").append(actualValue).append("\" ");
        return new Number(this);
    }

    public WebElement element(WebDriver driver, By locator) {
        reportMessageBuilder.append("The Element located by \"").append(formatLocatorToString(locator)).append("\" ");
        return new WebElement(validationCategory, driver, locator, reportMessageBuilder);
    }

    /**
     * Build a WebDriver browser validation to check against the target browser
     *
     * @return a WebDriverBrowserValidationsBuilder object to continue building your validation
     */
    public WebBrowser browser(WebDriver driver) {
        reportMessageBuilder.append("The Browser ");
        return new WebBrowser(validationCategory, driver, reportMessageBuilder);
    }

    /**
     * Build an API response validation to check against the target API response
     *
     * @param response the target API response object
     * @return a RestValidationsBuilder object to continue building your validation
     */
    public API response(Object response) {
        reportMessageBuilder.append("The API response ");
        return new API(validationCategory, response, reportMessageBuilder);
    }

    /**
     * Build a file validation to check against the target file
     *
     * @param folderRelativePath relative path to the targetDirectory
     * @param fileName           target fileName
     * @return a FileValidationsBuilder object to continue building your validation
     */
    public File file(String folderRelativePath, String fileName) {
        reportMessageBuilder.append("The File \"").append(folderRelativePath).append(fileName).append("\" ");
        return new File(validationCategory, folderRelativePath, fileName, reportMessageBuilder);
    }

    /**
     * Force fails the current validation
     *
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public GenericExecutor forceFail() {
        reportMessageBuilder.append("Force fail.");
        this.validationMethod = "forceFail";
        return new GenericExecutor(this);
    }
}
