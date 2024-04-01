package com.shaft.validation.internal;

import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.validation.ValidationEnums;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

@SuppressWarnings("unused")
public class ValidationsBuilder {
    protected final ValidationEnums.ValidationCategory validationCategory;
    protected final StringBuilder reportMessageBuilder = new StringBuilder();
    protected String validationMethod;
    protected ValidationEnums.ValidationType validationType;
    protected boolean condition;
    protected Object actualValue;

    public ValidationsBuilder(ValidationEnums.ValidationCategory validationCategory) {
        this.validationCategory = validationCategory;
    }

    /**
     * Build a native validation to check against the target object
     *
     * @param actualValue the actual object that will be compared against
     * @return a NativeValidationsBuilder object to continue building your validation
     */
    public NativeValidationsBuilder object(Object actualValue) {
        this.validationMethod = "objectsAreEqual";
        this.actualValue = actualValue;
        reportMessageBuilder.append("\"").append(actualValue).append("\" ");
        return new NativeValidationsBuilder(this);
    }

    /**
     * Build a number validation to check against the target number
     *
     * @param actualValue the actual number that will be compared against
     * @return a NumberValidationsBuilder object to continue building your validation
     */
    public NumberValidationsBuilder number(Number actualValue) {
        this.validationMethod = "comparativeRelationBetweenNumbers";
        this.actualValue = actualValue;
        reportMessageBuilder.append("\"").append(actualValue).append("\" ");
        return new NumberValidationsBuilder(this);
    }

    public WebDriverElementValidationsBuilder element(WebDriver driver, By locator) {
        reportMessageBuilder.append("the element located by \"").append(JavaHelper.formatLocatorToString(locator)).append("\" ");
        return new WebDriverElementValidationsBuilder(validationCategory, driver, locator, reportMessageBuilder);
    }

    /**
     * Build a WebDriver browser validation to check against the target browser
     *
     * @return a WebDriverBrowserValidationsBuilder object to continue building your validation
     */
    public WebDriverBrowserValidationsBuilder browser(WebDriver driver) {
        reportMessageBuilder.append("the browser ");
        return new WebDriverBrowserValidationsBuilder(validationCategory, driver, reportMessageBuilder);
    }

    /**
     * Build an API response validation to check against the target API response
     *
     * @param response the target API response object
     * @return a RestValidationsBuilder object to continue building your validation
     */
    public RestValidationsBuilder response(Object response) {
        reportMessageBuilder.append("the API response ");
        return new RestValidationsBuilder(validationCategory, response, reportMessageBuilder);
    }

    /**
     * Build a file validation to check against the target file
     *
     * @param folderRelativePath relative path to the targetDirectory
     * @param fileName           target fileName
     * @return a FileValidationsBuilder object to continue building your validation
     */
    public FileValidationsBuilder file(String folderRelativePath, String fileName) {
        reportMessageBuilder.append("the file \"").append(folderRelativePath).append(fileName).append("\" ");
        return new FileValidationsBuilder(validationCategory, folderRelativePath, fileName, reportMessageBuilder);
    }

    /**
     * Force fails the current validation
     *
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor forceFail() {
        reportMessageBuilder.append("Force fail.");
        this.validationMethod = "forceFail";
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }
}
