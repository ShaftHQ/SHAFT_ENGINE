package io.github.shafthq.shaft.validations.helpers;

import com.shaft.validation.ValidationEnums;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.locators.RelativeLocator;

public class ValidationsBuilder {
    protected final ValidationEnums.ValidationCategory validationCategory;
    protected String validationMethod;
    protected ValidationEnums.ValidationType validationType;
    protected boolean condition;
    protected Object actualValue;

    protected final StringBuilder reportMessageBuilder = new StringBuilder();

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

    /**
     * Build a webdriver element validation to check against the target element
     *
     * @param driver  the current instance of Selenium webdriver
     * @param locator the locator of the webElement under test (By xpath, id,
     *                selector, name ...etc)
     * @return a WebDriverElementValidationsBuilder object to continue building your validation
     */
    public WebDriverElementValidationsBuilder element(WebDriver driver, By locator) {
        var stringLocator = locator.toString();
        if (locator instanceof RelativeLocator.RelativeBy relativeLocator) {
            stringLocator = "Relative Locator: " + relativeLocator.getRemoteParameters().value().toString();
        }
        reportMessageBuilder.append("The Element located by \"").append(stringLocator).append("\" ");
        return new WebDriverElementValidationsBuilder(validationCategory, driver, locator, reportMessageBuilder);
    }

    /**
     * Build a webdriver browser validation to check against the target browser
     *
     * @param driver the current instance of Selenium webdriver
     * @return a WebDriverBrowserValidationsBuilder object to continue building your validation
     */
    public WebDriverBrowserValidationsBuilder browser(WebDriver driver) {
        reportMessageBuilder.append("The Browser ");
        return new WebDriverBrowserValidationsBuilder(validationCategory, driver, reportMessageBuilder);
    }

    /**
     * Build an API response validation to check against the target API response
     *
     * @param response the target API response object
     * @return a RestValidationsBuilder object to continue building your validation
     */
    public RestValidationsBuilder response(Object response) {
        reportMessageBuilder.append("The API response ");
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
        reportMessageBuilder.append("The File \"").append(folderRelativePath).append(fileName).append("\" ");
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
        return new ValidationsExecutor(this);
    }
}
