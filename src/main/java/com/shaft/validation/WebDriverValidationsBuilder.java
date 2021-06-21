package com.shaft.validation;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class WebDriverValidationsBuilder {
    ValidationEnums.ValidationCategory validationCategory;
    WebDriver driver;

    public WebDriverValidationsBuilder(ValidationEnums.ValidationCategory validationCategory, WebDriver driver) {
        this.validationCategory = validationCategory;
        this.driver = driver;
    }

    public WebDriverElementValidationsBuilder element(By locator) {
        return new WebDriverElementValidationsBuilder(validationCategory, driver, locator);
    }

    public WebDriverBrowserValidationsBuilder browser() {
        return new WebDriverBrowserValidationsBuilder(validationCategory, driver);
    }
}
