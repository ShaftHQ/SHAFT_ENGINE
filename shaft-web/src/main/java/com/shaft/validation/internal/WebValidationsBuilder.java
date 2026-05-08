package com.shaft.validation.internal;

import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.validation.ValidationEnums;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class WebValidationsBuilder extends ValidationsBuilder {

    public WebValidationsBuilder(ValidationEnums.ValidationCategory validationCategory) {
        super(validationCategory);
    }

    public WebDriverElementValidationsBuilder element(WebDriver driver, By locator) {
        reportMessageBuilder.append("the element located by \"").append(JavaHelper.formatLocatorToString(locator)).append("\" ");
        return new WebDriverElementValidationsBuilder(validationCategory, driver, locator, reportMessageBuilder);
    }

    public WebDriverBrowserValidationsBuilder browser(WebDriver driver) {
        reportMessageBuilder.append("the browser ");
        return new WebDriverBrowserValidationsBuilder(validationCategory, driver, reportMessageBuilder);
    }
}
