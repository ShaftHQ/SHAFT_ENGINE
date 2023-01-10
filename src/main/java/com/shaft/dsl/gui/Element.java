package com.shaft.dsl.gui;

import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import io.github.shafthq.shaft.gui.element.FluentElementActions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public abstract class Element {
    static WebDriver driver;
    final By locator;
    final FluentElementActions elementActions;

    protected Element(By locator) {
        this.locator = locator;
        elementActions = new FluentElementActions(driver);
    }

    public static WebDriver getDriver() {
        return Element.driver;
    }

    public static void setDriver(WebDriver driver) {
        Element.driver = driver;
    }

    public boolean isDisplayed() {
        return ElementActions.isElementDisplayed(driver, locator);
    }

    public void shouldBeDisplayed() {
        Validations.assertThat().object(isDisplayed()).isTrue().perform();
    }

    public void shouldExist() {
        Validations.assertThat().element(driver, locator).exists().perform();
    }

    public void shouldExist(String reportMsg) {
        Validations.assertThat().element(driver, locator).exists()
                .withCustomReportMessage(reportMsg).perform();
    }

    public void shouldNotExist() {
        Validations.assertThat().element(driver, locator).exists().perform();
    }

    public void shouldNotExist(String reportMsg) {
        Validations.assertThat().element(driver, locator).exists()
                .withCustomReportMessage(reportMsg).perform();
    }
}
