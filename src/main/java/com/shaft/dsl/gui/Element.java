package com.shaft.dsl.gui;

import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

@SuppressWarnings("unused")
public abstract class Element {
    static WebDriver driver;
    final By locator;
    final ElementActions elementActions;

    protected Element(By locator) {
        this.locator = locator;
        elementActions = ElementActions.getInstance();
    }

    public static WebDriver getDriver() {
        return Element.driver;
    }

    public static void setDriver(WebDriver driver) {
        Element.driver = driver;
    }

    public boolean isDisplayed() {
        return new ElementActions().isElementDisplayed(locator);
    }

    public void shouldBeDisplayed() {
        Validations.assertThat().object(isDisplayed()).isTrue().perform();
    }

    public void shouldExist() {
        Validations.assertThat().element(locator).exists().perform();
    }

    public void shouldExist(String reportMsg) {
        Validations.assertThat().element(locator).exists()
                .withCustomReportMessage(reportMsg).perform();
    }

    public void shouldNotExist() {
        Validations.assertThat().element(locator).exists().perform();
    }

    public void shouldNotExist(String reportMsg) {
        Validations.assertThat().element(locator).exists()
                .withCustomReportMessage(reportMsg).perform();
    }
}
