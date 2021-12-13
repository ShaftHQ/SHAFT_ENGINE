package com.shaft.gui.element;

import org.openqa.selenium.NoAlertPresentException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.time.Duration;

public class AlertActions {
    private static WebDriverWait webDriverWait;
    private final WebDriver driver;

    public AlertActions(WebDriver driver) {
        this.driver = driver;
        webDriverWait = new WebDriverWait(driver, Duration.ofSeconds(30));
    }

    public ElementActions performElementAction() {
        return new ElementActions(driver);
    }

    public static void waitForAlertToBePresent(WebDriver driver) {
        try {
            webDriverWait.until(ExpectedConditions.alertIsPresent());
            driver.switchTo().alert();
            System.out.println("Alert is present");
        } catch (NoAlertPresentException exception) {
            System.out.println("Alert is not present");
        }
    }

    public static boolean isAlertPresent(WebDriver driver) {
        try {
            waitForAlertToBePresent(driver);
            driver.switchTo().alert();
            System.out.println("Alert is present");
            return true;
        } catch (NoAlertPresentException exception) {
            System.out.println("Alert is not present");
            return false;
        }
    }

    public AlertActions acceptAlert() {
        try {
            waitForAlertToBePresent(driver);
            driver.switchTo().alert().accept();
        } catch (NoAlertPresentException exception) {
            System.out.println("Alert is not present");
        }
        return this;
    }

    public AlertActions dismissAlert() {
        try {
            waitForAlertToBePresent(driver);
            driver.switchTo().alert().dismiss();
        } catch (NoAlertPresentException exception) {
            System.out.println("Alert is not present");
        }
        return this;
    }

    public static String getAlertText(WebDriver driver) {
        waitForAlertToBePresent(driver);
        System.out.println(driver.switchTo().alert().getText());
        return driver.switchTo().alert().getText();
    }

    public static void typeIntoPromptAlert(WebDriver driver, String text) {
        waitForAlertToBePresent(driver);
        driver.switchTo().alert().sendKeys(text);
    }
}
