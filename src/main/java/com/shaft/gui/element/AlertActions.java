package com.shaft.gui.element;

import com.shaft.tools.io.ReportManager;
import org.openqa.selenium.NoAlertPresentException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.time.Duration;

public class AlertActions {
    private final WebDriver driver;

    public AlertActions(WebDriver driver) {
        this.driver = driver;
    }

    public ElementActions performElementAction() {
        return new ElementActions(driver);
    }

    private void waitForAlertToBePresent(WebDriver driver) {
        try {
            (new WebDriverWait(driver, Duration.ofSeconds(30))).until(ExpectedConditions.alertIsPresent());
            driver.switchTo().alert();
            ReportManager.logDiscrete("Alert is present");
        } catch (Exception rootCauseException) {
            ReportManager.logDiscrete("Alert is not present");
            WebDriverElementActions.failAction(driver, null, rootCauseException);
        }
    }

    public boolean isAlertPresent() {
        try {
            driver.switchTo().alert();
            WebDriverElementActions.passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null,null);
            ReportManager.logDiscrete("Alert is present");
            return true;
        } catch (NoAlertPresentException exception) {
            WebDriverElementActions.failAction(driver, null, exception);
            ReportManager.logDiscrete("Alert is not present");
            return false;
        } catch (Exception rootCauseException) {
            WebDriverElementActions.failAction(driver, null, rootCauseException);
            return false;
        }
    }

    public AlertActions acceptAlert() {
        try {
            waitForAlertToBePresent(driver);
            driver.switchTo().alert().accept();
            WebDriverElementActions.passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null,null);
        } catch (Exception rootCauseException) {
            WebDriverElementActions.failAction(driver, null, rootCauseException);
        }
        return this;
    }

    public AlertActions dismissAlert() {
        try {
            waitForAlertToBePresent(driver);
            driver.switchTo().alert().dismiss();
            WebDriverElementActions.passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null,null);
        } catch (Exception rootCauseException) {
            WebDriverElementActions.failAction(driver, null, rootCauseException);
        }
        return this;
    }

    public String getAlertText() {
        try {
            waitForAlertToBePresent(driver);
            var alertText = driver.switchTo().alert().getText();
            ReportManager.logDiscrete("Alert Text is: [" + alertText + "]");
            WebDriverElementActions.passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null,null);
            return alertText;
        } catch (Exception rootCauseException) {
            WebDriverElementActions.failAction(driver, null, rootCauseException);
            return null;
        }
    }

    public void typeIntoPromptAlert(String text) {
        try {
            waitForAlertToBePresent(driver);
            driver.switchTo().alert().sendKeys(text);
            ReportManager.logDiscrete("Text typed into Alert is: [" + text + "]");
            WebDriverElementActions.passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null,null);
        } catch (Exception rootCauseException) {
            WebDriverElementActions.failAction(driver, null, rootCauseException);
        }

    }
}
