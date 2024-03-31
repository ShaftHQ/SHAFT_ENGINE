package com.shaft.gui.element;

import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.tools.io.ReportManager;
import org.openqa.selenium.NoAlertPresentException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.ExpectedConditions;

@SuppressWarnings({"unused", "UnusedReturnValue"})
public class AlertActions extends FluentWebDriverAction {
    public AlertActions() {
        initialize();
        waitForAlertToBePresent();
    }

    public AlertActions(WebDriver driver) {
        initialize(driver);
        waitForAlertToBePresent();
    }

    public AlertActions(DriverFactoryHelper helper) {
        initialize(helper);
        waitForAlertToBePresent();
    }

    private void waitForAlertToBePresent() {
        try {
            new SynchronizationManager(driver).fluentWait(false)
                    .until(f -> ExpectedConditions.alertIsPresent());
            driverFactoryHelper.getDriver().switchTo().alert();
            ReportManager.logDiscrete("Alert is present");
        } catch (Exception rootCauseException) {
            ReportManager.logDiscrete("Alert is not present");
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null, rootCauseException);
        }
    }

    public boolean isAlertPresent() {
        try {
            waitForAlertToBePresent();
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
            ReportManager.logDiscrete("Alert is present");
            return true;
        } catch (NoAlertPresentException exception) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null, exception);
            ReportManager.logDiscrete("Alert is not present");
            return false;
        } catch (Exception rootCauseException) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null, rootCauseException);
            return false;
        }
    }

    @SuppressWarnings("UnusedReturnValue")
    public AlertActions acceptAlert() {
        try {
            waitForAlertToBePresent();
            driverFactoryHelper.getDriver().switchTo().alert().accept();
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
        } catch (Exception rootCauseException) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null, rootCauseException);
        }
        return this;
    }

    @SuppressWarnings("UnusedReturnValue")
    public AlertActions dismissAlert() {
        try {
            waitForAlertToBePresent();
            driverFactoryHelper.getDriver().switchTo().alert().dismiss();
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
        } catch (Exception rootCauseException) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null, rootCauseException);
        }
        return this;
    }

    public String getAlertText() {
        try {
            waitForAlertToBePresent();
            var alertText = driverFactoryHelper.getDriver().switchTo().alert().getText();
            ReportManager.logDiscrete("Alert Text is: [" + alertText + "]");
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
            return alertText;
        } catch (Exception rootCauseException) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null, rootCauseException);
            return null;
        }
    }

    @SuppressWarnings("UnusedReturnValue")
    public AlertActions typeIntoPromptAlert(String text) {
        try {
            waitForAlertToBePresent();
            driverFactoryHelper.getDriver().switchTo().alert().sendKeys(text);
            ReportManager.logDiscrete("Text typed into Alert is: [" + text + "]");
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
        } catch (Exception rootCauseException) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null, rootCauseException);
        }
        return this;
    }
}
