package com.shaft.gui.element;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.tools.io.ReportManager;
import org.openqa.selenium.NoAlertPresentException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.time.Duration;

@SuppressWarnings("unused")
public class AlertActions extends FluentWebDriverAction {
    public AlertActions() {
        initialize();
    }

    public AlertActions(WebDriver driver) {
        initialize(driver);
    }

    public AlertActions(DriverFactoryHelper helper) {
        initialize(helper);
    }

    private void waitForAlertToBePresent() {
        try {
            (new WebDriverWait(helper.getDriver(), Duration.ofSeconds((long) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout()))).until(ExpectedConditions.alertIsPresent());
            helper.getDriver().switchTo().alert();
            ReportManager.logDiscrete("Alert is present");
        } catch (Exception rootCauseException) {
            ReportManager.logDiscrete("Alert is not present");
            ElementActionsHelper.failAction(helper.getDriver(), null, rootCauseException);
        }
    }

    public boolean isAlertPresent() {
        try {
            helper.getDriver().switchTo().alert();
            ElementActionsHelper.passAction(helper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
            ReportManager.logDiscrete("Alert is present");
            return true;
        } catch (NoAlertPresentException exception) {
            ElementActionsHelper.failAction(helper.getDriver(), null, exception);
            ReportManager.logDiscrete("Alert is not present");
            return false;
        } catch (Exception rootCauseException) {
            ElementActionsHelper.failAction(helper.getDriver(), null, rootCauseException);
            return false;
        }
    }

    @SuppressWarnings("UnusedReturnValue")
    public AlertActions acceptAlert() {
        try {
            waitForAlertToBePresent();
            helper.getDriver().switchTo().alert().accept();
            ElementActionsHelper.passAction(helper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
        } catch (Exception rootCauseException) {
            ElementActionsHelper.failAction(helper.getDriver(), null, rootCauseException);
        }
        return this;
    }

    @SuppressWarnings("UnusedReturnValue")
    public AlertActions dismissAlert() {
        try {
            waitForAlertToBePresent();
            helper.getDriver().switchTo().alert().dismiss();
            ElementActionsHelper.passAction(helper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
        } catch (Exception rootCauseException) {
            ElementActionsHelper.failAction(helper.getDriver(), null, rootCauseException);
        }
        return this;
    }

    public String getAlertText() {
        try {
            waitForAlertToBePresent();
            var alertText = helper.getDriver().switchTo().alert().getText();
            ReportManager.logDiscrete("Alert Text is: [" + alertText + "]");
            ElementActionsHelper.passAction(helper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
            return alertText;
        } catch (Exception rootCauseException) {
            ElementActionsHelper.failAction(helper.getDriver(), null, rootCauseException);
            return null;
        }
    }

    @SuppressWarnings("UnusedReturnValue")
    public AlertActions typeIntoPromptAlert(String text) {
        try {
            waitForAlertToBePresent();
            helper.getDriver().switchTo().alert().sendKeys(text);
            ReportManager.logDiscrete("Text typed into Alert is: [" + text + "]");
            ElementActionsHelper.passAction(helper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
        } catch (Exception rootCauseException) {
            ElementActionsHelper.failAction(helper.getDriver(), null, rootCauseException);
        }
        return this;
    }
}
