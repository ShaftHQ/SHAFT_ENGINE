package com.shaft.gui.element;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactoryHelper;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.tools.io.ReportManager;
import org.openqa.selenium.NoAlertPresentException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.time.Duration;

@SuppressWarnings("unused")
public class AlertActions {

    public AlertActions(WebDriver driver) {
        new AlertActions();
    }

    public AlertActions() {
    }

    public static AlertActions getInstance() {
        return new AlertActions();
    }

    public TouchActions touch() {
        return new TouchActions();
    }

    public BrowserActions browser() {
        return BrowserActions.getInstance();
    }

    public AlertActions and() {
        return this;
    }

    private static void waitForAlertToBePresent() {
        try {
            (new WebDriverWait(DriverFactoryHelper.getDriver(), Duration.ofSeconds((long) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout()))).until(ExpectedConditions.alertIsPresent());
            DriverFactoryHelper.getDriver().switchTo().alert();
            ReportManager.logDiscrete("Alert is present");
        } catch (Exception rootCauseException) {
            ReportManager.logDiscrete("Alert is not present");
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver(), null, rootCauseException);
        }
    }

    public ElementActions performElementAction() {
        return ElementActions.getInstance();
    }

    public ElementActions element() {
        return ElementActions.getInstance();
    }

    public boolean isAlertPresent() {
        try {
            DriverFactoryHelper.getDriver().switchTo().alert();
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
            ReportManager.logDiscrete("Alert is present");
            return true;
        } catch (NoAlertPresentException exception) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver(), null, exception);
            ReportManager.logDiscrete("Alert is not present");
            return false;
        } catch (Exception rootCauseException) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver(), null, rootCauseException);
            return false;
        }
    }

    @SuppressWarnings("UnusedReturnValue")
    public AlertActions acceptAlert() {
        try {
            waitForAlertToBePresent();
            DriverFactoryHelper.getDriver().switchTo().alert().accept();
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
        } catch (Exception rootCauseException) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver(), null, rootCauseException);
        }
        return this;
    }

    @SuppressWarnings("UnusedReturnValue")
    public AlertActions dismissAlert() {
        try {
            waitForAlertToBePresent();
            DriverFactoryHelper.getDriver().switchTo().alert().dismiss();
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
        } catch (Exception rootCauseException) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver(), null, rootCauseException);
        }
        return this;
    }

    public String getAlertText() {
        try {
            waitForAlertToBePresent();
            var alertText = DriverFactoryHelper.getDriver().switchTo().alert().getText();
            ReportManager.logDiscrete("Alert Text is: [" + alertText + "]");
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
            return alertText;
        } catch (Exception rootCauseException) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver(), null, rootCauseException);
            return null;
        }
    }

    @SuppressWarnings("UnusedReturnValue")
    public AlertActions typeIntoPromptAlert(String text) {
        try {
            waitForAlertToBePresent();
            DriverFactoryHelper.getDriver().switchTo().alert().sendKeys(text);
            ReportManager.logDiscrete("Text typed into Alert is: [" + text + "]");
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
        } catch (Exception rootCauseException) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver(), null, rootCauseException);
        }
        return this;
    }
}
