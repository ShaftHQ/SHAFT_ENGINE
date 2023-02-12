package com.shaft.gui.element;

import com.shaft.tools.io.ReportManager;
import io.github.shafthq.shaft.driver.helpers.DriverFactoryHelper;
import io.github.shafthq.shaft.gui.browser.FluentBrowserActions;
import io.github.shafthq.shaft.gui.element.ElementActionsHelper;
import io.github.shafthq.shaft.gui.element.FluentElementActions;
import org.openqa.selenium.NoAlertPresentException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.time.Duration;

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

    public FluentBrowserActions browser() {
        return new FluentBrowserActions();
    }

    public AlertActions and() {
        return this;
    }

    private static void waitForAlertToBePresent() {
        try {
            (new WebDriverWait(DriverFactoryHelper.getDriver().get(), Duration.ofSeconds(Integer.parseInt(System.getProperty("defaultElementIdentificationTimeout"))))).until(ExpectedConditions.alertIsPresent());
            DriverFactoryHelper.getDriver().get().switchTo().alert();
            ReportManager.logDiscrete("Alert is present");
        } catch (Exception rootCauseException) {
            ReportManager.logDiscrete("Alert is not present");
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, rootCauseException);
        }
    }

    public FluentElementActions performElementAction() {
        return new FluentElementActions(DriverFactoryHelper.getDriver().get());
    }

    public boolean isAlertPresent() {
        try {
            DriverFactoryHelper.getDriver().get().switchTo().alert();
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
            ReportManager.logDiscrete("Alert is present");
            return true;
        } catch (NoAlertPresentException exception) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, exception);
            ReportManager.logDiscrete("Alert is not present");
            return false;
        } catch (Exception rootCauseException) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, rootCauseException);
            return false;
        }
    }

    public AlertActions acceptAlert() {
        try {
            waitForAlertToBePresent();
            DriverFactoryHelper.getDriver().get().switchTo().alert().accept();
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
        } catch (Exception rootCauseException) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, rootCauseException);
        }
        return this;
    }

    public AlertActions dismissAlert() {
        try {
            waitForAlertToBePresent();
            DriverFactoryHelper.getDriver().get().switchTo().alert().dismiss();
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
        } catch (Exception rootCauseException) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, rootCauseException);
        }
        return this;
    }

    public String getAlertText() {
        try {
            waitForAlertToBePresent();
            var alertText = DriverFactoryHelper.getDriver().get().switchTo().alert().getText();
            ReportManager.logDiscrete("Alert Text is: [" + alertText + "]");
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
            return alertText;
        } catch (Exception rootCauseException) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, rootCauseException);
            return null;
        }
    }

    public AlertActions typeIntoPromptAlert(String text) {
        try {
            waitForAlertToBePresent();
            DriverFactoryHelper.getDriver().get().switchTo().alert().sendKeys(text);
            ReportManager.logDiscrete("Text typed into Alert is: [" + text + "]");
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
        } catch (Exception rootCauseException) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, rootCauseException);
        }
        return this;
    }
}
