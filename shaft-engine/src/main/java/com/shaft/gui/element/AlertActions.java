package com.shaft.gui.element;

import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.tools.io.ReportManager;
import org.openqa.selenium.NoAlertPresentException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.ExpectedConditions;

/**
 * Provides actions for interacting with browser alert, confirm, and prompt dialogs.
 * <p>
 * {@code AlertActions} extends {@link FluentWebDriverAction} and exposes a fluent API
 * for accepting or dismissing alerts, reading alert text, and typing into prompt dialogs.
 * Every constructor automatically waits for an alert to be present before returning,
 * so any subsequent action is guaranteed to operate on a live dialog.
 *
 * <p>Example usage:
 * <pre>{@code
 * driver.alert().acceptAlert();
 * driver.alert().typeIntoPromptAlert("my input").acceptAlert();
 * }</pre>
 *
 * @see FluentWebDriverAction
 */
@SuppressWarnings({"unused", "UnusedReturnValue"})
public class AlertActions extends FluentWebDriverAction {
    /**
     * Creates a new {@code AlertActions} instance using the currently active SHAFT-managed driver.
     * Waits for an alert to be present before returning.
     *
     * <p>Example:
     * <pre>{@code
     * AlertActions alert = new AlertActions();
     * alert.acceptAlert();
     * }</pre>
     */
    public AlertActions() {
        initialize();
        waitForAlertToBePresent();
    }

    /**
     * Creates a new {@code AlertActions} instance wrapping the provided {@link WebDriver}.
     * Waits for an alert to be present before returning.
     *
     * <p>Example:
     * <pre>{@code
     * AlertActions alert = new AlertActions(webDriver);
     * alert.dismissAlert();
     * }</pre>
     *
     * @param driver the {@link WebDriver} instance whose current window will be used to
     *               interact with the alert
     */
    public AlertActions(WebDriver driver) {
        initialize(driver);
        waitForAlertToBePresent();
    }

    /**
     * Creates a new {@code AlertActions} instance using an existing {@link DriverFactoryHelper}.
     * Waits for an alert to be present before returning.
     *
     * <p>Example:
     * <pre>{@code
     * AlertActions alert = new AlertActions(driverFactoryHelper);
     * alert.acceptAlert();
     * }</pre>
     *
     * @param helper the {@link DriverFactoryHelper} that provides the underlying driver and
     *               session configuration
     */
    public AlertActions(DriverFactoryHelper helper) {
        initialize(helper);
        waitForAlertToBePresent();
    }

    private void waitForAlertToBePresent() {
        try {
            new SynchronizationManager(driverFactoryHelper.getDriver()).fluentWait(false)
                    .until(f -> ExpectedConditions.alertIsPresent());
            driverFactoryHelper.getDriver().switchTo().alert();
            ReportManager.logDiscrete("Alert is present");
        } catch (Exception rootCauseException) {
            ReportManager.logDiscrete("Alert is not present");
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null, rootCauseException);
        }
    }

    /**
     * Checks whether a browser alert dialog is currently present.
     *
     * <p>Example:
     * <pre>{@code
     * if (driver.alert().isAlertPresent()) {
     *     driver.alert().acceptAlert();
     * }
     * }</pre>
     *
     * @return {@code true} if an alert is present and reachable; {@code false} otherwise
     */
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

    /**
     * Accepts (clicks "OK" on) the currently displayed alert, confirm, or prompt dialog.
     * If no alert is present, the action is recorded as a failure.
     *
     * <p>Example:
     * <pre>{@code
     * driver.alert().acceptAlert();
     * }</pre>
     *
     * @return this {@code AlertActions} instance for fluent method chaining
     */
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

    /**
     * Dismisses (clicks "Cancel" on) the currently displayed alert or confirm dialog.
     * If no alert is present, the action is recorded as a failure.
     *
     * <p>Example:
     * <pre>{@code
     * driver.alert().dismissAlert();
     * }</pre>
     *
     * @return this {@code AlertActions} instance for fluent method chaining
     */
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

    /**
     * Retrieves the text message displayed in the currently active browser alert dialog.
     *
     * <p>Example:
     * <pre>{@code
     * String message = driver.alert().getAlertText();
     * }</pre>
     *
     * @return the alert message text, or {@code null} if the alert is not present or
     *         an error occurs while retrieving the text
     */
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

    /**
     * Types the specified text into a JavaScript prompt dialog's input field.
     * The prompt must already be present; call {@link #acceptAlert()} afterwards to submit it.
     *
     * <p>Example:
     * <pre>{@code
     * driver.alert().typeIntoPromptAlert("myValue").acceptAlert();
     * }</pre>
     *
     * @param text the text to send to the prompt dialog's input field; must not be {@code null}
     * @return this {@code AlertActions} instance for fluent method chaining
     */
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
