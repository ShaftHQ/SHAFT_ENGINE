package com.shaft.driver.internal;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.internal.BrowserActionsHelper;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.element.internal.Actions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import org.openqa.selenium.WebDriver;

public class FluentWebDriverAction {
    protected DriverFactoryHelper driverFactoryHelper;
    protected ElementActionsHelper elementActionsHelper;
    protected BrowserActionsHelper browserActionsHelper;

    public void initialize() {
        this.driverFactoryHelper = new DriverFactory().getHelper();
        JavaScriptWaitManager.waitForLazyLoading(driverFactoryHelper.getDriver());
        this.elementActionsHelper = new ElementActionsHelper(false);
        this.browserActionsHelper = new BrowserActionsHelper(false);
    }

    public void initialize(WebDriver driver) {
        this.driverFactoryHelper = new DriverFactoryHelper(driver);
        JavaScriptWaitManager.waitForLazyLoading(driver);
        this.elementActionsHelper = new ElementActionsHelper(false);
        this.browserActionsHelper = new BrowserActionsHelper(false);
    }

    public void initialize(WebDriver driver, boolean isSilent) {
        initialize(driver);
        this.elementActionsHelper = new ElementActionsHelper(isSilent);
        this.browserActionsHelper = new BrowserActionsHelper(isSilent);
    }

    public void initialize(DriverFactoryHelper helper) {
        this.driverFactoryHelper = helper;
        JavaScriptWaitManager.waitForLazyLoading(helper.getDriver());
        this.elementActionsHelper = new ElementActionsHelper(false);
        this.browserActionsHelper = new BrowserActionsHelper(false);
    }


    public TouchActions performTouchAction() {
        return new TouchActions(driverFactoryHelper);
    }

    public AlertActions performAlertAction() {
        return new AlertActions(driverFactoryHelper);
    }

    public Actions performElementAction() {
        return new Actions(driverFactoryHelper);
    }

    public BrowserActions performBrowserAction() {
        return new BrowserActions(driverFactoryHelper);
    }

    public TouchActions touch() {
        return new TouchActions(driverFactoryHelper);
    }

    public AlertActions alert() {
        return new AlertActions(driverFactoryHelper);
    }

    public Actions element() {
        return new Actions(driverFactoryHelper);
    }

    public BrowserActions browser() {
        return new BrowserActions(driverFactoryHelper);
    }

    /**
     * Returns this instance to allow fluent (method-chaining) syntax between successive actions.
     * Using {@code .and()} improves readability by making multi-step test sequences read like
     * natural language.
     *
     * <p>Example:
     * <pre>{@code
     * driver.element().type(searchBox, "query")
     *       .and().browser().captureScreenshot();
     * }</pre>
     *
     * @return this {@code FluentWebDriverAction} instance, enabling continued method chaining
     */
    public FluentWebDriverAction and() {
        return this;
    }
}
