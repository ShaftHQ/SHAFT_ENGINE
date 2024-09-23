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
import com.shaft.gui.waits.WaitActions;
import org.openqa.selenium.WebDriver;

import java.util.function.Function;

public class FluentWebDriverAction {
    protected DriverFactoryHelper driverFactoryHelper;
    protected WebDriver driver;
    protected ElementActionsHelper elementActionsHelper;
    protected BrowserActionsHelper browserActionsHelper;

    public void initialize() {
        this.driverFactoryHelper = new DriverFactory().getHelper();
        this.driver = driverFactoryHelper.getDriver();
        JavaScriptWaitManager.waitForLazyLoading(this.driver);
        this.elementActionsHelper = new ElementActionsHelper(false);
        this.browserActionsHelper = new BrowserActionsHelper(false);
    }

    public void initialize(WebDriver driver) {
        this.driver = driver;
        this.driverFactoryHelper = new DriverFactoryHelper(this.driver);
        JavaScriptWaitManager.waitForLazyLoading(this.driver);
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
        this.driver = helper.getDriver();
        JavaScriptWaitManager.waitForLazyLoading(this.driver);
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

    public FluentWebDriverAction and() {
        return this;
    }

    public WaitActions waitUntil(Function<? super WebDriver, ?> conditions) {
        return new WaitActions(driverFactoryHelper).waitUntil(conditions);
    }
}
