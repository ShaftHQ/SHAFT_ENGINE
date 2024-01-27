package com.shaft.driver.internal;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.waits.WaitActions;
import org.openqa.selenium.WebDriver;

import java.util.function.Function;

public class FluentWebDriverAction {
    protected DriverFactoryHelper helper;
    protected WebDriver driver;

    public void initialize() {
        this.helper = new DriverFactory().getHelper();
        this.driver = helper.getDriver();
        JavaScriptWaitManager.waitForLazyLoading(this.driver);
    }

    public void initialize(WebDriver driver) {
        this.driver = driver;
        this.helper = new DriverFactoryHelper(this.driver);
        JavaScriptWaitManager.waitForLazyLoading(this.driver);
    }

    public void initialize(DriverFactoryHelper helper) {
        this.helper = helper;
        this.driver = helper.getDriver();
        JavaScriptWaitManager.waitForLazyLoading(this.driver);
    }


    public TouchActions performTouchAction() {
        return new TouchActions(helper);
    }

    public AlertActions performAlertAction() {
        return new AlertActions(helper);
    }

    public ElementActions performElementAction() {
        return new ElementActions(helper);
    }

    public BrowserActions performBrowserAction() {
        return new BrowserActions(helper);
    }

    public TouchActions touch() {
        return new TouchActions(helper);
    }

    public AlertActions alert() {
        return new AlertActions(helper);
    }

    public ElementActions element() {
        return new ElementActions(helper);
    }

    public BrowserActions browser() {
        return new BrowserActions(helper);
    }

    public FluentWebDriverAction and() {
        return this;
    }

    public WaitActions waitUntil(Function<? super WebDriver, ?> conditions) {
        return new WaitActions(helper).waitUntil(conditions);
    }
}
