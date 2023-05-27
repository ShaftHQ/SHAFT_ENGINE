package com.shaft.gui.browser;

import com.shaft.gui.browser.internal.FluentBrowserActions;
import org.openqa.selenium.WebDriver;

@SuppressWarnings("unused")
public class BrowserActions extends FluentBrowserActions {

    public BrowserActions() {
        super();
    }

    public BrowserActions(WebDriver driver) {
        super();
    }

    public static BrowserActions getInstance() {
        return new BrowserActions();
    }
}