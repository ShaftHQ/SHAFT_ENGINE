package com.shaft.gui.waits;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactoryHelper;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.tools.io.ReportManager;
import org.apache.poi.ss.formula.functions.T;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.Wait;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.testng.Assert;

import java.time.Duration;
import java.util.function.Function;

public class WaitActions {

    public TouchActions touch() {
        return new TouchActions();
    }

    public AlertActions alert() {
        return new AlertActions();
    }

    public BrowserActions browser() {
        return BrowserActions.getInstance();
    }

    public ElementActions element() {
        return ElementActions.getInstance();
    }

    public WaitActions and() {
        return this;
    }

    public WaitActions seleniumExplicitWaits(Function<? super WebDriver, ?> conditions) {
        try {
            new WebDriverWait(DriverFactoryHelper.getDriver(), Duration.ofSeconds(SHAFT.Properties.timeouts.webDriverTimeout()))
                    .until(conditions);
            ReportManager.log("Selenium Explicit wait \"" + conditions + "\".");
        } catch (TimeoutException toe) {
            ReportManager.logDiscrete(toe.getMessage());
            Assert.fail(toe.getMessage());
        }

        return this;
    }

    public WaitActions waitForLazyLoading() {
        JavaScriptWaitManager.waitForLazyLoading();
        return this;
    }

}
