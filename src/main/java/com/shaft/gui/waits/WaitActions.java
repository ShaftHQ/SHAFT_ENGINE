package com.shaft.gui.waits;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.time.Duration;
import java.util.function.Function;

public class WaitActions {
    private static DriverFactoryHelper helper;

    public TouchActions touch() {
        return new TouchActions(helper);
    }

    public AlertActions alert() {
        return new AlertActions(helper);
    }

    public BrowserActions browser() {
        return BrowserActions.getInstance(helper);
    }

    public ElementActions element() {
        return ElementActions.getInstance(helper);
    }

    public WaitActions and() {
        return this;
    }

    public WaitActions waitUntil(DriverFactoryHelper helper, Function<? super WebDriver, ?> conditions) {
        WaitActions.helper = helper;
        explicitWaits(conditions, SHAFT.Properties.timeouts.waitUntilTimeout());
        return this;
    }

    public static void explicitWaits(Function<? super WebDriver, ?> conditions, int timeoutWithSeconds) {
        try {
            new WebDriverWait(helper.getDriver(), Duration.ofSeconds(timeoutWithSeconds))
                    .until(conditions);
            ReportManager.log("Explicit wait until: \"" + conditions + "\".");
        } catch (TimeoutException toe) {
            FailureReporter.fail(toe.getMessage().split("\n")[0]);
        }
    }

}
