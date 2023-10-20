package com.shaft.gui.waits;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactoryHelper;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
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

    public WaitActions CustomExplicitWaits(Function<? super WebDriver, ?> conditions) {
        explicitWaits(conditions, SHAFT.Properties.timeouts.customExplicitWaitsTimeout());
        return this;
    }

    public static void explicitWaits(Function<? super WebDriver, ?> conditions, int timeoutWithSeconds) {
        try {
            new WebDriverWait(DriverFactoryHelper.getDriver(), Duration.ofSeconds(timeoutWithSeconds))
                    .until(conditions);
            ReportManager.log("Explicit wait until: \"" + conditions + "\".");
        } catch (TimeoutException toe) {
            FailureReporter.fail(toe.getMessage().split("\n")[0]);
        }
    }

}
