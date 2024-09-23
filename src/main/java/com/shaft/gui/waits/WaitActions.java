package com.shaft.gui.waits;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.tools.io.ReportManager;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.time.Duration;
import java.util.function.Function;

public class WaitActions extends FluentWebDriverAction {
    public WaitActions(DriverFactoryHelper helper) {
        initialize(helper);
    }

    @Override public WaitActions waitUntil(Function<? super WebDriver, ?> conditions) {
        explicitWaits(conditions, SHAFT.Properties.timeouts.waitUntilTimeout());
        return this;
    }

    public void explicitWaits(Function<? super WebDriver, ?> conditions, int timeoutWithSeconds) {
        try {
            new WebDriverWait(driver, Duration.ofSeconds(timeoutWithSeconds))
                    .until(conditions);
            ReportManager.log("Explicit wait until: \"" + conditions + "\".");
        } catch (TimeoutException toe) {
            elementActionsHelper.failAction(driver, null, toe);
        }
    }
}