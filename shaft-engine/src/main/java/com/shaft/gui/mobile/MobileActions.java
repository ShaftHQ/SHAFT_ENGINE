package com.shaft.gui.mobile;

import com.shaft.driver.SHAFT;
import com.shaft.gui.driver.DriverContract;
import com.shaft.gui.driver.MobileActionsContract;
import com.shaft.gui.driver.MobileApplicationActionsContract;
import com.shaft.gui.driver.MobileBiometricActionsContract;
import com.shaft.gui.driver.MobileContextActionsContract;
import com.shaft.gui.driver.MobileDeviceActionsContract;
import com.shaft.gui.driver.MobileEvidenceActionsContract;
import com.shaft.gui.driver.MobileFileActionsContract;
import com.shaft.gui.driver.MobileGestureActionsContract;
import com.shaft.gui.driver.MobileLogActionsContract;
import com.shaft.gui.driver.MobilePerformanceActionsContract;
import com.shaft.gui.driver.MobileRecordingActionsContract;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.PerformsTouchActions;
import com.shaft.gui.element.TouchActions;

/** Selenium/Appium implementation of the categorized mobile facade. */
public final class MobileActions implements MobileActionsContract {
    private final SHAFT.GUI.WebDriver owner;
    private final AppiumDriver driver;

    public MobileActions(SHAFT.GUI.WebDriver owner) {
        this.driver = liveAppiumDriver(owner);
        this.owner = owner;
    }

    @Override
    public MobileApplicationActionsContract app() {
        return new ApplicationActions(this);
    }

    @Override
    public MobileDeviceActionsContract device() {
        return new DeviceActions(this);
    }

    @Override
    public MobileGestureActionsContract gestures() {
        return new GestureActions(this);
    }

    @Override
    public MobileContextActionsContract context() {
        return new ContextActions(this);
    }

    @Override
    public MobileFileActionsContract files() {
        return new FileActions(this);
    }

    @Override
    public MobileLogActionsContract logs() {
        throw unsupported("device logs");
    }

    @Override
    public MobileBiometricActionsContract biometrics() {
        throw unsupported("biometrics");
    }

    @Override
    public MobilePerformanceActionsContract performance() {
        throw unsupported("performance data");
    }

    @Override
    public MobileRecordingActionsContract recording() {
        throw unsupported("screen recording");
    }

    @Override
    public MobileEvidenceActionsContract evidence() {
        throw unsupported("evidence capture");
    }

    @Override
    public DriverContract and() {
        return owner;
    }

    AppiumDriver driver() {
        if (driver.getSessionId() == null) {
            throw new UnsupportedOperationException("Mobile actions require a live Appium session.");
        }
        return driver;
    }

    AppiumDriver traceDriver() {
        return driver;
    }

    TouchActions touchActions() {
        if (!(driver() instanceof PerformsTouchActions)) {
            throw new UnsupportedOperationException(
                    "The live Appium session does not support native touch gestures.");
        }
        return owner.touch();
    }

    private UnsupportedOperationException unsupported(String category) {
        return new UnsupportedOperationException("Mobile " + category + " actions are not available yet for this session.");
    }

    private static AppiumDriver liveAppiumDriver(SHAFT.GUI.WebDriver owner) {
        if (owner == null) {
            throw new UnsupportedOperationException("Mobile actions require a live Appium session.");
        }
        Object candidate;
        try {
            candidate = owner.getNativeDriver();
        } catch (RuntimeException closedSession) {
            throw new UnsupportedOperationException("Mobile actions require a live Appium session.");
        }
        if (!(candidate instanceof AppiumDriver appiumDriver) || appiumDriver.getSessionId() == null) {
            throw new UnsupportedOperationException("Mobile actions require a live Appium session.");
        }
        return appiumDriver;
    }
}
