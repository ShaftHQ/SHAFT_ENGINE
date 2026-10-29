package com.shaft.gui.capabilities.internal;

import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.HasCapabilities;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.RemoteWebDriver;

import javax.annotation.Nullable;

/**
 * Appium java-client 11 no longer extends Selenium {@link RemoteWebDriver}.
 * Session and capability access must cover both Selenium remote drivers and
 * {@link AppiumDriver} (via {@link HasCapabilities} / Appium session id).
 */
public final class LiveSessionSupport {
    private LiveSessionSupport() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Reads negotiated capabilities from a Selenium or Appium driver.
     *
     * @return capabilities when the driver exposes them; {@code null} otherwise or on failure
     */
    @Nullable
    public static Capabilities capabilities(WebDriver driver) {
        if (!(driver instanceof HasCapabilities hasCapabilities)) {
            return null;
        }
        try {
            return hasCapabilities.getCapabilities();
        } catch (RuntimeException ignored) {
            return null;
        }
    }

    /**
     * Detects a Selenium or Appium remote session whose session id is already gone.
     *
     * @return {@code true} when a Selenium or Appium remote session exists and reports a null session id
     */
    public static boolean isClosedRemoteSession(WebDriver driver) {
        if (driver instanceof RemoteWebDriver remoteWebDriver) {
            return remoteWebDriver.getSessionId() == null;
        }
        if (driver instanceof AppiumDriver appiumDriver) {
            return appiumDriver.getSessionId() == null;
        }
        return false;
    }

    /**
     * Detects a Selenium or Appium remote session that still reports a session id.
     *
     * @return {@code true} when a Selenium or Appium remote session reports a non-null session id
     */
    public static boolean hasLiveRemoteSession(WebDriver driver) {
        if (driver instanceof RemoteWebDriver remoteWebDriver) {
            return remoteWebDriver.getSessionId() != null;
        }
        if (driver instanceof AppiumDriver appiumDriver) {
            return appiumDriver.getSessionId() != null;
        }
        return false;
    }

    /**
     * Identifies drivers that are either a Selenium remote session or an Appium session.
     *
     * @return {@code true} when the driver is a Selenium remote or Appium session
     */
    public static boolean isRemoteOrAppiumSession(WebDriver driver) {
        return driver instanceof RemoteWebDriver || driver instanceof AppiumDriver;
    }
}
