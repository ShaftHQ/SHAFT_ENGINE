package com.shaft.gui.browser;

import com.shaft.gui.driver.*;

/** Selenium/Appium browser emulation facade. */
public final class EmulationActions implements EmulationActionsContract {
    private final BrowserActions browser;

    EmulationActions(BrowserActions browser) {
        this.browser = browser;
    }

    @Override
    public ScreenEmulationActionsContract screen() {
        return new Screen(this);
    }

    @Override
    public LocationEmulationActionsContract location() {
        return new Location(this);
    }

    @Override
    public MediaEmulationActionsContract media() {
        return new MediaActions(this);
    }

    @Override
    public RuntimeEmulationActionsContract runtime() {
        return new RuntimeActions(this);
    }

    @Override
    public BrowserActions and() {
        return browser;
    }

    private record Screen(EmulationActions owner) implements ScreenEmulationActionsContract {
        @Override public ScreenEmulationActionsContract viewport(int width, int height) {
            owner.browser.emulateViewportNamespace(width, height); return this;
        }
        @Override public ScreenEmulationActionsContract clearViewport() {
            owner.browser.clearViewportEmulationNamespace(); return this;
        }
        @Override public ScreenEmulationActionsContract screenSize(int width, int height) {
            owner.browser.emulateScreenSizeNamespace(width, height); return this;
        }
        @Override public ScreenEmulationActionsContract clearScreenSize() {
            owner.browser.clearScreenSizeEmulationNamespace(); return this;
        }
        @Override public EmulationActions and() { return owner; }
    }

    private record Location(EmulationActions owner) implements LocationEmulationActionsContract {
        @Override public LocationEmulationActionsContract geolocation(double latitude, double longitude) {
            return geolocation(latitude, longitude, 0);
        }
        @Override public LocationEmulationActionsContract geolocation(double latitude, double longitude, double accuracy) {
            owner.browser.emulateGeolocationNamespace(latitude, longitude, accuracy); return this;
        }
        @Override public LocationEmulationActionsContract clearGeolocation() {
            owner.browser.clearGeolocationEmulationNamespace(); return this;
        }
        @Override public LocationEmulationActionsContract timezone(String timezoneId) {
            owner.browser.emulateTimezoneNamespace(timezoneId); return this;
        }
        @Override public LocationEmulationActionsContract clearTimezone() {
            owner.browser.clearTimezoneEmulationNamespace(); return this;
        }
        @Override public LocationEmulationActionsContract locale(String locale) {
            owner.browser.emulateLocaleNamespace(locale); return this;
        }
        @Override public LocationEmulationActionsContract clearLocale() {
            owner.browser.clearLocaleEmulationNamespace(); return this;
        }
        @Override public EmulationActions and() { return owner; }
    }

    private record MediaActions(EmulationActions owner) implements MediaEmulationActionsContract {
        @Override public MediaEmulationActionsContract type(EmulatedMediaType type) {
            owner.browser.emulateMediaTypeNamespace(type); return this;
        }
        @Override public MediaEmulationActionsContract colorScheme(EmulatedColorScheme scheme) {
            owner.browser.emulateColorSchemeNamespace(scheme); return this;
        }
        @Override public MediaEmulationActionsContract reducedMotion(EmulatedReducedMotion motion) {
            owner.browser.emulateReducedMotionNamespace(motion); return this;
        }
        @Override public MediaEmulationActionsContract reset() {
            owner.browser.resetMediaEmulationNamespace(); return this;
        }
        @Override public EmulationActions and() { return owner; }
    }

    private record RuntimeActions(EmulationActions owner) implements RuntimeEmulationActionsContract {
        @Override public RuntimeEmulationActionsContract userAgent(String userAgent) {
            owner.browser.emulateUserAgentNamespace(userAgent); return this;
        }
        @Override public RuntimeEmulationActionsContract clearUserAgent() {
            owner.browser.clearUserAgentEmulationNamespace(); return this;
        }
        @Override public RuntimeEmulationActionsContract disableScripting() {
            owner.browser.disableScriptingEmulationNamespace(); return this;
        }
        @Override public RuntimeEmulationActionsContract clearScriptingOverride() {
            owner.browser.clearScriptingEmulationNamespace(); return this;
        }
        @Override public EmulationActions and() { return owner; }
    }
}
