package com.shaft.gui.playwright.browser;

import com.shaft.gui.driver.*;

/** Playwright browser emulation facade. */
public final class EmulationActions implements EmulationActionsContract {
    private final BrowserActions browser;

    EmulationActions(BrowserActions browser) {
        this.browser = browser;
    }

    @Override public ScreenEmulationActionsContract screen() { return new Screen(this); }
    @Override public LocationEmulationActionsContract location() { return new Location(this); }
    @Override public MediaEmulationActionsContract media() { return new MediaActions(this); }
    @Override public RuntimeEmulationActionsContract runtime() { return new RuntimeActions(this); }
    @Override public BrowserActions and() { return browser; }

    private record Screen(EmulationActions owner) implements ScreenEmulationActionsContract {
        @Override public ScreenEmulationActionsContract viewport(int width, int height) {
            owner.browser.emulateViewportNamespace(width, height); return this;
        }
        @Override public ScreenEmulationActionsContract clearViewport() {
            owner.browser.clearViewportEmulationNamespace(); return this;
        }
        @Override public ScreenEmulationActionsContract screenSize(int width, int height) {
            owner.browser.unsupportedLiveContextEmulation("screen", "screen size", "playwright.deviceName"); return this;
        }
        @Override public ScreenEmulationActionsContract clearScreenSize() {
            owner.browser.unsupportedLiveContextEmulation("screen", "clear screen size", "playwright.deviceName"); return this;
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
            owner.browser.unsupportedLiveContextEmulation("location", "timezone", "playwright.timezoneId"); return this;
        }
        @Override public LocationEmulationActionsContract clearTimezone() {
            owner.browser.unsupportedLiveContextEmulation("location", "clear timezone", "playwright.timezoneId"); return this;
        }
        @Override public LocationEmulationActionsContract locale(String locale) {
            owner.browser.unsupportedLiveContextEmulation("location", "locale", "playwright.locale"); return this;
        }
        @Override public LocationEmulationActionsContract clearLocale() {
            owner.browser.unsupportedLiveContextEmulation("location", "clear locale", "playwright.locale"); return this;
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
            owner.browser.unsupportedLiveContextEmulation("runtime", "user agent", "playwright.userAgent", userAgent);
            return this;
        }
        @Override public RuntimeEmulationActionsContract clearUserAgent() {
            owner.browser.unsupportedLiveContextEmulation("runtime", "clear user agent", "playwright.userAgent"); return this;
        }
        @Override public RuntimeEmulationActionsContract disableScripting() {
            owner.browser.unsupportedLiveContextEmulation(
                    "runtime", "disable scripting", "playwright.javaScriptEnabled"); return this;
        }
        @Override public RuntimeEmulationActionsContract clearScriptingOverride() {
            owner.browser.unsupportedLiveContextEmulation("runtime", "clear scripting", "playwright.javaScriptEnabled"); return this;
        }
        @Override public EmulationActions and() { return owner; }
    }
}
