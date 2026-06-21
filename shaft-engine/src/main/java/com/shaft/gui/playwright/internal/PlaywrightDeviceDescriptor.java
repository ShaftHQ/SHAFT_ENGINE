package com.shaft.gui.playwright.internal;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.microsoft.playwright.Browser;
import com.microsoft.playwright.Playwright;
import com.microsoft.playwright.impl.PlaywrightImpl;

import java.util.Locale;
import java.util.Optional;

final class PlaywrightDeviceDescriptor {
    private static final String GALAXY_S26_ULTRA_USER_AGENT = "Mozilla/5.0 (Linux; Android 16; SM-S948U) "
            + "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.7778.96 Mobile Safari/537.36";
    private static final String IPHONE_17_PRO_MAX_USER_AGENT = "Mozilla/5.0 (iPhone; CPU iPhone OS 26_0 like Mac OS X) "
            + "AppleWebKit/605.1.15 (KHTML, like Gecko) Version/26.4 Mobile/15E148 Safari/604.1";

    private final String name;
    private final String userAgent;
    private final int viewportWidth;
    private final int viewportHeight;
    private final int screenWidth;
    private final int screenHeight;
    private final double deviceScaleFactor;
    private final boolean mobile;
    private final boolean touch;
    private final String defaultBrowserType;

    private PlaywrightDeviceDescriptor(String name, String userAgent, int viewportWidth, int viewportHeight,
                                       int screenWidth, int screenHeight, double deviceScaleFactor, boolean mobile,
                                       boolean touch, String defaultBrowserType) {
        this.name = name;
        this.userAgent = userAgent;
        this.viewportWidth = viewportWidth;
        this.viewportHeight = viewportHeight;
        this.screenWidth = screenWidth;
        this.screenHeight = screenHeight;
        this.deviceScaleFactor = deviceScaleFactor;
        this.mobile = mobile;
        this.touch = touch;
        this.defaultBrowserType = defaultBrowserType;
    }

    static Optional<PlaywrightDeviceDescriptor> resolve(Playwright playwright, String deviceName) {
        if (deviceName == null || deviceName.isBlank()) {
            return Optional.empty();
        }

        if (playwright != null) {
            Optional<PlaywrightDeviceDescriptor> registryDescriptor =
                    fromRegistry(deviceDescriptors(playwright), deviceName);
            if (registryDescriptor.isPresent()) {
                return registryDescriptor;
            }
        }

        Optional<PlaywrightDeviceDescriptor> latestDescriptor = latestDeviceAlias(deviceName);
        if (latestDescriptor.isPresent()) {
            return latestDescriptor;
        }

        throw new IllegalArgumentException("Unsupported Playwright device descriptor: " + deviceName);
    }

    static Optional<PlaywrightDeviceDescriptor> fromRegistry(JsonArray descriptors, String deviceName) {
        if (descriptors == null || deviceName == null || deviceName.isBlank()) {
            return Optional.empty();
        }

        String expected = normalize(deviceName);
        for (var element : descriptors) {
            JsonObject entry = element.getAsJsonObject();
            String name = entry.get("name").getAsString();
            if (expected.equals(normalize(name))) {
                return Optional.of(fromDescriptor(name, entry.getAsJsonObject("descriptor")));
            }
        }
        return Optional.empty();
    }

    void applyTo(Browser.NewContextOptions contextOptions) {
        contextOptions
                .setUserAgent(userAgent)
                .setViewportSize(viewportWidth, viewportHeight)
                .setScreenSize(screenWidth, screenHeight)
                .setDeviceScaleFactor(deviceScaleFactor)
                .setIsMobile(mobile)
                .setHasTouch(touch);
    }

    String defaultBrowserType() {
        return defaultBrowserType;
    }

    String name() {
        return name;
    }

    private static PlaywrightDeviceDescriptor fromDescriptor(String name, JsonObject descriptor) {
        JsonObject viewport = descriptor.getAsJsonObject("viewport");
        JsonObject screen = descriptor.has("screen")
                ? descriptor.getAsJsonObject("screen")
                : viewport;
        return new PlaywrightDeviceDescriptor(
                name,
                descriptor.get("userAgent").getAsString(),
                viewport.get("width").getAsInt(),
                viewport.get("height").getAsInt(),
                screen.get("width").getAsInt(),
                screen.get("height").getAsInt(),
                descriptor.get("deviceScaleFactor").getAsDouble(),
                descriptor.get("isMobile").getAsBoolean(),
                descriptor.get("hasTouch").getAsBoolean(),
                descriptor.get("defaultBrowserType").getAsString());
    }

    private static JsonArray deviceDescriptors(Playwright playwright) {
        if (playwright instanceof PlaywrightImpl playwrightImpl) {
            return playwrightImpl.deviceDescriptors();
        }
        throw new IllegalStateException("Playwright runtime does not expose device descriptors.");
    }

    private static Optional<PlaywrightDeviceDescriptor> latestDeviceAlias(String deviceName) {
        return switch (normalize(deviceName)) {
            case "galaxy s26 ultra" -> Optional.of(new PlaywrightDeviceDescriptor(
                    "Galaxy S26 Ultra",
                    GALAXY_S26_ULTRA_USER_AGENT,
                    412,
                    891,
                    412,
                    891,
                    3.5,
                    true,
                    true,
                    "chromium"));
            case "iphone 17 pro max" -> Optional.of(new PlaywrightDeviceDescriptor(
                    "iPhone 17 Pro Max",
                    IPHONE_17_PRO_MAX_USER_AGENT,
                    440,
                    763,
                    440,
                    956,
                    3,
                    true,
                    true,
                    "webkit"));
            default -> Optional.empty();
        };
    }

    private static String normalize(String name) {
        return name.trim().replaceAll("\\s+", " ").toLowerCase(Locale.ROOT);
    }
}
