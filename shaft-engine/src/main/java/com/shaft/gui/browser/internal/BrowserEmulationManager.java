package com.shaft.gui.browser.internal;

import com.shaft.gui.driver.EmulatedColorScheme;
import com.shaft.gui.driver.EmulatedMediaType;
import com.shaft.gui.driver.EmulatedReducedMotion;
import com.shaft.gui.capabilities.internal.AutomationCapabilityResolver;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.bidi.Command;
import org.openqa.selenium.bidi.HasBiDi;
import org.openqa.selenium.bidi.emulation.Emulation;
import org.openqa.selenium.bidi.emulation.GeolocationCoordinates;
import org.openqa.selenium.bidi.emulation.ScreenArea;
import org.openqa.selenium.bidi.emulation.SetGeolocationOverrideParameters;
import org.openqa.selenium.bidi.emulation.SetScreenSettingsOverrideParameters;
import org.openqa.selenium.bidi.emulation.SetScriptingEnabledParameters;
import org.openqa.selenium.bidi.emulation.SetTimezoneOverrideParameters;
import org.openqa.selenium.bidi.emulation.SetUserAgentOverrideParameters;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.remote.RemoteWebDriver;

import java.util.IllformedLocaleException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.function.UnaryOperator;

/**
 * Applies browser emulation through the narrowest live protocol supported by a Selenium/Appium session.
 * Standardized WebDriver BiDi owns screen, location, locale, user-agent, and scripting overrides;
 * Selenium DevTools owns viewport and CSS-media overrides until those operations are standardized.
 */
public final class BrowserEmulationManager {
    private static final Map<WebDriver, CdpMediaState> CDP_MEDIA_STATES = new WeakHashMap<>();

    private BrowserEmulationManager() {
        throw new IllegalStateException("Utility class");
    }

    public static void setScreenSize(WebDriver driver, int width, int height) {
        requirePositiveDimensions(width, height);
        emulation(driver, "screen size").setScreenSettingsOverride(
                new SetScreenSettingsOverrideParameters(new ScreenArea(width, height))
                        .contexts(contexts(driver, "screen size")));
    }

    public static void clearScreenSize(WebDriver driver) {
        emulation(driver, "clear screen size").setScreenSettingsOverride(
                new SetScreenSettingsOverrideParameters(null)
                        .contexts(contexts(driver, "clear screen size")));
    }

    public static void setViewport(WebDriver driver, int width, int height) {
        requirePositiveDimensions(width, height);
        Map<String, Object> parameters = new LinkedHashMap<>();
        parameters.put("width", width);
        parameters.put("height", height);
        parameters.put("deviceScaleFactor", 1);
        parameters.put("mobile", false);
        devTools(driver, "viewport").send(new org.openqa.selenium.devtools.Command<>(
                "Emulation.setDeviceMetricsOverride", parameters));
    }

    public static void clearViewport(WebDriver driver) {
        devTools(driver, "clear viewport").send(new org.openqa.selenium.devtools.Command<>(
                "Emulation.clearDeviceMetricsOverride", Map.of()));
    }

    public static void setMediaType(WebDriver driver, EmulatedMediaType mediaType) {
        requireValue(mediaType, "Media type");
        updateMedia(driver, state -> state.withMedia(switch (mediaType) {
            case SCREEN -> "screen";
            case PRINT -> "print";
        }));
    }

    public static void setColorScheme(WebDriver driver, EmulatedColorScheme colorScheme) {
        requireValue(colorScheme, "Color scheme");
        updateMedia(driver, state -> state.withColorScheme(switch (colorScheme) {
            case LIGHT -> "light";
            case DARK -> "dark";
            case NO_PREFERENCE -> "no-preference";
        }));
    }

    public static void setReducedMotion(WebDriver driver, EmulatedReducedMotion reducedMotion) {
        requireValue(reducedMotion, "Reduced motion");
        updateMedia(driver, state -> state.withReducedMotion(switch (reducedMotion) {
            case REDUCE -> "reduce";
            case NO_PREFERENCE -> "no-preference";
        }));
    }

    public static void resetMedia(WebDriver driver) {
        synchronized (CDP_MEDIA_STATES) {
            devTools(driver, "media reset").send(new org.openqa.selenium.devtools.Command<>(
                    "Emulation.setEmulatedMedia", Map.of()));
            CDP_MEDIA_STATES.remove(driver);
        }
    }

    /** Removes all local emulation state owned by a closing driver session. */
    public static void clearAndRemove(WebDriver driver) {
        if (driver != null) {
            synchronized (CDP_MEDIA_STATES) {
                CDP_MEDIA_STATES.remove(driver);
            }
        }
    }

    public static void setGeolocation(WebDriver driver, double latitude, double longitude, double accuracy) {
        requireValidGeolocation(latitude, longitude, accuracy);
        emulation(driver, "geolocation").setGeolocationOverride(new SetGeolocationOverrideParameters(
                new GeolocationCoordinates(latitude, longitude).accuracy(accuracy))
                .contexts(contexts(driver, "geolocation")));
    }

    public static void clearGeolocation(WebDriver driver) {
        emulation(driver, "clear geolocation").setGeolocationOverride(
                new SetGeolocationOverrideParameters((GeolocationCoordinates) null)
                        .contexts(contexts(driver, "clear geolocation")));
    }

    public static void setTimezone(WebDriver driver, String timezoneId) {
        requireTimezone(timezoneId);
        emulation(driver, "timezone").setTimezoneOverride(new SetTimezoneOverrideParameters(timezoneId)
                .contexts(contexts(driver, "timezone")));
    }

    public static void clearTimezone(WebDriver driver) {
        emulation(driver, "clear timezone").setTimezoneOverride(new SetTimezoneOverrideParameters(null)
                .contexts(contexts(driver, "clear timezone")));
    }

    public static void setLocale(WebDriver driver, String locale) {
        requireLocale(locale);
        sendLocale(driver, locale);
    }

    public static void clearLocale(WebDriver driver) {
        sendLocale(driver, null);
    }

    public static void setUserAgent(WebDriver driver, String userAgent) {
        if (userAgent == null || userAgent.isBlank()) {
            throw new IllegalArgumentException("User agent must not be null or blank.");
        }
        emulation(driver, "user agent").setUserAgentOverride(new SetUserAgentOverrideParameters(userAgent)
                .contexts(contexts(driver, "user agent")));
    }

    public static void clearUserAgent(WebDriver driver) {
        emulation(driver, "clear user agent").setUserAgentOverride(new SetUserAgentOverrideParameters(null)
                .contexts(contexts(driver, "clear user agent")));
    }

    public static void disableScripting(WebDriver driver) {
        emulation(driver, "disable scripting").setScriptingEnabled(new SetScriptingEnabledParameters(false)
                .contexts(contexts(driver, "disable scripting")));
    }

    public static void clearScriptingOverride(WebDriver driver) {
        emulation(driver, "clear scripting").setScriptingEnabled(new SetScriptingEnabledParameters(null)
                .contexts(contexts(driver, "clear scripting")));
    }

    private static void sendLocale(WebDriver driver, String locale) {
        HasBiDi hasBiDi = requireBiDi(driver, locale == null ? "clear locale" : "locale");
        Map<String, Object> parameters = new LinkedHashMap<>();
        parameters.put("locale", locale);
        parameters.put("contexts", contexts(driver, locale == null ? "clear locale" : "locale"));
        hasBiDi.getBiDi().send(new Command<>("emulation.setLocaleOverride", parameters));
    }

    private static Emulation emulation(WebDriver driver, String operation) {
        requireBiDi(driver, operation);
        return new Emulation(driver);
    }

    private static HasBiDi requireBiDi(WebDriver driver, String operation) {
        if (!AutomationCapabilityResolver.hasNegotiatedBiDi(driver)
                || !(driver instanceof HasBiDi hasBiDi)) {
            throw new UnsupportedOperationException("Emulation " + operation
                    + " requires a live Selenium or Appium session with negotiated WebDriver BiDi.");
        }
        return hasBiDi;
    }

    private static DevTools devTools(WebDriver driver, String operation) {
        if (driver == null
                || (driver instanceof RemoteWebDriver remote && remote.getSessionId() == null)
                || !(driver instanceof HasDevTools hasDevTools)
                || hasDevTools.maybeGetDevTools().isEmpty()) {
            throw new UnsupportedOperationException("Emulation " + operation
                    + " requires a live DevTools-capable Selenium session.");
        }
        DevTools devTools = hasDevTools.getDevTools();
        devTools.createSessionIfThereIsNotOne();
        return devTools;
    }

    private static void updateMedia(WebDriver driver, UnaryOperator<CdpMediaState> update) {
        synchronized (CDP_MEDIA_STATES) {
            CdpMediaState candidate = update.apply(CDP_MEDIA_STATES.getOrDefault(driver, CdpMediaState.EMPTY));
            Map<String, Object> parameters = new LinkedHashMap<>();
            if (candidate.media() != null) {
                parameters.put("media", candidate.media());
            }
            List<Map<String, String>> features = new ArrayList<>();
            if (candidate.colorScheme() != null) {
                features.add(Map.of("name", "prefers-color-scheme", "value", candidate.colorScheme()));
            }
            if (candidate.reducedMotion() != null) {
                features.add(Map.of("name", "prefers-reduced-motion", "value", candidate.reducedMotion()));
            }
            parameters.put("features", List.copyOf(features));
            devTools(driver, "media").send(new org.openqa.selenium.devtools.Command<>(
                    "Emulation.setEmulatedMedia", parameters));
            CDP_MEDIA_STATES.put(driver, candidate);
        }
    }

    private static List<String> contexts(WebDriver driver, String operation) {
        requireBiDi(driver, operation);
        String handle;
        try {
            handle = driver.getWindowHandle();
        } catch (RuntimeException exception) {
            throw new UnsupportedOperationException("Emulation " + operation
                    + " requires a current WebDriver browsing context.", exception);
        }
        if (handle == null || handle.isBlank()) {
            throw new UnsupportedOperationException("Emulation " + operation
                    + " requires a current WebDriver browsing context.");
        }
        return List.of(handle);
    }

    private static void requirePositiveDimensions(int width, int height) {
        if (width < 1 || height < 1) {
            throw new IllegalArgumentException("Screen width and height must both be positive.");
        }
    }

    private static void requireValidGeolocation(double latitude, double longitude, double accuracy) {
        if (!Double.isFinite(latitude) || latitude < -90 || latitude > 90) {
            throw new IllegalArgumentException("Latitude must be finite and between -90 and 90.");
        }
        if (!Double.isFinite(longitude) || longitude < -180 || longitude > 180) {
            throw new IllegalArgumentException("Longitude must be finite and between -180 and 180.");
        }
        if (!Double.isFinite(accuracy) || accuracy < 0) {
            throw new IllegalArgumentException("Geolocation accuracy must be finite and non-negative.");
        }
    }

    private static void requireTimezone(String timezoneId) {
        if (timezoneId == null || timezoneId.isBlank()) {
            throw new IllegalArgumentException("Timezone must not be null or blank.");
        }
        try {
            java.time.ZoneId.of(timezoneId);
        } catch (RuntimeException exception) {
            throw new IllegalArgumentException("Timezone must be a valid IANA timezone identifier.", exception);
        }
    }

    private static void requireLocale(String locale) {
        if (locale == null || locale.isBlank()) {
            throw new IllegalArgumentException("Locale must not be null or blank.");
        }
        try {
            new Locale.Builder().setLanguageTag(locale).build();
        } catch (IllformedLocaleException exception) {
            throw new IllegalArgumentException("Locale must be a valid BCP 47 language tag.", exception);
        }
    }

    private static void requireValue(Object value, String name) {
        if (value == null) {
            throw new IllegalArgumentException(name + " must not be null.");
        }
    }

    private record CdpMediaState(String media, String colorScheme, String reducedMotion) {
        private static final CdpMediaState EMPTY = new CdpMediaState(null, null, null);

        private CdpMediaState withMedia(String value) {
            return new CdpMediaState(value, colorScheme, reducedMotion);
        }

        private CdpMediaState withColorScheme(String value) {
            return new CdpMediaState(media, value, reducedMotion);
        }

        private CdpMediaState withReducedMotion(String value) {
            return new CdpMediaState(media, colorScheme, value);
        }
    }
}
