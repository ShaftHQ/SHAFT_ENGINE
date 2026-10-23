package com.shaft.driver.internal.DriverFactory;

import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.SessionNotCreatedException;

import java.util.Locale;
import java.util.Map;
import java.util.Optional;

/**
 * Verifies the locale an Android device is actually running after session start (#7209): Appium's
 * {@code language}/{@code locale} capabilities can succeed while the running configuration keeps the
 * previous locale, because appium-adb only compares {@code persist.sys.locale}.
 */
final class RunningLocaleCheck {
    private RunningLocaleCheck() {
        throw new IllegalStateException("Utility class");
    }

    static void verify(JavascriptExecutor driver, String language, String locale) {
        if (language == null || language.isBlank()) {
            return;
        }
        Object info;
        try {
            info = driver.executeScript("mobile: deviceInfo", Map.of());
        } catch (RuntimeException unsupported) {
            return; // driver without mobile: deviceInfo; nothing to verify
        }
        String running = info instanceof Map<?, ?> map ? String.valueOf(map.get("locale")) : "";
        Optional<String> reason = mismatch(language, locale, running);
        if (reason.isPresent()) {
            throw new SessionNotCreatedException("Preflight: " + reason.get());
        }
    }

    /**
     * Compares the requested language and region with the running locale.
     *
     * @return empty when they match or the running locale is unknown, otherwise the reason
     */
    static Optional<String> mismatch(String language, String locale, String running) {
        if (running.isBlank() || "null".equals(running)) {
            return Optional.empty();
        }
        String[] parts = running.trim().replace('-', '_').split("_", -1);
        String runningLanguage = parts[0].toLowerCase(Locale.ROOT);
        String runningRegion = parts.length > 1 ? parts[1].toUpperCase(Locale.ROOT) : "";
        String wantLanguage = language.trim().toLowerCase(Locale.ROOT);
        String wantRegion = locale.trim().toUpperCase(Locale.ROOT);
        boolean languageOk = runningLanguage.equals(wantLanguage);
        boolean regionOk = wantRegion.isEmpty() || runningRegion.isEmpty() || runningRegion.equals(wantRegion);
        if (languageOk && regionOk) {
            return Optional.empty();
        }
        String want = wantLanguage + (wantRegion.isEmpty() ? "" : "_" + wantRegion);
        return Optional.of("the Android device runs locale " + running.trim() + " after session start; mobile_language/"
                + "mobile_locale asked for " + want + ". Appium's language capability did not apply. Start one session "
                + "in another locale and one back in " + want + ", or set mobile_verifyRunningLocale=false to skip.");
    }
}
