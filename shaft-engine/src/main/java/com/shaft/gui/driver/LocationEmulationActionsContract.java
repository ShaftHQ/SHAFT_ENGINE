package com.shaft.gui.driver;

/** Geolocation, timezone, and locale emulation controls. */
public interface LocationEmulationActionsContract {
    /** Overrides geolocation with zero-metre default accuracy through Playwright or negotiated BiDi. */
    LocationEmulationActionsContract geolocation(double latitude, double longitude);

    /**
     * Overrides geolocation with an explicit non-negative accuracy in metres.
     *
     * @throws IllegalArgumentException for non-finite/out-of-range coordinates or negative accuracy
     * @throws UnsupportedOperationException when the live session cannot emulate geolocation
     */
    LocationEmulationActionsContract geolocation(double latitude, double longitude, double accuracy);

    /** Clears the geolocation override. */
    LocationEmulationActionsContract clearGeolocation();

    /**
     * Overrides the current browsing context's IANA timezone through negotiated BiDi. Playwright
     * contexts use {@code playwright.timezoneId} before session creation.
     */
    LocationEmulationActionsContract timezone(String timezoneId);

    /** Clears the timezone override. */
    LocationEmulationActionsContract clearTimezone();

    /**
     * Overrides the current browsing context's BCP 47 locale through negotiated BiDi. Playwright
     * contexts use {@code playwright.locale} before session creation.
     */
    LocationEmulationActionsContract locale(String locale);

    /** Clears the locale override. */
    LocationEmulationActionsContract clearLocale();

    /** @return owning emulation facade */
    EmulationActionsContract and();
}
