package com.shaft.gui.driver;

/** Cohesive browser environment emulation actions grouped by the state they affect. */
public interface EmulationActionsContract {
    /** @return viewport and web-exposed screen controls */
    ScreenEmulationActionsContract screen();

    /** @return geolocation, timezone, and locale controls */
    LocationEmulationActionsContract location();

    /** @return CSS media preference controls */
    MediaEmulationActionsContract media();

    /** @return user-agent and scripting controls */
    RuntimeEmulationActionsContract runtime();

    /** @return owning browser facade */
    BrowserActionsContract and();
}
