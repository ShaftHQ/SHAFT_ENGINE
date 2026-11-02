package com.shaft.gui.driver;

/** Exact viewport and web-exposed screen emulation controls. */
public interface ScreenEmulationActionsContract {
    /**
     * Sets the current page viewport in CSS pixels through Playwright or Selenium DevTools.
     *
     * @throws IllegalArgumentException when either dimension is not positive
     * @throws UnsupportedOperationException when the live session cannot emulate a viewport
     */
    ScreenEmulationActionsContract viewport(int width, int height);

    /**
     * Clears SHAFT's viewport override. Playwright restores the viewport captured before the first
     * SHAFT override; Selenium returns control to the underlying DevTools/browser defaults.
     *
     * @throws UnsupportedOperationException when the live session cannot clear its viewport override
     */
    ScreenEmulationActionsContract clearViewport();

    /**
     * Overrides the web-exposed screen width and height in CSS pixels through negotiated WebDriver BiDi.
     *
     * @throws IllegalArgumentException when either dimension is not positive
     * @throws UnsupportedOperationException when the live session has no negotiated BiDi channel
     */
    ScreenEmulationActionsContract screenSize(int width, int height);

    /** Clears the current-context BiDi screen-size override. */
    ScreenEmulationActionsContract clearScreenSize();

    /** @return owning emulation facade */
    EmulationActionsContract and();
}
