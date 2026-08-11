package com.shaft.gui.driver;

/** CSS media and user-preference emulation controls. */
public interface MediaEmulationActionsContract {
    /** Overrides the CSS media type through Playwright or Selenium DevTools. */
    MediaEmulationActionsContract type(EmulatedMediaType type);

    /** Overrides {@code prefers-color-scheme} without clearing the namespace's other media overrides. */
    MediaEmulationActionsContract colorScheme(EmulatedColorScheme scheme);

    /** Overrides {@code prefers-reduced-motion} without clearing the namespace's other media overrides. */
    MediaEmulationActionsContract reducedMotion(EmulatedReducedMotion motion);

    /** Clears every CSS media override owned by this namespace. */
    MediaEmulationActionsContract reset();

    /** @return owning emulation facade */
    EmulationActionsContract and();
}
