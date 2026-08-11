package com.shaft.gui.driver;

import java.util.List;

/** Appium hybrid/native contexts and Playwright's web context identity. */
public interface ContextActionsContract {
    /** @return the parent browser actions facade */
    BrowserActionsContract and();

    /** @return the current Appium context or the Playwright web-context identity */
    String current();

    /** @return the available context handles */
    List<String> handles();

    /** Switches to the requested context. */
    ContextActionsContract switchTo(String context);
}
