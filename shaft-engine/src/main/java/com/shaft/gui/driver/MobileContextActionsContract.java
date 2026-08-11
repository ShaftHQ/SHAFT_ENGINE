package com.shaft.gui.driver;

import java.util.List;

/** Native and web context actions owned by the mobile namespace. */
public interface MobileContextActionsContract {
    /** Returns the current native or web context. */
    String current();

    /** Returns the available native and web context handles. */
    List<String> handles();

    /** Switches to an exact context handle. */
    MobileContextActionsContract switchTo(String context);

    /** Switches to the standard Appium native application context. */
    MobileContextActionsContract nativeApp();

    /** Switches to the first available web-view context. */
    MobileContextActionsContract webView();

    /** Returns the owning mobile namespace. */
    MobileActionsContract and();
}
