package com.shaft.gui.driver;

import java.util.List;

/** Bounded console observations for the active browser session. */
public interface ConsoleActionsContract {
    /** @return the parent browser actions facade */
    BrowserActionsContract and();

    /** @return an immutable snapshot of observed console messages, oldest first */
    List<BrowserConsoleMessage> messages();

    /** @return only error-level messages from the current snapshot */
    List<BrowserConsoleMessage> errors();

    /** @return whether the current snapshot contains an error-level message */
    boolean hasErrors();

    /** Clears messages already observed by SHAFT. */
    ConsoleActionsContract clear();
}
