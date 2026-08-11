package com.shaft.gui.driver;

import java.util.List;

/** Mobile device log observation actions. */
public interface MobileLogActionsContract {
    /** Starts bounded continuous device-log capture for the live session. */
    MobileLogActionsContract start();

    /** Returns an immutable snapshot of captured device-log messages. */
    List<MobileLogMessage> messages();

    /** Returns an immutable snapshot of listener/provider errors. */
    List<MobileLogError> errors();

    /** Clears captured messages and errors for this session without stopping capture. */
    MobileLogActionsContract clear();

    /** Stops SHAFT-owned capture without removing foreign provider listeners. */
    MobileLogActionsContract stop();

    /** Returns the owning mobile namespace. */
    MobileActionsContract and();
}
