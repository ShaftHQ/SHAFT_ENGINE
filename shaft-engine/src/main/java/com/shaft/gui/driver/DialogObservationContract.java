package com.shaft.gui.driver;

/** Backend-appropriate dialog observation. Playwright values describe the latest observed dialog. */
public interface DialogObservationContract {
    /** @return the parent dialog namespace */
    DialogActionsContract and();

    /** @return whether this session has observed a dialog */
    boolean wasSeen();

    /** @return the latest observed dialog text, or an empty string when none was observed */
    String lastText();
}
