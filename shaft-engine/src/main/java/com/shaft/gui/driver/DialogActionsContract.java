package com.shaft.gui.driver;

/** Browser-dialog actions separated by observation, current-dialog, and next-dialog semantics. */
public interface DialogActionsContract {
    /** @return the parent browser actions facade */
    BrowserActionsContract and();

    /** @return historical dialog observation when the backend provides it */
    DialogObservationContract observation();

    /** @return actions for a dialog that is currently open */
    CurrentDialogActionsContract current();

    /** @return policy to apply to the next callback-driven dialog */
    NextDialogActionsContract next();
}
