package com.shaft.gui.driver;

/** Operations on a currently open synchronous dialog. */
public interface CurrentDialogActionsContract {
    /** @return the parent dialog namespace */
    DialogActionsContract and();

    /** @return whether a dialog is currently open */
    boolean isPresent();

    /** @return text of the currently open dialog */
    String text();

    /** Accepts the current dialog. */
    CurrentDialogActionsContract accept();

    /** Dismisses the current dialog. */
    CurrentDialogActionsContract dismiss();

    /** Types into the current prompt dialog. */
    CurrentDialogActionsContract type(String text);
}
