package com.shaft.gui.driver;

/** Policy for the next callback-driven dialog. */
public interface NextDialogActionsContract {
    /** @return the parent dialog namespace */
    DialogActionsContract and();

    /** Arms acceptance of the next dialog. */
    NextDialogActionsContract accept();

    /** Arms dismissal of the next dialog. */
    NextDialogActionsContract dismiss();

    /** Arms text entry and acceptance for the next prompt dialog. */
    NextDialogActionsContract type(String text);
}
