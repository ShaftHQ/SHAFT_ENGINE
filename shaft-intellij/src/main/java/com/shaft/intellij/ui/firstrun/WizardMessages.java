package com.shaft.intellij.ui.firstrun;

import java.text.MessageFormat;
import java.util.ResourceBundle;

/** Wizard copy from {@code messages.ShaftBundle}. */
public final class WizardMessages {
    private static final ResourceBundle BUNDLE = ResourceBundle.getBundle("messages.ShaftBundle");

    private WizardMessages() {
    }

    public static String get(String key) {
        return BUNDLE.getString(key);
    }

    public static String format(String key, Object... arguments) {
        return MessageFormat.format(get(key), arguments);
    }
}
