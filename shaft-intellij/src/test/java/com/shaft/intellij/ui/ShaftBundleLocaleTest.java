package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.util.Locale;
import java.util.ResourceBundle;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

class ShaftBundleLocaleTest {

    @Test
    void spanishBundleLoadsInsteadOfFallingBackToRoot() {
        ResourceBundle bundle = ResourceBundle.getBundle(
                "messages.ShaftBundle", Locale.forLanguageTag("es"));

        assertEquals("es", bundle.getLocale().getLanguage());
        assertEquals("SHAFT", bundle.getString("toolwindow.stripe.SHAFT"));
        String version = bundle.getString("shaft.plugin.version");
        assertFalse(version.isBlank());
        assertFalse(version.contains("${"), version);
    }
}
