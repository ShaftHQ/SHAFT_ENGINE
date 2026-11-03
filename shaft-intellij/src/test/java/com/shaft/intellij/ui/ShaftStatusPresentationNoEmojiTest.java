package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftStatusPresentationNoEmojiTest {
    @Test
    void statusIconsAreAsciiMarkersNotEmoji() throws Exception {
        for (String name : new String[] {"SUCCESS_ICON", "PENDING_ICON", "ERROR_ICON", "WARNING_ICON", "DISCONNECTED_ICON"}) {
            Field field = ShaftStatusPresentation.class.getField(name);
            String value = (String) field.get(null);
            assertFalse(value.codePoints().anyMatch(cp -> cp > 0x7F), name + " must be ASCII, was: " + value);
            assertTrue(value.length() >= 2, name + " should be a readable marker");
        }
    }

    @Test
    void dualLegColorsArePresent() {
        assertNotNull(ShaftStatusPresentation.success());
        assertNotNull(ShaftStatusPresentation.error());
                assertNotNull(ShaftStatusPresentation.pending());
        assertNotNull(ShaftStatusPresentation.progress());
        assertNotNull(ShaftStatusPresentation.disconnected());
        assertEquals("12 ms", ShaftStatusPresentation.formatElapsed(12));
        assertNotNull(ShaftStatusPresentation.tint(
                ShaftStatusPresentation.success(), ShaftStatusPresentation.error(), 0.5));
    }
}
