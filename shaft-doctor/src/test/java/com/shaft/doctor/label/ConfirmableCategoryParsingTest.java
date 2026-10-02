package com.shaft.doctor.label;

import com.shaft.doctor.model.CauseCategory;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Issue #6375: confirmable cause tokens map onto doctor categories, with aliases.
 */
class ConfirmableCategoryParsingTest {
    @Test
    void mapsTokensAndAliasesCaseInsensitively() {
        assertEquals(CauseCategory.PRODUCT, ConfirmedCauseLabelStore.parseConfirmableCategory(" product "));
        assertEquals(CauseCategory.TEST, ConfirmedCauseLabelStore.parseConfirmableCategory("TEST"));
        assertEquals(CauseCategory.ENVIRONMENT_CONFIGURATION,
                ConfirmedCauseLabelStore.parseConfirmableCategory("configuration"));
        assertEquals(CauseCategory.LOCATOR, ConfirmedCauseLabelStore.parseConfirmableCategory("Locator"));
        assertEquals(CauseCategory.TIMING_SYNCHRONIZATION,
                ConfirmedCauseLabelStore.parseConfirmableCategory("timing-synchronization"));
    }

    @Test
    void rejectsBlankAndUnknownTokens() {
        assertThrows(IllegalArgumentException.class, () -> ConfirmedCauseLabelStore.parseConfirmableCategory(" "));
        assertThrows(IllegalArgumentException.class, () -> ConfirmedCauseLabelStore.parseConfirmableCategory(null));
        assertThrows(IllegalArgumentException.class, () -> ConfirmedCauseLabelStore.parseConfirmableCategory("cosmic-ray"));
    }
}
