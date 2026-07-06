package com.shaft.capture.generate;

import com.shaft.capture.model.CaptureEvent;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AssertionCatalogTest {
    @Test
    void elementAssertionsAreTheFixedDeterministicCatalog() {
        List<CaptureEvent.VerificationKind> kinds = AssertionCatalog.elementAssertions().stream()
                .map(AssertionCatalog.Entry::kind)
                .toList();
        assertEquals(List.of(
                CaptureEvent.VerificationKind.ELEMENT_VISIBLE,
                CaptureEvent.VerificationKind.ELEMENT_ENABLED,
                CaptureEvent.VerificationKind.ELEMENT_SELECTED,
                CaptureEvent.VerificationKind.TEXT_EQUALS,
                CaptureEvent.VerificationKind.TEXT_CONTAINS,
                CaptureEvent.VerificationKind.ATTRIBUTE_EQUALS,
                CaptureEvent.VerificationKind.ELEMENT_IMAGE_MATCHES),
                kinds);
    }

    @Test
    void browserAssertionsAreTheFixedDeterministicCatalog() {
        List<CaptureEvent.VerificationKind> kinds = AssertionCatalog.browserAssertions().stream()
                .map(AssertionCatalog.Entry::kind)
                .toList();
        assertEquals(List.of(
                CaptureEvent.VerificationKind.URL_EQUALS,
                CaptureEvent.VerificationKind.URL_CONTAINS,
                CaptureEvent.VerificationKind.TITLE_EQUALS,
                CaptureEvent.VerificationKind.TITLE_CONTAINS,
                CaptureEvent.VerificationKind.PAGE_TEXT_CONTAINS),
                kinds);
    }

    @Test
    void onlyAttributeEqualsPromptsForAnAttributeName() {
        for (AssertionCatalog.Entry entry : AssertionCatalog.elementAssertions()) {
            assertEquals(
                    entry.kind() == CaptureEvent.VerificationKind.ATTRIBUTE_EQUALS,
                    entry.needsAttribute(),
                    entry.kind() + " attribute-name prompt flag");
        }
        for (AssertionCatalog.Entry entry : AssertionCatalog.browserAssertions()) {
            assertEquals(false, entry.needsAttribute(), entry.kind() + " attribute-name prompt flag");
        }
    }

    @Test
    void everyBrowserAssertionPromptsForAnExpectedValue() {
        for (AssertionCatalog.Entry entry : AssertionCatalog.browserAssertions()) {
            assertEquals(true, entry.needsValue(), entry.kind() + " expected-value prompt flag");
        }
    }
}
