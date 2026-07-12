package com.shaft.capture.generate;

import com.shaft.capture.model.CaptureEvent;

import java.util.List;

/**
 * Deterministic, fixed assertion catalogs offered by the recorder overlay's single
 * assertion entry point. Element and browser branches each expose a static, ordered list
 * of {@link CaptureEvent.VerificationKind} choices; nothing here is generated or inferred
 * at runtime, and every entry maps 1:1 to the SHAFT Validations API call rendered by
 * {@link CaptureGenerator}.
 */
public final class AssertionCatalog {
    private static final List<Entry> ELEMENT_ASSERTIONS = List.of(
            entry(CaptureEvent.VerificationKind.ELEMENT_VISIBLE, "Element is visible", false, false),
            entry(CaptureEvent.VerificationKind.ELEMENT_ENABLED, "Element is enabled", false, false),
            entry(CaptureEvent.VerificationKind.ELEMENT_SELECTED, "Element is selected", false, false),
            entry(CaptureEvent.VerificationKind.TEXT_EQUALS, "Text equals", true, false),
            entry(CaptureEvent.VerificationKind.TEXT_CONTAINS, "Text contains", true, false),
            entry(CaptureEvent.VerificationKind.ATTRIBUTE_EQUALS, "Attribute equals", true, true),
            entry(CaptureEvent.VerificationKind.ELEMENT_IMAGE_MATCHES, "Image matches reference", false, false),
            entry(CaptureEvent.VerificationKind.ARIA_SNAPSHOT_MATCHES, "Aria snapshot matches", true, false),
            entry(CaptureEvent.VerificationKind.SCREENSHOT_MATCHES, "Screenshot matches baseline", false, false));

    private static final List<Entry> BROWSER_ASSERTIONS = List.of(
            entry(CaptureEvent.VerificationKind.URL_EQUALS, "URL equals", true, false),
            entry(CaptureEvent.VerificationKind.URL_CONTAINS, "URL contains", true, false),
            entry(CaptureEvent.VerificationKind.TITLE_EQUALS, "Title equals", true, false),
            entry(CaptureEvent.VerificationKind.TITLE_CONTAINS, "Title contains", true, false),
            entry(CaptureEvent.VerificationKind.PAGE_TEXT_CONTAINS, "Page text contains", true, false));

    private AssertionCatalog() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns the deterministic element-assertion catalog offered after locator confirmation.
     *
     * @return immutable ordered element assertion catalog
     */
    public static List<Entry> elementAssertions() {
        return ELEMENT_ASSERTIONS;
    }

    /**
     * Returns the deterministic browser-assertion catalog offered for the browser branch.
     *
     * @return immutable ordered browser assertion catalog
     */
    public static List<Entry> browserAssertions() {
        return BROWSER_ASSERTIONS;
    }

    private static Entry entry(
            CaptureEvent.VerificationKind kind,
            String label,
            boolean needsValue,
            boolean needsAttribute) {
        return new Entry(kind, label, needsValue, needsAttribute);
    }

    /**
     * One deterministic assertion catalog choice.
     *
     * @param kind matching SHAFT verification kind
     * @param label reviewer-facing label shown in the recorder overlay
     * @param needsValue whether the choice prompts for an expected value
     * @param needsAttribute whether the choice prompts for an attribute name
     */
    public record Entry(
            CaptureEvent.VerificationKind kind,
            String label,
            boolean needsValue,
            boolean needsAttribute) {
    }
}
