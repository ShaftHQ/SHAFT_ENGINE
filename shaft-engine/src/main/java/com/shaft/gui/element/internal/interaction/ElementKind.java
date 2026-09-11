package com.shaft.gui.element.internal.interaction;

/**
 * Conservative element kinds for Selenium click/type strategy selection (epic #5732 Wave B).
 * Unknown or ambiguous nodes stay {@link #UNKNOWN} and keep the legacy Actions path.
 */
public enum ElementKind {
    TEXT_LIKE,
    CHECKBOX,
    RADIO,
    SELECT,
    FILE,
    DATE_LIKE,
    RANGE,
    COLOR,
    BUTTON,
    LINK,
    CONTENTEDITABLE,
    COMBOBOX,
    IFRAME,
    /** Disabled or aria-disabled — refuse click and type. */
    DISABLED,
    /** Readonly — click allowed (focus); type refused. */
    READONLY,
    UNKNOWN
}
