package com.shaft.enums.internal;

import lombok.Getter;

/**
 * Defines clipboard-related keyboard actions that can be performed on
 * web and mobile elements (copy, paste, cut, select all, unselect).
 */
@Getter
public enum ClipboardAction {
    /** Copies the selected content to the clipboard. */
    COPY("copy"),
    /** Pastes the clipboard content at the current cursor position. */
    PASTE("paste"),
    /** Cuts the selected content and places it on the clipboard. */
    CUT("cut"),
    /** Selects all content in the target element or context. */
    SELECT_ALL("select all"),
    /** Clears any active selection in the target element or context. */
    UNSELECT_ALL("unselect");
    final String value;

    ClipboardAction(String value) {
        this.value = value;
    }

}