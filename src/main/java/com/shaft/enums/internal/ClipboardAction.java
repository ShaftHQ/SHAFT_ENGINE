package com.shaft.enums.internal;

import lombok.Getter;

@Getter
public enum ClipboardAction {
    COPY("copy"), PASTE("paste"), CUT("cut"), SELECT_ALL("select all"), UNSELECT_ALL("unselect");
    final String value;

    ClipboardAction(String value) {
        this.value = value;
    }

}