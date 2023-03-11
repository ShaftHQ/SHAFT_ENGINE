package io.github.shafthq.shaft.enums;

public enum ClipboardAction {
    COPY("copy"), PASTE("paste"), CUT("cut"), SELECT_ALL("select all"), UNSELECT_ALL("unselect");
    final String value;

    ClipboardAction(String value) {
        this.value = value;
    }

    public String getValue() {
        return this.value;
    }
}