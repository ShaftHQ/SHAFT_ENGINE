package com.shaft.enums.internal;

import lombok.Getter;

@Getter
public enum Screenshots {
    ELEMENT("element"), VIEWPORT("regular"), FULL("fullpage");
    final String value;

    Screenshots(String value) {
        this.value = value;
    }

}