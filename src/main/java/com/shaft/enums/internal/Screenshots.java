package com.shaft.enums.internal;

import com.shaft.driver.SHAFT;
import lombok.Getter;

@Getter
public enum Screenshots {
    ELEMENT, VIEWPORT, FULL;

    public static Screenshots getType() {
        switch (SHAFT.Properties.visuals.screenshotParamsScreenshotType().toLowerCase()) {
            case "element" -> {
                return ELEMENT;
            }
            case "fullpage", "full", "full page", "fullscreen", "full screen" -> {
                return FULL;
            }
            default -> {
                return VIEWPORT;
            }
        }
    }
}