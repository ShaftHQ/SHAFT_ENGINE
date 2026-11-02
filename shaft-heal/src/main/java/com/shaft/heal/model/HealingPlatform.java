package com.shaft.heal.model;

/**
 * Supported element evidence platforms.
 */
public enum HealingPlatform {
    WEB,
    ANDROID,
    IOS,
    NATIVE,
    UNKNOWN;

    /**
     * Returns whether the platform uses a native accessibility tree.
     *
     * @return {@code true} for native mobile platforms
     */
    public boolean nativePlatform() {
        return this == ANDROID || this == IOS || this == NATIVE;
    }
}
