package com.shaft.intellij.ui;

import com.intellij.ui.JBColor;

import java.awt.Color;

/** Exact cross-product SHAFT color tokens, paired for IntelliJ light and dark themes. */
final class ShaftDesignTokens {
    private ShaftDesignTokens() {
        throw new IllegalStateException("Utility class");
    }

    static Color primary() {
        return new JBColor(new Color(0x006EC0), new Color(0x4CC2FF));
    }

    static Color muted() {
        return new JBColor(new Color(0xC8D6E7), new Color(0xDFF5F4));
    }

    static Color pass() {
        return new JBColor(new Color(0x14804A), new Color(0x4ADE80));
    }

    static Color warning() {
        return new JBColor(new Color(0x8A5A00), new Color(0xFBBF24));
    }

    static Color fail() {
        return new JBColor(new Color(0xC53030), new Color(0xFF6B6B));
    }
}
