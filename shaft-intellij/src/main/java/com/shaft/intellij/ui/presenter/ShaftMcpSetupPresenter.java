package com.shaft.intellij.ui.presenter;

import com.shaft.intellij.ui.ShaftStatusPresentation;
import org.jetbrains.annotations.NotNull;

import java.awt.Color;

/**
 * Behaviour-preserving facade for MCP setup status presentation (W1.8).
 *
 * <p>Keeps mastery rules: badges/status text derive from real verification outcomes only.
 */
public final class ShaftMcpSetupPresenter {
    private ShaftMcpSetupPresenter() {
        throw new IllegalStateException("Utility class");
    }

    public static @NotNull String verifiedBadgeText(@NotNull String label) {
        return ShaftStatusPresentation.SUCCESS_ICON + " " + label;
    }

    public static @NotNull String failedBadgeText(@NotNull String label) {
        return ShaftStatusPresentation.ERROR_ICON + " " + label;
    }

    public static @NotNull String warningBadgeText(@NotNull String label) {
        return ShaftStatusPresentation.WARNING_ICON + " " + label;
    }

    public static @NotNull Color colorForSuccess(boolean success) {
        return success ? ShaftStatusPresentation.success() : ShaftStatusPresentation.error();
    }
}
