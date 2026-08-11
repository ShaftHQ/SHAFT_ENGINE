package com.shaft.gui.browser.internal;

import org.openqa.selenium.WebDriver;

import java.util.List;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.function.Function;

/** Session-owned inventory used to restore SHAFT-managed BiDi permissions to prompt. */
public final class BidiPermissionState {
    private static final Map<WebDriver, SessionState> STATES = Collections.synchronizedMap(new WeakHashMap<>());

    private BidiPermissionState() {
        throw new IllegalStateException("Utility class");
    }

    /** Serializes the native mutation and its reset inventory for one driver. */
    public static <T> T exclusively(WebDriver driver, Function<Set<Change>, T> operation) {
        SessionState state;
        synchronized (STATES) {
            state = STATES.computeIfAbsent(driver, ignored -> new SessionState());
        }
        synchronized (state) {
            return operation.apply(state.changes);
        }
    }

    public static void clearAndRemove(WebDriver driver) {
        if (driver != null) {
            STATES.remove(driver);
        }
    }

    public record Change(String origin, String permission) { }

    private static final class SessionState {
        private final Set<Change> changes = new LinkedHashSet<>();
    }
}
