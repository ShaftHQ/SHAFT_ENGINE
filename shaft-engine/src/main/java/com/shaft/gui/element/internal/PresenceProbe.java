package com.shaft.gui.element.internal;

import org.openqa.selenium.By;
import org.openqa.selenium.InvalidSelectorException;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebDriverException;

import java.time.Duration;
import java.util.Optional;

/**
 * Report-free presence polling for optional screens (issue #6842). Uses only {@code findElements},
 * so it behaves the same for web and native sessions and never fails or logs a step.
 */
public final class PresenceProbe {
    static final Duration POLL_INTERVAL = Duration.ofMillis(250);

    private PresenceProbe() {
    }

    /**
     * Polls until any locator matches at least one element or the timeout elapses.
     *
     * @param context  the driver (or element) to search in
     * @param timeout  the maximum time to wait; {@link Duration#ZERO} checks exactly once
     * @param locators the candidate locators, checked in order on every poll
     * @return the first locator that matched, or empty when none appeared in time
     */
    public static Optional<By> firstPresent(SearchContext context, Duration timeout, By... locators) {
        return firstPresent(context, timeout, POLL_INTERVAL, locators);
    }

    static Optional<By> firstPresent(SearchContext context, Duration timeout, Duration poll, By... locators) {
        if (locators == null || locators.length == 0) {
            throw new IllegalArgumentException("At least one locator is required.");
        }
        long deadline = System.nanoTime() + Math.max(0, timeout.toNanos());
        while (true) {
            Optional<By> found = matchOnce(context, locators);
            if (found.isPresent() || System.nanoTime() >= deadline) {
                return found;
            }
            try {
                Thread.sleep(Math.max(1, Math.min(poll.toMillis(), (deadline - System.nanoTime()) / 1_000_000)));
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                return Optional.empty();
            }
        }
    }

    private static Optional<By> matchOnce(SearchContext context, By... locators) {
        for (By locator : locators) {
            try {
                if (!context.findElements(locator).isEmpty()) {
                    return Optional.of(locator);
                }
            } catch (InvalidSelectorException invalidLocator) {
                throw invalidLocator;
            } catch (WebDriverException transientLookupFailure) {
                // A stale or mid-transition page is "not present yet"; keep polling.
            }
        }
        return Optional.empty();
    }
}
