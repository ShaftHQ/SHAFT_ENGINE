/**
 * Shared click/type strategy layer for Selenium, Playwright, and Appium (#5732).
 *
 * <p>Waves B/C cover web kind routing. Wave D extends the same package for mobile native:
 * focus → {@code sendKeys} / {@code mobile: replaceElementValue} / {@code mobile: type},
 * W3C touch click fallback, and checkbox/switch toggle semantics.
 *
 * <p>Compose: enable {@code testTagsAsResourceId} and tap before type.
 * Flutter: prefer ValueKey/Semantics; type after focus.
 */
package com.shaft.gui.element.internal.interaction;
