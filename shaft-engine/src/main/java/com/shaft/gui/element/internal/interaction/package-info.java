/**
 * Shared click/type strategy layer for Selenium, Playwright, and Appium (#5732).
 *
 * <p>Waves B/C cover web kind routing. Wave D extends the same package for mobile native:
 * focus → {@code sendKeys} / {@code mobile: replaceElementValue} / {@code mobile: type},
 * W3C touch click fallback, and checkbox/switch toggle semantics.
 *
 * <p>Wave E covers Windows desktop (WinAppDriver / UIA): Edit/Document click→clear→sendKeys
 * ({@code windows: keys} fallback; {@code Value.Value} observed when present), Button/Hyperlink
 * Invoke via click/{@code windows: click}, CheckBox/RadioButton toggle (not sendKeys), ComboBox
 * expand → type-to-filter → Enter, and Window/Pane foreground focus before type.
 *
 * <p>Compose: enable {@code testTagsAsResourceId} and tap before type.
 * Flutter: prefer ValueKey/Semantics; type after focus.
 */
package com.shaft.gui.element.internal.interaction;
