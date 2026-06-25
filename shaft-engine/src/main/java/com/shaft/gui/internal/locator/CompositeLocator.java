package com.shaft.gui.internal.locator;

import org.openqa.selenium.By;

import java.util.List;

/**
 * Internal marker for locator implementations that wrap multiple fallback candidates.
 */
public interface CompositeLocator {
    List<By> alternatives();
}
