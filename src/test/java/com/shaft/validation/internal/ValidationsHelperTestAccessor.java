package com.shaft.validation.internal;

import org.openqa.selenium.By;

import java.util.ArrayList;

public final class ValidationsHelperTestAccessor {

    private ValidationsHelperTestAccessor() {
        throw new IllegalStateException("Utility class");
    }

    public static void setOptionalCustomLogMessage(ArrayList<String> messages) {
        ValidationsHelper.setOptionalCustomLogMessageForTesting(messages);
    }

    public static ArrayList<String> getOptionalCustomLogMessage() {
        return ValidationsHelper.getOptionalCustomLogMessageForTesting();
    }

    public static void setLastUsedElementLocator(By locator) {
        ValidationsHelper.setLastUsedElementLocatorForTesting(locator);
    }

    public static By getLastUsedElementLocator() {
        return ValidationsHelper.getLastUsedElementLocatorForTesting();
    }
}
