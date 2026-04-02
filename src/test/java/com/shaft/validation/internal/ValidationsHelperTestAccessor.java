package com.shaft.validation.internal;

import org.openqa.selenium.By;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

public final class ValidationsHelperTestAccessor {

    private ValidationsHelperTestAccessor() {
        throw new IllegalStateException("Utility class");
    }

    public static void setOptionalCustomLogMessage(List<String> messages) {
        try {
            Field field = ValidationsHelper.class.getDeclaredField("optionalCustomLogMessage");
            field.setAccessible(true);
            @SuppressWarnings("unchecked")
            ThreadLocal<ArrayList<String>> tl = (ThreadLocal<ArrayList<String>>) field.get(null);
            tl.set(messages == null ? null : new ArrayList<>(messages));
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException("Failed to set optionalCustomLogMessage via reflection", e);
        }
    }

    @SuppressWarnings("unchecked")
    public static ArrayList<String> getOptionalCustomLogMessage() {
        try {
            Field field = ValidationsHelper.class.getDeclaredField("optionalCustomLogMessage");
            field.setAccessible(true);
            ThreadLocal<ArrayList<String>> tl = (ThreadLocal<ArrayList<String>>) field.get(null);
            return tl.get();
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException("Failed to get optionalCustomLogMessage via reflection", e);
        }
    }

    public static void setLastUsedElementLocator(By locator) {
        try {
            Field field = ValidationsHelper.class.getDeclaredField("lastUsedElementLocator");
            field.setAccessible(true);
            @SuppressWarnings("unchecked")
            ThreadLocal<By> tl = (ThreadLocal<By>) field.get(null);
            tl.set(locator);
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException("Failed to set lastUsedElementLocator via reflection", e);
        }
    }

    @SuppressWarnings("unchecked")
    public static By getLastUsedElementLocator() {
        try {
            Field field = ValidationsHelper.class.getDeclaredField("lastUsedElementLocator");
            field.setAccessible(true);
            ThreadLocal<By> tl = (ThreadLocal<By>) field.get(null);
            return tl.get();
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException("Failed to get lastUsedElementLocator via reflection", e);
        }
    }
}
