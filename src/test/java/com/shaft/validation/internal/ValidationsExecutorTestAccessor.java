package com.shaft.validation.internal;

import org.openqa.selenium.WebDriver;

public final class ValidationsExecutorTestAccessor {
    private ValidationsExecutorTestAccessor() {
        throw new IllegalStateException("Utility class");
    }

    public static WebDriver getDriver(ValidationsExecutor executor) throws Exception {
        var field = ValidationsExecutor.class.getDeclaredField("driver");
        field.setAccessible(true);
        @SuppressWarnings("unchecked")
        ThreadLocal<WebDriver> driver = (ThreadLocal<WebDriver>) field.get(executor);
        return driver.get();
    }

    public static Object getResponse(ValidationsExecutor executor) throws Exception {
        var field = ValidationsExecutor.class.getDeclaredField("response");
        field.setAccessible(true);
        @SuppressWarnings("unchecked")
        ThreadLocal<Object> response = (ThreadLocal<Object>) field.get(executor);
        return response.get();
    }
}
