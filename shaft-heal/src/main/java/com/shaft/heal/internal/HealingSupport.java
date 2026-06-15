package com.shaft.heal.internal;

import com.shaft.heal.model.HealingContext;
import com.shaft.heal.model.HealingPlatform;
import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.remote.RemoteWebDriver;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.Normalizer;
import java.util.Locale;
import java.util.Objects;

final class HealingSupport {
    private HealingSupport() {
        throw new IllegalStateException("Utility class");
    }

    static String pageKey(WebDriver driver) {
        HealingContext context = context(driver, null, null, null);
        if (context.platform().nativePlatform()) {
            return sanitize(context.applicationId() + "/" + context.screenId());
        }
        try {
            URI uri = new URI(Objects.requireNonNullElse(driver.getCurrentUrl(), ""));
            if (uri.getScheme() == null) {
                return sanitize(uri.getPath());
            }
            return sanitize(new URI(uri.getScheme(), uri.getAuthority(), uri.getPath(), null, null).toString());
        } catch (RuntimeException | URISyntaxException ignored) {
            return "";
        }
    }

    static String contextKey(By frameLocator, By shadowHostLocator, By shadowContentLocator) {
        return "frame=" + locator(frameLocator)
                + ";shadowHost=" + locator(shadowHostLocator)
                + ";shadowContent=" + locator(shadowContentLocator);
    }

    static HealingContext context(
            WebDriver driver,
            By frameLocator,
            By shadowHostLocator,
            By shadowContentLocator) {
        HealingPlatform platform = platform(driver);
        String applicationId = "";
        String screenId = "";
        String automationContext = "";
        String windowHandle = "";
        if (driver instanceof RemoteWebDriver remote) {
            applicationId = firstNonBlank(
                    capability(remote, "appium:appPackage"),
                    capability(remote, "appium:bundleId"),
                    capability(remote, "appPackage"),
                    capability(remote, "bundleId"),
                    capability(remote, "app"));
        }
        if (platform.nativePlatform()) {
            applicationId = firstNonBlank(
                    invoke(driver, "getCurrentPackage"),
                    applicationId);
            screenId = firstNonBlank(
                    invoke(driver, "currentActivity"),
                    invoke(driver, "getCurrentActivity"),
                    rootFingerprint(driver));
            automationContext = firstNonBlank(invoke(driver, "getContext"), "NATIVE_APP");
        }
        try {
            windowHandle = sanitize(driver.getWindowHandle());
        } catch (RuntimeException ignored) {
            // Some native drivers do not expose window handles.
        }
        return new HealingContext(
                HealingContext.CURRENT_SCHEMA_VERSION,
                platform,
                sanitize(applicationId),
                sanitize(screenId),
                sanitize(automationContext),
                windowHandle,
                locator(frameLocator),
                locator(shadowHostLocator),
                locator(shadowContentLocator));
    }

    static String contextKey(
            WebDriver driver,
            By frameLocator,
            By shadowHostLocator,
            By shadowContentLocator) {
        return context(driver, frameLocator, shadowHostLocator, shadowContentLocator).stableKey();
    }

    static String locator(By locator) {
        return locator == null ? "" : sanitize(locator.toString());
    }

    static String normalize(String value) {
        String normalized = Normalizer.normalize(Objects.requireNonNullElse(value, ""), Normalizer.Form.NFKC)
                .trim()
                .replaceAll("\\s+", " ")
                .toLowerCase(Locale.ROOT);
        return sanitize(normalized);
    }

    static String sanitize(String value) {
        return PrivacySanitizer.sanitize(value).value();
    }

    static String sha256(String value) {
        try {
            byte[] digest = MessageDigest.getInstance("SHA-256")
                    .digest(Objects.requireNonNullElse(value, "").getBytes(StandardCharsets.UTF_8));
            return java.util.HexFormat.of().formatHex(digest);
        } catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException("SHA-256 is unavailable.", exception);
        }
    }

    private static HealingPlatform platform(WebDriver driver) {
        if (!(driver instanceof AppiumDriver) && !(driver instanceof RemoteWebDriver)) {
            return HealingPlatform.WEB;
        }
        String name = "";
        if (driver instanceof RemoteWebDriver remote) {
            name = firstNonBlank(
                    capability(remote, "platformName"),
                    capability(remote, "appium:platformName"));
        }
        if ("android".equalsIgnoreCase(name)) {
            return HealingPlatform.ANDROID;
        }
        if ("ios".equalsIgnoreCase(name)) {
            return HealingPlatform.IOS;
        }
        return driver instanceof AppiumDriver ? HealingPlatform.NATIVE : HealingPlatform.WEB;
    }

    private static String capability(RemoteWebDriver driver, String name) {
        try {
            Object value = driver.getCapabilities().getCapability(name);
            return value == null ? "" : sanitize(String.valueOf(value));
        } catch (RuntimeException exception) {
            return "";
        }
    }

    private static String invoke(WebDriver driver, String methodName) {
        try {
            Method method = driver.getClass().getMethod(methodName);
            Object value = method.invoke(driver);
            return value == null ? "" : sanitize(String.valueOf(value));
        } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | RuntimeException ignored) {
            return "";
        }
    }

    private static String rootFingerprint(WebDriver driver) {
        try {
            java.util.List<WebElement> roots = driver.findElements(By.xpath("/*"));
            if (roots.isEmpty()) {
                return "";
            }
            WebElement root = roots.getFirst();
            String evidence = safeCall(root::getTagName)
                    + "|" + safeAttribute(root, "class")
                    + "|" + safeAttribute(root, "resource-id")
                    + "|" + safeAttribute(root, "name");
            return "root-" + sha256(evidence).substring(0, 24);
        } catch (RuntimeException exception) {
            return "";
        }
    }

    private static String safeAttribute(WebElement element, String name) {
        try {
            return sanitize(element.getAttribute(name));
        } catch (RuntimeException exception) {
            return "";
        }
    }

    private static String safeCall(java.util.function.Supplier<String> supplier) {
        try {
            return sanitize(supplier.get());
        } catch (RuntimeException exception) {
            return "";
        }
    }

    private static String firstNonBlank(String... values) {
        for (String value : values) {
            if (value != null && !value.isBlank()) {
                return value;
            }
        }
        return "";
    }
}
