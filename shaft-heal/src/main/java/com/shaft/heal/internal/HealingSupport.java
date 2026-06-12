package com.shaft.heal.internal;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

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
}
