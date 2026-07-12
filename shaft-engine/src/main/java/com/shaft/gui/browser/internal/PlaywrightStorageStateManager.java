package com.shaft.gui.browser.internal;

import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.options.Cookie;
import com.microsoft.playwright.options.SameSiteAttribute;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.SerializationFeature;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Saves and restores browser cookies, localStorage, and sessionStorage for one Playwright context.
 *
 * <p>Reads and writes the exact JSON schema produced by {@link BrowserStorageStateManager}, so
 * storage-state files are interchangeable between the WebDriver and Playwright backends.
 */
public final class PlaywrightStorageStateManager {
    private static final ObjectMapper JSON = JsonMapper.builder()
            .enable(SerializationFeature.INDENT_OUTPUT)
            .build();
    private static final String STORAGE_SNAPSHOT_SCRIPT = "() => ({localStorage: Object.fromEntries(Object.entries(window.localStorage)), "
            + "sessionStorage: Object.fromEntries(Object.entries(window.sessionStorage))})";
    private static final String LOAD_STORAGE_SCRIPT = "(state) => { window.localStorage.clear(); window.sessionStorage.clear(); "
            + "for (const [key, value] of Object.entries(state.localStorage)) { window.localStorage.setItem(key, value); } "
            + "for (const [key, value] of Object.entries(state.sessionStorage)) { window.sessionStorage.setItem(key, value); } }";

    private PlaywrightStorageStateManager() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Saves cookies, localStorage, and sessionStorage to a JSON file.
     *
     * @param context active Playwright browser context
     * @param page    active Playwright page
     * @param filePath target JSON file
     */
    public static void save(BrowserContext context, Page page, String filePath) {
        requireSession(context, page);
        Path path = path(filePath);
        BrowserStorageStateManager.StorageState state = new BrowserStorageStateManager.StorageState();
        state.schemaVersion = "1.0";
        state.origin = origin(page);
        state.cookies = context.cookies().stream()
                .map(PlaywrightStorageStateManager::cookieState)
                .toList();
        BrowserStorageStateManager.OriginStorage originStorage = new BrowserStorageStateManager.OriginStorage();
        originStorage.origin = state.origin;
        Map<String, Map<String, String>> storage = storageSnapshot(page);
        originStorage.localStorage = storage.getOrDefault("localStorage", Map.of());
        originStorage.sessionStorage = storage.getOrDefault("sessionStorage", Map.of());
        state.origins = List.of(originStorage);
        try {
            if (path.getParent() != null) {
                Files.createDirectories(path.getParent());
            }
            JSON.writeValue(path.toFile(), state);
        } catch (IOException e) {
            throw new IllegalStateException("Could not save browser storage state to `" + path + "`.", e);
        }
    }

    /**
     * Loads cookies, localStorage, and sessionStorage from a JSON file.
     *
     * @param context active Playwright browser context
     * @param page    active Playwright page
     * @param filePath source JSON file
     */
    public static void load(BrowserContext context, Page page, String filePath) {
        requireSession(context, page);
        Path path = path(filePath);
        try {
            BrowserStorageStateManager.StorageState state = JSON.readValue(path.toFile(), BrowserStorageStateManager.StorageState.class);
            context.clearCookies();
            List<Cookie> cookies = nullSafe(state.cookies).stream()
                    .map(cookie -> toCookie(cookie, page.url()))
                    .toList();
            if (!cookies.isEmpty()) {
                context.addCookies(cookies);
            }
            BrowserStorageStateManager.OriginStorage origin = nullSafe(state.origins).isEmpty()
                    ? new BrowserStorageStateManager.OriginStorage() : state.origins.getFirst();
            Map<String, Object> snapshot = new LinkedHashMap<>();
            snapshot.put("localStorage", origin.localStorage == null ? Map.of() : origin.localStorage);
            snapshot.put("sessionStorage", origin.sessionStorage == null ? Map.of() : origin.sessionStorage);
            page.evaluate(LOAD_STORAGE_SCRIPT, snapshot);
        } catch (RuntimeException e) {
            throw new IllegalStateException("Could not load browser storage state from `" + path + "`.", e);
        }
    }

    private static void requireSession(BrowserContext context, Page page) {
        if (context == null || page == null) {
            throw new IllegalArgumentException("An active Playwright session is required for browser storage state.");
        }
    }

    private static Path path(String filePath) {
        if (filePath == null || filePath.isBlank()) {
            throw new IllegalArgumentException("Storage state path must not be blank.");
        }
        return Path.of(filePath).toAbsolutePath().normalize();
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Map<String, String>> storageSnapshot(Page page) {
        Object result = page.evaluate(STORAGE_SNAPSHOT_SCRIPT);
        if (!(result instanceof Map<?, ?> root)) {
            return Map.of("localStorage", Map.of(), "sessionStorage", Map.of());
        }
        Map<String, Map<String, String>> storage = new LinkedHashMap<>();
        Object localStorage = root.get("localStorage");
        Object sessionStorage = root.get("sessionStorage");
        storage.put("localStorage", localStorage instanceof Map<?, ?> values ? stringMap(values) : Map.of());
        storage.put("sessionStorage", sessionStorage instanceof Map<?, ?> values ? stringMap(values) : Map.of());
        return storage;
    }

    private static String origin(Page page) {
        try {
            URI uri = URI.create(page.url());
            return uri.getScheme() + "://" + uri.getAuthority();
        } catch (RuntimeException e) {
            return "";
        }
    }

    private static Map<String, String> stringMap(Map<?, ?> source) {
        if (source == null || source.isEmpty()) {
            return Map.of();
        }
        Map<String, String> values = new LinkedHashMap<>();
        source.forEach((key, value) -> values.put(String.valueOf(key), value == null ? "" : String.valueOf(value)));
        return values;
    }

    private static BrowserStorageStateManager.CookieState cookieState(Cookie cookie) {
        BrowserStorageStateManager.CookieState state = new BrowserStorageStateManager.CookieState();
        state.name = cookie.name;
        state.value = cookie.value;
        state.domain = cookie.domain;
        state.path = cookie.path;
        state.expiry = toExpiryMillis(cookie.expires);
        state.secure = Boolean.TRUE.equals(cookie.secure);
        state.httpOnly = Boolean.TRUE.equals(cookie.httpOnly);
        state.sameSite = fromSameSite(cookie.sameSite);
        return state;
    }

    private static Cookie toCookie(BrowserStorageStateManager.CookieState state, String fallbackUrl) {
        Cookie cookie = new Cookie(value(state.name), value(state.value));
        if (state.domain != null && !state.domain.isBlank()) {
            cookie.setDomain(state.domain);
            cookie.setPath(state.path != null && !state.path.isBlank() ? state.path : "/");
        } else {
            cookie.setUrl(fallbackUrl);
        }
        cookie.setExpires(state.expiry != null ? state.expiry / 1000.0 : -1);
        if (state.secure) {
            cookie.setSecure(true);
        }
        if (state.httpOnly) {
            cookie.setHttpOnly(true);
        }
        SameSiteAttribute sameSite = toSameSite(state.sameSite);
        if (sameSite != null) {
            cookie.setSameSite(sameSite);
        }
        return cookie;
    }

    private static Long toExpiryMillis(Double expiresInSeconds) {
        if (expiresInSeconds == null || expiresInSeconds < 0) {
            return null;
        }
        return Math.round(expiresInSeconds * 1000.0);
    }

    private static SameSiteAttribute toSameSite(String value) {
        if (value == null || value.isBlank()) {
            return null;
        }
        return switch (value.toUpperCase(Locale.ROOT)) {
            case "STRICT" -> SameSiteAttribute.STRICT;
            case "LAX" -> SameSiteAttribute.LAX;
            case "NONE" -> SameSiteAttribute.NONE;
            default -> null;
        };
    }

    private static String fromSameSite(SameSiteAttribute sameSite) {
        if (sameSite == null) {
            return null;
        }
        return switch (sameSite) {
            case STRICT -> "Strict";
            case LAX -> "Lax";
            case NONE -> "None";
        };
    }

    private static <T> List<T> nullSafe(List<T> values) {
        return values == null ? List.of() : values;
    }

    private static String value(String value) {
        return value == null ? "" : value;
    }
}
