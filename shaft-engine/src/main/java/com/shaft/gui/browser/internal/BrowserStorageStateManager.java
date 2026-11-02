package com.shaft.gui.browser.internal;

import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.SerializationFeature;
import tools.jackson.databind.json.JsonMapper;
import org.openqa.selenium.Cookie;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Saves and restores browser cookies, localStorage, and sessionStorage for one WebDriver context.
 */
public final class BrowserStorageStateManager {
    private static final ObjectMapper JSON = JsonMapper.builder()
            .enable(SerializationFeature.INDENT_OUTPUT)
            .build();
    private static final String STORAGE_SNAPSHOT_SCRIPT = "return {localStorage: Object.fromEntries(Object.entries(window.localStorage)), "
            + "sessionStorage: Object.fromEntries(Object.entries(window.sessionStorage))};";
    private static final String LOAD_STORAGE_SCRIPT = "window.localStorage.clear(); window.sessionStorage.clear(); "
            + "for (const [key, value] of Object.entries(arguments[0])) { window.localStorage.setItem(key, value); } "
            + "for (const [key, value] of Object.entries(arguments[1])) { window.sessionStorage.setItem(key, value); }";

    private BrowserStorageStateManager() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Saves cookies, localStorage, and sessionStorage to a JSON file.
     *
     * @param driver   active WebDriver
     * @param filePath target JSON file
     */
    public static void save(WebDriver driver, String filePath) {
        requireDriver(driver);
        Path path = path(filePath);
        StorageState state = new StorageState();
        state.schemaVersion = "1.0";
        state.origin = origin(driver);
        state.cookies = driver.manage().getCookies().stream()
                .map(BrowserStorageStateManager::cookieState)
                .toList();
        OriginStorage originStorage = new OriginStorage();
        originStorage.origin = state.origin;
        Map<String, Map<String, String>> storage = storageSnapshot(driver);
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
     * @param driver   active WebDriver
     * @param filePath source JSON file
     */
    public static void load(WebDriver driver, String filePath) {
        requireDriver(driver);
        Path path = path(filePath);
        try {
            StorageState state = JSON.readValue(path.toFile(), StorageState.class);
            driver.manage().deleteAllCookies();
            for (CookieState cookie : nullSafe(state.cookies)) {
                driver.manage().addCookie(toCookie(cookie));
            }
            OriginStorage origin = nullSafe(state.origins).isEmpty() ? new OriginStorage() : state.origins.getFirst();
            javascript(driver).executeScript(LOAD_STORAGE_SCRIPT,
                    origin.localStorage == null ? Map.of() : origin.localStorage,
                    origin.sessionStorage == null ? Map.of() : origin.sessionStorage);
        } catch (RuntimeException e) {
            throw new IllegalStateException("Could not load browser storage state from `" + path + "`.", e);
        }
    }

    private static void requireDriver(WebDriver driver) {
        if (driver == null) {
            throw new IllegalArgumentException("A WebDriver session is required for browser storage state.");
        }
    }

    private static Path path(String filePath) {
        if (filePath == null || filePath.isBlank()) {
            throw new IllegalArgumentException("Storage state path must not be blank.");
        }
        return Path.of(filePath).toAbsolutePath().normalize();
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Map<String, String>> storageSnapshot(WebDriver driver) {
        Object result = javascript(driver).executeScript(STORAGE_SNAPSHOT_SCRIPT);
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

    private static JavascriptExecutor javascript(WebDriver driver) {
        if (driver instanceof JavascriptExecutor executor) {
            return executor;
        }
        throw new IllegalArgumentException("Browser storage state requires a JavaScript-capable WebDriver.");
    }

    private static String origin(WebDriver driver) {
        try {
            URI uri = URI.create(driver.getCurrentUrl());
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

    private static CookieState cookieState(Cookie cookie) {
        CookieState state = new CookieState();
        state.name = cookie.getName();
        state.value = cookie.getValue();
        state.domain = cookie.getDomain();
        state.path = cookie.getPath();
        state.expiry = cookie.getExpiry() == null ? null : cookie.getExpiry().getTime();
        state.secure = cookie.isSecure();
        state.httpOnly = cookie.isHttpOnly();
        state.sameSite = cookie.getSameSite();
        return state;
    }

    private static Cookie toCookie(CookieState state) {
        Cookie.Builder builder = new Cookie.Builder(value(state.name), value(state.value));
        addDomain(builder, state);
        addPath(builder, state);
        addExpiry(builder, state);
        addSecureFlag(builder, state);
        addHttpOnlyFlag(builder, state);
        addSameSite(builder, state);
        return builder.build();
    }

    private static void addDomain(Cookie.Builder builder, CookieState state) {
        if (state.domain != null && !state.domain.isBlank()) {
            builder.domain(state.domain);
        }
    }

    private static void addPath(Cookie.Builder builder, CookieState state) {
        if (state.path != null && !state.path.isBlank()) {
            builder.path(state.path);
        }
    }

    private static void addExpiry(Cookie.Builder builder, CookieState state) {
        if (state.expiry != null) {
            builder.expiresOn(Date.from(Instant.ofEpochMilli(state.expiry)));
        }
    }

    private static void addSecureFlag(Cookie.Builder builder, CookieState state) {
        if (state.secure) {
            builder.isSecure(true);
        }
    }

    private static void addHttpOnlyFlag(Cookie.Builder builder, CookieState state) {
        if (state.httpOnly) {
            builder.isHttpOnly(true);
        }
    }

    private static void addSameSite(Cookie.Builder builder, CookieState state) {
        if (state.sameSite != null && !state.sameSite.isBlank()) {
            builder.sameSite(state.sameSite);
        }
    }

    private static <T> List<T> nullSafe(List<T> values) {
        return values == null ? List.of() : values;
    }

    private static String value(String value) {
        return value == null ? "" : value;
    }

    @SuppressWarnings("java:S1104")
    public static class StorageState {
        public String schemaVersion;
        public String origin;
        public List<CookieState> cookies = List.of();
        public List<OriginStorage> origins = List.of();
    }

    @SuppressWarnings("java:S1104")
    public static class CookieState {
        public String name;
        public String value;
        public String domain;
        public String path;
        public Long expiry;
        public boolean secure;
        public boolean httpOnly;
        public String sameSite;
    }

    @SuppressWarnings("java:S1104")
    public static class OriginStorage {
        public String origin;
        public Map<String, String> localStorage = Map.of();
        public Map<String, String> sessionStorage = Map.of();
    }
}
