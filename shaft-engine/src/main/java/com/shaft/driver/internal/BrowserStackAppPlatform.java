package com.shaft.driver.internal;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Base64;
import java.util.Locale;

/**
 * Checks that a native BrowserStack app matches {@code targetOperatingSystem} before a session starts.
 * A local path is classified from its file name ({@code .apk} or {@code .aab} is ANDROID, {@code .ipa}
 * is IOS). A {@code bs://} id is classified from the app name or filename, which the caller supplies
 * or {@link #lookupAppName} reads from recent apps. Credentials and the full id are never put in the
 * failure message. {@code -DbrowserStack.appUrl} remains the property that selects the app; this class
 * does not read a second secret.
 */
public final class BrowserStackAppPlatform {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final URI RECENT_APPS = URI.create("https://api-cloud.browserstack.com/app-automate/recent_apps");

    private BrowserStackAppPlatform() {
    }

    /**
     * @param name file name, URL, or BrowserStack app name
     * @return {@code ANDROID}, {@code IOS}, or blank when the name has no known app suffix
     */
    public static String platformFromName(String name) {
        if (name == null || name.isBlank()) {
            return "";
        }
        String leaf = name.toLowerCase(Locale.ROOT).split("\\?", 2)[0];
        int slash = Math.max(leaf.lastIndexOf('/'), leaf.lastIndexOf('\\'));
        if (slash >= 0) {
            leaf = leaf.substring(slash + 1);
        }
        if (leaf.endsWith(".ipa")) {
            return "IOS";
        }
        if (leaf.endsWith(".apk") || leaf.endsWith(".aab")) {
            return "ANDROID";
        }
        return "";
    }

    /**
     * @param targetOperatingSystem configured {@code targetOperatingSystem}
     * @return {@code true} when the target is Android or iOS
     */
    public static boolean isMobileTarget(String targetOperatingSystem) {
        return !mobileOs(targetOperatingSystem).isEmpty();
    }

    /**
     * Fails when a native app and the mobile target disagree.
     * A non-mobile target is ignored. An unclassified local path is ignored. An unclassified
     * {@code bs://} app fails closed.
     *
     * @param appReference          local path, {@code http} URL, or {@code bs://} id
     * @param targetOperatingSystem {@code targetOperatingSystem}
     * @param resolvedAppName       app name for a {@code bs://} id; ignored for a local path that already has a suffix
     */
    public static void requireMatch(String appReference, String targetOperatingSystem, String resolvedAppName) {
        String target = mobileOs(targetOperatingSystem);
        if (target.isEmpty()) {
            return;
        }
        String kind = classify(appReference, resolvedAppName);
        if (kind.isEmpty()) {
            if (appReference != null && appReference.startsWith("bs://")) {
                throw new IllegalArgumentException(
                        "browserStack.appUrl could not be classified as ANDROID or IOS from its app name.");
            }
            return;
        }
        if (!kind.equals(target)) {
            throw new IllegalArgumentException(
                    "browserStack.appUrl is " + kind + " but targetOperatingSystem is " + target + ".");
        }
    }

    /**
     * Finds the app name for {@code appUrl} in a recent-apps JSON body. No network.
     *
     * @param recentAppsJson array body, or an object with an {@code apps} array
     * @param appUrl         {@code bs://} id to match against {@code app_url}
     * @return {@code app_name}, otherwise {@code filename}, otherwise blank
     */
    public static String appNameFromRecentApps(String recentAppsJson, String appUrl) {
        if (recentAppsJson == null || recentAppsJson.isBlank() || appUrl == null || appUrl.isBlank()) {
            return "";
        }
        JsonNode root;
        try {
            root = JSON.readTree(recentAppsJson);
        } catch (IOException exception) {
            throw new IllegalArgumentException(
                    "BrowserStack recent apps response was not JSON, so browserStack.appUrl could not be classified.");
        }
        JsonNode apps = root.isArray() ? root : root.path("apps");
        if (!apps.isArray()) {
            return "";
        }
        for (JsonNode app : apps) {
            if (!appUrl.equals(app.path("app_url").asText(""))) {
                continue;
            }
            String name = app.path("app_name").asText("");
            if (name.isBlank()) {
                name = app.path("filename").asText("");
            }
            return name;
        }
        return "";
    }

    /**
     * Reads {@code app_name} for {@code appUrl} from BrowserStack recent apps.
     * Uses the existing username and access key. The id and the credentials are not included in errors.
     *
     * @param appUrl    {@code bs://} id
     * @param username  {@code browserStack.userName}
     * @param accessKey {@code browserStack.accessKey}
     * @return the remote app name or filename
     */
    public static String lookupAppName(String appUrl, String username, String accessKey) {
        if (username == null || username.isBlank() || accessKey == null || accessKey.isBlank()) {
            throw new IllegalArgumentException(
                    "BrowserStack credentials are required to classify browserStack.appUrl, and none are configured.");
        }
        String name = appNameFromRecentApps(readRecentApps(username, accessKey), appUrl);
        if (name.isBlank()) {
            throw new IllegalArgumentException(
                    "browserStack.appUrl was not found in recent BrowserStack apps, so its platform could not be checked.");
        }
        return name;
    }

    private static String readRecentApps(String username, String accessKey) {
        String token = Base64.getEncoder().encodeToString((username + ":" + accessKey).getBytes(StandardCharsets.UTF_8));
        HttpRequest request = HttpRequest.newBuilder(RECENT_APPS)
                .timeout(Duration.ofSeconds(20))
                .header("Authorization", "Basic " + token)
                .GET()
                .build();
        try {
            HttpResponse<String> response = HttpClient.newBuilder().build()
                    .send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8));
            if (response.statusCode() / 100 != 2) {
                throw new IllegalStateException(
                        "Could not classify browserStack.appUrl because recent apps returned HTTP "
                                + response.statusCode() + ".");
            }
            return response.body();
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(
                    "Could not classify browserStack.appUrl because the recent apps request was interrupted.",
                    exception);
        } catch (IOException exception) {
            throw new IllegalStateException(
                    "Could not classify browserStack.appUrl because the recent apps request failed.", exception);
        }
    }

    private static String classify(String appReference, String resolvedAppName) {
        if (appReference != null && appReference.startsWith("bs://")) {
            return platformFromName(resolvedAppName);
        }
        String fromReference = platformFromName(appReference);
        if (!fromReference.isEmpty()) {
            return fromReference;
        }
        return platformFromName(resolvedAppName);
    }

    private static String mobileOs(String targetOperatingSystem) {
        if (targetOperatingSystem == null || targetOperatingSystem.isBlank()) {
            return "";
        }
        String value = targetOperatingSystem.trim().toUpperCase(Locale.ROOT);
        if (value.contains("ANDROID")) {
            return "ANDROID";
        }
        if ("IOS".equals(value) || value.startsWith("IOS ") || value.startsWith("IOS-")) {
            return "IOS";
        }
        return "";
    }
}
