package com.shaft.intellij.settings;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;

/**
 * One-shot, lightweight provider API key validity checks for the settings panel's per-provider
 * "Test" buttons (issue #3627). Each check is a single minimal-cost authenticated GET against the
 * provider's own API, strictly timed out, reporting only pass/fail + a short reason phrase --
 * never the key value or a response body -- so results are safe to render directly in the UI and
 * never appear in logs.
 */
final class ProviderKeyProbe {
    private static final Duration CONNECT_TIMEOUT = Duration.ofSeconds(5);
    private static final Duration REQUEST_TIMEOUT = Duration.ofSeconds(10);

    private ProviderKeyProbe() {
    }

    // The boolean component is named "success" rather than "ok" so the record's auto-generated
    // instance accessor does not collide with the static Result.ok() factory below (a record
    // component and a static method of the same name is a compile error: "invalid accessor
    // method in record Result").
    record Result(boolean success, String reason) {
        static Result ok() {
            return new Result(true, "");
        }

        static Result fail(String reason) {
            return new Result(false, reason);
        }
    }

    static Result testOpenAi(char[] key) {
        HttpRequest request = HttpRequest.newBuilder(URI.create("https://api.openai.com/v1/models"))
                .timeout(REQUEST_TIMEOUT)
                .header("Authorization", "Bearer " + new String(key))
                .GET()
                .build();
        return send(request, "OpenAI");
    }

    static Result testAnthropic(char[] key) {
        HttpRequest request = HttpRequest.newBuilder(URI.create("https://api.anthropic.com/v1/models"))
                .timeout(REQUEST_TIMEOUT)
                .header("x-api-key", new String(key))
                .header("anthropic-version", "2023-06-01")
                .GET()
                .build();
        return send(request, "Anthropic");
    }

    static Result testGemini(char[] key) {
        String url = "https://generativelanguage.googleapis.com/v1beta/models?key=" + new String(key);
        HttpRequest request = HttpRequest.newBuilder(URI.create(url))
                .timeout(REQUEST_TIMEOUT)
                .GET()
                .build();
        return send(request, "Gemini");
    }

    static Result testGithub(char[] key) {
        HttpRequest request = HttpRequest.newBuilder(URI.create("https://api.github.com/user"))
                .timeout(REQUEST_TIMEOUT)
                .header("Authorization", "Bearer " + new String(key))
                .header("Accept", "application/vnd.github+json")
                .GET()
                .build();
        return send(request, "GitHub");
    }

    private static Result send(HttpRequest request, String providerLabel) {
        // No try-with-resources: HttpClient is only AutoCloseable on JDK 21+, and this module
        // still compiles with --release 17 (mirrors SetupPrerequisites#releaseVersionFromMavenCentral).
        try {
            HttpClient client = HttpClient.newBuilder()
                    .connectTimeout(CONNECT_TIMEOUT)
                    .followRedirects(HttpClient.Redirect.NORMAL)
                    .build();
            HttpResponse<Void> response = client.send(request, HttpResponse.BodyHandlers.discarding());
            int status = response.statusCode();
            if (status == 200) {
                return Result.ok();
            }
            if (status == 401 || status == 403) {
                return Result.fail("Invalid or expired API key.");
            }
            return Result.fail(providerLabel + " returned HTTP " + status + ".");
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            return Result.fail("Check interrupted.");
        } catch (Exception exception) {
            return Result.fail("Could not reach " + providerLabel + " (check network).");
        }
    }
}
