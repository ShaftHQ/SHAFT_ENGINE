package com.shaft.tools.internal.support;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;

/**
 * Lightweight HTTP ping using {@link HttpURLConnection} — no REST-Assured dependency.
 * Used by shaft-web (DriverFactoryHelper) to check remote Grid/Appium server readiness
 * without creating a compile-time dependency on shaft-api.
 */
public class HttpPingUtility {

    private HttpPingUtility() {
    }

    /**
     * Opens a GET connection to {@code url} and returns the HTTP response status code.
     *
     * @param url       fully-qualified URL to ping (e.g. {@code http://localhost:4444/status/})
     * @param timeoutMs connect and read timeout in milliseconds
     * @return HTTP status code (e.g. 200, 503)
     * @throws IOException if the connection cannot be opened or the response cannot be read
     */
    public static int getStatusCode(String url, int timeoutMs) throws IOException {
        HttpURLConnection connection = (HttpURLConnection) new URL(url).openConnection();
        connection.setConnectTimeout(timeoutMs);
        connection.setReadTimeout(timeoutMs);
        connection.setRequestMethod("GET");
        try {
            return connection.getResponseCode();
        } finally {
            connection.disconnect();
        }
    }
}
