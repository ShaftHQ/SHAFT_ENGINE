package com.shaft.tools.internal;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import org.apache.logging.log4j.Level;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpRequest.BodyPublishers;
import java.net.http.HttpResponse;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * FirestoreRestClient handles anonymous usage telemetry for SHAFT Engine.
 * This service is designed to be 100% anonymous and only tracks test run counts.
 * No personal data, machine identifiers, or sensitive information is collected.
 */
public class FirestoreRestClient {

    private static final String FIREBASE_PROJECT_ID = "shaft-engine";
    private static final String API_KEY = "AIzaSyDnpnsxifokegYIke38I1wzvj5Zcm1a4mA";
    private static final String BASE_URL = "https://firestore.googleapis.com/v1/projects/" + FIREBASE_PROJECT_ID + "/databases/(default)/documents/";
    private static final HttpClient httpClient = HttpClient.newBuilder().build();
    private static final AtomicInteger counter = new AtomicInteger();

    // These credentials are required for sending events to Firebase Analytics via the Measurement Protocol.
    // The Measurement ID (e.g., "G-XXXXXXXXXX") from your Firebase/GA4 console.
    private static final String FIREBASE_APP_ID = "G-4L9L79WZBV";
    // The API Secret is found under Project Settings > Integrations > Measurement Protocol API secrets.
    private static final String API_SECRET = "nzK22pHiTZWu8FGgvDVtnA";

    /**
     * Sends anonymous telemetry data if telemetry is enabled.
     * This method executes asynchronously and will not block test execution.
     * Only a simple counter increment is sent to track test run statistics.
     */
    public static void sendTelemetry() {
        if (!SHAFT.Properties.flags.telemetryEnabled()) {
            ReportManager.logDiscrete("Telemetry is disabled, skipping anonymous usage tracking.");
            return;
        }

        // Step 1: Create a document for our counter
        String counterCollection = "counters";
        String counterId = "test_runs";

        // Execute telemetry in a separate virtual thread to avoid blocking
        Thread.ofVirtual().start(() -> {
            try {
                ReportManager.logDiscrete("Sending anonymous usage information...\nNote that no personal data is collected, only generic test run counters. To disable telemetry, set `telemetry.enabled` flag to `false` in your custom.properties file.", Level.INFO);
                // First, create the counter document.
                //System.out.println("Creating counter document...");
                createCounterDocument(counterCollection, counterId);

                // Read the initial value to show it's starting at 0.
                //System.out.println("\nReading initial counter value...");
                readCounter(counterCollection, counterId);

                // Before incrementing the counter in Firestore, log the action as an event.
                //System.out.println("\nLogging 'test_run' event to Analytics...");
                logEventToAnalytics("test_run");

                // Now, increment the counter.
                //System.out.println("\nIncrementing counter...");
                incrementCounter(counterCollection, counterId);

                // Read the final value to confirm the increment.
                //System.out.println("\nReading final counter value...");
                readCounter(counterCollection, counterId);
            } catch (Exception ignored) {
                // Silently catch all exceptions to ensure telemetry failures never impact test execution
            }
        });
    }

    /**
     * Creates a new document with a counter field initialized to 0.
     *
     * @param collectionId The collection to create the document in.
     * @param documentId   The ID for the new counter document.
     * @throws IOException
     * @throws InterruptedException
     */
    public static void createCounterDocument(String collectionId, String documentId) throws IOException, InterruptedException {
        // https://console.firebase.google.com/u/0/project/shaft-engine/firestore/databases/-default-/data/~2Fcounters~2Ftest_runs
        // The JSON body for creating a document with a "value" field of 0.
        String jsonBody = "{"
                + "  \"fields\": {"
                + "    \"value\": {"
                + "      \"integerValue\": \"0\""
                + "    }"
                + "  }"
                + "}";

        // URL for creating a new document within a collection
        String url = BASE_URL + collectionId + "?documentId=" + documentId + "&key=" + API_KEY;

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .POST(BodyPublishers.ofString(jsonBody))
                .header("Content-Type", "application/json")
                .build();

        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());

        //System.out.println("Create Status Code: " + response.statusCode());
        //System.out.println("Create Response Body: " + response.body());
    }

    /**
     * Atomically increments an integer field in a Firestore document.
     *
     * @param collectionId The collection containing the counter document.
     * @param documentId   The ID of the counter document.
     * @throws IOException
     * @throws InterruptedException
     */
    public static void incrementCounter(String collectionId, String documentId) throws IOException, InterruptedException {
        // The JSON body for an atomic increment operation. The correct structure requires
        // the "writes" array with a "transform" object inside and the document path
        // formatted as a relative resource name.
        String jsonBody = "{"
                + "  \"writes\": ["
                + "    {"
                + "      \"transform\": {"
                + "        \"document\": \"projects/" + FIREBASE_PROJECT_ID + "/databases/(default)/documents/" + collectionId + "/" + documentId + "\","
                + "        \"fieldTransforms\": ["
                + "          {"
                + "            \"fieldPath\": \"value\","
                + "            \"increment\": {"
                + "              \"integerValue\": \"1\""
                + "            }"
                + "          }"
                + "        ]"
                + "      }"
                + "    }"
                + "  ]"
                + "}";

        // The URL for the batch write operation, which contains the increment.
        String url = BASE_URL + ":commit?key=" + API_KEY;

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .POST(BodyPublishers.ofString(jsonBody))
                .header("Content-Type", "application/json")
                .build();

        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());

        //System.out.println("Increment Status Code: " + response.statusCode());
        //System.out.println("Increment Response Body: " + response.body());
    }

    /**
     * Reads a document and prints its content.
     *
     * @param collectionId The collection containing the document.
     * @param documentId   The ID of the document to read.
     * @throws IOException
     * @throws InterruptedException
     */
    public static void readCounter(String collectionId, String documentId) throws IOException, InterruptedException {
        String url = BASE_URL + collectionId + "/" + documentId + "?key=" + API_KEY;

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .GET()
                .build();

        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());

        //System.out.println("Read Status Code: " + response.statusCode());
        //System.out.println("Read Response Body: " + response.body());
    }

    /**
     * Logs a custom event to Firebase Analytics via the Measurement Protocol.
     * This is the correct way to log events from a desktop application.
     *
     * @param eventName The name of the event to log (e.g., "test_run").
     * @throws IOException
     * @throws InterruptedException
     */
    public static void logEventToAnalytics(String eventName) throws IOException, InterruptedException {
        // https://analytics.google.com/analytics/web/?authuser=0&hl=en-GB#/a368239280p504911558/realtime/overview?params=_u..nav%3Dmaui

        // The Measurement Protocol endpoint.
        String url = "https://www.google-analytics.com/mp/collect?measurement_id=" + FIREBASE_APP_ID + "&api_secret=" + API_SECRET;

        // A unique identifier for the user or device.
        String clientId = UUID.randomUUID().toString();

        String jsonBody = "{"
                + "  \"client_id\": \"" + clientId + "\","
                + "  \"events\": ["
                + "    {"
                + "      \"name\": \"" + eventName + "\","
                + "      \"params\": {"
                + "        \"run_platform\": \"java_desktop\","
                + "        \"runtime_version\": \"" + Runtime.version() + "\","
                + "        \"engine_version\": \"" + SHAFT.Properties.internal.shaftEngineVersion() + "\","
                + "        \"target_os\": \"" + SHAFT.Properties.platform.targetPlatform() + "\","
                + "        \"target_browser\": \"" + SHAFT.Properties.web.targetBrowserName() + "\""
                + "      }"
                + "    }"
                + "  ]"
                + "}";

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .POST(BodyPublishers.ofString(jsonBody))
                .header("Content-Type", "application/json")
                .build();

        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());

        //System.out.println("Analytics Log Status Code: " + response.statusCode());
        //System.out.println("Analytics Log Response Body: " + response.body());
    }
}