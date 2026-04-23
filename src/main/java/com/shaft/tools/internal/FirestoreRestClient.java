package com.shaft.tools.internal;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import org.apache.logging.log4j.Level;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpRequest.BodyPublishers;
import java.net.http.HttpResponse;
import java.util.UUID;

/**
 * FirestoreRestClient handles anonymous usage telemetry for SHAFT Engine.
 * <p>
 * Each test run sends one {@code test_run} event to Google Analytics (GA4) via the
 * Measurement Protocol and atomically increments a global counter in Firestore.
 * The event captures per-test-method outcome counts (passed, failed, skipped),
 * engine version, target platform, and anonymous geographic region (city/country).
 * No personal data beyond geographic region is collected; telemetry can be
 * disabled by setting {@code telemetry.enabled=false} in {@code custom.properties}.
 */
public class FirestoreRestClient {

    private static final String FIREBASE_PROJECT_ID = "shaft-engine";
    private static final String API_KEY = "AIzaSyDnpnsxifokegYIke38I1wzvj5Zcm1a4mA";
    private static final String BASE_URL = "https://firestore.googleapis.com/v1/projects/" + FIREBASE_PROJECT_ID + "/databases/(default)/documents/";
    private static final HttpClient httpClient = HttpClient.newBuilder().build();

    // These credentials are required for sending events to Firebase Analytics via the Measurement Protocol.
    // https://analytics.google.com/analytics/web/?authuser=0&hl=en-GB#/a327683500p504950575/admin/streams/table/12157123249
    // https://developers.google.com/analytics/devguides/collection/protocol/ga4/reference?client_type=gtag#payload
    // https://analytics.google.com/analytics/web/?authuser=0&hl=en-GB#/a368239280p504911558/realtime/overview?params=_u..nav%3Dmaui&collectionId=user
    // The Measurement ID (e.g., "G-XXXXXXXXXX") from your Firebase/GA4 console.
    /**
     * api_secret
     * Required. The API Secret from the Google Analytics UI.
     * Found under Admin > Data Streams > Choose your stream > Measurement Protocol > Create.
     * Private to your organization. Should be regularly updated to avoid excessive SPAM.
     * measurement_id
     * Measurement ID. The identifier for a Data Stream. Found in the Google Analytics UI under Admin > Data Streams > choose your stream > Measurement ID
     */
    private static final String MEASUREMENT_ID = "G-4L9L79WZBV";
    // The API Secret is found under Admin > Data Streams > Choose your stream > Measurement Protocol > Create
    private static final String API_SECRET = "nzK22pHiTZWu8FGgvDVtnA";

    /**
     * Sends anonymous telemetry data if telemetry is enabled.
     * This method executes asynchronously and will not block test execution.
     * A counter increment and an analytics event are sent to track test run statistics,
     * including the number of passed, failed, and skipped test methods.
     *
     * @param executionStartTime the epoch-millisecond timestamp when the test run began
     * @param executionEndTime   the epoch-millisecond timestamp when the test run finished
     * @param passedTests        the number of test methods that passed
     * @param failedTests        the number of test methods that failed
     * @param skippedTests       the number of test methods that were skipped
     */
    public static void sendTelemetry(long executionStartTime, long executionEndTime,
                                     int passedTests, int failedTests, int skippedTests) {
        if (!SHAFT.Properties.flags.telemetryEnabled()) {
            ReportManager.logDiscrete("Telemetry is disabled, skipping anonymous usage tracking.");
            return;
        }

        // Execute telemetry in a separate virtual thread to avoid blocking
        Thread.ofVirtual().start(() -> {
            try {
                ReportManager.logDiscrete("Sending anonymous usage information...\n"
                        + "Note: geographic region (country/city) and OS information are collected anonymously.\n"
                        + "To disable telemetry, set `telemetry.enabled=false` in your custom.properties file.", Level.INFO);

                // Log the 'test_run' event to Analytics with per-method outcome counts.
                logEventToAnalytics("test_run", executionStartTime, executionEndTime - executionStartTime,
                        passedTests, failedTests, skippedTests);

                // Atomically increment the global test-run counter in Firestore.
                incrementCounter("counters", "test_runs");
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
     * @throws IOException  in case of a network or IO issue
     * @throws InterruptedException if the operation is interrupted
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

        httpClient.send(request, HttpResponse.BodyHandlers.ofString());
    }

    /**
     * Atomically increments an integer field in a Firestore document.
     *
     * @param collectionId The collection containing the counter document.
     * @param documentId   The ID of the counter document.
     * @throws IOException  in case of a network or IO issue
     * @throws InterruptedException if the operation is interrupted
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

        httpClient.send(request, HttpResponse.BodyHandlers.ofString());
    }

    /**
     * Reads a document and prints its content.
     *
     * @param collectionId The collection containing the document.
     * @param documentId   The ID of the document to read.
     * @throws IOException  in case of a network or IO issue
     * @throws InterruptedException if the operation is interrupted
     */
    public static void readCounter(String collectionId, String documentId) throws IOException, InterruptedException {
        String url = BASE_URL + collectionId + "/" + documentId + "?key=" + API_KEY;

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .GET()
                .build();

        httpClient.send(request, HttpResponse.BodyHandlers.ofString());
    }

    /**
     * Logs a custom event to Firebase Analytics via the Measurement Protocol.
     * This is the correct way to log events from a desktop application.
     *
     * <p>The event includes per-test-method outcome counts (passed, failed, skipped)
     * as well as OS and runtime information to help understand the usage environment.
     * Geographic data (city, country) is collected anonymously via the client IP.
     *
     * @param eventName              the name of the event to log (e.g., "test_run")
     * @param executionStartTime     the epoch-millisecond timestamp when the test run began;
     *                               used as the stable {@code session_id}
     * @param durationInMilliseconds the total execution duration in milliseconds
     * @param passedTests            the number of test methods that passed
     * @param failedTests            the number of test methods that failed
     * @param skippedTests           the number of test methods that were skipped
     * @throws IOException        in case of a network or IO issue
     * @throws InterruptedException if the operation is interrupted
     * @throws URISyntaxException if the URI is malformed
     */
    public static void logEventToAnalytics(String eventName, long executionStartTime,
                                           long durationInMilliseconds,
                                           int passedTests, int failedTests, int skippedTests)
            throws IOException, InterruptedException, URISyntaxException, JSONException {
        // https://analytics.google.com/analytics/web/?authuser=0&hl=en-GB#/a368239280p504911558/realtime/overview?params=_u..nav%3Dmaui
        // https://developers.google.com/analytics/devguides/collection/protocol/ga4/reference?client_type=gtag#payload

        // The Measurement Protocol endpoint.
        String url = "https://www.google-analytics.com/mp/collect?measurement_id=" + MEASUREMENT_ID + "&api_secret=" + API_SECRET;

        JSONObject requestBody = new JSONObject();

        // Read or create a persistent UUID to identify this installation across runs.
        // This UUID is used as client_id (the primary GA4 device/installation identifier).
        String uuidFilePath = "src/test/resources/META-INF/services/uuid";
        var fileActions = FileActions.getInstance(true);
        if (!fileActions.doesFileExist(uuidFilePath)) {
            fileActions.writeToFile(uuidFilePath, UUID.randomUUID().toString());
        }
        String clientId = fileActions.readFile(uuidFilePath);
        // client_id is the primary GA4 identifier for a device/installation; must be stable across runs.
        requestBody.put("client_id", clientId);
        // user_id is set to the same installation UUID so GA4 can correlate client and user dimensions,
        // enabling unified reporting across both device-level and user-level metrics.
        requestBody.put("user_id", clientId);

        // https://ipv4.icanhazip.com/
        // http://myexternalip.com/raw
        // http://ipecho.net/plain
        var ip = new BufferedReader(new InputStreamReader(new URI("https://checkip.amazonaws.com").toURL().openStream())).readLine();
        requestBody.put("ip_override", ip);

        // Resolve geographic region (city, country) from the client IP for anonymous usage analytics.
        // https://ipinfo.io/dashboard/lite
        // https://ipinfo.io/?token=0a5dc997f79f6a
        // https://ipinfo.io/156.214.138.86?token=0a5dc997f79f6a
        HttpRequest userLocationRequest = HttpRequest.newBuilder()
                .uri(URI.create("https://ipinfo.io/" + ip + "?token=0a5dc997f79f6a"))
                .GET()
                .build();
        HttpResponse<String> userLocationResponse = httpClient.send(userLocationRequest, HttpResponse.BodyHandlers.ofString());
        if (userLocationResponse.statusCode() == 200) {
            try {
                var userLocation = new JSONObject();
                var locationData = new org.json.JSONObject(userLocationResponse.body());
                userLocation.put("city", locationData.get("city"));
                userLocation.put("country_id", locationData.get("country"));
                requestBody.put("user_location", userLocation);
            } catch (Exception e) {
                // silently ignore and use default values
            }
        }

        var deviceInformation = new JSONObject();
        deviceInformation.put("operating_system", System.getProperty("os.name"));
        deviceInformation.put("operating_system_version", System.getProperty("os.version"));
        requestBody.put("device", deviceInformation);

        var eventInformation = new JSONObject();
        eventInformation.put("name", eventName);

        var eventParams = new JSONObject();
        eventParams.put("engagement_time_msec", durationInMilliseconds);
        // session_id must match ^\d+$ and should represent when this test session started.
        eventParams.put("session_id", String.valueOf(executionStartTime / 1000));
        eventParams.put("engine_version", SHAFT.Properties.internal.shaftEngineVersion());
        eventParams.put("target_os", SHAFT.Properties.platform.targetPlatform());
        eventParams.put("target_browser", SHAFT.Properties.web.targetBrowserName());
        eventParams.put("run_platform", "java_desktop");
        eventParams.put("runtime_version", Runtime.version().toString());
        // Per-test-method outcome counts so each method's result is represented in analytics.
        eventParams.put("passed_tests", passedTests);
        eventParams.put("failed_tests", failedTests);
        eventParams.put("skipped_tests", skippedTests);
        eventInformation.put("params", eventParams);

        JSONArray events = new JSONArray();
        events.put(0, eventInformation);
        requestBody.put("events", events);

        String stringRequestBody = requestBody.toString();

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .POST(BodyPublishers.ofString(stringRequestBody))
                .header("Content-Type", "application/json")
                .build();

        httpClient.send(request, HttpResponse.BodyHandlers.ofString());
    }
}