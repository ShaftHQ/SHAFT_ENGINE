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
 * Measurement Protocol. The event captures per-test-method outcome counts
 * (passed, failed, skipped, total), engine version, target platform, OS name,
 * and anonymous geographic region — derived automatically by GA4 from the
 * {@code ip_override} field, so no separate geo-lookup call is required.
 * <p>
 * <strong>Privacy</strong>: no personal data is collected.  Only the client IP is
 * forwarded to GA4 (via {@code ip_override}) to enable anonymous country-level
 * reporting; the raw IP is not stored by SHAFT.  Telemetry can be disabled by
 * setting {@code telemetry.enabled=false} in {@code custom.properties}.
 * <p>
 * <strong>Identity model</strong>:
 * <ul>
 *   <li>{@code client_id} / {@code user_id} — a <em>static</em>, persistent UUID stored
 *       on disk at {@code src/test/resources/META-INF/services/uuid}.  It identifies the
 *       installation across test runs.</li>
 *   <li>{@code session_id} (event param) — a <em>dynamic</em> value derived from the
 *       epoch-second timestamp at which the current test run started.  It uniquely
 *       identifies a single test-run session and changes with every execution.</li>
 * </ul>
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

    // Lazily initialized, stable installation UUID.  Loaded once from disk and cached for the
    // lifetime of the JVM so that repeated telemetry calls avoid redundant file I/O.
    private static volatile String cachedClientId;
    private static final Object CLIENT_ID_LOCK = new Object();

    /**
     * Sends anonymous telemetry data if telemetry is enabled.
     * This method executes asynchronously and will not block test execution.
     * One GA4 analytics event ({@code test_run}) is sent, carrying deduplicated
     * per-method outcome counts and key environment dimensions.
     * Network overhead is minimal: only two HTTP calls are made (one to resolve the
     * client IP for geographic attribution, one to the GA4 Measurement Protocol endpoint).
     *
     * @param executionStartTime the epoch-millisecond timestamp when the test run began
     * @param executionEndTime   the epoch-millisecond timestamp when the test run finished
     * @param passedTests        the number of unique test methods that passed
     * @param failedTests        the number of unique test methods that failed (never passed)
     * @param skippedTests       the number of unique test methods that were skipped
     * @param flakyTests         the number of test methods that failed on at least one attempt
     *                           but eventually passed (i.e. retried to success); 0 if none
     */
    public static void sendTelemetry(long executionStartTime, long executionEndTime,
                                     int passedTests, int failedTests, int skippedTests, int flakyTests) {
        if (!SHAFT.Properties.flags.telemetryEnabled()) {
            ReportManager.logDiscrete("Telemetry is disabled, skipping anonymous usage tracking.");
            return;
        }

        // Execute telemetry in a separate virtual thread to avoid blocking
        Thread.ofVirtual().start(() -> {
            try {
                ReportManager.logDiscrete("Sending anonymous usage information...\n"
                        + "Note: the client IP is forwarded to Google Analytics for anonymous country-level reporting only.\n"
                        + "To disable telemetry, set `telemetry.enabled=false` in your custom.properties file.", Level.INFO);

                // Single GA4 Measurement Protocol call carries all telemetry.
                // Country/city attribution is handled automatically by GA4 via ip_override —
                // no separate geo-lookup call is needed.
                logEventToAnalytics("test_run", executionStartTime, executionEndTime - executionStartTime,
                        passedTests, failedTests, skippedTests, flakyTests);
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
     * @throws IOException          in case of a network or IO issue
     * @throws InterruptedException if the operation is interrupted
     * @deprecated Firestore counters are no longer used for telemetry; GA4 counts events natively.
     */
    @Deprecated(since = "10.2.20260422", forRemoval = true)
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
     * @throws IOException          in case of a network or IO issue
     * @throws InterruptedException if the operation is interrupted
     * @deprecated Firestore counters are no longer used for telemetry; GA4 counts events natively.
     */
    @Deprecated(since = "10.2.20260422", forRemoval = true)
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
     * Reads a document and returns its content.
     *
     * @param collectionId The collection containing the document.
     * @param documentId   The ID of the document to read.
     * @throws IOException          in case of a network or IO issue
     * @throws InterruptedException if the operation is interrupted
     * @deprecated Firestore counters are no longer used for telemetry; GA4 counts events natively.
     */
    @Deprecated(since = "10.2.20260422", forRemoval = true)
    public static void readCounter(String collectionId, String documentId) throws IOException, InterruptedException {
        String url = BASE_URL + collectionId + "/" + documentId + "?key=" + API_KEY;

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .GET()
                .build();

        httpClient.send(request, HttpResponse.BodyHandlers.ofString());
    }

    /**
     * Returns the persistent installation UUID used as {@code client_id} and {@code user_id}.
     * The UUID is read from disk on the first call and cached for the JVM lifetime to avoid
     * repeated file I/O on subsequent telemetry events.
     *
     * @return the stable installation UUID string
     */
    private static String resolveClientId() {
        if (cachedClientId == null) {
            synchronized (CLIENT_ID_LOCK) {
                if (cachedClientId == null) {
                    // The UUID is stored in src/test/resources/ so that it is local to each
                    // project working directory. It should be added to .gitignore to ensure each
                    // cloned checkout generates its own unique identifier rather than sharing
                    // the same UUID across all users of the same repository.
                    String uuidFilePath = "src/test/resources/META-INF/services/uuid";
                    var fileActions = FileActions.getInstance(true);
                    if (!fileActions.doesFileExist(uuidFilePath)) {
                        fileActions.writeToFile(uuidFilePath, UUID.randomUUID().toString());
                    }
                    cachedClientId = fileActions.readFile(uuidFilePath);
                }
            }
        }
        return cachedClientId;
    }

    /**
     * Sends one {@code test_run} event to the GA4 Measurement Protocol endpoint.
     *
     * <p><strong>Network calls made</strong> (two total):
     * <ol>
     *   <li>{@code checkip.amazonaws.com} — resolves the outbound IP so GA4 can attribute
     *       the event to the correct country via {@code ip_override}.</li>
     *   <li>GA4 Measurement Protocol — posts the event payload.</li>
     * </ol>
     * No separate geo-lookup service (e.g., ipinfo.io) is called; GA4 derives the
     * country/city automatically from {@code ip_override}.
     *
     * <p><strong>Identity fields</strong>:
     * <ul>
     *   <li>{@code client_id} / {@code user_id} — the <em>static</em> installation UUID
     *       (persisted on disk).  Stable across runs; used for unique-user counting.</li>
     *   <li>{@code session_id} (event param) — the <em>dynamic</em> epoch-second timestamp of the
     *       test-run start.  Unique per execution; used for session-scoped analysis.</li>
     * </ul>
     *
     * @param eventName              the name of the event to log (e.g., {@code "test_run"})
     * @param executionStartTime     the epoch-millisecond timestamp when the test run began;
     *                               divided by 1000 to produce the {@code session_id}
     * @param durationInMilliseconds the total execution duration in milliseconds
     * @param passedTests            the number of unique test methods that passed
     * @param failedTests            the number of unique test methods that failed (never passed)
     * @param skippedTests           the number of unique test methods that were skipped
     * @param flakyTests             the number of test methods that failed then passed on retry; 0 if none
     * @throws IOException          in case of a network or IO issue
     * @throws InterruptedException if the operation is interrupted
     * @throws URISyntaxException   if a constructed URI is malformed
     * @throws JSONException        if the JSON payload cannot be constructed
     */
    public static void logEventToAnalytics(String eventName, long executionStartTime,
                                           long durationInMilliseconds,
                                           int passedTests, int failedTests, int skippedTests, int flakyTests)
            throws IOException, InterruptedException, URISyntaxException, JSONException {
        // https://analytics.google.com/analytics/web/?authuser=0&hl=en-GB#/a368239280p504911558/realtime/overview?params=_u..nav%3Dmaui
        // https://developers.google.com/analytics/devguides/collection/protocol/ga4/reference?client_type=gtag#payload

        // The Measurement Protocol endpoint.
        String url = "https://www.google-analytics.com/mp/collect?measurement_id=" + MEASUREMENT_ID + "&api_secret=" + API_SECRET;

        JSONObject requestBody = new JSONObject();

        // --- Static identity: client_id / user_id ---
        // Resolve the persistent installation UUID once per JVM run, then reuse the cached value.
        // client_id is the primary GA4 device/installation identifier and must be stable.
        // user_id is set to the same value so GA4 can correlate client and user dimensions,
        // enabling unique-installation counting in both device-level and user-level reports.
        String clientId = resolveClientId();
        requestBody.put("client_id", clientId);
        requestBody.put("user_id", clientId);

        // --- Geographic attribution ---
        // Forward the client's outbound IP so GA4 can derive country/city automatically.
        // This eliminates the need for a separate geo-lookup HTTP call (e.g. ipinfo.io).
        // https://checkip.amazonaws.com returns the plain outbound IPv4 address.
        var ip = new BufferedReader(new InputStreamReader(
                new URI("https://checkip.amazonaws.com").toURL().openStream())).readLine();
        requestBody.put("ip_override", ip);

        // --- Event payload ---
        var eventInformation = new JSONObject();
        eventInformation.put("name", eventName);

        var eventParams = new JSONObject();

        // GA4 standard session params
        eventParams.put("engagement_time_msec", durationInMilliseconds);
        // session_id is DYNAMIC: unique per test run (epoch-seconds of session start).
        // It is intentionally different from the static client_id / user_id.
        eventParams.put("session_id", String.valueOf(executionStartTime / 1000));

        // Environment dimensions
        eventParams.put("engine_version", SHAFT.Properties.internal.shaftEngineVersion());
        eventParams.put("target_os", SHAFT.Properties.platform.targetPlatform());
        eventParams.put("target_browser", SHAFT.Properties.web.targetBrowserName());
        eventParams.put("os_name", System.getProperty("os.name"));
        eventParams.put("run_platform", "java_desktop");
        eventParams.put("runtime_version", Runtime.version().toString());

        // Deduplicated per-method outcome counts — categories are mutually exclusive.
        // passed  = first-attempt passes (never recorded as failed)
        // failed  = permanent failures (never passed)
        // skipped = never executed
        // flaky   = failed on at least one attempt then passed on retry
        // total   = sum of all four categories (no double-counting)
        eventParams.put("passed_tests", passedTests);
        eventParams.put("failed_tests", failedTests);
        eventParams.put("skipped_tests", skippedTests);
        // flaky_tests = tests that failed on at least one attempt but eventually passed (retried to success).
        // Always reported; 0 when no flaky tests were observed.
        eventParams.put("flaky_tests", flakyTests);
        eventParams.put("total_tests", passedTests + failedTests + skippedTests + flakyTests);

        eventInformation.put("params", eventParams);

        JSONArray events = new JSONArray();
        events.put(0, eventInformation);
        requestBody.put("events", events);

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .POST(BodyPublishers.ofString(requestBody.toString()))
                .header("Content-Type", "application/json")
                .build();

        httpClient.send(request, HttpResponse.BodyHandlers.ofString());
    }
}