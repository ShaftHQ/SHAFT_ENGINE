package com.shaft.mcp;

import com.shaft.infrastructure.ManagedEnvironment;
import com.shaft.infrastructure.SetupProfile;
import com.shaft.infrastructure.SetupReceipt;
import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class McpMobileInspectorRecordingServiceTest {
    @TempDir
    Path temp;

    @Test
    void prepareRequiresAppDetailsBeforeStart() {
        McpMobileInspectorRecordingService service = service();

        McpMobileInspectorPlan plan = service.prepare(
                "Android",
                "recordings/native.json",
                true,
                "",
                "",
                "",
                "",
                "",
                "",
                "",
                "",
                0,
                "",
                "",
                "",
                0,
                0,
                true);

        assertFalse(plan.readyToStart());
        assertTrue(plan.warnings().stream().anyMatch(warning -> warning.contains("appPackage")));
        assertTrue(plan.toolchainStatus().missingDependencies().contains("android platform-tools"));
        assertThrows(IllegalStateException.class,
                () -> service.start(plan.confirmationToken(), "", false));
    }

    @Test
    void prepareBuildsProvisioningProposalWhenNoAndroidDeviceExists() {
        String originalHome = System.getProperty("user.home");
        System.setProperty("user.home", temp.toString());
        try {
            McpMobileInspectorRecordingService service = service();

            McpMobileInspectorPlan plan = service.prepare(
                    "Android",
                    "recordings/native.json",
                    false,
                    "",
                    "com.example",
                    ".MainActivity",
                    "",
                    "",
                    "Pixel 8",
                    "",
                    "",
                    36,
                    "",
                    "",
                    "",
                    0,
                    0,
                    true);

            assertTrue(plan.readyToStart());
            assertTrue(plan.willProvisionAndroidEmulator());
            assertTrue(plan.confirmationRequired());
            assertTrue(plan.androidEmulatorProposal().sdkPackages().contains("platform-tools"));
            assertTrue(plan.appiumCapabilities().containsKey("appium:appPackage"));
            assertTrue(plan.nextSteps().stream().anyMatch(step -> step.contains("confirmation token")));
        } finally {
            System.setProperty("user.home", originalHome);
        }
    }

    @Test
    void prepareSurfacesToolchainDiagnosticWarnings() {
        McpMobileInspectorRecordingService service = service();

        McpMobileInspectorPlan plan = service.prepare(
                "Android",
                "recordings/native.json",
                false,
                "",
                "com.example",
                ".MainActivity",
                "",
                "",
                "Pixel 8",
                "",
                "",
                0,
                "",
                "",
                "",
                0,
                0,
                false);

        assertFalse(plan.readyToStart());
        assertTrue(plan.warnings().stream().anyMatch(warning -> warning.contains("adb was not found")
                && warning.contains("platform-tools")));
        assertTrue(plan.warnings().stream().anyMatch(warning -> warning.contains("Android emulator")
                && warning.contains("sdkmanager")));
    }

    @Test
    void prepareBlocksIosInspectorOnNonMacHost() {
        McpMobileInspectorRecordingService service = service();

        McpMobileInspectorPlan plan = service.prepare(
                "iOS",
                "recordings/native-ios.json",
                false,
                "",
                "",
                "",
                "com.example.ios",
                "",
                "iPhone 15",
                "",
                "",
                0,
                "",
                "",
                "",
                0,
                0,
                false);

        assertFalse(plan.readyToStart());
        assertTrue(plan.warnings().stream().anyMatch(warning -> warning.contains("ios-host")
                && warning.contains("requires macOS")));
    }

    @Test
    void rejectsUnknownConfirmationToken() {
        McpMobileInspectorRecordingService service = service();

        assertThrows(IllegalArgumentException.class,
                () -> service.start("missing", "", false));
    }

    @Test
    void startPauseCheckpointAndStopRecordsCommandsProxiedToTheManagedRuntime() throws Exception {
        String originalHome = System.getProperty("user.home");
        System.setProperty("user.home", temp.toString());
        HttpServer backend = backend();
        try {
            int backendPort = backend.getAddress().getPort();
            ManagedRuntimeOwner owner = new ManagedRuntimeOwner(temp, URI.create("http://127.0.0.1:" + backendPort + "/"));
            McpMobileInspectorRecordingService service = service(owner);
            McpMobileInspectorPlan plan = androidPlan(service, true);

            assertTrue(plan.readyToStart());
            McpMobileInspectorRecordingStatus started = service.start(plan.confirmationToken(), "", false);

            assertTrue(started.active());
            assertFalse(started.paused());
            assertTrue(started.managedEmulator());
            assertEquals("emulator-5554", started.deviceId());
            assertTrue(started.appiumServerUrl().contains("127.0.0.1:" + backendPort));
            assertTrue(started.inspectorUrl().endsWith("/shaft-inspector"));
            assertTrue(started.warnings().stream().anyMatch(warning -> warning.contains(started.inspectorUrl())));
            assertTrue(started.codeBlocks().stream().anyMatch(block -> block.code().contains("com.example.app")));

            HttpClient client = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).build();
            HttpResponse<String> page = client.send(HttpRequest.newBuilder(URI.create(started.inspectorUrl())).GET().build(),
                    HttpResponse.BodyHandlers.ofString());
            assertEquals(200, page.statusCode());
            assertTrue(page.body().contains("--shaft-primary"));
            assertTrue(page.body().contains("platformName"));

            HttpResponse<String> rejected = client.send(HttpRequest.newBuilder(URI.create(started.inspectorUrl()))
                    .POST(HttpRequest.BodyPublishers.noBody()).build(), HttpResponse.BodyHandlers.ofString());
            assertEquals(405, rejected.statusCode());

            HttpResponse<String> paused = client.send(HttpRequest.newBuilder(URI.create(controlUrl(started.inspectorUrl())))
                    .header("Content-Type", "application/json")
                    .POST(HttpRequest.BodyPublishers.ofString("{\"action\":\"pause\"}")).build(),
                    HttpResponse.BodyHandlers.ofString());
            assertEquals(200, paused.statusCode());
            assertTrue(paused.body().replace(" ", "").contains("\"paused\":true"), paused.body());

            McpMobileInspectorRecordingStatus checkpoint = service.control("checkpoint", "Login");
            assertTrue(checkpoint.warnings().stream().anyMatch(warning -> warning.contains("Login")));
            McpMobileInspectorRecordingStatus resumed = service.control("play", "");
            assertFalse(resumed.paused());

            URI proxyRoot = URI.create(started.inspectorUrl().replace("/shaft-inspector", ""));
            post(client, proxyRoot.resolve("/session/abc/element"),
                    "{\"using\":\"id\",\"value\":\"com.example:id/user\"}");
            post(client, proxyRoot.resolve("/session/abc/element/el-1/click"), "{}");

            McpMobileInspectorRecordingStatus afterCommands = service.status();
            assertTrue(afterCommands.actionCount() >= 1, afterCommands.toString());

            McpMobileInspectorRecordingStatus stopped = service.stop(false);
            assertFalse(stopped.active());
            assertFalse(service.status().active());
            assertThrows(IOException.class, () -> client.send(
                    HttpRequest.newBuilder(URI.create(started.inspectorUrl())).GET().build(),
                    HttpResponse.BodyHandlers.ofString()));
            assertTrue(owner.closed);
        } finally {
            backend.stop(0);
            System.setProperty("user.home", originalHome);
        }
    }

    @Test
    void failedRuntimeStartLeavesNoActiveSessionAndASecondStartIsRejectedUntilStop() throws Exception {
        String originalHome = System.getProperty("user.home");
        System.setProperty("user.home", temp.toString());
        try {
            ManagedRuntimeOwner owner = new ManagedRuntimeOwner(temp, URI.create("http://127.0.0.1:9/"));
            owner.failure.set(new IOException("runtime socket closed"));
            McpMobileInspectorRecordingService service = service(owner);
            McpMobileInspectorPlan plan = androidPlan(service, true);

            IllegalStateException failure = assertThrows(IllegalStateException.class,
                    () -> service.start(plan.confirmationToken(), "", false));
            assertTrue(failure.getMessage().contains("could not be started"));
            assertFalse(service.status().active());

            owner.failure.set(null);
            McpMobileInspectorRecordingStatus started = service.start(plan.confirmationToken(), "", false);
            assertTrue(started.active());
            assertThrows(IllegalStateException.class,
                    () -> service.start(plan.confirmationToken(), "", false));
            assertThrows(IllegalArgumentException.class, () -> service.control("rewind", ""));
            McpMobileInspectorRecordingStatus discarded = service.control("discard", "");
            assertFalse(discarded.active());
            assertFalse(service.control("rewind", "").active());
        } finally {
            System.setProperty("user.home", originalHome);
        }
    }

    @Test
    void missingManagedEndpointFailsStartAndReleasesTheRuntime() throws Exception {
        String originalHome = System.getProperty("user.home");
        System.setProperty("user.home", temp.toString());
        try {
            ManagedRuntimeOwner owner = new ManagedRuntimeOwner(temp, null);
            McpMobileInspectorRecordingService service = service(owner);
            McpMobileInspectorPlan plan = androidPlan(service, true);

            IllegalStateException failure = assertThrows(IllegalStateException.class,
                    () -> service.start(plan.confirmationToken(), "Pixel_Override", false));
            assertTrue(failure.getMessage().contains("did not publish"));
            assertFalse(service.status().active());
            assertTrue(owner.closed);
        } finally {
            System.setProperty("user.home", originalHome);
        }
    }

    @Test
    void proxyForwardsInspectorCommandsAndFailsClosedWhenTheBackendIsDown() throws Exception {
        HttpServer backend = backend();
        AtomicReference<String> seenQuery = new AtomicReference<>();
        AtomicReference<String> seenMarker = new AtomicReference<>();
        backend.removeContext("/");
        backend.createContext("/", exchange -> {
            seenQuery.set(exchange.getRequestURI().getRawQuery());
            seenMarker.set(exchange.getRequestHeaders().getFirst("X-Shaft-Marker"));
            byte[] body = "{\"value\":{}}".getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().set("Content-Type", "application/json");
            exchange.getResponseHeaders().set("X-Frame-Options", "DENY");
            exchange.getResponseHeaders().set("X-Backend", "seen");
            exchange.sendResponseHeaders(200, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        int closedPort;
        try (java.net.ServerSocket socket = new java.net.ServerSocket(
                0, 1, java.net.InetAddress.getByName("127.0.0.1"))) {
            closedPort = socket.getLocalPort();
        }
        McpMobileRecordingService recorder = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        recorder.start(temp.resolve("recording.json").toString(), "mobile-inspector-android", false);
        McpAppiumCommandRecorder commandRecorder = new McpAppiumCommandRecorder(recorder, () -> false);
        McpMobileInspectorRecordingStatus active = new McpMobileInspectorRecordingStatus(
                true, false, "Android", "emulator-5554", "Pixel_8", true,
                temp.resolve("recording.json"), "", "", 0, List.of(), List.of());
        McpAppiumInspectorProxy proxy = new McpAppiumInspectorProxy(
                URI.create("http://127.0.0.1:" + backend.getAddress().getPort() + "/"),
                commandRecorder,
                (action, checkpoint) -> active,
                "{\"platformName\":\"Android\"}");
        McpAppiumInspectorProxy broken = new McpAppiumInspectorProxy(
                URI.create("http://127.0.0.1:" + closedPort + "/"),
                commandRecorder,
                (action, checkpoint) -> active,
                "{}");
        HttpClient client = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).build();
        try {
            String inspector = proxy.start(0);
            HttpResponse<String> status = client.send(
                    HttpRequest.newBuilder(URI.create(inspector + "/status")).GET().build(),
                    HttpResponse.BodyHandlers.ofString());
            assertEquals(200, status.statusCode());
            assertTrue(status.body().replace(" ", "").contains("\"active\":true"), status.body());

            HttpResponse<String> rejected = client.send(
                    HttpRequest.newBuilder(URI.create(inspector + "/control")).GET().build(),
                    HttpResponse.BodyHandlers.ofString());
            assertEquals(405, rejected.statusCode());

            URI command = URI.create(inspector.replace("/shaft-inspector", "")
                    + "/session/abc/element?strategy=id");
            HttpResponse<String> forwarded = client.send(HttpRequest.newBuilder(command)
                    .header("Content-Type", "application/json")
                    .header("X-Shaft-Marker", "keep")
                    .header("Accept-Encoding", "gzip")
                    .POST(HttpRequest.BodyPublishers.ofString("{\"using\":\"id\",\"value\":\"user\"}"))
                    .build(), HttpResponse.BodyHandlers.ofString());
            assertEquals(200, forwarded.statusCode(), forwarded.body());
            assertEquals("strategy=id", seenQuery.get());
            assertEquals("keep", seenMarker.get());
            assertEquals("seen", forwarded.headers().firstValue("X-Backend").orElse(""));
            assertTrue(forwarded.headers().firstValue("X-Frame-Options").isEmpty());

            String brokenInspector = broken.start(0);
            HttpResponse<String> failed = client.send(HttpRequest.newBuilder(
                    URI.create(brokenInspector.replace("/shaft-inspector", "") + "/session/abc/source"))
                    .GET().build(), HttpResponse.BodyHandlers.ofString());
            assertEquals(502, failed.statusCode());
            assertTrue(failed.body().contains("Appium proxy failed"), failed.body());

            HttpResponse<String> stopped = client.send(HttpRequest.newBuilder(URI.create(inspector + "/control"))
                    .header("Content-Type", "application/json")
                    .POST(HttpRequest.BodyPublishers.ofString("{\"action\":\"stop\"}")).build(),
                    HttpResponse.BodyHandlers.ofString());
            assertEquals(200, stopped.statusCode());
            boolean closed = false;
            for (int attempt = 0; attempt < 20; attempt++) {
                try {
                    client.send(HttpRequest.newBuilder(URI.create(inspector)).GET().build(),
                            HttpResponse.BodyHandlers.ofString());
                    Thread.sleep(50);
                } catch (IOException exception) {
                    closed = true;
                    break;
                }
            }
            assertTrue(closed);
        } finally {
            broken.close();
            proxy.close();
            backend.stop(0);
        }
    }

    @Test
    void appiumInspectorWrapperUsesShaftUiTheme() throws Exception {
        McpMobileRecordingService recorder = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        McpAppiumCommandRecorder commandRecorder = new McpAppiumCommandRecorder(recorder, () -> false);
        McpAppiumInspectorProxy proxy = new McpAppiumInspectorProxy(
                URI.create("http://127.0.0.1:4723/"),
                commandRecorder,
                (action, checkpoint) -> new McpMobileInspectorRecordingStatus(
                        true, false, "Android", "", "", false, temp.resolve("recording.json"),
                        "http://127.0.0.1:3000", "http://127.0.0.1:4723", 3, List.of(), List.of()),
                "{\"platformName\":\"Android\"}");
        Method wrapperHtml = McpAppiumInspectorProxy.class.getDeclaredMethod("wrapperHtml");
        wrapperHtml.setAccessible(true);

        String html = (String) wrapperHtml.invoke(proxy);

        assertTrue(html.contains("--shaft-primary"), html);
        assertTrue(html.contains("status-chip"), html);
        assertTrue(html.contains("Recorder ready"), html);
    }

    private McpMobileInspectorRecordingService service() {
        return service(new ManagedRuntimeOwner(temp, URI.create("http://127.0.0.1:9/")));
    }

    private McpMobileInspectorRecordingService service(McpMobileToolchainService.AndroidSetupOwner owner) {
        McpMobileRecordingService recorder = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        McpMobileToolchainService toolchain = new McpMobileToolchainService(new NoopRunner(),
                Map.of("PATH", "", "ANDROID_AVD_HOME", temp.resolve("avd").toString()),
                temp.resolve("tools"), "Windows 11", "amd64", owner);
        return new McpMobileInspectorRecordingService(McpWorkspacePolicy.of(temp), recorder, toolchain);
    }

    private McpMobileInspectorPlan androidPlan(McpMobileInspectorRecordingService service, boolean provision) {
        return service.prepare(
                "Android",
                "recordings/native.json",
                false,
                "",
                "com.example.app",
                ".MainActivity",
                "",
                "",
                "Pixel 8",
                "",
                "",
                36,
                "",
                "",
                "",
                0,
                0,
                provision);
    }

    private static HttpServer backend() throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/", exchange -> {
            byte[] body = "{\"value\":{\"element-6066-11e4-a52e-4f735466cecf\":\"el-1\"}}"
                    .getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().set("Content-Type", "application/json");
            exchange.getResponseHeaders().set("X-Frame-Options", "DENY");
            exchange.sendResponseHeaders(200, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        server.start();
        return server;
    }

    private static void post(HttpClient client, URI uri, String body) throws Exception {
        HttpResponse<String> response = client.send(HttpRequest.newBuilder(uri)
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(body)).build(), HttpResponse.BodyHandlers.ofString());
        assertEquals(200, response.statusCode(), response.body());
    }

    private static String controlUrl(String inspectorUrl) {
        return inspectorUrl.replace("/shaft-inspector", "/shaft-inspector/control");
    }

    private static final class ManagedRuntimeOwner implements McpMobileToolchainService.AndroidSetupOwner {
        private final Path root;
        private final URI endpoint;
        private final AtomicReference<IOException> failure = new AtomicReference<>();
        private volatile boolean closed;

        private ManagedRuntimeOwner(Path root, URI endpoint) {
            this.root = root;
            this.endpoint = endpoint;
        }

        @Override
        public Path sdkRoot(int apiLevel, String abi) {
            return root.resolve("sdk");
        }

        @Override
        public Path avdHome() {
            return root.resolve("avd-home");
        }

        @Override
        public void install(McpAndroidEmulatorProposal proposal) {
        }

        @Override
        public ManagedEnvironment start(McpAndroidEmulatorProposal proposal) throws IOException {
            IOException pending = failure.get();
            if (pending != null) {
                throw pending;
            }
            return new ManagedEnvironment(
                    SetupProfile.MOBILE_ANDROID,
                    new SetupReceipt("test-digest", Instant.EPOCH, List.of()),
                    Optional.ofNullable(endpoint),
                    Map.of("ANDROID_SERIAL", "emulator-5554"),
                    () -> closed = true);
        }
    }

    private static final class NoopRunner implements McpProcessRunner {
        @Override
        public ProcessResult run(
                List<String> command,
                Path workingDirectory,
                Map<String, String> environment,
                Duration timeout) {
            return new ProcessResult(0, "", "", false);
        }

        @Override
        public Process start(List<String> command, Path workingDirectory, Map<String, String> environment) {
            throw new UnsupportedOperationException("No process starts in unit tests.");
        }
    }
}
