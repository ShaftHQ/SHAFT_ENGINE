package com.shaft.mcp;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.awt.Desktop;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Orchestrates wrapped Appium Inspector recording sessions.
 */
final class McpMobileInspectorRecordingService {
    private static final String DEFAULT_APPIUM_SERVER = "http://127.0.0.1:4723";

    private final ObjectMapper mapper = new ObjectMapper();
    private final McpWorkspacePolicy workspacePolicy;
    private final McpMobileRecordingService recorder;
    private final McpMobileToolchainService toolchain;
    private final Map<String, McpMobileInspectorPlan> preparedPlans = new ConcurrentHashMap<>();
    private Session activeSession;

    McpMobileInspectorRecordingService(McpWorkspacePolicy workspacePolicy, McpMobileRecordingService recorder) {
        this(workspacePolicy, recorder, new McpMobileToolchainService());
    }

    McpMobileInspectorRecordingService(
            McpWorkspacePolicy workspacePolicy,
            McpMobileRecordingService recorder,
            McpMobileToolchainService toolchain) {
        this.workspacePolicy = workspacePolicy;
        this.recorder = recorder;
        this.toolchain = toolchain;
    }

    synchronized McpMobileToolchainStatus toolchainStatus(String platformName) {
        return toolchain.status(platformName);
    }

    synchronized McpMobileInspectorPlan prepare(
            String platformName,
            String outputPath,
            boolean includeSensitiveValues,
            String app,
            String appPackage,
            String appActivity,
            String bundleId,
            String udid,
            String deviceName,
            String platformVersion,
            String selectedAndroidAvdName,
            int androidApiLevel,
            String androidDeviceProfile,
            String androidImageTag,
            String androidAbi,
            int androidRamMb,
            int androidCores,
            boolean provisionAndroidEmulator) {
        String platform = platform(platformName);
        String automation = "Android".equals(platform) ? "UiAutomator2" : "XCUITest";
        McpMobileToolchainStatus status = toolchain.status(platform);
        List<String> warnings = new ArrayList<>(status.warnings());
        warnings.addAll(McpMobileCode.nativeWarnings(platform, app, appPackage, appActivity, bundleId));
        List<String> nextSteps = new ArrayList<>();

        boolean appReady = appDetailsReady(platform, app, appPackage, appActivity, bundleId);
        String deviceId = text(udid);
        if (deviceId.isBlank()) {
            deviceId = readyAndroidDevice(status).map(McpMobileDevice::id).orElse("");
        }
        String plannedDeviceId = deviceId;
        boolean realDeviceAvailable = !plannedDeviceId.isBlank()
                && status.androidDevices().stream().anyMatch(device -> device.id().equals(plannedDeviceId)
                && !device.emulator()
                && "device".equals(device.state()));
        String selectedAvd = text(selectedAndroidAvdName);
        boolean willProvision = false;
        McpAndroidEmulatorProposal proposal = null;
        boolean readyToStart = appReady;

        if ("Android".equals(platform)) {
            boolean androidReadyDevice = !plannedDeviceId.isBlank()
                    && status.androidDevices().stream().anyMatch(device -> device.id().equals(plannedDeviceId)
                    && "device".equals(device.state()));
            if (!androidReadyDevice) {
                if (!selectedAvd.isBlank()) {
                    nextSteps.add("Confirm this plan to start cached Android emulator `" + selectedAvd + "`.");
                } else if (!status.cachedAndroidEmulators().isEmpty()) {
                    readyToStart = false;
                    nextSteps.add("Choose one cached Android emulator and call prepare again with selectedAndroidAvdName.");
                    warnings.add("No connected Android devices were found. Cached emulators are available.");
                } else if (provisionAndroidEmulator) {
                    willProvision = true;
                    proposal = toolchain.defaultAndroidProposal("", androidApiLevel, androidDeviceProfile,
                            androidImageTag, androidAbi, androidRamMb, androidCores);
                    nextSteps.add("Review the Android emulator proposal, then call start with this confirmation token.");
                } else {
                    readyToStart = false;
                    warnings.add("No connected Android devices were found. Enable provisionAndroidEmulator or choose a cached AVD.");
                }
            }
            if (status.adbAvailable()) {
                nextSteps.add("Connected Android devices were checked with `adb devices -l`.");
            } else {
                nextSteps.add("adb is missing; start can install Android platform-tools under the SHAFT MCP cache.");
            }
        } else {
            if (!System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("mac")) {
                warnings.add("iOS Appium recording requires macOS with Xcode command-line tools and a reachable iOS target.");
            }
            if (text(udid).isBlank() && text(deviceName).isBlank()) {
                warnings.add("Provide udid or deviceName for iOS Inspector sessions.");
            }
        }
        if (!appReady) {
            readyToStart = false;
        }

        String token = toolchain.newConfirmationToken();
        String plannedOutput = outputPath == null || outputPath.isBlank()
                ? "recordings/mobile-inspector-" + platform.toLowerCase(Locale.ROOT) + ".json"
                : outputPath;
        Map<String, Object> capabilities = capabilities(platform, automation, app, appPackage, appActivity, bundleId,
                deviceId, deviceName, platformVersion, selectedAvd);
        McpMobileInspectorPlan plan = new McpMobileInspectorPlan(
                token,
                platform,
                readyToStart,
                true,
                willProvision,
                realDeviceAvailable,
                deviceId,
                selectedAvd,
                plannedOutput,
                includeSensitiveValues,
                capabilities,
                status,
                proposal,
                List.of(McpMobileCode.nativeSetupBlock(platform, deviceName, DEFAULT_APPIUM_SERVER, automation,
                        platformVersion, deviceId, app, appPackage, appActivity, bundleId)),
                nextSteps,
                warnings);
        preparedPlans.put(token, plan);
        return plan;
    }

    synchronized McpMobileInspectorRecordingStatus start(String confirmationToken, String selectedAndroidAvdName,
            boolean openInspector) {
        if (activeSession != null) {
            throw new IllegalStateException("A mobile Inspector recording session is already active.");
        }
        McpMobileInspectorPlan plan = preparedPlans.get(text(confirmationToken));
        if (plan == null) {
            throw new IllegalArgumentException("Unknown mobile Inspector confirmation token. Call prepare first.");
        }
        if (!plan.readyToStart()) {
            throw new IllegalStateException("Mobile Inspector plan is not ready to start: "
                    + String.join(" ", plan.warnings()));
        }

        List<String> warnings = new ArrayList<>(plan.warnings());
        Process emulatorProcess = null;
        boolean managedEmulator = false;
        String androidAvdName = text(selectedAndroidAvdName).isBlank()
                ? plan.selectedAndroidAvdName()
                : text(selectedAndroidAvdName);
        String deviceId = plan.selectedDeviceId();
        try {
            toolchain.ensureAppium(plan.platformName());
            if ("Android".equals(plan.platformName()) && deviceId.isBlank()) {
                if (plan.willProvisionAndroidEmulator()) {
                    toolchain.ensureAndroidEmulator(plan.androidEmulatorProposal());
                    androidAvdName = plan.androidEmulatorProposal().avdName();
                } else if (androidAvdName.isBlank()) {
                    throw new IllegalArgumentException(
                            "selectedAndroidAvdName is required when no real Android device is connected.");
                }
                emulatorProcess = toolchain.startAndroidEmulator(androidAvdName, plan.androidEmulatorProposal());
                managedEmulator = true;
                if (!toolchain.waitForAndroidDevice(Duration.ofMinutes(3))) {
                    throw new IllegalStateException("Android emulator did not become ready within 3 minutes.");
                }
                McpMobileToolchainStatus status = toolchain.status("Android");
                deviceId = status.androidDevices().stream()
                        .filter(device -> device.emulator() && "device".equals(device.state()))
                        .map(McpMobileDevice::id)
                        .findFirst()
                        .orElse("");
            }

            int appiumPort = freePort();
            recorder.start(plan.outputPath(), "mobile-inspector-" + plan.platformName().toLowerCase(Locale.ROOT),
                    plan.includeSensitiveValues());
            Process appiumProcess = toolchain.startAppiumServer(appiumPort);
            URI backend = URI.create("http://127.0.0.1:" + appiumPort);
            waitForAppium(backend, warnings);
            Session session = new Session(
                    plan,
                    appiumProcess,
                    emulatorProcess,
                    managedEmulator,
                    deviceId,
                    androidAvdName,
                    backend.toString(),
                    List.of(setupBlock(plan, backend.toString(), deviceId)));
            activeSession = session;
            McpAppiumCommandRecorder commandRecorder = new McpAppiumCommandRecorder(recorder, () -> session.paused);
            session.proxy = new McpAppiumInspectorProxy(backend, commandRecorder, this::controlFromProxy,
                    capabilitiesJson(plan));
            session.inspectorUrl = session.proxy.start(freePort());
            if (openInspector) {
                openBrowser(session.inspectorUrl, warnings);
            }
            warnings.add("Set Appium Inspector's remote server URL to " + session.inspectorUrl
                    + " if the embedded Inspector asks for a server endpoint.");
            return status(session, warnings, session.setupBlocks);
        } catch (RuntimeException exception) {
            cleanupFailedStart(emulatorProcess, managedEmulator, deviceId);
            activeSession = null;
            throw exception;
        }
    }

    synchronized McpMobileInspectorRecordingStatus status() {
        if (activeSession == null) {
            McpMobileRecordingStatus recordingStatus = recorder.status();
            return new McpMobileInspectorRecordingStatus(false, false, "", "", "", false,
                    recordingStatus.outputPath(), "", "", recordingStatus.actionCount(), List.of(),
                    recordingStatus.warnings());
        }
        return status(activeSession, List.of(), activeSession.setupBlocks);
    }

    synchronized McpMobileInspectorRecordingStatus control(String action, String checkpointName) {
        return control(action, checkpointName, true);
    }

    synchronized McpMobileInspectorRecordingStatus stop(boolean discard) {
        return stop(discard, true);
    }

    private McpMobileInspectorRecordingStatus controlFromProxy(String action, String checkpointName) {
        return control(action, checkpointName, false);
    }

    private synchronized McpMobileInspectorRecordingStatus control(String action, String checkpointName,
            boolean closeProxy) {
        if (activeSession == null) {
            return status();
        }
        String normalized = text(action).toLowerCase(Locale.ROOT);
        return switch (normalized) {
            case "", "status" -> status(activeSession, List.of(), activeSession.setupBlocks);
            case "pause" -> {
                activeSession.paused = true;
                yield status(activeSession, List.of("Recording paused."), activeSession.setupBlocks);
            }
            case "resume", "play" -> {
                activeSession.paused = false;
                yield status(activeSession, List.of("Recording resumed."), activeSession.setupBlocks);
            }
            case "checkpoint" -> {
                String checkpoint = text(checkpointName).isBlank() ? "Checkpoint" : text(checkpointName);
                recorder.recordWarning("Checkpoint: " + checkpoint);
                yield status(activeSession, List.of("Checkpoint recorded: " + checkpoint), activeSession.setupBlocks);
            }
            case "stop" -> stop(false, closeProxy);
            case "discard" -> stop(true, closeProxy);
            default -> throw new IllegalArgumentException(
                    "Unsupported Inspector recording control action: " + action);
        };
    }

    private synchronized McpMobileInspectorRecordingStatus stop(boolean discard, boolean closeProxy) {
        if (activeSession == null) {
            return status();
        }
        Session session = activeSession;
        McpMobileRecordingStatus stopped = recorder.stop(discard);
        List<String> warnings = new ArrayList<>(stopped.warnings());
        List<McpCodeBlock> blocks = new ArrayList<>(session.setupBlocks);
        if (!discard && stopped.outputPath() != null) {
            McpMobileReplayResult replay = recorder.codeBlocks(stopped.outputPath().toString(), "driver");
            blocks.addAll(replay.codeBlocks());
            warnings.addAll(replay.warnings());
            warnings.add("Session-managed Appium and emulator processes were stopped. Update executionAddress "
                    + "before replaying against a different Appium server.");
        }
        destroy(session.appiumProcess);
        if (session.managedEmulator) {
            toolchain.stopAndroidEmulator(session.deviceId);
            destroy(session.emulatorProcess);
        }
        if (closeProxy && session.proxy != null) {
            session.proxy.close();
        }
        activeSession = null;
        return new McpMobileInspectorRecordingStatus(
                false,
                false,
                session.plan.platformName(),
                session.deviceId,
                session.androidAvdName,
                session.managedEmulator,
                stopped.outputPath(),
                session.inspectorUrl,
                session.appiumServerUrl,
                stopped.actionCount(),
                blocks,
                warnings);
    }

    private McpMobileInspectorRecordingStatus status(Session session, List<String> extraWarnings,
            List<McpCodeBlock> blocks) {
        McpMobileRecordingStatus recordingStatus = recorder.status();
        List<String> warnings = new ArrayList<>(recordingStatus.warnings());
        warnings.addAll(extraWarnings);
        return new McpMobileInspectorRecordingStatus(
                true,
                session.paused,
                session.plan.platformName(),
                session.deviceId,
                session.androidAvdName,
                session.managedEmulator,
                recordingStatus.outputPath(),
                session.inspectorUrl,
                session.appiumServerUrl,
                recordingStatus.actionCount(),
                blocks,
                warnings);
    }

    private McpCodeBlock setupBlock(McpMobileInspectorPlan plan, String appiumServerUrl, String deviceId) {
        String automation = "Android".equals(plan.platformName()) ? "UiAutomator2" : "XCUITest";
        String deviceName = value(plan.appiumCapabilities(), "appium:deviceName");
        String platformVersion = value(plan.appiumCapabilities(), "appium:platformVersion");
        String app = value(plan.appiumCapabilities(), "appium:app");
        String appPackage = value(plan.appiumCapabilities(), "appium:appPackage");
        String appActivity = value(plan.appiumCapabilities(), "appium:appActivity");
        String bundleId = value(plan.appiumCapabilities(), "appium:bundleId");
        return McpMobileCode.nativeSetupBlock(plan.platformName(), deviceName, appiumServerUrl, automation,
                platformVersion, deviceId, app, appPackage, appActivity, bundleId);
    }

    private Map<String, Object> capabilities(
            String platform,
            String automation,
            String app,
            String appPackage,
            String appActivity,
            String bundleId,
            String udid,
            String deviceName,
            String platformVersion,
            String selectedAvd) {
        Map<String, Object> caps = new LinkedHashMap<>();
        caps.put("platformName", platform);
        caps.put("appium:automationName", automation);
        putIfPresent(caps, "appium:deviceName", defaultText(deviceName, selectedAvd));
        putIfPresent(caps, "appium:platformVersion", platformVersion);
        putIfPresent(caps, "appium:udid", udid);
        putIfPresent(caps, "appium:app", app);
        putIfPresent(caps, "appium:appPackage", appPackage);
        putIfPresent(caps, "appium:appActivity", appActivity);
        putIfPresent(caps, "appium:bundleId", bundleId);
        caps.put("appium:noReset", true);
        return Map.copyOf(caps);
    }

    private String capabilitiesJson(McpMobileInspectorPlan plan) {
        try {
            return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(plan.appiumCapabilities());
        } catch (JsonProcessingException exception) {
            return "{}";
        }
    }

    private java.util.Optional<McpMobileDevice> readyAndroidDevice(McpMobileToolchainStatus status) {
        return status.androidDevices().stream()
                .filter(device -> "device".equals(device.state()))
                .findFirst();
    }

    private boolean appDetailsReady(String platform, String app, String appPackage, String appActivity,
            String bundleId) {
        if (!text(app).isBlank()) {
            return true;
        }
        if ("Android".equals(platform)) {
            return !text(appPackage).isBlank() && !text(appActivity).isBlank();
        }
        return !text(bundleId).isBlank();
    }

    private String platform(String platformName) {
        String platform = text(platformName).toLowerCase(Locale.ROOT);
        return switch (platform) {
            case "", "android" -> "Android";
            case "ios" -> "iOS";
            default -> throw new IllegalArgumentException("platformName must be Android or iOS.");
        };
    }

    private void cleanupFailedStart(Process emulatorProcess, boolean managedEmulator, String deviceId) {
        if (managedEmulator) {
            toolchain.stopAndroidEmulator(deviceId);
            destroy(emulatorProcess);
        }
        try {
            if (recorder.status().active()) {
                recorder.stop(true);
            }
        } catch (RuntimeException ignored) {
            // Failed start cleanup is best effort.
        }
    }

    private void openBrowser(String url, List<String> warnings) {
        try {
            if (!Desktop.isDesktopSupported()) {
                warnings.add("Desktop browser launch is not supported in this environment.");
                return;
            }
            Desktop.getDesktop().browse(URI.create(url));
        } catch (IOException | RuntimeException exception) {
            warnings.add("Inspector URL could not be opened automatically: " + exception.getMessage());
        }
    }

    private void waitForAppium(URI backend, List<String> warnings) {
        HttpClient client = HttpClient.newBuilder()
                .connectTimeout(Duration.ofSeconds(2))
                .build();
        HttpRequest request = HttpRequest.newBuilder(backend.resolve("/status"))
                .timeout(Duration.ofSeconds(2))
                .GET()
                .build();
        long deadline = System.nanoTime() + Duration.ofSeconds(30).toNanos();
        while (System.nanoTime() < deadline) {
            try {
                HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
                if (response.statusCode() / 100 == 2) {
                    return;
                }
            } catch (IOException ignored) {
                // Appium is still starting.
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
                warnings.add("Interrupted while waiting for Appium server readiness.");
                return;
            }
            sleep(Duration.ofMillis(500));
        }
        warnings.add("Appium server did not answer /status within 30 seconds; the Inspector may need a browser refresh.");
    }

    private int freePort() {
        try (ServerSocket socket = new ServerSocket(0)) {
            socket.setReuseAddress(true);
            return socket.getLocalPort();
        } catch (IOException exception) {
            throw new IllegalStateException("Free local port could not be reserved.", exception);
        }
    }

    private void destroy(Process process) {
        if (process != null && process.isAlive()) {
            process.destroy();
            try {
                if (!process.waitFor(5, java.util.concurrent.TimeUnit.SECONDS)) {
                    process.destroyForcibly();
                }
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
                process.destroyForcibly();
            }
        }
    }

    private void sleep(Duration duration) {
        try {
            Thread.sleep(duration.toMillis());
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
        }
    }

    private void putIfPresent(Map<String, Object> map, String key, String value) {
        if (!text(value).isBlank()) {
            map.put(key, text(value));
        }
    }

    private String value(Map<String, Object> map, String key) {
        Object value = map.get(key);
        return value == null ? "" : value.toString();
    }

    private static String defaultText(String value, String fallback) {
        String text = text(value);
        return text.isBlank() ? text(fallback) : text;
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }

    private static final class Session {
        private final McpMobileInspectorPlan plan;
        private final Process appiumProcess;
        private final Process emulatorProcess;
        private final boolean managedEmulator;
        private final String deviceId;
        private final String androidAvdName;
        private final String appiumServerUrl;
        private final List<McpCodeBlock> setupBlocks;
        private volatile boolean paused;
        private McpAppiumInspectorProxy proxy;
        private String inspectorUrl = "";

        private Session(
                McpMobileInspectorPlan plan,
                Process appiumProcess,
                Process emulatorProcess,
                boolean managedEmulator,
                String deviceId,
                String androidAvdName,
                String appiumServerUrl,
                List<McpCodeBlock> setupBlocks) {
            this.plan = plan;
            this.appiumProcess = appiumProcess;
            this.emulatorProcess = emulatorProcess;
            this.managedEmulator = managedEmulator;
            this.deviceId = text(deviceId);
            this.androidAvdName = text(androidAvdName);
            this.appiumServerUrl = text(appiumServerUrl);
            this.setupBlocks = List.copyOf(setupBlocks);
        }
    }
}
