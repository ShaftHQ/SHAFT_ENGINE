package com.shaft.infrastructure;

import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/** Real filesystem/network/process implementation behind the Android setup transaction. */
final class DefaultAndroidToolchainOperations implements AndroidToolchainOperations {
    private static final JsonMapper JSON = JsonMapper.builder().build();
    private final ShaftCachePaths paths;
    private final SetupPlatform platform;
    private final SetupArchitecture architecture;
    private final AndroidSetupRequest request;
    private final ReportingSetupService.ArtifactFetcher fetcher;
    private final AndroidCommandRunner runner;
    private final ReportingSetupService nodeService;
    private final boolean offline;

    DefaultAndroidToolchainOperations(ShaftCachePaths paths, SetupPlatform platform,
                                      SetupArchitecture architecture, AndroidSetupRequest request,
                                      boolean offline) {
        this(paths, platform, architecture, request,
                action -> new VerifiedArtifactStore(paths.downloads()).fetch(action, offline),
                AndroidCommandRunner.system(paths, platform, architecture), offline);
    }

    DefaultAndroidToolchainOperations(ShaftCachePaths paths, SetupPlatform platform,
                                      SetupArchitecture architecture, AndroidSetupRequest request,
                                      ReportingSetupService.ArtifactFetcher fetcher,
                                      AndroidCommandRunner runner, boolean offline) {
        this.paths = paths;
        this.platform = platform;
        this.architecture = architecture;
        this.request = request.resolve(architecture);
        this.fetcher = fetcher;
        this.runner = runner;
        this.offline = offline;
        this.nodeService = new ReportingSetupService(paths, platform, architecture, fetcher,
                (command, log, timeout) -> runner.run(command, paths.cacheRoot(), Map.of(), Set.of(), null,
                        log, timeout), offline);
    }

    @Override
    public void preflight(List<SetupAction> actions, boolean requireOffline) throws IOException {
        requireSafePaths();
        if (!requireOffline) return;
        if (!actions.stream().allMatch(this::structurallyInstalled)) {
            throw new IOException("Offline Android setup requires a complete verified installation; "
                    + "cold or partial setup cannot run sdkmanager or npm without network access.");
        }
        boolean ready = actions.stream().allMatch(action -> status(action).readiness() == SetupReadiness.READY);
        if (!ready) {
            throw new IOException("Offline Android setup requires a complete verified installation; "
                    + "cold or partial setup cannot run sdkmanager or npm without network access.");
        }
    }

    private boolean structurallyInstalled(SetupAction action) {
        try {
            return switch (action.target()) {
                case NODE -> Files.isRegularFile(nodeExecutable(), LinkOption.NOFOLLOW_LINKS);
                case APPIUM_SERVER -> appiumBundleStructureReady();
                case APPIUM_INSPECTOR_PLUGIN -> Files.isRegularFile(
                        appiumRoot().resolve("node_modules/appium-inspector-plugin/package.json"),
                        LinkOption.NOFOLLOW_LINKS);
                case APPIUM_UIAUTOMATOR2_DRIVER -> Files.isRegularFile(
                        appiumRoot().resolve("node_modules/appium-uiautomator2-driver/package.json"),
                        LinkOption.NOFOLLOW_LINKS);
                case ANDROID_SDK -> sdkFilesReady(sdkRoot());
                case ANDROID_EMULATOR -> Files.isRegularFile(avdRoot().resolve("config.ini"),
                        LinkOption.NOFOLLOW_LINKS) && Files.isRegularFile(
                        avdRoot().resolve("shaft-request.properties"), LinkOption.NOFOLLOW_LINKS);
                default -> false;
            };
        } catch (IOException failure) {
            return false;
        }
    }

    @Override
    public void install(SetupAction action) throws IOException {
        switch (action.target()) {
            case NODE -> nodeService.installNodeAction(action);
            case APPIUM_SERVER -> installAppiumBundle(action);
            case APPIUM_INSPECTOR_PLUGIN, APPIUM_UIAUTOMATOR2_DRIVER -> requireReady(action);
            case ANDROID_SDK -> installAndroidSdk(action);
            case ANDROID_EMULATOR -> installAvd(action);
            default -> throw new IOException("Android provider cannot install " + action.target());
        }
    }

    @Override
    public SetupStatus status(SetupAction action) {
        try {
            return switch (action.target()) {
                case NODE -> nodeService.nodeStatus();
                case APPIUM_SERVER -> appiumStatus(action);
                case APPIUM_INSPECTOR_PLUGIN -> extensionStatus(action, "plugin", "inspector",
                        "appium-inspector-plugin");
                case APPIUM_UIAUTOMATOR2_DRIVER -> extensionStatus(action, "driver", "uiautomator2",
                        "appium-uiautomator2-driver");
                case ANDROID_SDK -> sdkStatus(action);
                case ANDROID_EMULATOR -> avdStatus(action);
                default -> new SetupStatus(action.target(), SetupReadiness.DEGRADED, "",
                        "Unexpected target in Android plan.");
            };
        } catch (IOException failure) {
            return new SetupStatus(action.target(), SetupReadiness.DEGRADED, "", failure.getMessage());
        }
    }

    private void installAppiumBundle(SetupAction serverAction) throws IOException {
        if (appiumBundleReady()) return;
        if (nodeService.nodeStatus().readiness() != SetupReadiness.READY) {
            throw new IOException("Portable Node must be ready before Appium installation.");
        }
        SetupPlan manifest = AndroidSetupPlanner.plan(platform, architecture, SetupMode.MANAGED, request);
        List<SetupAction> packages = manifest.actions().subList(1, 4);
        List<Path> archives = new ArrayList<>();
        for (SetupAction action : packages) archives.add(fetcher.fetch(action));
        Path destination = appiumRoot();
        Files.createDirectories(destination.getParent());
        Path staging = Files.createTempDirectory(destination.getParent(), "appium.staging-");
        try {
            copyAppiumManifest(staging, serverAction.dependencyLockChecksum());
            Path log = logFile();
            Files.createDirectories(log.getParent());
            for (Path archive : archives) {
                List<String> cache = new ArrayList<>(List.of(nodeExecutable().toString(), npmCli().toString(),
                        "cache", "add", archive.toString()));
                if (offline) cache.add("--offline");
                requireSuccess(run(cache, staging, null, log, Duration.ofMinutes(2)),
                        "Appium npm cache preparation failed");
            }
            List<String> install = new ArrayList<>(List.of(nodeExecutable().toString(), npmCli().toString(), "ci",
                    "--prefix", staging.toString(), "--ignore-scripts", "--no-audit", "--no-fund"));
            if (offline) install.add("--offline");
            requireSuccess(run(install, staging, null, log, Duration.ofMinutes(10)),
                    "Appium npm installation failed");
            requireExactPackage(staging.resolve("node_modules/appium/package.json"),
                    AndroidSetupPlanner.APPIUM_VERSION);
            requireExactPackage(staging.resolve("node_modules/appium-inspector-plugin/package.json"),
                    AndroidSetupPlanner.INSPECTOR_PLUGIN_VERSION);
            requireExactPackage(staging.resolve("node_modules/appium-uiautomator2-driver/package.json"),
                    AndroidSetupPlanner.UIAUTOMATOR2_VERSION);
            ReportingSetupService.ProcessResult version = run(List.of(nodeExecutable().toString(),
                    staging.resolve("node_modules/appium/index.js").toString(), "--version"), staging,
                    null, log, Duration.ofSeconds(30));
            requireSuccess(version, "Appium verification failed");
            requireExactVersion(version.output(), AndroidSetupPlanner.APPIUM_VERSION, "Appium");
            requireExtension(staging, "plugin", "inspector", "appium-inspector-plugin",
                    AndroidSetupPlanner.INSPECTOR_PLUGIN_VERSION, log);
            requireExtension(staging, "driver", "uiautomator2", "appium-uiautomator2-driver",
                    AndroidSetupPlanner.UIAUTOMATOR2_VERSION, log);
            ReportingSetupService.publish(staging, destination, VerifiedArtifactStore::move);
        } finally {
            deleteTree(staging);
        }
    }

    private void installAndroidSdk(SetupAction action) throws IOException {
        if (sdkStatus(action).readiness() == SetupReadiness.READY) return;
        Path archive = fetcher.fetch(action);
        Path destination = sdkRoot();
        Files.createDirectories(destination.getParent());
        Path staging = Files.createTempDirectory(destination.getParent(), "android-sdk.staging-");
        Path extracted = Files.createDirectory(staging.resolve("extract"));
        try {
            SafeZipExtractor.extract(archive, extracted);
            Path commandTools = extracted.resolve("cmdline-tools");
            if (!Files.isDirectory(commandTools, LinkOption.NOFOLLOW_LINKS)) {
                throw new IOException("Android command-line-tools archive has an unexpected layout.");
            }
            Path latest = staging.resolve("cmdline-tools/latest");
            Files.createDirectories(latest.getParent());
            VerifiedArtifactStore.move(commandTools, latest);
            deleteTree(extracted);
            Path sdkManager = sdkManager(staging);
            Map<String, String> environment = androidEnvironment(staging, avdHome());
            Path log = logFile();
            Files.createDirectories(log.getParent());
            requireSuccess(run(List.of(sdkManager.toString(), "--sdk_root=" + staging, "--licenses"), staging,
                    "y\n".repeat(80), log, Duration.ofMinutes(5), environment),
                    "Android SDK license acceptance failed");
            List<String> command = new ArrayList<>(List.of(sdkManager.toString(), "--sdk_root=" + staging));
            command.addAll(sdkPackages());
            requireSuccess(run(command, staging, null, log, Duration.ofMinutes(30), environment),
                    "Android SDK package installation failed");
            requireSdkFiles(staging);
            requireSdkTools(staging);
            ReportingSetupService.publish(staging, destination, VerifiedArtifactStore::move);
        } finally {
            deleteTree(staging);
        }
    }

    private void installAvd(SetupAction action) throws IOException {
        if (avdStatus(action).readiness() == SetupReadiness.READY) return;
        if (sdkStatus(AndroidSetupPlanner.plan(platform, architecture, SetupMode.MANAGED, request)
                .actions().get(4)).readiness() != SetupReadiness.READY) {
            throw new IOException("Android SDK must be ready before AVD creation.");
        }
        Path destination = avdRoot();
        if (avdDirectoryReady()) {
            writeAvdPointer(destination);
            requireReady(action);
            return;
        }
        Files.createDirectories(destination.getParent());
        Path staging = Files.createTempDirectory(destination.getParent(), request.avdName() + ".staging-");
        try {
            List<String> command = List.of(avdManager(sdkRoot()).toString(), "create", "avd", "--force",
                    "--name", request.avdName(), "--package", systemImage(), "--device", request.deviceProfile(),
                    "--path", staging.toString());
            requireSuccess(run(command, sdkRoot(), "no\n", logFile(), Duration.ofMinutes(3),
                    androidEnvironment(sdkRoot(), avdHome())), "Android AVD creation failed");
            Files.writeString(staging.resolve("shaft-request.properties"), requestMetadata());
            ReportingSetupService.publish(staging, destination, VerifiedArtifactStore::move);
            writeAvdPointer(destination);
        } finally {
            deleteTree(staging);
        }
    }

    private SetupStatus appiumStatus(SetupAction action) throws IOException {
        if (!appiumBundleStructureReady()) return missing(action, "Appium is not installed.");
        ReportingSetupService.ProcessResult result = run(List.of(nodeExecutable().toString(),
                appiumRoot().resolve("node_modules/appium/index.js").toString(), "--version"), appiumRoot(),
                null, null, Duration.ofSeconds(20));
        String version = result.output().trim();
        return result.exitCode() == 0 && version.equals(action.version())
                ? ready(action, version) : degraded(action, version, "Appium version or execution check failed.");
    }

    private SetupStatus extensionStatus(SetupAction action, String type, String extensionName,
                                        String packageName) throws IOException {
        Path manifest = appiumRoot().resolve("node_modules").resolve(packageName).resolve("package.json");
        VerifiedArtifactStore.requireUnlinkedAncestors(manifest);
        if (!Files.isRegularFile(manifest, LinkOption.NOFOLLOW_LINKS)) return missing(action, "Not installed.");
        String version = packageVersion(manifest);
        if (!version.equals(action.version())) {
            return degraded(action, version, "Installed npm package version does not match the release manifest.");
        }
        try {
            requireExtension(appiumRoot(), type, extensionName, packageName, action.version(), null);
            return ready(action, version);
        } catch (IOException failure) {
            return degraded(action, version, failure.getMessage());
        }
    }

    private void requireExtension(Path root, String type, String extensionName, String packageName,
                                  String expectedVersion, Path log) throws IOException {
        ReportingSetupService.ProcessResult result = run(List.of(nodeExecutable().toString(),
                root.resolve("node_modules/appium/index.js").toString(), type, "list", "--installed", "--json"),
                root, null, log, Duration.ofSeconds(30));
        requireSuccess(result, "Appium " + type + " discovery failed");
        tools.jackson.databind.JsonNode extension;
        try {
            extension = JSON.readTree(result.output()).path(extensionName);
        } catch (tools.jackson.core.JacksonException invalid) {
            throw new IOException("Appium " + type + " list returned invalid JSON.", invalid);
        }
        if (!packageName.equals(extension.path("pkgName").asText())
                || !expectedVersion.equals(extension.path("version").asText())) {
            throw new IOException("Appium " + type + ' ' + extensionName
                    + " is not registered at the approved version.");
        }
    }

    private SetupStatus sdkStatus(SetupAction action) throws IOException {
        VerifiedArtifactStore.requireUnlinkedAncestors(sdkRoot());
        if (!sdkFilesReady(sdkRoot())) return missing(action, "Android SDK package set is incomplete.");
        requireSdkTools(sdkRoot());
        return ready(action, action.version());
    }

    private SetupStatus avdStatus(SetupAction action) throws IOException {
        VerifiedArtifactStore.requireUnlinkedAncestors(avdRoot());
        if (!avdDirectoryReady()) {
            return missing(action, "SHAFT-owned AVD is not configured.");
        }
        Path pointer = avdHome().resolve(request.avdName() + ".ini");
        VerifiedArtifactStore.requireUnlinkedAncestors(pointer);
        if (!Files.isRegularFile(pointer, LinkOption.NOFOLLOW_LINKS)) {
            return missing(action, "SHAFT-owned AVD pointer is not published.");
        }
        return Files.readString(pointer, StandardCharsets.UTF_8).equals(avdPointerContent(avdRoot()))
                ? ready(action, action.version())
                : degraded(action, "", "AVD pointer does not match the reviewed request.");
    }

    private boolean avdDirectoryReady() throws IOException {
        Path metadata = avdRoot().resolve("shaft-request.properties");
        Path config = avdRoot().resolve("config.ini");
        if (!Files.isRegularFile(metadata, LinkOption.NOFOLLOW_LINKS)
                || !Files.isRegularFile(config, LinkOption.NOFOLLOW_LINKS)) return false;
        return Files.readString(metadata, StandardCharsets.UTF_8).equals(requestMetadata());
    }

    private boolean appiumBundleReady() throws IOException {
        return appiumBundleStructureReady()
                && appiumStatus(AndroidSetupPlanner.plan(platform, architecture, SetupMode.MANAGED, request)
                .actions().get(1)).readiness() == SetupReadiness.READY;
    }

    private boolean appiumBundleStructureReady() throws IOException {
        Path lock = appiumRoot().resolve("package-lock.json");
        VerifiedArtifactStore.requireUnlinkedAncestors(appiumRoot());
        return Files.isRegularFile(appiumRoot().resolve("node_modules/appium/index.js"), LinkOption.NOFOLLOW_LINKS)
                && Files.isRegularFile(lock, LinkOption.NOFOLLOW_LINKS)
                && VerifiedArtifactStore.digest(lock).equalsIgnoreCase(AndroidSetupPlanner.APPIUM_LOCK_SHA256);
    }

    private void requireReady(SetupAction action) throws IOException {
        SetupStatus status = status(action);
        if (status.readiness() != SetupReadiness.READY) {
            throw new IOException(action.target() + " verification failed: " + status.detail());
        }
    }

    private void copyAppiumManifest(Path staging, String expectedLock) throws IOException {
        for (String name : List.of("package.json", "package-lock.json")) {
            try (InputStream input = getClass().getResourceAsStream("/com/shaft/infrastructure/appium/" + name)) {
                if (input == null) throw new IOException("Missing bundled Appium manifest: " + name);
                byte[] content = input.readAllBytes();
                if (name.equals("package-lock.json")) content = new String(content, StandardCharsets.UTF_8)
                        .replace("\r\n", "\n").replace('\r', '\n').getBytes(StandardCharsets.UTF_8);
                Files.write(staging.resolve(name), content);
            }
        }
        String digest = "sha256:" + VerifiedArtifactStore.digest(staging.resolve("package-lock.json"));
        if (!digest.equalsIgnoreCase(expectedLock)) {
            throw new IOException("Bundled Appium lock does not match the approved plan.");
        }
    }

    private void requireSdkFiles(Path root) throws IOException {
        if (!sdkFilesReady(root)) throw new IOException("Android SDK installation is missing a required package.");
    }

    private boolean sdkFilesReady(Path root) throws IOException {
        for (Path file : List.of(sdkManager(root), avdManager(root), executable(root.resolve("platform-tools"), "adb"),
                executable(root.resolve("emulator"), "emulator"),
                root.resolve("platforms/android-" + request.apiLevel() + "/android.jar"),
                executable(root.resolve("build-tools").resolve(AndroidSetupPlanner.BUILD_TOOLS_VERSION), "aapt2"),
                root.resolve(systemImage().replace(';', java.io.File.separatorChar)).resolve("package.xml"))) {
            VerifiedArtifactStore.requireUnlinkedAncestors(file);
            if (!Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS)) return false;
        }
        return true;
    }

    private List<String> sdkPackages() {
        return List.of("platform-tools", "emulator", "platforms;android-" + request.apiLevel(),
                "build-tools;" + AndroidSetupPlanner.BUILD_TOOLS_VERSION, systemImage());
    }

    private void requireSdkTools(Path root) throws IOException {
        Map<String, String> environment = androidEnvironment(root, avdHome());
        ReportingSetupService.ProcessResult installed = run(List.of(sdkManager(root).toString(),
                "--sdk_root=" + root, "--list_installed"), root, null, null, Duration.ofMinutes(1), environment);
        requireSuccess(installed, "Android SDK installed-package discovery failed");
        Map<String, String> installedPackages = new LinkedHashMap<>();
        installed.output().lines().forEach(line -> {
            String[] columns = line.split("\\|", 3);
            if (columns.length >= 2) installedPackages.put(columns[0].trim(), columns[1].trim());
        });
        for (Map.Entry<String, String> required : sdkPackageRevisions().entrySet()) {
            String actual = installedPackages.get(required.getKey());
            if (actual == null) {
                throw new IOException("Android SDK installed package set is missing " + required.getKey() + '.');
            }
            if (!actual.equals(required.getValue())) {
                throw new IOException("Android SDK package " + required.getKey() + " has revision " + actual
                        + " instead of the approved " + required.getValue() + '.');
            }
        }
        for (List<String> probe : List.of(
                List.of(executable(root.resolve("platform-tools"), "adb").toString(), "version"),
                List.of(executable(root.resolve("emulator"), "emulator").toString(), "-version"),
                List.of(executable(root.resolve("build-tools").resolve(AndroidSetupPlanner.BUILD_TOOLS_VERSION),
                        "aapt2").toString(), "version"))) {
            requireSuccess(run(probe, root, null, null, Duration.ofSeconds(30), environment),
                    "Android SDK tool probe failed: " + probe.getFirst());
        }
    }

    private Map<String, String> sdkPackageRevisions() {
        return Map.of("platform-tools", AndroidSetupPlanner.PLATFORM_TOOLS_VERSION,
                "emulator", AndroidSetupPlanner.EMULATOR_VERSION,
                "platforms;android-" + request.apiLevel(), AndroidSetupPlanner.ANDROID_PLATFORM_REVISION,
                "build-tools;" + AndroidSetupPlanner.BUILD_TOOLS_VERSION,
                AndroidSetupPlanner.BUILD_TOOLS_VERSION,
                systemImage(), AndroidSetupPlanner.SYSTEM_IMAGE_REVISION);
    }

    private String systemImage() {
        return "system-images;android-" + request.apiLevel() + ';' + request.imageTag() + ';' + request.abi();
    }

    private Map<String, String> androidEnvironment(Path sdk, Path avd) {
        return Map.of("ANDROID_HOME", sdk.toString(), "ANDROID_SDK_ROOT", sdk.toString(),
                "ANDROID_AVD_HOME", avd.toString());
    }

    private ReportingSetupService.ProcessResult run(List<String> command, Path workingDirectory, String input,
                                                    Path log, Duration timeout) throws IOException {
        return run(command, workingDirectory, input, log, timeout, Map.of());
    }

    private ReportingSetupService.ProcessResult run(List<String> command, Path workingDirectory, String input,
                                                    Path log, Duration timeout,
                                                    Map<String, String> environment) throws IOException {
        return runner.run(command, workingDirectory, environment,
                Set.of("APPIUM_HOME", "REPO_OS_OVERRIDE"), input, log, timeout);
    }

    private void requireSafePaths() throws IOException {
        for (Path path : List.of(paths.cacheRoot(), paths.dataRoot(), paths.downloads(), paths.tools(), paths.state(),
                paths.receipts(), nodeRoot(), appiumRoot(), sdkRoot(), avdHome(), avdRoot())) {
            VerifiedArtifactStore.requireUnlinkedAncestors(path);
        }
    }

    private Path nodeRoot() {
        return paths.tools().resolve("node").resolve(ReportingSetupPlanner.NODE_VERSION)
                .resolve(platform.name().toLowerCase() + '-' + architecture.artifactName());
    }

    private Path nodeExecutable() { return executable(nodeRoot(), "node"); }

    private Path npmCli() {
        return platform == SetupPlatform.WINDOWS ? nodeRoot().resolve("node_modules/npm/bin/npm-cli.js")
                : nodeRoot().resolve("lib/node_modules/npm/bin/npm-cli.js");
    }

    private Path appiumRoot() { return paths.tools().resolve("appium").resolve(AndroidSetupPlanner.APPIUM_VERSION); }

    private Path sdkRoot() {
        return paths.tools().resolve("android-sdk").resolve(AndroidSetupPlanner.COMMAND_LINE_TOOLS_VERSION
                + "-api" + request.apiLevel() + '-' + request.abi());
    }

    private Path avdHome() { return paths.tools().resolve("android-avd"); }

    private Path avdRoot() { return avdHome().resolve(request.avdName() + ".avd"); }

    private Path sdkManager(Path root) {
        return executable(root.resolve("cmdline-tools/latest/bin"), "sdkmanager");
    }

    private Path avdManager(Path root) {
        return executable(root.resolve("cmdline-tools/latest/bin"), "avdmanager");
    }

    private Path executable(Path directory, String name) {
        if (platform != SetupPlatform.WINDOWS) return directory.resolve(name);
        return directory.resolve(name + switch (name) {
            case "sdkmanager", "avdmanager" -> ".bat";
            default -> ".exe";
        });
    }

    private Path logFile() { return paths.state().resolve("logs/mobile-android-install.log"); }

    private void writeAvdPointer(Path destination) throws IOException {
        Path pointer = avdHome().resolve(request.avdName() + ".ini");
        VerifiedArtifactStore.requireUnlinkedAncestors(pointer);
        Path temporary = Files.createTempFile(avdHome(), request.avdName(), ".ini.tmp");
        try {
            Files.writeString(temporary, avdPointerContent(destination));
            VerifiedArtifactStore.move(temporary, pointer);
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    private String avdPointerContent(Path destination) {
        return "path=" + destination + System.lineSeparator()
                + "path.rel=avd/" + destination.getFileName() + System.lineSeparator()
                + "target=android-" + request.apiLevel() + System.lineSeparator();
    }

    private String requestMetadata() {
        return String.join("\n", "api=" + request.apiLevel(), "device=" + request.deviceProfile(),
                "tag=" + request.imageTag(), "abi=" + request.abi(), "avd=" + request.avdName(),
                "ramMb=" + request.ramMb(), "cores=" + request.cores(), "port=" + request.appiumPort()) + "\n";
    }

    private static void requireExactPackage(Path manifest, String expected) throws IOException {
        String actual = packageVersion(manifest);
        if (!actual.equals(expected)) throw new IOException("Installed npm package version " + actual
                + " does not match " + expected + '.');
    }

    private static String packageVersion(Path manifest) throws IOException {
        VerifiedArtifactStore.requireUnlinkedAncestors(manifest);
        if (!Files.isRegularFile(manifest, LinkOption.NOFOLLOW_LINKS)) return "";
        String json = Files.readString(manifest, StandardCharsets.UTF_8);
        var matcher = java.util.regex.Pattern.compile("\\\"version\\\"\\s*:\\s*\\\"([^\\\"]+)\\\"")
                .matcher(json);
        if (!matcher.find()) throw new IOException("npm package manifest has no version: " + manifest);
        return matcher.group(1);
    }

    private static void requireExactVersion(String output, String expected, String tool) throws IOException {
        if (!output.trim().equals(expected)) {
            throw new IOException(tool + " verification returned unexpected version: " + output.trim());
        }
    }

    private static void requireSuccess(ReportingSetupService.ProcessResult result, String message) throws IOException {
        if (result.exitCode() != 0) throw new IOException(message + System.lineSeparator() + result.output());
    }

    private static SetupStatus ready(SetupAction action, String version) {
        return new SetupStatus(action.target(), SetupReadiness.READY, version, "Verified managed installation.");
    }

    private static SetupStatus missing(SetupAction action, String detail) {
        return new SetupStatus(action.target(), SetupReadiness.MISSING, "", detail);
    }

    private static SetupStatus degraded(SetupAction action, String version, String detail) {
        return new SetupStatus(action.target(), SetupReadiness.DEGRADED, version, detail);
    }

    private static void deleteTree(Path root) throws IOException {
        if (root == null || Files.notExists(root)) return;
        try (var stream = Files.walk(root)) {
            for (Path path : stream.sorted(java.util.Comparator.reverseOrder()).toList()) Files.deleteIfExists(path);
        }
    }
}
