package com.shaft.mcp;

import com.shaft.driver.SHAFT;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * Discovers and bootstraps the local Appium and Android toolchain for MCP sessions.
 */
final class McpMobileToolchainService {
    private static final Duration QUICK_TIMEOUT = Duration.ofSeconds(12);
    private static final Duration INSTALL_TIMEOUT = Duration.ofMinutes(20);
    private static final Pattern DEVICE_NAME = Pattern.compile("\\bmodel:([^\\s]+)");

    private final McpProcessRunner runner;
    private final Map<String, String> environment;
    private final Path toolRoot;
    private final String osName;
    private final String osArch;
    private final HttpClient httpClient;

    McpMobileToolchainService() {
        this(McpProcessRunner.system(), System.getenv(), McpRuntimePaths.applicationDataRoot().resolve("tools"),
                System.getProperty("os.name", ""), System.getProperty("os.arch", ""));
    }

    McpMobileToolchainService(
            McpProcessRunner runner,
            Map<String, String> environment,
            Path toolRoot,
            String osName,
            String osArch) {
        this.runner = runner;
        this.environment = environment == null ? Map.of() : Map.copyOf(environment);
        this.toolRoot = toolRoot.toAbsolutePath().normalize();
        this.osName = osName == null ? "" : osName;
        this.osArch = osArch == null ? "" : osArch;
        this.httpClient = HttpClient.newBuilder()
                .connectTimeout(Duration.ofSeconds(30))
                .followRedirects(HttpClient.Redirect.NORMAL)
                .build();
    }

    McpMobileToolchainStatus status(String platformName) {
        String platform = normalizePlatform(platformName);
        Path androidSdkRoot = androidSdkRoot();
        Path androidAvdHome = androidAvdHome();
        Path appiumRoot = appiumRoot();
        List<String> warnings = new ArrayList<>();

        Optional<Path> node = resolveExecutable("node", List.of(nodeBin()));
        Optional<Path> npm = resolveExecutable("npm", List.of(nodeBin()));
        Optional<Path> appium = resolveExecutable("appium", List.of(appiumBin()));
        Optional<Path> adb = resolveExecutable("adb", List.of(androidSdkRoot.resolve("platform-tools")));
        Optional<Path> emulator = resolveExecutable("emulator", List.of(androidSdkRoot.resolve("emulator")));
        Optional<Path> sdkManager = resolveExecutable("sdkmanager", List.of(androidSdkRoot.resolve("cmdline-tools")
                .resolve("latest").resolve("bin")));
        Optional<Path> avdManager = resolveExecutable("avdmanager", List.of(androidSdkRoot.resolve("cmdline-tools")
                .resolve("latest").resolve("bin")));

        List<McpMobileDevice> devices = adb.map(this::androidDevices).orElseGet(() -> {
            warnings.add("adb was not found on PATH or in the SHAFT-managed Android SDK cache.");
            return List.of();
        });
        List<String> avds = cachedAndroidEmulators(emulator.orElse(null), androidAvdHome);
        boolean inspector = appium.filter(this::hasInspectorPlugin).isPresent()
                || Files.isDirectory(appiumRoot.resolve("node_modules").resolve("appium-inspector-plugin"));

        List<String> missing = new ArrayList<>();
        if (node.isEmpty()) {
            missing.add("node");
        }
        if (npm.isEmpty()) {
            missing.add("npm");
        }
        if (appium.isEmpty()) {
            missing.add("appium@" + SHAFT.Properties.internal.appiumServerVersion());
        }
        if (!inspector) {
            missing.add("appium-inspector-plugin@" + SHAFT.Properties.internal.appiumInspectorPluginVersion());
        }
        if ("Android".equals(platform)) {
            if (adb.isEmpty()) {
                missing.add("android platform-tools");
            }
            if (emulator.isEmpty()) {
                missing.add("android emulator");
            }
            if (sdkManager.isEmpty()) {
                missing.add("android sdkmanager");
            }
            if (avdManager.isEmpty()) {
                missing.add("android avdmanager");
            }
        }

        return new McpMobileToolchainStatus(
                platform,
                node.isPresent(),
                npm.isPresent(),
                appium.isPresent(),
                inspector,
                adb.isPresent(),
                emulator.isPresent(),
                sdkManager.isPresent(),
                avdManager.isPresent(),
                toolRoot,
                androidSdkRoot,
                androidAvdHome,
                appiumRoot,
                appiumVersion(appium.orElse(null)),
                SHAFT.Properties.internal.appiumInspectorPluginVersion(),
                devices,
                avds,
                missing,
                warnings);
    }

    McpAndroidEmulatorProposal defaultAndroidProposal(
            String avdName,
            int apiLevel,
            String deviceProfile,
            String imageTag,
            String abi,
            int ramMb,
            int cores) {
        int resolvedApiLevel = apiLevel > 0 ? apiLevel : SHAFT.Properties.internal.androidEmulatorApiLevel();
        String resolvedDevice = defaultText(deviceProfile, SHAFT.Properties.internal.androidEmulatorDeviceProfile());
        String resolvedTag = defaultText(imageTag, SHAFT.Properties.internal.androidEmulatorImageTag());
        String resolvedAbi = defaultText(abi, hostAndroidAbi());
        String resolvedName = defaultText(avdName, "shaft_pixel_8_api_" + resolvedApiLevel + "_"
                + resolvedAbi.replace('-', '_'));
        int resolvedRam = ramMb > 0 ? ramMb : SHAFT.Properties.internal.androidEmulatorRamMb();
        int resolvedCores = cores > 0 ? cores : SHAFT.Properties.internal.androidEmulatorCores();
        Path sdkRoot = androidSdkRoot();
        Path avdHome = androidAvdHome();
        String imagePackage = "system-images;android-" + resolvedApiLevel + ";" + resolvedTag + ";" + resolvedAbi;
        List<String> packages = List.of(
                "platform-tools",
                "emulator",
                "platforms;android-" + resolvedApiLevel,
                imagePackage);
        List<String> commands = List.of(
                commandLineToolsUrl(),
                "sdkmanager --sdk_root=" + sdkRoot + " " + String.join(" ", quotePackages(packages)),
                "avdmanager create avd --force --name " + resolvedName + " --package " + quote(imagePackage)
                        + " --device " + resolvedDevice,
                "emulator -avd " + resolvedName + " -no-snapshot-save");
        return new McpAndroidEmulatorProposal(
                resolvedName,
                resolvedDevice,
                resolvedApiLevel,
                resolvedTag,
                resolvedAbi,
                resolvedRam,
                resolvedCores,
                sdkRoot,
                avdHome,
                packages,
                commands,
                List.of(
                        "The Android SDK and AVD are installed under the current user's SHAFT MCP cache.",
                        "Confirming this proposal lets SHAFT pass Android SDK package license prompts for these packages."));
    }

    void ensureAppium(String platformName) {
        String platform = normalizePlatform(platformName);
        Path npm = ensureNpm();
        try {
            Files.createDirectories(appiumRoot());
            runChecked(List.of(npm.toString(), "--prefix", appiumRoot().toString(), "install",
                    "appium@" + SHAFT.Properties.internal.appiumServerVersion(),
                    "appium-inspector-plugin@" + SHAFT.Properties.internal.appiumInspectorPluginVersion()),
                    appiumRoot(), appiumEnvironment(), INSTALL_TIMEOUT, "Appium npm install failed.");
            Path appium = appiumCommand();
            runChecked(List.of(appium.toString(), "driver", "install", "--source=npm",
                    "appium-uiautomator2-driver@" + SHAFT.Properties.internal.appiumUiAutomator2DriverVersion()),
                    appiumRoot(), appiumEnvironment(), INSTALL_TIMEOUT, "UiAutomator2 driver install failed.");
            if ("iOS".equals(platform) && isMac()) {
                runChecked(List.of(appium.toString(), "driver", "install", "--source=npm",
                        "appium-xcuitest-driver@" + SHAFT.Properties.internal.appiumXcuitestDriverVersion()),
                        appiumRoot(), appiumEnvironment(), INSTALL_TIMEOUT, "XCUITest driver install failed.");
            }
            runChecked(List.of(appium.toString(), "plugin", "install", "--source=npm",
                    "appium-inspector-plugin@" + SHAFT.Properties.internal.appiumInspectorPluginVersion()),
                    appiumRoot(), appiumEnvironment(), INSTALL_TIMEOUT, "Appium Inspector plugin install failed.");
        } catch (IOException exception) {
            throw new IllegalStateException("Appium tool cache could not be prepared.", exception);
        }
    }

    void ensureAndroidEmulator(McpAndroidEmulatorProposal proposal) {
        if (proposal == null) {
            throw new IllegalArgumentException("Android emulator proposal is required.");
        }
        ensureAndroidCommandLineTools();
        Path sdkManager = sdkManagerCommand();
        Path avdManager = avdManagerCommand();
        try {
            Files.createDirectories(proposal.avdHome());
        } catch (IOException exception) {
            throw new IllegalStateException("Android AVD home could not be created.", exception);
        }
        runner.runWithInput(List.of(sdkManager.toString(), "--sdk_root=" + androidSdkRoot(), "--licenses"),
                androidSdkRoot(), androidEnvironment(), INSTALL_TIMEOUT, "y\n".repeat(80));
        List<String> sdkCommand = new ArrayList<>(List.of(sdkManager.toString(), "--sdk_root=" + androidSdkRoot()));
        sdkCommand.addAll(proposal.sdkPackages());
        runChecked(sdkCommand, androidSdkRoot(), androidEnvironment(), INSTALL_TIMEOUT,
                "Android SDK package installation failed.");
        runChecked(List.of(avdManager.toString(), "create", "avd", "--force",
                "--name", proposal.avdName(),
                "--package", "system-images;android-" + proposal.apiLevel() + ";" + proposal.imageTag() + ";"
                        + proposal.abi(),
                "--device", proposal.deviceProfile()),
                androidSdkRoot(), androidEnvironment(), INSTALL_TIMEOUT, "Android AVD creation failed.");
    }

    Process startAndroidEmulator(String avdName, McpAndroidEmulatorProposal proposal) {
        String name = defaultText(avdName, proposal == null ? "" : proposal.avdName());
        if (name.isBlank()) {
            throw new IllegalArgumentException("Android AVD name is required.");
        }
        Path emulator = emulatorCommand();
        List<String> command = new ArrayList<>(List.of(
                emulator.toString(),
                "-avd",
                name,
                "-no-snapshot-save",
                "-no-boot-anim"));
        if (proposal != null) {
            command.addAll(List.of(
                    "-memory", String.valueOf(proposal.ramMb()),
                    "-cores", String.valueOf(proposal.cores())));
        }
        return runner.start(command, androidSdkRoot(), androidEnvironment());
    }

    Process startAppiumServer(int port) {
        Path appium = appiumCommand();
        return runner.start(List.of(
                appium.toString(),
                "--address", "127.0.0.1",
                "--port", String.valueOf(port),
                "--use-plugins=inspector",
                "--relaxed-security"),
                appiumRoot(), appiumEnvironment());
    }

    boolean waitForAndroidDevice(Duration timeout) {
        Optional<Path> adb = resolveExecutable("adb", List.of(androidSdkRoot().resolve("platform-tools")));
        if (adb.isEmpty()) {
            return false;
        }
        long deadline = System.nanoTime() + timeout.toNanos();
        while (System.nanoTime() < deadline) {
            if (androidDevices(adb.get()).stream().anyMatch(device -> "device".equals(device.state()))) {
                return true;
            }
            sleep(Duration.ofSeconds(2));
        }
        return false;
    }

    void stopAndroidEmulator(String deviceId) {
        Optional<Path> adb = resolveExecutable("adb", List.of(androidSdkRoot().resolve("platform-tools")));
        if (adb.isEmpty() || text(deviceId).isBlank()) {
            return;
        }
        runner.run(List.of(adb.get().toString(), "-s", deviceId, "emu", "kill"),
                androidSdkRoot(), androidEnvironment(), Duration.ofSeconds(8));
    }

    String newConfirmationToken() {
        return UUID.randomUUID().toString();
    }

    Path appiumCommand() {
        return resolveExecutable("appium", List.of(appiumBin()))
                .orElse(appiumBin().resolve(isWindows() ? "appium.cmd" : "appium"));
    }

    private Path ensureNpm() {
        Optional<Path> npm = resolveExecutable("npm", List.of(nodeBin()));
        if (npm.isPresent()) {
            return npm.get();
        }
        downloadPortableNode();
        return resolveExecutable("npm", List.of(nodeBin()))
                .orElseThrow(() -> new IllegalStateException("npm was not found after portable Node.js setup."));
    }

    private void ensureAndroidCommandLineTools() {
        if (Files.isRegularFile(sdkManagerCommand()) || Files.isRegularFile(sdkManagerCommand().resolveSibling(
                sdkManagerCommand().getFileName().toString() + ".bat"))) {
            return;
        }
        Path zip = toolRoot.resolve("downloads").resolve("android-commandlinetools.zip");
        Path temp = toolRoot.resolve("downloads").resolve("android-commandlinetools-" + System.nanoTime());
        try {
            Files.createDirectories(zip.getParent());
            download(commandLineToolsUrl(), zip);
            unzip(zip, temp);
            Path extracted = temp.resolve("cmdline-tools");
            Path target = androidSdkRoot().resolve("cmdline-tools").resolve("latest");
            deleteIfExists(target);
            Files.createDirectories(target.getParent());
            Files.move(extracted, target, StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException exception) {
            throw new IllegalStateException("Android command-line tools could not be prepared.", exception);
        } finally {
            deleteIfExists(temp);
        }
    }

    private void downloadPortableNode() {
        String archiveName = nodeArchiveName();
        String extension = isWindows() ? ".zip" : isMac() ? ".tar.gz" : ".tar.xz";
        Path archive = toolRoot.resolve("downloads").resolve(archiveName + extension);
        Path target = toolRoot.resolve("node").resolve(archiveName);
        if (Files.isDirectory(target)) {
            return;
        }
        try {
            Files.createDirectories(archive.getParent());
            download("https://nodejs.org/dist/v" + SHAFT.Properties.internal.nodeLtsVersion() + "/"
                    + archiveName + extension, archive);
            Files.createDirectories(target.getParent());
            if (isWindows()) {
                Path temp = target.getParent().resolve(archiveName + "-extract");
                unzip(archive, temp);
                deleteIfExists(target);
                Files.move(temp.resolve(archiveName), target, StandardCopyOption.REPLACE_EXISTING);
                deleteIfExists(temp);
            } else {
                runChecked(List.of("tar", "-xf", archive.toString(), "-C", target.getParent().toString()),
                        target.getParent(), environment, INSTALL_TIMEOUT, "Portable Node.js archive extraction failed.");
            }
        } catch (IOException exception) {
            throw new IllegalStateException("Portable Node.js could not be downloaded.", exception);
        }
    }

    private void download(String url, Path target) throws IOException {
        if (Files.isRegularFile(target) && Files.size(target) > 0) {
            return;
        }
        HttpRequest request = HttpRequest.newBuilder(URI.create(url))
                .timeout(Duration.ofMinutes(5))
                .GET()
                .build();
        try {
            HttpResponse<Path> response = httpClient.send(request, HttpResponse.BodyHandlers.ofFile(target));
            if (response.statusCode() / 100 != 2) {
                Files.deleteIfExists(target);
                throw new IOException("Download failed with HTTP " + response.statusCode() + ": " + url);
            }
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IOException("Download interrupted: " + url, exception);
        }
    }

    private void unzip(Path zip, Path target) throws IOException {
        Files.createDirectories(target);
        try (ZipInputStream input = new ZipInputStream(Files.newInputStream(zip))) {
            ZipEntry entry;
            while ((entry = input.getNextEntry()) != null) {
                Path output = target.resolve(entry.getName()).normalize();
                if (!output.startsWith(target)) {
                    throw new IOException("Zip entry escapes target: " + entry.getName());
                }
                if (entry.isDirectory()) {
                    Files.createDirectories(output);
                } else {
                    Files.createDirectories(output.getParent());
                    Files.copy(input, output, StandardCopyOption.REPLACE_EXISTING);
                }
            }
        }
    }

    private void runChecked(
            List<String> command,
            Path workingDirectory,
            Map<String, String> commandEnvironment,
            Duration timeout,
            String failureMessage) {
        McpProcessRunner.ProcessResult result = runner.run(command, workingDirectory, commandEnvironment, timeout);
        if (result.timedOut() || result.exitCode() != 0) {
            throw new IllegalStateException(failureMessage + " Exit code: " + result.exitCode()
                    + ". Output: " + safeOutput(result.stdout(), result.stderr()));
        }
    }

    private List<McpMobileDevice> androidDevices(Path adb) {
        McpProcessRunner.ProcessResult result = runner.run(List.of(adb.toString(), "devices", "-l"),
                androidSdkRoot(), androidEnvironment(), QUICK_TIMEOUT);
        if (result.exitCode() != 0 || result.timedOut()) {
            return List.of(new McpMobileDevice("", "", "Android", "unavailable", false,
                    List.of("adb devices failed: " + safeOutput(result.stdout(), result.stderr()))));
        }
        List<McpMobileDevice> devices = new ArrayList<>();
        for (String line : result.stdout().split("\\R")) {
            String trimmed = line.trim();
            if (trimmed.isBlank() || trimmed.startsWith("List of devices")) {
                continue;
            }
            String[] parts = trimmed.split("\\s+", 3);
            if (parts.length < 2) {
                continue;
            }
            String id = parts[0];
            String state = parts[1];
            List<String> warnings = "device".equals(state) ? List.of()
                    : List.of("Android device is not ready: " + state);
            devices.add(new McpMobileDevice(
                    id,
                    androidModel(trimmed, id),
                    "Android",
                    state,
                    id.startsWith("emulator-"),
                    warnings));
        }
        return List.copyOf(devices);
    }

    private List<String> cachedAndroidEmulators(Path emulator, Path androidAvdHome) {
        Set<String> avds = new LinkedHashSet<>();
        if (emulator != null) {
            McpProcessRunner.ProcessResult result = runner.run(List.of(emulator.toString(), "-list-avds"),
                    androidSdkRoot(), androidEnvironment(), QUICK_TIMEOUT);
            if (result.exitCode() == 0 && !result.timedOut()) {
                Arrays.stream(result.stdout().split("\\R"))
                        .map(String::trim)
                        .filter(line -> !line.isBlank())
                        .forEach(avds::add);
            }
        }
        scanAvdHome(androidAvdHome, avds);
        scanAvdHome(defaultUserAvdHome(), avds);
        return List.copyOf(avds);
    }

    private void scanAvdHome(Path avdHome, Set<String> avds) {
        if (!Files.isDirectory(avdHome)) {
            return;
        }
        try (var stream = Files.list(avdHome)) {
            stream.filter(path -> path.getFileName().toString().endsWith(".avd"))
                    .map(path -> path.getFileName().toString().replaceFirst("\\.avd$", ""))
                    .forEach(avds::add);
        } catch (IOException ignored) {
            // Directory scans are best effort; emulator -list-avds remains authoritative.
        }
    }

    private boolean hasInspectorPlugin(Path appium) {
        McpProcessRunner.ProcessResult result = runner.run(List.of(appium.toString(), "plugin", "list", "--installed"),
                appiumRoot(), appiumEnvironment(), QUICK_TIMEOUT);
        return result.exitCode() == 0 && result.stdout().toLowerCase(Locale.ROOT).contains("inspector");
    }

    private String appiumVersion(Path appium) {
        if (appium == null) {
            return "";
        }
        McpProcessRunner.ProcessResult result = runner.run(List.of(appium.toString(), "--version"),
                appiumRoot(), appiumEnvironment(), QUICK_TIMEOUT);
        return result.exitCode() == 0 ? result.stdout().trim() : "";
    }

    private Optional<Path> resolveExecutable(String name, List<Path> additionalDirectories) {
        List<Path> candidates = new ArrayList<>();
        if (additionalDirectories != null) {
            for (Path directory : additionalDirectories) {
                if (directory != null) {
                    candidates.addAll(executableCandidates(directory, name));
                }
            }
        }
        String path = environment.getOrDefault("PATH", System.getenv("PATH"));
        if (path != null) {
            for (String entry : path.split(Pattern.quote(java.io.File.pathSeparator))) {
                if (!entry.isBlank()) {
                    candidates.addAll(executableCandidates(Path.of(entry), name));
                }
            }
        }
        return candidates.stream()
                .filter(Files::isRegularFile)
                .findFirst()
                .map(pathValue -> pathValue.toAbsolutePath().normalize());
    }

    private List<Path> executableCandidates(Path directory, String name) {
        if (isWindows()) {
            return List.of(directory.resolve(name + ".cmd"), directory.resolve(name + ".bat"),
                    directory.resolve(name + ".exe"), directory.resolve(name));
        }
        return List.of(directory.resolve(name));
    }

    private Map<String, String> androidEnvironment() {
        return mergedEnvironment(Map.of(
                "ANDROID_HOME", androidSdkRoot().toString(),
                "ANDROID_SDK_ROOT", androidSdkRoot().toString(),
                "ANDROID_AVD_HOME", androidAvdHome().toString()));
    }

    private Map<String, String> appiumEnvironment() {
        return mergedEnvironment(Map.of(
                "APPIUM_HOME", appiumRoot().resolve("home").toString(),
                "ANDROID_HOME", androidSdkRoot().toString(),
                "ANDROID_SDK_ROOT", androidSdkRoot().toString(),
                "ANDROID_AVD_HOME", androidAvdHome().toString()));
    }

    private Map<String, String> mergedEnvironment(Map<String, String> additions) {
        java.util.HashMap<String, String> merged = new java.util.HashMap<>(environment);
        merged.putAll(additions);
        String path = environment.getOrDefault("PATH", System.getenv("PATH"));
        String extraPath = String.join(java.io.File.pathSeparator,
                nodeBin().toString(),
                appiumBin().toString(),
                androidSdkRoot().resolve("platform-tools").toString(),
                androidSdkRoot().resolve("emulator").toString(),
                androidSdkRoot().resolve("cmdline-tools").resolve("latest").resolve("bin").toString());
        merged.put("PATH", path == null || path.isBlank() ? extraPath : extraPath + java.io.File.pathSeparator + path);
        return Map.copyOf(merged);
    }

    private Path androidSdkRoot() {
        String configured = firstNonBlank(environment.get("ANDROID_SDK_ROOT"), environment.get("ANDROID_HOME"));
        return configured == null ? toolRoot.resolve("android-sdk") : Path.of(configured);
    }

    private Path androidAvdHome() {
        String configured = environment.get("ANDROID_AVD_HOME");
        return configured == null || configured.isBlank() ? toolRoot.resolve("android-avd") : Path.of(configured);
    }

    private Path defaultUserAvdHome() {
        return Path.of(System.getProperty("user.home", ".")).resolve(".android").resolve("avd");
    }

    private Path appiumRoot() {
        return toolRoot.resolve("appium");
    }

    private Path appiumBin() {
        return appiumRoot().resolve("node_modules").resolve(".bin");
    }

    private Path nodeBin() {
        return toolRoot.resolve("node").resolve(nodeArchiveName()).resolve(isWindows() ? "" : "bin");
    }

    private Path sdkManagerCommand() {
        Path bin = androidSdkRoot().resolve("cmdline-tools").resolve("latest").resolve("bin");
        return bin.resolve(isWindows() ? "sdkmanager.bat" : "sdkmanager");
    }

    private Path avdManagerCommand() {
        Path bin = androidSdkRoot().resolve("cmdline-tools").resolve("latest").resolve("bin");
        return bin.resolve(isWindows() ? "avdmanager.bat" : "avdmanager");
    }

    private Path emulatorCommand() {
        return resolveExecutable("emulator", List.of(androidSdkRoot().resolve("emulator")))
                .orElseThrow(() -> new IllegalStateException("Android emulator executable was not found."));
    }

    private String commandLineToolsUrl() {
        String platform = isWindows() ? "win" : isMac() ? "mac" : "linux";
        return "https://dl.google.com/android/repository/commandlinetools-" + platform + "-"
                + SHAFT.Properties.internal.androidCommandLineToolsVersion() + "_latest.zip";
    }

    private String nodeArchiveName() {
        String platform = isWindows() ? "win" : isMac() ? "darwin" : "linux";
        String arch = osArch.toLowerCase(Locale.ROOT).contains("aarch64")
                || osArch.toLowerCase(Locale.ROOT).contains("arm64") ? "arm64" : "x64";
        return "node-v" + SHAFT.Properties.internal.nodeLtsVersion() + "-" + platform + "-" + arch;
    }

    private String hostAndroidAbi() {
        String arch = osArch.toLowerCase(Locale.ROOT);
        return arch.contains("aarch64") || arch.contains("arm64") ? "arm64-v8a" : "x86_64";
    }

    private String normalizePlatform(String platformName) {
        String platform = text(platformName).toLowerCase(Locale.ROOT);
        return switch (platform) {
            case "", "android" -> "Android";
            case "ios" -> "iOS";
            default -> throw new IllegalArgumentException("platformName must be Android or iOS.");
        };
    }

    private String androidModel(String line, String fallback) {
        Matcher matcher = DEVICE_NAME.matcher(line);
        return matcher.find() ? matcher.group(1).replace('_', ' ') : fallback;
    }

    private boolean isWindows() {
        return osName.toLowerCase(Locale.ROOT).contains("win");
    }

    private boolean isMac() {
        String os = osName.toLowerCase(Locale.ROOT);
        return os.contains("mac") || os.contains("darwin");
    }

    private static String firstNonBlank(String first, String second) {
        if (first != null && !first.isBlank()) {
            return first;
        }
        return second == null || second.isBlank() ? null : second;
    }

    private static String defaultText(String value, String fallback) {
        String text = text(value);
        return text.isBlank() ? fallback : text;
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }

    private static String quote(String value) {
        return "\"" + value + "\"";
    }

    private static List<String> quotePackages(List<String> packages) {
        return packages.stream().map(McpMobileToolchainService::quote).toList();
    }

    private static String safeOutput(String stdout, String stderr) {
        String combined = (stdout == null ? "" : stdout) + "\n" + (stderr == null ? "" : stderr);
        return combined.length() > 4000 ? combined.substring(0, 4000) : combined.trim();
    }

    private static void sleep(Duration duration) {
        try {
            Thread.sleep(duration.toMillis());
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
        }
    }

    private static void deleteIfExists(Path path) {
        if (path == null || !Files.exists(path)) {
            return;
        }
        try {
            Files.walkFileTree(path, new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                    Files.deleteIfExists(file);
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                    Files.deleteIfExists(dir);
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (IOException ignored) {
            // Best-effort cleanup only.
        }
    }
}
