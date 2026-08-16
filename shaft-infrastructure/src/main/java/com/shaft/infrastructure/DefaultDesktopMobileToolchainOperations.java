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
import java.util.List;
import java.util.Map;
import java.util.Set;

/** Verified filesystem/network/process implementation for desktop-mobile managed toolchains. */
final class DefaultDesktopMobileToolchainOperations implements DesktopMobileToolchainOperations {
    private static final JsonMapper JSON = JsonMapper.builder().build();

    private final ShaftCachePaths paths;
    private final SetupPlan plan;
    private final SetupPlatform platform;
    private final SetupArchitecture architecture;
    private final ReportingSetupService.ArtifactFetcher fetcher;
    private final AndroidCommandRunner runner;
    private final DesktopMobileHostProbe host;
    private final ReportingSetupService nodeService;
    private final boolean offline;

    DefaultDesktopMobileToolchainOperations(ShaftCachePaths paths, SetupPlan plan, boolean offline) {
        this(paths, plan, action -> new VerifiedArtifactStore(paths.downloads()).fetch(action, offline),
                AndroidCommandRunner.system(paths, plan.platform(), plan.architecture()),
                systemHost(paths, plan), offline);
    }

    DefaultDesktopMobileToolchainOperations(ShaftCachePaths paths, SetupPlan plan,
                                             ReportingSetupService.ArtifactFetcher fetcher,
                                             AndroidCommandRunner runner, DesktopMobileHostProbe host,
                                             boolean offline) {
        this.paths = java.util.Objects.requireNonNull(paths, "paths");
        this.plan = java.util.Objects.requireNonNull(plan, "plan");
        this.platform = plan.platform();
        this.architecture = plan.architecture();
        this.fetcher = java.util.Objects.requireNonNull(fetcher, "fetcher");
        this.runner = java.util.Objects.requireNonNull(runner, "runner");
        this.host = java.util.Objects.requireNonNull(host, "host");
        this.offline = offline;
        this.nodeService = new ReportingSetupService(paths, platform, architecture, fetcher,
                (command, log, timeout) -> runner.run(command, paths.cacheRoot(), Map.of(),
                        Set.of("APPIUM_HOME", "REPO_OS_OVERRIDE"), null, log, timeout), offline);
    }

    private static DesktopMobileHostProbe systemHost(ShaftCachePaths paths, SetupPlan plan) {
        AndroidCommandRunner runner = AndroidCommandRunner.system(paths, plan.platform(), plan.architecture());
        return new SystemDesktopMobileHostProbe(plan.platform(),
                Path.of(System.getProperty("user.dir")).toAbsolutePath().normalize(), runner);
    }

    @Override
    public void hostPreflight(List<SetupAction> actions) throws IOException {
        host.requireReady(actions);
    }

    @Override
    public void lockedPreflight(List<SetupAction> actions, boolean requireOffline) throws IOException {
        requireSafePaths();
        requireOfflineReady(actions, requireOffline);
    }

    @Override
    public void preStatePreflight(List<SetupAction> actions, boolean requireOffline) throws IOException {
        requireSafePaths();
        requireOfflineReady(actions, requireOffline);
    }

    private void requireOfflineReady(List<SetupAction> actions, boolean requireOffline) throws IOException {
        if (!requireOffline) return;
        if (!actions.stream().allMatch(this::structurallyInstalled)) {
            throw new IOException("Offline desktop-mobile setup requires a complete verified installation; "
                    + "cold or partial setup cannot run npm without network access.");
        }
        if (!actions.stream().allMatch(action -> status(action).readiness() == SetupReadiness.READY)) {
            throw new IOException("Offline desktop-mobile setup requires a complete verified installation; "
                    + "cold or partial setup cannot run npm without network access.");
        }
    }

    @Override
    public void install(SetupAction action) throws IOException {
        switch (action.target()) {
            case NODE -> nodeService.installNodeAction(action);
            case APPIUM_SERVER -> installAppiumBundle(action);
            case APPIUM_INSPECTOR_PLUGIN, APPIUM_XCUITEST_DRIVER, APPIUM_WINDOWS_DRIVER -> requireReady(action);
            case XCODE, IOS_SIMULATOR, WINAPPDRIVER -> requireHostReady(action);
            default -> throw new IOException("Desktop-mobile provider cannot install " + action.target());
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
                case APPIUM_XCUITEST_DRIVER -> extensionStatus(action, "driver", "xcuitest",
                        "appium-xcuitest-driver");
                case APPIUM_WINDOWS_DRIVER -> extensionStatus(action, "driver", "windows",
                        "appium-windows-driver");
                case XCODE, IOS_SIMULATOR, WINAPPDRIVER -> host.status(action);
                default -> degraded(action, "", "Unexpected target in desktop-mobile plan.");
            };
        } catch (IOException | RuntimeException failure) {
            return degraded(action, "", safeMessage(failure));
        }
    }

    private void installAppiumBundle(SetupAction serverAction) throws IOException {
        if (appiumBundleReady(serverAction)) return;
        if (nodeService.nodeStatus().readiness() != SetupReadiness.READY) {
            throw new IOException("Portable Node must be ready before Appium installation.");
        }
        List<SetupAction> packages = plan.actions().subList(1, 4);
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
                requireSuccess(run(cache, staging, log, Duration.ofMinutes(2)),
                        "Appium npm cache preparation failed");
            }
            List<String> install = new ArrayList<>(List.of(nodeExecutable().toString(), npmCli().toString(), "ci",
                    "--prefix", staging.toString(), "--ignore-scripts", "--no-audit", "--no-fund"));
            if (offline) install.add("--offline");
            requireSuccess(run(install, staging, log, Duration.ofMinutes(10)), "Appium npm installation failed");
            requireExactPackage(staging.resolve("node_modules/appium/package.json"),
                    AndroidSetupPlanner.APPIUM_VERSION);
            requireExactPackage(staging.resolve("node_modules/appium-inspector-plugin/package.json"),
                    AndroidSetupPlanner.INSPECTOR_PLUGIN_VERSION);
            SetupAction driver = plan.actions().get(3);
            requireExactPackage(staging.resolve("node_modules").resolve(driverPackage()).resolve("package.json"),
                    driver.version());
            ReportingSetupService.ProcessResult version = run(List.of(nodeExecutable().toString(),
                    staging.resolve("node_modules/appium/index.js").toString(), "--version"), staging, log,
                    Duration.ofSeconds(30));
            requireSuccess(version, "Appium verification failed");
            requireExactVersion(version.output(), AndroidSetupPlanner.APPIUM_VERSION, "Appium");
            requireExtension(staging, "plugin", "inspector", "appium-inspector-plugin",
                    AndroidSetupPlanner.INSPECTOR_PLUGIN_VERSION, log);
            requireExtension(staging, "driver", driverName(), driverPackage(), driver.version(), log);
            clearExtensionCache(staging);
            ReportingSetupService.publish(staging, destination, VerifiedArtifactStore::move);
        } finally {
            deleteTree(staging);
        }
    }

    private SetupStatus appiumStatus(SetupAction action) throws IOException {
        if (!appiumBundleStructureReady(action.dependencyLockChecksum())) {
            return missing(action, "Appium is not installed for this desktop-mobile profile.");
        }
        ReportingSetupService.ProcessResult result = run(List.of(nodeExecutable().toString(),
                appiumRoot().resolve("node_modules/appium/index.js").toString(), "--version"), appiumRoot(), null,
                Duration.ofSeconds(20));
        String version = reportedVersion(result.output());
        return result.exitCode() == 0 && version.equals(action.version())
                ? ready(action, version) : degraded(action, version, "Appium version or execution check failed.");
    }

    private SetupStatus extensionStatus(SetupAction action, String type, String name, String packageName)
            throws IOException {
        Path manifest = appiumRoot().resolve("node_modules").resolve(packageName).resolve("package.json");
        VerifiedArtifactStore.requireUnlinkedAncestors(manifest);
        if (!Files.isRegularFile(manifest, LinkOption.NOFOLLOW_LINKS)) return missing(action, "Not installed.");
        String version = packageVersion(manifest);
        if (!version.equals(action.version())) {
            return degraded(action, version, "Installed npm package version does not match the release manifest.");
        }
        try {
            requireExtension(appiumRoot(), type, name, packageName, action.version(), null);
            return ready(action, version);
        } catch (IOException failure) {
            return degraded(action, version, failure.getMessage());
        }
    }

    private void requireExtension(Path root, String type, String name, String packageName,
                                  String expectedVersion, Path log) throws IOException {
        ReportingSetupService.ProcessResult result = run(List.of(nodeExecutable().toString(),
                root.resolve("node_modules/appium/index.js").toString(), type, "list", "--installed", "--json"),
                root, log, Duration.ofSeconds(30));
        requireSuccess(result, "Appium " + type + " discovery failed");
        tools.jackson.databind.JsonNode extension;
        try {
            String output = result.output();
            int objectStart = output.indexOf('{');
            int objectEnd = output.lastIndexOf('}');
            if (objectStart < 0 || objectEnd < objectStart) {
                throw new IOException("Appium " + type + " list did not contain a JSON object.");
            }
            extension = JSON.readTree(output.substring(objectStart, objectEnd + 1)).path(name);
        } catch (tools.jackson.core.JacksonException invalid) {
            throw new IOException("Appium " + type + " list returned invalid JSON.", invalid);
        }
        if (!packageName.equals(extension.path("pkgName").asText())
                || !expectedVersion.equals(extension.path("version").asText())) {
            throw new IOException("Appium " + type + ' ' + name + " is not registered at the approved version.");
        }
    }

    private boolean structurallyInstalled(SetupAction action) {
        try {
            return switch (action.target()) {
                case NODE -> Files.isRegularFile(nodeExecutable(), LinkOption.NOFOLLOW_LINKS);
                case APPIUM_SERVER -> appiumBundleStructureReady(action.dependencyLockChecksum());
                case APPIUM_INSPECTOR_PLUGIN -> Files.isRegularFile(appiumRoot()
                        .resolve("node_modules/appium-inspector-plugin/package.json"), LinkOption.NOFOLLOW_LINKS);
                case APPIUM_XCUITEST_DRIVER, APPIUM_WINDOWS_DRIVER -> Files.isRegularFile(appiumRoot()
                        .resolve("node_modules").resolve(driverPackage()).resolve("package.json"),
                        LinkOption.NOFOLLOW_LINKS);
                case XCODE, IOS_SIMULATOR, WINAPPDRIVER -> true;
                default -> false;
            };
        } catch (IOException failure) {
            return false;
        }
    }

    private boolean appiumBundleReady(SetupAction action) throws IOException {
        if (!appiumBundleStructureReady(action.dependencyLockChecksum())) return false;
        return plan.actions().subList(1, 4).stream()
                .allMatch(candidate -> status(candidate).readiness() == SetupReadiness.READY);
    }

    private boolean appiumBundleStructureReady(String expectedLock) throws IOException {
        Path lock = appiumRoot().resolve("package-lock.json");
        VerifiedArtifactStore.requireUnlinkedAncestors(appiumRoot());
        return Files.isRegularFile(appiumRoot().resolve("node_modules/appium/index.js"), LinkOption.NOFOLLOW_LINKS)
                && Files.isRegularFile(lock, LinkOption.NOFOLLOW_LINKS)
                && ("sha256:" + VerifiedArtifactStore.digest(lock)).equalsIgnoreCase(expectedLock);
    }

    private void requireReady(SetupAction action) throws IOException {
        SetupStatus status = status(action);
        if (status.readiness() != SetupReadiness.READY) {
            throw new IOException(action.target() + " verification failed: " + status.detail());
        }
    }

    private void requireHostReady(SetupAction action) throws IOException {
        SetupStatus status = host.status(action);
        if (status.readiness() != SetupReadiness.READY) {
            throw new IOException(action.target() + " prerequisite is not ready: " + status.detail());
        }
    }

    private void copyAppiumManifest(Path staging, String expectedLock) throws IOException {
        String resourceRoot = plan.profile() == SetupProfile.MOBILE_IOS ? "appium-ios" : "appium-windows";
        for (String name : List.of("package.json", "package-lock.json")) {
            try (InputStream input = getClass().getResourceAsStream(
                    "/com/shaft/infrastructure/" + resourceRoot + '/' + name)) {
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

    private void requireSafePaths() throws IOException {
        for (Path path : List.of(paths.cacheRoot(), paths.dataRoot(), paths.downloads(), paths.tools(), paths.state(),
                paths.receipts(), nodeRoot(), appiumRoot())) VerifiedArtifactStore.requireUnlinkedAncestors(path);
    }

    private Path nodeRoot() {
        return paths.tools().resolve("node").resolve(ReportingSetupPlanner.NODE_VERSION)
                .resolve(platform.name().toLowerCase() + '-' + architecture.artifactName());
    }

    private Path nodeExecutable() {
        return platform == SetupPlatform.WINDOWS ? nodeRoot().resolve("node.exe") : nodeRoot().resolve("bin/node");
    }

    private Path npmCli() {
        return platform == SetupPlatform.WINDOWS ? nodeRoot().resolve("node_modules/npm/bin/npm-cli.js")
                : nodeRoot().resolve("lib/node_modules/npm/bin/npm-cli.js");
    }

    private Path appiumRoot() {
        String profile = plan.profile() == SetupProfile.MOBILE_IOS ? "appium-ios" : "appium-windows";
        return paths.tools().resolve(profile).resolve(AndroidSetupPlanner.APPIUM_VERSION);
    }

    private String driverName() {
        return plan.profile() == SetupProfile.MOBILE_IOS ? "xcuitest" : "windows";
    }

    private String driverPackage() {
        return plan.profile() == SetupProfile.MOBILE_IOS ? "appium-xcuitest-driver" : "appium-windows-driver";
    }

    private Path logFile() {
        String profile = plan.profile() == SetupProfile.MOBILE_IOS ? "mobile-ios" : "mobile-windows";
        return paths.state().resolve("logs/" + profile + "-install.log");
    }

    private ReportingSetupService.ProcessResult run(List<String> command, Path workingDirectory, Path log,
                                                    Duration timeout) throws IOException {
        return runner.run(command, workingDirectory, Map.of(), Set.of("APPIUM_HOME", "REPO_OS_OVERRIDE"),
                null, log, timeout);
    }

    private static void requireExactPackage(Path manifest, String expected) throws IOException {
        String actual = packageVersion(manifest);
        if (!actual.equals(expected)) throw new IOException("Installed npm package version " + actual
                + " does not match " + expected + '.');
    }

    private static String packageVersion(Path manifest) throws IOException {
        VerifiedArtifactStore.requireUnlinkedAncestors(manifest);
        if (!Files.isRegularFile(manifest, LinkOption.NOFOLLOW_LINKS)) return "";
        var matcher = java.util.regex.Pattern.compile("\\\"version\\\"\\s*:\\s*\\\"([^\\\"]+)\\\"")
                .matcher(Files.readString(manifest, StandardCharsets.UTF_8));
        if (!matcher.find()) throw new IOException("npm package manifest has no version: " + manifest);
        return matcher.group(1);
    }

    private static void requireExactVersion(String output, String expected, String tool) throws IOException {
        if (!reportedVersion(output).equals(expected)) {
            throw new IOException(tool + " verification returned unexpected version: " + output.trim());
        }
    }

    private static String reportedVersion(String output) {
        return output.lines().map(String::trim).filter(line -> !line.isEmpty())
                .reduce((ignored, last) -> last).orElse("");
    }

    private static void requireSuccess(ReportingSetupService.ProcessResult result, String message) throws IOException {
        if (result.exitCode() != 0) throw new IOException(message + System.lineSeparator() + result.output());
    }

    private static void clearExtensionCache(Path root) throws IOException {
        Path cache = root.resolve("node_modules/.cache/appium");
        VerifiedArtifactStore.requireUnlinkedAncestors(cache);
        deleteTree(cache);
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

    private static String safeMessage(Throwable failure) {
        return failure.getMessage() == null ? failure.getClass().getSimpleName() : failure.getMessage();
    }

    private static void deleteTree(Path root) throws IOException {
        if (root == null || Files.notExists(root)) return;
        try (var stream = Files.walk(root)) {
            for (Path path : stream.sorted(java.util.Comparator.reverseOrder()).toList()) Files.deleteIfExists(path);
        }
    }
}
