package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesktopMobileToolchainOperationsTest {
    private static final JsonMapper JSON = JsonMapper.builder().build();

    @Test
    void iosBundleUsesItsOwnLockfileAndXcuiTestDriver(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        createNodeFixture(paths, SetupPlatform.MACOS, SetupArchitecture.ARM64);
        SetupPlan plan = DesktopMobileSetupPlanner.ios(SetupPlatform.MACOS, SetupArchitecture.ARM64,
                SetupMode.MANAGED, new SetupSelection(List.of(
                "simulator_00000000_0000_0000_0000_000000000001")));
        List<SetupTarget> fetched = new ArrayList<>();
        ReportingSetupService.ArtifactFetcher fetcher = action -> {
            fetched.add(action.target());
            Path archive = temp.resolve(action.target().name().toLowerCase() + ".tgz");
            Files.writeString(archive, "fixture");
            return archive;
        };
        List<List<String>> commands = new ArrayList<>();
        DesktopMobileHostProbe host = action -> new SetupStatus(action.target(), SetupReadiness.READY,
                action.version(), "fixture");
        DefaultDesktopMobileToolchainOperations operations = new DefaultDesktopMobileToolchainOperations(
                paths, plan, fetcher, fixtureRunner(commands), host, false);

        new DesktopMobileSetupService(paths, plan, operations, false)
                .install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()));

        Path root = paths.tools().resolve("appium-ios/" + AndroidSetupPlanner.APPIUM_VERSION);
        assertEquals(DesktopMobileSetupPlanner.IOS_LOCK_SHA256,
                "sha256:" + VerifiedArtifactStore.digest(root.resolve("package-lock.json")));
        assertEquals("{\"version\":\"" + DesktopMobileSetupPlanner.XCUITEST_VERSION + "\"}",
                Files.readString(root.resolve("node_modules/appium-xcuitest-driver/package.json")));
        assertExactManifestAndLockDependencies(root, "appium-xcuitest-driver",
                DesktopMobileSetupPlanner.XCUITEST_VERSION);
        assertFalse(Files.exists(paths.tools().resolve("appium-windows")));
        assertEquals(List.of(SetupTarget.APPIUM_SERVER, SetupTarget.APPIUM_INSPECTOR_PLUGIN,
                SetupTarget.APPIUM_XCUITEST_DRIVER), fetched);
        assertHardenedNpmCi(commands);
    }

    @Test
    void windowsBundleUsesItsOwnLockfileAndVerifiesRegisteredExtensions(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        createNodeFixture(paths, SetupPlatform.WINDOWS, SetupArchitecture.X64);
        SetupPlan plan = DesktopMobileSetupPlanner.windows(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, SetupSelection.defaults());
        List<SetupTarget> fetched = new ArrayList<>();
        List<List<String>> commands = new ArrayList<>();
        ReportingSetupService.ArtifactFetcher fetcher = action -> {
            fetched.add(action.target());
            Path archive = temp.resolve(action.target().name().toLowerCase() + ".tgz");
            Files.writeString(archive, "fixture");
            return archive;
        };
        AndroidCommandRunner runner = fixtureRunner(commands);
        DesktopMobileHostProbe host = action -> new SetupStatus(action.target(), SetupReadiness.READY,
                action.version(), "fixture");
        DefaultDesktopMobileToolchainOperations operations = new DefaultDesktopMobileToolchainOperations(
                paths, plan, fetcher, runner, host, false);

        SetupReceipt receipt = new DesktopMobileSetupService(paths, plan, operations, false)
                .install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()));

        Path root = paths.tools().resolve("appium-windows/" + AndroidSetupPlanner.APPIUM_VERSION);
        assertTrue(Files.isRegularFile(root.resolve("package-lock.json")));
        assertEquals(DesktopMobileSetupPlanner.WINDOWS_LOCK_SHA256,
                "sha256:" + VerifiedArtifactStore.digest(root.resolve("package-lock.json")));
        assertFalse(Files.exists(paths.tools().resolve("appium-ios")));
        assertEquals(List.of(SetupTarget.APPIUM_SERVER, SetupTarget.APPIUM_INSPECTOR_PLUGIN,
                SetupTarget.APPIUM_WINDOWS_DRIVER), fetched);
        assertEquals("{\"version\":\"" + DesktopMobileSetupPlanner.WINDOWS_DRIVER_VERSION + "\"}",
                Files.readString(root.resolve("node_modules/appium-windows-driver/package.json")));
        assertExactManifestAndLockDependencies(root, "appium-windows-driver",
                DesktopMobileSetupPlanner.WINDOWS_DRIVER_VERSION);
        assertHardenedNpmCi(commands);
        assertEquals(plan.actions(), receipt.completedActions());
        assertEquals(SetupReadiness.READY, new DesktopMobileSetupService(paths, plan, operations, false)
                .status().readiness());
    }

    @ParameterizedTest
    @EnumSource(value = SetupProfile.class, names = {"MOBILE_IOS", "MOBILE_WINDOWS"})
    void onlineInstallRepairsAPartialAppiumBundle(SetupProfile profile, @TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = plan(profile);
        createNodeFixture(paths, plan.platform(), plan.architecture());
        AtomicInteger fetches = new AtomicInteger();
        ReportingSetupService.ArtifactFetcher fetcher = action -> {
            fetches.incrementAndGet();
            Path archive = temp.resolve(action.target().name().toLowerCase() + ".tgz");
            Files.writeString(archive, "fixture");
            return archive;
        };
        DesktopMobileHostProbe host = action -> new SetupStatus(action.target(), SetupReadiness.READY,
                action.version(), "fixture");
        List<List<String>> commands = new ArrayList<>();
        DefaultDesktopMobileToolchainOperations operations = new DefaultDesktopMobileToolchainOperations(
                paths, plan, fetcher, fixtureRunner(commands), host, false);
        DesktopMobileSetupService service = new DesktopMobileSetupService(paths, plan, operations, false);
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.EPOCH, Set.of());
        service.install(plan, approval);
        Path driver = paths.tools().resolve(appiumDirectory(profile)).resolve(AndroidSetupPlanner.APPIUM_VERSION)
                .resolve("node_modules").resolve(driverPackage(profile)).resolve("package.json");
        Files.delete(driver);

        service.install(plan, approval);

        assertTrue(Files.isRegularFile(driver));
        assertEquals(6, fetches.get());
        assertHardenedNpmCi(commands);
        assertEquals(SetupReadiness.READY, service.status().readiness());
    }

    @ParameterizedTest
    @EnumSource(value = SetupProfile.class, names = {"MOBILE_IOS", "MOBILE_WINDOWS"})
    void offlineColdSetupFailsBeforeFetchOrNpm(SetupProfile profile, @TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = plan(profile);
        AtomicInteger fetches = new AtomicInteger();
        AtomicInteger commands = new AtomicInteger();
        DefaultDesktopMobileToolchainOperations operations = new DefaultDesktopMobileToolchainOperations(
                paths, plan, action -> {
                    fetches.incrementAndGet();
                    throw new IOException("must not fetch");
                }, (command, workingDirectory, environment, removed, input, log, timeout) -> {
                    commands.incrementAndGet();
                    return new ReportingSetupService.ProcessResult(1, "must not run");
                }, action -> new SetupStatus(action.target(), SetupReadiness.READY, action.version(), "fixture"),
                true);

        IOException failure = assertThrows(IOException.class,
                () -> new DesktopMobileSetupService(paths, plan, operations, true)
                        .install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of())));

        assertTrue(failure.getMessage().contains("Offline"));
        assertEquals(0, fetches.get());
        assertEquals(0, commands.get());
        assertFalse(Files.exists(paths.cacheRoot()));
        assertFalse(Files.exists(paths.dataRoot()));
    }

    @ParameterizedTest
    @EnumSource(value = SetupProfile.class, names = {"MOBILE_IOS", "MOBILE_WINDOWS"})
    void offlinePartialSetupLeavesExistingStateByteForByteUnchanged(SetupProfile profile, @TempDir Path temp)
            throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = plan(profile);
        createNodeFixture(paths, plan.platform(), plan.architecture());
        DesktopMobileHostProbe host = action -> new SetupStatus(action.target(), SetupReadiness.READY,
                action.version(), "fixture");
        ReportingSetupService.ArtifactFetcher onlineFetcher = action -> {
            Path archive = temp.resolve(action.target().name().toLowerCase() + ".tgz");
            Files.writeString(archive, "fixture");
            return archive;
        };
        new DesktopMobileSetupService(paths, plan, new DefaultDesktopMobileToolchainOperations(
                paths, plan, onlineFetcher, fixtureRunner(new ArrayList<>()), host, false), false)
                .install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()));
        Path driver = paths.tools().resolve(appiumDirectory(profile)).resolve(AndroidSetupPlanner.APPIUM_VERSION)
                .resolve("node_modules").resolve(driverPackage(profile)).resolve("package.json");
        Files.delete(driver);
        Map<String, String> before = snapshot(paths.cacheRoot(), paths.dataRoot());
        AtomicInteger fetches = new AtomicInteger();
        List<List<String>> commands = new ArrayList<>();
        DefaultDesktopMobileToolchainOperations offlineOperations = new DefaultDesktopMobileToolchainOperations(
                paths, plan, action -> {
                    fetches.incrementAndGet();
                    throw new IOException("must not fetch");
                }, fixtureRunner(commands), host, true);

        IOException failure = assertThrows(IOException.class,
                () -> new DesktopMobileSetupService(paths, plan, offlineOperations, true)
                        .install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of())));

        assertTrue(failure.getMessage().contains("Offline"));
        assertEquals(0, fetches.get());
        assertTrue(commands.isEmpty());
        assertEquals(before, snapshot(paths.cacheRoot(), paths.dataRoot()));
    }

    @ParameterizedTest
    @EnumSource(value = SetupProfile.class, names = {"MOBILE_IOS", "MOBILE_WINDOWS"})
    void missingHostPrerequisiteFailsBeforeStateOrFetch(SetupProfile profile, @TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = plan(profile);
        AtomicInteger fetches = new AtomicInteger();
        DesktopMobileHostProbe host = action -> new SetupStatus(action.target(), SetupReadiness.MISSING, "",
                "Host prerequisite is disabled.");
        DefaultDesktopMobileToolchainOperations operations = new DefaultDesktopMobileToolchainOperations(
                paths, plan, action -> {
                    fetches.incrementAndGet();
                    throw new IOException("must not fetch");
                }, fixtureRunner(new ArrayList<>()), host, false);

        IOException failure = assertThrows(IOException.class,
                () -> new DesktopMobileSetupService(paths, plan, operations, false)
                        .install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of())));

        assertTrue(failure.getMessage().contains("Host prerequisite"));
        assertEquals(0, fetches.get());
        assertFalse(Files.exists(paths.cacheRoot()));
        assertFalse(Files.exists(paths.dataRoot()));
    }

    private static void assertHardenedNpmCi(List<List<String>> commands) {
        List<List<String>> installs = commands.stream().filter(command -> command.contains("ci")).toList();
        assertFalse(installs.isEmpty());
        assertTrue(installs.stream().allMatch(command -> command.contains("--ignore-scripts")
                && command.contains("--no-audit") && command.contains("--no-fund")));
    }

    private static void assertExactManifestAndLockDependencies(Path root, String driverPackage,
                                                               String driverVersion) throws IOException {
        JsonNode manifestDependencies = JSON.readTree(root.resolve("package.json").toFile()).path("dependencies");
        JsonNode lock = JSON.readTree(root.resolve("package-lock.json").toFile());
        JsonNode lockDependencies = lock.path("packages").path("").path("dependencies");

        assertEquals(3, manifestDependencies.size());
        assertEquals(manifestDependencies, lockDependencies);
        assertEquals(AndroidSetupPlanner.APPIUM_VERSION, manifestDependencies.path("appium").asText());
        assertEquals(AndroidSetupPlanner.APPIUM_VERSION,
                lock.path("packages").path("node_modules/appium").path("version").asText());
        assertEquals(AndroidSetupPlanner.INSPECTOR_PLUGIN_VERSION,
                manifestDependencies.path("appium-inspector-plugin").asText());
        assertEquals(AndroidSetupPlanner.INSPECTOR_PLUGIN_VERSION,
                lock.path("packages").path("node_modules/appium-inspector-plugin").path("version").asText());
        assertEquals(driverVersion, manifestDependencies.path(driverPackage).asText());
        assertEquals(driverVersion,
                lock.path("packages").path("node_modules/" + driverPackage).path("version").asText());
    }

    private static SetupPlan plan(SetupProfile profile) {
        return profile == SetupProfile.MOBILE_IOS
                ? DesktopMobileSetupPlanner.ios(SetupPlatform.MACOS, SetupArchitecture.ARM64,
                SetupMode.MANAGED, new SetupSelection(List.of(
                "simulator_00000000_0000_0000_0000_000000000001")))
                : DesktopMobileSetupPlanner.windows(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, SetupSelection.defaults());
    }

    private static String appiumDirectory(SetupProfile profile) {
        return profile == SetupProfile.MOBILE_IOS ? "appium-ios" : "appium-windows";
    }

    private static String driverPackage(SetupProfile profile) {
        return profile == SetupProfile.MOBILE_IOS ? "appium-xcuitest-driver" : "appium-windows-driver";
    }

    private static AndroidCommandRunner fixtureRunner(List<List<String>> commands) {
        return (command, workingDirectory, environment, removed, input, log, timeout) -> {
            commands.add(List.copyOf(command));
            if (command.size() == 2 && command.getLast().equals("--version")
                    && (command.getFirst().endsWith("node.exe") || command.getFirst().endsWith("/node")
                    || command.getFirst().endsWith("\\node"))) {
                return result(0, "v" + ReportingSetupPlanner.NODE_VERSION);
            }
            if (command.contains("ci")) createAppiumFixture(workingDirectory);
            if (command.contains("--version")) return result(0, AndroidSetupPlanner.APPIUM_VERSION);
            if (command.contains("plugin")) return result(0, "{\"inspector\":{\"pkgName\":"
                    + "\"appium-inspector-plugin\",\"version\":\""
                    + AndroidSetupPlanner.INSPECTOR_PLUGIN_VERSION + "\"}}");
            if (command.contains("driver")) {
                boolean ios = workingDirectory.toString().contains("appium-ios");
                return ios ? result(0, "{\"xcuitest\":{\"pkgName\":\"appium-xcuitest-driver\","
                        + "\"version\":\"" + DesktopMobileSetupPlanner.XCUITEST_VERSION + "\"}}")
                        : result(0, "{\"windows\":{\"pkgName\":\"appium-windows-driver\",\"version\":\""
                        + DesktopMobileSetupPlanner.WINDOWS_DRIVER_VERSION + "\"}}");
            }
            return result(0, "fixture");
        };
    }

    private static void createNodeFixture(ShaftCachePaths paths, SetupPlatform platform,
                                          SetupArchitecture architecture) throws IOException {
        Path root = paths.tools().resolve("node").resolve(ReportingSetupPlanner.NODE_VERSION)
                .resolve(platform.name().toLowerCase() + '-' + architecture.artifactName());
        if (platform == SetupPlatform.WINDOWS) {
            Files.createDirectories(root.resolve("node_modules/npm/bin"));
            Files.writeString(root.resolve("node.exe"), "fixture");
            Files.writeString(root.resolve("node_modules/npm/bin/npm-cli.js"), "fixture");
        } else {
            Files.createDirectories(root.resolve("bin"));
            Files.createDirectories(root.resolve("lib/node_modules/npm/bin"));
            Files.writeString(root.resolve("bin/node"), "fixture");
            Files.writeString(root.resolve("lib/node_modules/npm/bin/npm-cli.js"), "fixture");
        }
    }

    private static void createAppiumFixture(Path root) throws IOException {
        Path modules = root.resolve("node_modules");
        Files.createDirectories(modules.resolve("appium"));
        Files.writeString(modules.resolve("appium/index.js"), "fixture");
        writePackage(modules.resolve("appium/package.json"), AndroidSetupPlanner.APPIUM_VERSION);
        writePackage(modules.resolve("appium-inspector-plugin/package.json"),
                AndroidSetupPlanner.INSPECTOR_PLUGIN_VERSION);
        if (root.toString().contains("appium-ios")) {
            writePackage(modules.resolve("appium-xcuitest-driver/package.json"),
                    DesktopMobileSetupPlanner.XCUITEST_VERSION);
        } else {
            writePackage(modules.resolve("appium-windows-driver/package.json"),
                    DesktopMobileSetupPlanner.WINDOWS_DRIVER_VERSION);
        }
    }

    private static void writePackage(Path path, String version) throws IOException {
        Files.createDirectories(path.getParent());
        Files.writeString(path, "{\"version\":\"" + version + "\"}");
    }

    private static ReportingSetupService.ProcessResult result(int exitCode, String output) {
        return new ReportingSetupService.ProcessResult(exitCode, output);
    }

    private static Map<String, String> snapshot(Path... roots) throws IOException {
        Map<String, String> snapshot = new TreeMap<>();
        for (Path root : roots) {
            if (Files.notExists(root)) continue;
            try (var entries = Files.walk(root)) {
                for (Path entry : entries.toList()) {
                    String key = root.getFileName() + "/" + root.relativize(entry).toString().replace('\\', '/');
                    snapshot.put(key, Files.isDirectory(entry) ? "directory"
                            : "sha256:" + VerifiedArtifactStore.digest(entry));
                }
            }
        }
        return snapshot;
    }

    private static ShaftCachePaths paths(Path root) {
        Path cache = root.resolve("cache");
        Path data = root.resolve("data");
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }
}
