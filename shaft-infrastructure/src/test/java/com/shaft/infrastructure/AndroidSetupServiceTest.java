package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AndroidSetupServiceTest {
    @Test
    @EnabledOnOs(OS.LINUX)
    void extractedLinuxSdkCommandsAreExecutableBeforeFirstInvocation(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path commandTools = createLinuxCommandToolsZip(temp.resolve("command-tools.zip"));
        AndroidSetupRequest request = AndroidSetupRequest.defaults();
        AndroidCommandRunner runner = (command, workingDirectory, environment, removed, input, log, timeout) -> {
            Path executable = Path.of(command.getFirst());
            assertTrue(Files.isExecutable(executable), "SDK command must be executable: " + executable);
            if (command.contains("--sdk_root=" + workingDirectory)) createLinuxSdkFixture(workingDirectory);
            return new ReportingSetupService.ProcessResult(0,
                    command.contains("--list_installed") ? exactInstalledPackages() : "fixture");
        };
        DefaultAndroidToolchainOperations operations = new DefaultAndroidToolchainOperations(paths,
                SetupPlatform.LINUX, SetupArchitecture.X64, request,
                action -> commandTools, runner, false);
        SetupAction sdkAction = AndroidSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64,
                SetupMode.MANAGED, request).actions().get(4);

        operations.install(sdkAction);

        assertEquals(SetupReadiness.READY, operations.status(sdkAction).readiness());
    }

    @Test
    void separateJvmInstallersConvergeOnOneReceiptAndPublicationSet(@TempDir Path temp) throws Exception {
        Path javaExecutable = Path.of(System.getProperty("java.home"), "bin",
                SetupPlatform.current() == SetupPlatform.WINDOWS ? "java.exe" : "java").toAbsolutePath();
        Path gate = temp.resolve("start.gate");
        List<Process> children = new ArrayList<>();
        try {
            for (int index = 0; index < 2; index++) {
                Path output = temp.resolve("child-" + index + ".log");
                children.add(new ProcessBuilder(javaExecutable.toString(), "-Xmx64m", "-XX:+UseSerialGC", "-cp",
                        System.getProperty("java.class.path"), SeparateProcessInstaller.class.getName(),
                        temp.toString(), gate.toString(), temp.resolve("result-" + index).toString())
                        .redirectErrorStream(true).redirectOutput(output.toFile()).start());
            }
            Files.writeString(gate, "go");

            for (int index = 0; index < children.size(); index++) {
                Process child = children.get(index);
                assertTrue(child.waitFor(15, java.util.concurrent.TimeUnit.SECONDS),
                        "Timed out: " + Files.readString(temp.resolve("child-" + index + ".log")));
                assertEquals(0, child.exitValue(), Files.readString(temp.resolve("child-" + index + ".log")));
            }

            try (var markers = Files.list(temp.resolve("data/markers"))) {
                assertEquals(6, markers.count());
            }
            String first = Files.readString(temp.resolve("result-0"));
            assertEquals(first, Files.readString(temp.resolve("result-1")));
            assertTrue(Files.isRegularFile(temp.resolve("data/receipts/mobile-android.json")));
        } finally {
            children.stream().filter(Process::isAlive).forEach(Process::destroyForcibly);
        }
    }

    @Test
    void threeJvmWaitersConvergeWithoutOverlappingMutation(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        AndroidSetupRequest request = AndroidSetupRequest.defaults();
        SetupPlan plan = AndroidSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64,
                SetupMode.MANAGED, request);
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.EPOCH,
                Set.of(AndroidSetupPlanner.ANDROID_SDK_LICENSE));
        ConvergingOperations operations = new ConvergingOperations();
        java.util.concurrent.ExecutorService executor = java.util.concurrent.Executors.newFixedThreadPool(3);
        java.util.concurrent.CountDownLatch callersReady = new java.util.concurrent.CountDownLatch(3);
        java.util.concurrent.CountDownLatch start = new java.util.concurrent.CountDownLatch(1);
        List<java.util.concurrent.Future<SetupReceipt>> futures = new ArrayList<>();
        try {
            for (int index = 0; index < 3; index++) {
                futures.add(executor.submit(() -> {
                    callersReady.countDown();
                    start.await();
                    return new AndroidSetupService(paths, SetupPlatform.LINUX, SetupArchitecture.X64,
                            request, operations, false).install(plan, approval);
                }));
            }
            assertTrue(callersReady.await(5, java.util.concurrent.TimeUnit.SECONDS));
            start.countDown();

            List<SetupReceipt> receipts = new ArrayList<>();
            for (java.util.concurrent.Future<SetupReceipt> future : futures) {
                receipts.add(future.get(10, java.util.concurrent.TimeUnit.SECONDS));
            }

            assertTrue(receipts.stream().allMatch(receipt -> receipt.planDigest().equals(plan.digest())
                    && receipt.completedActions().equals(plan.actions())));
            assertEquals(plan.actions().size(), operations.firstPublications.get());
            assertEquals(1, operations.maximumActiveMutations.get());
        } finally {
            executor.shutdownNow();
        }
    }

    @Test
    void retryRecoversAvdDirectoryPublishedBeforePointerFailure(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        AndroidSetupRequest request = AndroidSetupRequest.defaults().resolve(SetupArchitecture.X64);
        Path sdk = paths.tools().resolve("android-sdk/15859902-api36-x86_64");
        createSdkFixture(sdk);
        for (Path tool : List.of(sdk.resolve("cmdline-tools/latest/bin/sdkmanager.bat"),
                sdk.resolve("cmdline-tools/latest/bin/avdmanager.bat"))) {
            Files.createDirectories(tool.getParent());
            Files.writeString(tool, "fixture");
        }
        AndroidCommandRunner runner = (command, workingDirectory, environment, removed, input, log, timeout) -> {
            if (command.stream().anyMatch(part -> part.contains("avdmanager"))) {
                Path staging = Path.of(command.get(command.indexOf("--path") + 1));
                Files.createDirectories(staging);
                Files.writeString(staging.resolve("config.ini"), exactAvdConfig(request));
            }
            return new ReportingSetupService.ProcessResult(0, command.contains("--list_installed")
                    ? exactInstalledPackages() : "fixture");
        };
        DefaultAndroidToolchainOperations operations = new DefaultAndroidToolchainOperations(paths,
                SetupPlatform.WINDOWS, SetupArchitecture.X64, request,
                action -> { throw new AssertionError("AVD recovery must not fetch."); }, runner, false);
        SetupAction avdAction = AndroidSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, request).actions().getLast();
        Path avdHome = paths.tools().resolve("android-avd");
        Path pointer = avdHome.resolve(request.avdName() + ".ini");
        Files.createDirectories(pointer);

        assertThrows(IOException.class, () -> operations.install(avdAction));
        assertTrue(Files.isDirectory(avdHome.resolve(request.avdName() + ".avd")));
        Files.delete(pointer);

        operations.install(avdAction);

        assertTrue(Files.isRegularFile(pointer));
        assertEquals(SetupReadiness.READY, operations.status(avdAction).readiness());
    }

    @Test
    void sdkStatusRejectsWrongInstalledPackageRevision(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path sdk = paths.tools().resolve("android-sdk/15859902-api36-x86_64");
        createSdkFixture(sdk);
        for (Path tool : List.of(sdk.resolve("cmdline-tools/latest/bin/sdkmanager.bat"),
                sdk.resolve("cmdline-tools/latest/bin/avdmanager.bat"))) {
            Files.createDirectories(tool.getParent());
            Files.writeString(tool, "fixture");
        }
        AndroidCommandRunner runner = (command, workingDirectory, environment, removed, input, log, timeout) ->
                new ReportingSetupService.ProcessResult(0,
                        "Installed packages:\nPath | Version | Description\n"
                                + "platform-tools | 37.0.1 | fixture\n"
                                + "emulator | 37.1.11 | fixture\n"
                                + "platforms;android-36 | 2 | fixture\n"
                                + "build-tools;36.0.0 | 35.0.0 | fixture\n"
                                + "system-images;android-36;google_apis;x86_64 | 7 | fixture\n");
        DefaultAndroidToolchainOperations operations = new DefaultAndroidToolchainOperations(paths,
                SetupPlatform.WINDOWS, SetupArchitecture.X64, AndroidSetupRequest.defaults(),
                action -> { throw new AssertionError("Status must not fetch."); }, runner, false);
        SetupAction sdkAction = AndroidSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, AndroidSetupRequest.defaults()).actions().get(4);

        SetupStatus status = operations.status(sdkAction);

        assertEquals(SetupReadiness.DEGRADED, status.readiness());
        assertTrue(status.detail().contains("build-tools;36.0.0"));
        assertTrue(status.detail().contains("35.0.0"));
    }

    @Test
    void avdStatusUsesOfficialAccelerationProbeAndReportsMissingHostSupport(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        AndroidSetupRequest request = AndroidSetupRequest.defaults().resolve(SetupArchitecture.X64);
        Path sdk = paths.tools().resolve("android-sdk/15859902-api36-x86_64");
        createSdkFixture(sdk);
        Path avdHome = paths.tools().resolve("android-avd");
        Path avd = avdHome.resolve(request.avdName() + ".avd");
        Files.createDirectories(avd);
        Files.writeString(avd.resolve("config.ini"), exactAvdConfig(request));
        Files.writeString(avd.resolve("shaft-request.properties"), requestMetadata(request));
        Files.writeString(avdHome.resolve(request.avdName() + ".ini"),
                "path=" + avd.toAbsolutePath().normalize() + System.lineSeparator()
                        + "path.rel=avd/" + request.avdName() + ".avd" + System.lineSeparator()
                        + "target=android-" + request.apiLevel() + System.lineSeparator());
        List<List<String>> commands = new ArrayList<>();
        AndroidCommandRunner runner = (command, workingDirectory, environment, removed, input, log, timeout) -> {
            commands.add(List.copyOf(command));
            if (command.contains("-accel-check")) {
                return new ReportingSetupService.ProcessResult(1, "accel: host virtualization is unavailable");
            }
            return new ReportingSetupService.ProcessResult(0, "fixture");
        };
        DefaultAndroidToolchainOperations operations = new DefaultAndroidToolchainOperations(paths,
                SetupPlatform.WINDOWS, SetupArchitecture.X64, request,
                action -> { throw new AssertionError("Status must not fetch."); }, runner, false);
        SetupAction avdAction = AndroidSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, request).actions().getLast();

        SetupStatus status = operations.status(avdAction);

        assertEquals(SetupReadiness.DEGRADED, status.readiness());
        assertTrue(status.detail().contains("host virtualization is unavailable"));
        assertTrue(commands.stream().anyMatch(command -> command.equals(List.of(
                sdk.resolve("emulator/emulator.exe").toString(), "-accel-check"))));
    }

    @Test
    void avdStatusRejectsConfigPointingAtDifferentSystemImage(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        AndroidSetupRequest request = AndroidSetupRequest.defaults().resolve(SetupArchitecture.X64);
        Path sdk = paths.tools().resolve("android-sdk/15859902-api36-x86_64");
        createSdkFixture(sdk);
        Path avdHome = paths.tools().resolve("android-avd");
        Path avd = avdHome.resolve(request.avdName() + ".avd");
        Files.createDirectories(avd);
        Files.writeString(avd.resolve("config.ini"),
                "image.sysdir.1=system-images/android-35/default/x86_64/\n");
        Files.writeString(avd.resolve("shaft-request.properties"), requestMetadata(request));
        Files.writeString(avdHome.resolve(request.avdName() + ".ini"),
                "path=" + avd.toAbsolutePath().normalize() + System.lineSeparator()
                        + "path.rel=avd/" + request.avdName() + ".avd" + System.lineSeparator()
                        + "target=android-" + request.apiLevel() + System.lineSeparator());
        AndroidCommandRunner runner = (command, workingDirectory, environment, removed, input, log, timeout) ->
                new ReportingSetupService.ProcessResult(0, "accel: available");
        DefaultAndroidToolchainOperations operations = new DefaultAndroidToolchainOperations(paths,
                SetupPlatform.WINDOWS, SetupArchitecture.X64, request,
                action -> { throw new AssertionError("Status must not fetch."); }, runner, false);
        SetupAction avdAction = AndroidSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, request).actions().getLast();

        SetupStatus status = operations.status(avdAction);

        assertEquals(SetupReadiness.DEGRADED, status.readiness());
        assertTrue(status.detail().contains("system image"));
    }

    @Test
    void wrongRegisteredAppiumExtensionFailsBeforePublication(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path nodeArchive = ReportingSetupServiceTest.createNodeZip(temp.resolve("node.zip"));
        Path npmArchive = Files.writeString(temp.resolve("package.tgz"), "package");
        AndroidSetupRequest request = AndroidSetupRequest.defaults();
        AndroidCommandRunner runner = (command, workingDirectory, environment, removed, input, log, timeout) -> {
            int prefix = command.indexOf("--prefix");
            if (command.contains("ci") && prefix >= 0) createAppiumFixture(Path.of(command.get(prefix + 1)));
            String output;
            if (command.contains("plugin")) {
                output = "{\"inspector\":{\"pkgName\":\"appium-inspector-plugin\",\"version\":\"0.0.0\"}}";
            } else if (command.contains("driver")) {
                output = "{\"uiautomator2\":{\"pkgName\":\"appium-uiautomator2-driver\",\"version\":\"8.2.2\"}}";
            } else {
                output = command.stream().anyMatch(part -> part.endsWith("appium/index.js")
                        || part.endsWith("appium\\index.js")) ? "3.6.0" : "v24.19.0";
            }
            return new ReportingSetupService.ProcessResult(0, output);
        };
        DefaultAndroidToolchainOperations operations = new DefaultAndroidToolchainOperations(paths,
                SetupPlatform.WINDOWS, SetupArchitecture.X64, request,
                action -> action.target() == SetupTarget.NODE ? nodeArchive : npmArchive, runner, false);
        AndroidSetupService service = new AndroidSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, request, operations, false);
        SetupPlan plan = AndroidSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, request);

        SetupExecutionException failure = assertThrows(SetupExecutionException.class, () -> service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH,
                        Set.of(AndroidSetupPlanner.ANDROID_SDK_LICENSE))));

        assertTrue(failure.getCause().getCause().getMessage().contains("not registered at the approved version"));
        assertFalse(Files.exists(paths.tools().resolve("appium/3.6.0")));
        assertFalse(Files.exists(paths.receipts().resolve("mobile-android.json")));
    }

    @Test
    void missingAndroidLicenseIsRejectedBeforePreflightOrFilesystemMutation(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        RecordingOperations operations = new RecordingOperations();
        AndroidSetupService service = new AndroidSetupService(paths, SetupPlatform.LINUX,
                SetupArchitecture.X64, AndroidSetupRequest.defaults(), operations, false);
        SetupPlan plan = AndroidSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64,
                SetupMode.MANAGED, AndroidSetupRequest.defaults());

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class, () -> service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of())));
        assertTrue(failure.getMessage().contains(AndroidSetupPlanner.ANDROID_SDK_LICENSE));

        assertEquals(0, operations.preflights);
        assertTrue(operations.installs.isEmpty());
        assertFalse(Files.exists(paths.cacheRoot()));
        assertFalse(Files.exists(paths.dataRoot()));
    }

    @Test
    void exactApprovedPlanInstallsInOrderAndWritesOneCompleteReceipt(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        RecordingOperations operations = new RecordingOperations();
        AndroidSetupRequest request = new AndroidSetupRequest(36, "pixel_8", "google_apis", "x86_64",
                "approved_avd", 6144, 4, 4823);
        AndroidSetupService service = new AndroidSetupService(paths, SetupPlatform.LINUX,
                SetupArchitecture.X64, request, operations, false);
        SetupPlan plan = AndroidSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64,
                SetupMode.MANAGED, request);

        SetupReceipt receipt = service.install(plan, new SetupApproval(plan.digest(), Instant.EPOCH,
                Set.of(AndroidSetupPlanner.ANDROID_SDK_LICENSE)));

        assertEquals(2, operations.preflights, "Preflight must run before mutation and again under the lock.");
        assertEquals(plan.actions(), operations.installs);
        assertEquals(plan.actions(), receipt.completedActions());
        assertTrue(Files.isRegularFile(paths.receipts().resolve("mobile-android.json")));
        assertEquals(SetupReadiness.READY, service.status().readiness());
    }

    @Test
    void failedActionExposesPartialReceiptAndNeverWritesFinalReceipt(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        RecordingOperations operations = new RecordingOperations();
        operations.failTarget = SetupTarget.ANDROID_SDK;
        AndroidSetupService service = new AndroidSetupService(paths, SetupPlatform.LINUX,
                SetupArchitecture.X64, AndroidSetupRequest.defaults(), operations, false);
        SetupPlan plan = AndroidSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64,
                SetupMode.MANAGED, AndroidSetupRequest.defaults());

        SetupExecutionException failure = assertThrows(SetupExecutionException.class, () -> service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH,
                        Set.of(AndroidSetupPlanner.ANDROID_SDK_LICENSE))));

        assertEquals(SetupTarget.ANDROID_SDK, failure.failedAction().target());
        assertEquals(plan.actions().subList(0, 4), failure.partialReceipt().completedActions());
        assertFalse(Files.exists(paths.receipts().resolve("mobile-android.json")));
    }

    @Test
    void readyFilesWithoutACompatibleReceiptAreDegraded(@TempDir Path temp) {
        RecordingOperations operations = new RecordingOperations();
        SetupPlan plan = AndroidSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64,
                SetupMode.MANAGED, AndroidSetupRequest.defaults());
        plan.actions().forEach(action -> operations.readiness.put(action.target(), SetupReadiness.READY));
        AndroidSetupService service = new AndroidSetupService(paths(temp), SetupPlatform.LINUX,
                SetupArchitecture.X64, AndroidSetupRequest.defaults(), operations, false);

        assertEquals(SetupReadiness.DEGRADED, service.status().readiness());
    }

    @Test
    void partialOfflineInstallationStartsNoProbeOrMutation(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path node = paths.tools().resolve("node/24.19.0/windows-x64/node.exe");
        Files.createDirectories(node.getParent());
        Files.writeString(node, "partial node");
        java.util.concurrent.atomic.AtomicInteger processes = new java.util.concurrent.atomic.AtomicInteger();
        AndroidCommandRunner runner = (command, workingDirectory, environment, removed, input, log, timeout) -> {
            processes.incrementAndGet();
            throw new AssertionError("Offline partial preflight must not execute tools.");
        };
        DefaultAndroidToolchainOperations operations = new DefaultAndroidToolchainOperations(paths,
                SetupPlatform.WINDOWS, SetupArchitecture.X64, AndroidSetupRequest.defaults(),
                action -> { throw new AssertionError("Offline partial preflight must not fetch."); }, runner, true);
        AndroidSetupService service = new AndroidSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, AndroidSetupRequest.defaults(), operations, true);
        SetupPlan plan = AndroidSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, AndroidSetupRequest.defaults());

        IOException failure = assertThrows(IOException.class, () -> service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH,
                        Set.of(AndroidSetupPlanner.ANDROID_SDK_LICENSE))));

        assertTrue(failure.getMessage().contains("complete verified installation"));
        assertEquals(0, processes.get());
        assertFalse(Files.exists(paths.state()));
        assertFalse(Files.exists(paths.receipts()));
    }

    @Test
    void realOperationsInstallAndVerifyAppiumBuildToolsAndAvdWithoutGlobalTools(@TempDir Path temp)
            throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path nodeArchive = ReportingSetupServiceTest.createNodeZip(temp.resolve("node.zip"));
        Path commandTools = createCommandToolsZip(temp.resolve("command-tools.zip"));
        Path npmArchive = Files.writeString(temp.resolve("package.tgz"), "package");
        AndroidSetupRequest request = new AndroidSetupRequest(36, "pixel_8", "google_apis", "x86_64",
                "integration_avd", 4096, 2, 4723);
        FullInstallRunner runner = new FullInstallRunner();
        ReportingSetupService.ArtifactFetcher fetcher = fixtureFetcher(nodeArchive, commandTools, npmArchive);
        DefaultAndroidToolchainOperations operations = new DefaultAndroidToolchainOperations(paths,
                SetupPlatform.WINDOWS, SetupArchitecture.X64, request, fetcher, runner, false);
        AndroidSetupService service = new AndroidSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, request, operations, false);
        SetupPlan plan = AndroidSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, request);

        SetupReceipt receipt = service.install(plan, new SetupApproval(plan.digest(), Instant.EPOCH,
                Set.of(AndroidSetupPlanner.ANDROID_SDK_LICENSE)));

        assertEquals(plan.actions(), receipt.completedActions());
        assertEquals(SetupReadiness.READY, service.status().readiness());
        assertTrue(Files.isRegularFile(paths.tools().resolve("android-sdk/15859902-api36-x86_64/"
                + "build-tools/36.0.0/aapt2.exe")));
        String extensionManifest = Files.readString(paths.tools().resolve(
                "appium/3.6.0/node_modules/.cache/appium/extensions.yaml"));
        assertFalse(extensionManifest.contains(".staging-"));
        assertTrue(extensionManifest.contains(paths.tools().resolve("appium/3.6.0").toString()));
        assertTrue(runner.commands.stream().noneMatch(command -> command.contains("--licenses")));
        assertEquals(List.of("y\n"), runner.sdkPackageInputs);
        assertTrue(runner.commands.stream().anyMatch(command -> command.contains("build-tools;36.0.0")));
        assertTrue(runner.commands.stream().anyMatch(command -> command.contains("--list_installed")));
        assertTrue(runner.commands.stream().anyMatch(command -> command.stream()
                .anyMatch(part -> part.endsWith("aapt2.exe"))
                && command.contains("version")));
        assertTrue(runner.commands.stream().anyMatch(command -> command.containsAll(
                List.of("driver", "list", "--installed", "--json"))));
        assertTrue(runner.commands.stream().anyMatch(command -> command.containsAll(
                List.of("plugin", "list", "--installed", "--json"))));
        assertTrue(runner.commands.stream().noneMatch(command -> command.contains("--relaxed-security")));
    }

    private static ReportingSetupService.ArtifactFetcher fixtureFetcher(Path nodeArchive, Path commandTools,
                                                                          Path npmArchive) {
        return action -> switch (action.target()) {
            case NODE -> nodeArchive;
            case ANDROID_SDK -> commandTools;
            default -> npmArchive;
        };
    }

    private static ShaftCachePaths paths(Path temp) {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }

    private static Path createCommandToolsZip(Path destination) throws IOException {
        try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(destination))) {
            for (String name : List.of("cmdline-tools/bin/sdkmanager.bat",
                    "cmdline-tools/bin/avdmanager.bat", "cmdline-tools/lib/repository.jar")) {
                output.putNextEntry(new ZipEntry(name));
                output.write("fixture".getBytes(java.nio.charset.StandardCharsets.UTF_8));
                output.closeEntry();
            }
        }
        return destination;
    }

    private static Path createLinuxCommandToolsZip(Path destination) throws IOException {
        try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(destination))) {
            for (String name : List.of("cmdline-tools/bin/sdkmanager",
                    "cmdline-tools/bin/avdmanager", "cmdline-tools/lib/repository.jar")) {
                output.putNextEntry(new ZipEntry(name));
                output.write("fixture".getBytes(java.nio.charset.StandardCharsets.UTF_8));
                output.closeEntry();
            }
        }
        return destination;
    }

    private static void createAppiumFixture(Path staging) throws IOException {
        Files.createDirectories(staging.resolve("node_modules/appium"));
        Files.createDirectories(staging.resolve("node_modules/appium-inspector-plugin"));
        Files.createDirectories(staging.resolve("node_modules/appium-uiautomator2-driver"));
        Files.writeString(staging.resolve("node_modules/appium/index.js"), "appium");
        Files.writeString(staging.resolve("node_modules/appium/package.json"), "{\"version\":\"3.6.0\"}");
        Files.writeString(staging.resolve("node_modules/appium-inspector-plugin/package.json"),
                "{\"version\":\"2026.7.1\"}");
        Files.writeString(staging.resolve("node_modules/appium-uiautomator2-driver/package.json"),
                "{\"version\":\"8.2.2\"}");
    }

    private static void createSdkFixture(Path root) throws IOException {
        for (Path file : List.of(root.resolve("platform-tools/adb.exe"), root.resolve("emulator/emulator.exe"),
                root.resolve("platforms/android-36/android.jar"), root.resolve("build-tools/36.0.0/aapt2.exe"),
                root.resolve("system-images/android-36/google_apis/x86_64/package.xml"))) {
            Files.createDirectories(file.getParent());
            Files.writeString(file, "fixture");
        }
    }

    private static String requestMetadata(AndroidSetupRequest request) {
        return String.join("\n", "api=" + request.apiLevel(), "device=" + request.deviceProfile(),
                "tag=" + request.imageTag(), "abi=" + request.abi(), "avd=" + request.avdName(),
                "ramMb=" + request.ramMb(), "cores=" + request.cores(), "port=" + request.appiumPort()) + "\n";
    }

    private static String exactAvdConfig(AndroidSetupRequest request) {
        return "image.sysdir.1=system-images/android-" + request.apiLevel() + '/' + request.imageTag() + '/'
                + request.abi() + "/\n";
    }

    private static void createLinuxSdkFixture(Path root) throws IOException {
        for (Path file : List.of(root.resolve("platform-tools/adb"), root.resolve("emulator/emulator"),
                root.resolve("platforms/android-36/android.jar"), root.resolve("build-tools/36.0.0/aapt2"),
                root.resolve("system-images/android-36/google_apis/x86_64/package.xml"))) {
            Files.createDirectories(file.getParent());
            Files.writeString(file, "fixture");
            assertTrue(file.toFile().setExecutable(true, false));
        }
    }

    private static String exactInstalledPackages() {
        return "Installed packages:\nPath | Version | Description\n"
                + "platform-tools | 37.0.1 | fixture\n"
                + "emulator | 37.1.11 | fixture\n"
                + "platforms;android-36 | 2 | fixture\n"
                + "build-tools;36.0.0 | 36.0.0 | fixture\n"
                + "system-images;android-36;google_apis;x86_64 | 7 | fixture\n";
    }

    private static final class FullInstallRunner implements AndroidCommandRunner {
        private final List<List<String>> commands = new ArrayList<>();
        private final List<String> sdkPackageInputs = new ArrayList<>();

        @Override
        public ReportingSetupService.ProcessResult run(List<String> command, Path workingDirectory,
                                                       Map<String, String> environment, Set<String> removed,
                                                       String input, Path log, Duration timeout) throws IOException {
            commands.add(List.copyOf(command));
            recordExtensionManifest(command, workingDirectory);
            createInstalledFixtures(command, workingDirectory, input);
            return new ReportingSetupService.ProcessResult(0, output(command));
        }

        private void recordExtensionManifest(List<String> command, Path workingDirectory) throws IOException {
            if (!command.contains("driver") && !command.contains("plugin")) return;
            Path manifest = workingDirectory.resolve("node_modules/.cache/appium/extensions.yaml");
            if (Files.exists(manifest)) return;
            Files.createDirectories(manifest.getParent());
            Files.writeString(manifest, "installPath: " + workingDirectory);
        }

        private void createInstalledFixtures(List<String> command, Path workingDirectory, String input)
                throws IOException {
            createAppiumInstall(command);
            createSdkInstall(command, workingDirectory, input);
            createAvdInstall(command);
        }

        private static void createAppiumInstall(List<String> command) throws IOException {
            int prefix = command.indexOf("--prefix");
            if (command.contains("ci") && prefix >= 0) {
                createAppiumFixture(Path.of(command.get(prefix + 1)));
            }
        }

        private void createSdkInstall(List<String> command, Path workingDirectory, String input) throws IOException {
            boolean sdkManager = command.stream().anyMatch(part -> part.contains("sdkmanager"));
            if (!sdkManager || command.contains("--licenses")) return;
            createSdkFixture(workingDirectory);
            if (command.stream().anyMatch(part -> part.startsWith("system-images;"))) {
                sdkPackageInputs.add(input);
            }
        }

        private static void createAvdInstall(List<String> command) throws IOException {
            int avdPath = command.indexOf("--path");
            boolean avdManager = command.stream().anyMatch(part -> part.contains("avdmanager"));
            if (!avdManager || avdPath < 0) return;
            Path root = Path.of(command.get(avdPath + 1));
            Files.createDirectories(root);
            Files.writeString(root.resolve("config.ini"),
                    "image.sysdir.1=system-images/android-36/google_apis/x86_64\n");
        }

        private static String output(List<String> command) {
            if (command.contains("--list_installed")) return exactInstalledPackages();
            if (command.contains("driver")) return "dbug Appium refreshed extension cache\n"
                    + "{\"uiautomator2\":{\"pkgName\":\"appium-uiautomator2-driver\",\"version\":\"8.2.2\"}}";
            if (command.contains("plugin")) return "dbug Appium refreshed extension cache\n"
                    + "{\"inspector\":{\"pkgName\":\"appium-inspector-plugin\",\"version\":\"2026.7.1\"}}";
            return isAppium(command) ? "dbug Appium refreshed extension cache\n3.6.0" : "v24.19.0";
        }

        private static boolean isAppium(List<String> command) {
            return command.stream().anyMatch(part -> part.endsWith("appium/index.js")
                    || part.endsWith("appium\\index.js"));
        }
    }

    private static final class RecordingOperations implements AndroidToolchainOperations {
        private final List<SetupAction> installs = new ArrayList<>();
        private final EnumMap<SetupTarget, SetupReadiness> readiness = new EnumMap<>(SetupTarget.class);
        private int preflights;
        private SetupTarget failTarget;

        @Override
        public void preflight(List<SetupAction> actions, boolean offline) {
            preflights++;
        }

        @Override
        public void install(SetupAction action) throws IOException {
            if (action.target() == failTarget) throw new IOException("simulated " + failTarget + " failure");
            installs.add(action);
            readiness.put(action.target(), SetupReadiness.READY);
        }

        @Override
        public SetupStatus status(SetupAction action) {
            SetupReadiness state = readiness.getOrDefault(action.target(), SetupReadiness.MISSING);
            return new SetupStatus(action.target(), state, state == SetupReadiness.READY ? action.version() : "",
                    state == SetupReadiness.READY ? "Verified test installation." : "Not installed.");
        }
    }

    private static final class ConvergingOperations implements AndroidToolchainOperations {
        private final Set<SetupTarget> installed = java.util.concurrent.ConcurrentHashMap.newKeySet();
        private final java.util.concurrent.atomic.AtomicInteger preflights = new java.util.concurrent.atomic.AtomicInteger();
        private final java.util.concurrent.atomic.AtomicInteger activeMutations = new java.util.concurrent.atomic.AtomicInteger();
        private final java.util.concurrent.atomic.AtomicInteger maximumActiveMutations =
                new java.util.concurrent.atomic.AtomicInteger();
        private final java.util.concurrent.atomic.AtomicInteger firstPublications =
                new java.util.concurrent.atomic.AtomicInteger();

        @Override
        public void preflight(List<SetupAction> actions, boolean offline) {
            preflights.incrementAndGet();
        }

        @Override
        public void install(SetupAction action) throws IOException {
            int active = activeMutations.incrementAndGet();
            maximumActiveMutations.accumulateAndGet(active, Math::max);
            try {
                if (firstPublications.get() == 0) {
                    long deadline = System.nanoTime() + Duration.ofSeconds(5).toNanos();
                    while (preflights.get() < 4 && System.nanoTime() < deadline) Thread.onSpinWait();
                    if (preflights.get() < 4) throw new IOException("Concurrent waiters did not reach preflight.");
                }
                if (installed.add(action.target())) firstPublications.incrementAndGet();
            } finally {
                activeMutations.decrementAndGet();
            }
        }

        @Override
        public SetupStatus status(SetupAction action) {
            return new SetupStatus(action.target(), installed.contains(action.target())
                    ? SetupReadiness.READY : SetupReadiness.MISSING, action.version(), "fixture");
        }
    }

    public static final class SeparateProcessInstaller {
        public static void main(String[] args) throws Exception {
            Path root = Path.of(args[0]).toAbsolutePath();
            Path gate = Path.of(args[1]);
            long deadline = System.nanoTime() + Duration.ofSeconds(10).toNanos();
            while (Files.notExists(gate) && System.nanoTime() < deadline) Thread.sleep(Duration.ofMillis(10));
            if (Files.notExists(gate)) throw new IOException("Timed out waiting for the installer gate.");
            Path cache = root.resolve("cache");
            Path data = root.resolve("data");
            ShaftCachePaths paths = new ShaftCachePaths(cache, data, cache.resolve("downloads"),
                    data.resolve("tools"), data.resolve("state"), data.resolve("receipts"));
            AndroidSetupRequest request = AndroidSetupRequest.defaults();
            SetupPlan plan = AndroidSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64,
                    SetupMode.MANAGED, request);
            SetupReceipt receipt = new AndroidSetupService(paths, SetupPlatform.LINUX, SetupArchitecture.X64,
                    request, new FileConvergingOperations(data.resolve("markers")), false).install(plan,
                    new SetupApproval(plan.digest(), Instant.EPOCH,
                            Set.of(AndroidSetupPlanner.ANDROID_SDK_LICENSE)));
            Files.writeString(Path.of(args[2]), receipt.planDigest());
        }
    }

    private record FileConvergingOperations(Path markers) implements AndroidToolchainOperations {
        @Override public void preflight(List<SetupAction> actions, boolean offline) { }

        @Override
        public void install(SetupAction action) throws IOException {
            Files.createDirectories(markers);
            try {
                Files.createFile(markers.resolve(action.target().name()));
            } catch (java.nio.file.FileAlreadyExistsException alreadyPublished) {
                // The other process published this exact immutable target under the shared setup lock.
            }
        }

        @Override
        public SetupStatus status(SetupAction action) {
            return new SetupStatus(action.target(), Files.isRegularFile(markers.resolve(action.target().name()))
                    ? SetupReadiness.READY : SetupReadiness.MISSING, action.version(), "fixture");
        }
    }
}
