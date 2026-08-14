package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PlaywrightSetupServiceTest {
    @Test
    void separateJvmProcessesConvergeOnOneInstallation(@TempDir Path temp) throws Exception {
        Path java = Path.of(System.getProperty("java.home"), "bin",
                SetupPlatform.current() == SetupPlatform.WINDOWS ? "java.exe" : "java");
        List<String> command = List.of(java.toString(), "-cp", System.getProperty("java.class.path"),
                PlaywrightSetupProcessProbe.class.getName(), temp.toString());
        Process first = new ProcessBuilder(command).redirectErrorStream(true).start();
        Process second = new ProcessBuilder(command).redirectErrorStream(true).start();
        try {
            assertTrue(first.waitFor(20, TimeUnit.SECONDS), "first setup process timed out");
            assertTrue(second.waitFor(20, TimeUnit.SECONDS), "second setup process timed out");
            String firstOutput = read(first);
            String secondOutput = read(second);
            assertEquals(0, first.exitValue(), firstOutput);
            assertEquals(0, second.exitValue(), secondOutput);
        } finally {
            if (first.isAlive()) first.destroyForcibly();
            if (second.isAlive()) second.destroyForcibly();
        }

        assertEquals(List.of("installed"), Files.readAllLines(temp.resolve("install-count.txt")));
        assertTrue(Files.isRegularFile(PlaywrightSetupProcessProbe.paths(temp).receipts()
                .resolve("playwright.json")));
    }

    @Test
    void threeJvmWaitersConvergeOnOnePublishedReceipt(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path node = Files.writeString(temp.resolve("node.exe"), "node");
        AtomicInteger fetches = new AtomicInteger();
        AtomicInteger installs = new AtomicInteger();
        CountDownLatch installerEntered = new CountDownLatch(1);
        CountDownLatch allowPublication = new CountDownLatch(1);
        PlaywrightSetupService.ArtifactFetcher fetcher = action -> Files.writeString(
                temp.resolve("archive-" + fetches.incrementAndGet()), action.version());
        PlaywrightSetupService.BrowserInstaller installer = (nodePath, driverRoot, browserRoot,
                                                               archives, log, timeout) -> {
            installs.incrementAndGet();
            installerEntered.countDown();
            try {
                if (!allowPublication.await(5, TimeUnit.SECONDS)) throw new java.io.IOException("test timeout");
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                throw new java.io.IOException("test interrupted", interrupted);
            }
            createReadyWindowsLayout(browserRoot);
        };
        PlaywrightSetupService.DriverExtractor extractor = (nodePath, destination) -> {
            Path cli = destination.resolve("package/cli.js");
            Files.createDirectories(cli.getParent());
            Files.writeString(cli, "cli");
        };
        List<PlaywrightSetupService> services = java.util.stream.IntStream.range(0, 3)
                .mapToObj(ignored -> new PlaywrightSetupService(paths, SetupPlatform.WINDOWS,
                        SetupArchitecture.X64, readyNode(node), fetcher, extractor, installer)).toList();
        SetupPlan plan = PlaywrightSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.EPOCH, Set.of());
        try (var executor = java.util.concurrent.Executors.newFixedThreadPool(3)) {
            List<java.util.concurrent.Future<SetupReceipt>> futures = services.stream()
                    .map(service -> executor.submit(() -> service.install(plan, approval))).toList();
            assertTrue(installerEntered.await(5, TimeUnit.SECONDS));
            allowPublication.countDown();
            List<SetupReceipt> receipts = new ArrayList<>();
            for (var future : futures) receipts.add(future.get(10, TimeUnit.SECONDS));

            assertEquals(1, installs.get());
            assertEquals(6, fetches.get());
            assertTrue(receipts.stream().allMatch(receipt -> receipt.equals(receipts.getFirst())));
        }
    }

    @Test
    void ubuntuInstallPublishesFiveArtifactLayoutAndBecomesReady(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path node = Files.writeString(temp.resolve("node"), "node");
        AtomicInteger fetched = new AtomicInteger();
        PlaywrightSetupService service = new PlaywrightSetupService(paths,
                PlaywrightHostPlatform.UBUNTU_24_04_X64, SetupArchitecture.X64, readyNode(node),
                action -> Files.writeString(temp.resolve("archive-" + fetched.incrementAndGet()), action.version()),
                (nodePath, destination) -> {
                    Path cli = destination.resolve("package/cli.js");
                    Files.createDirectories(cli.getParent());
                    Files.writeString(cli, "cli");
                },
                (nodePath, driverRoot, browserRoot, archives, log, timeout) -> createReadyUbuntuLayout(browserRoot));
        SetupPlan plan = PlaywrightSetupPlanner.plan(PlaywrightHostPlatform.UBUNTU_24_04_X64,
                SetupMode.MANAGED);

        service.install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()));

        assertEquals(5, fetched.get());
        assertEquals("ubuntu24.04-x64", service.browserRoot().getFileName().toString());
        assertEquals(SetupReadiness.READY, service.status().readiness());
        assertFalse(Files.exists(service.browserRoot().resolve("winldd-1007")));
    }

    @Test
    void installPublishesOneExactBrowserRootAndIsIdempotent(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        AtomicBoolean nodeReady = new AtomicBoolean();
        AtomicInteger nodeInstalls = new AtomicInteger();
        Path node = temp.resolve("node.exe");
        PlaywrightSetupService.NodeOwner nodeOwner = new PlaywrightSetupService.NodeOwner() {
            @Override public SetupReadiness readiness() {
                return nodeReady.get() ? SetupReadiness.READY : SetupReadiness.MISSING;
            }
            @Override public void install(SetupAction action) throws java.io.IOException {
                nodeInstalls.incrementAndGet();
                Files.writeString(node, "node");
                nodeReady.set(true);
            }
            @Override public Path executable() { return node; }
        };
        List<String> fetched = new ArrayList<>();
        PlaywrightSetupService.ArtifactFetcher fetcher = action -> {
            fetched.add(action.version());
            return Files.writeString(temp.resolve(action.target() + "-" + fetched.size() + ".zip"), action.version());
        };
        AtomicInteger installs = new AtomicInteger();
        PlaywrightSetupService.BrowserInstaller installer = (nodePath, driverRoot, browserRoot, archives, log, timeout) -> {
            installs.incrementAndGet();
            createReadyWindowsLayout(browserRoot);
        };
        PlaywrightSetupService.DriverExtractor driverExtractor = (nodePath, destination) -> {
            Path cli = destination.resolve("package/cli.js");
            Files.createDirectories(cli.getParent());
            Files.writeString(cli, "cli");
        };
        PlaywrightSetupService service = new PlaywrightSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, nodeOwner, fetcher, driverExtractor, installer);
        SetupPlan plan = PlaywrightSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.EPOCH, Set.of());

        SetupReceipt receipt = service.install(plan, approval);
        SetupReceipt repeated = service.install(plan, approval);

        assertEquals(plan.actions(), receipt.completedActions());
        assertEquals(receipt.planDigest(), repeated.planDigest());
        assertEquals(1, nodeInstalls.get());
        assertEquals(6, fetched.size());
        assertEquals(1, installs.get());
        assertEquals(SetupReadiness.READY, service.status().readiness());
        assertTrue(Files.isRegularFile(paths.receipts().resolve("playwright.json")));
        assertTrue(Files.isRegularFile(service.browserRoot().resolve("SHAFT_PLAYWRIGHT_VERSION")));
    }

    @Test
    void linkedInnerBrowserDirectoryIsNeverAcceptedAsManaged(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path node = Files.writeString(temp.resolve("node.exe"), "node");
        PlaywrightSetupService service = new PlaywrightSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, readyNode(node), action -> { throw new AssertionError(); },
                (nodePath, destination) -> { throw new AssertionError(); },
                (nodePath, driverRoot, browserRoot, archives, log, timeout) -> { throw new AssertionError(); });
        Path root = service.browserRoot();
        createReadyWindowsLayout(root);
        Path external = temp.resolve("external-chromium");
        Files.move(root.resolve("chromium-1234"), external);
        try {
            Files.createSymbolicLink(root.resolve("chromium-1234"), external);
        } catch (UnsupportedOperationException | java.io.IOException unsupported) {
            org.junit.jupiter.api.Assumptions.abort("Symbolic links unavailable: " + unsupported.getMessage());
        }

        SetupStatus chromium = service.status().targets().stream()
                .filter(status -> status.target() == SetupTarget.PLAYWRIGHT_CHROMIUM).findFirst().orElseThrow();

        assertEquals(SetupReadiness.DEGRADED, chromium.readiness());
        assertTrue(Files.isRegularFile(external.resolve("chrome-win64/chrome.exe")));
    }

    @Test
    void transitiveFetchFailureReturnsNodeOnlyPartialReceiptAndPublishesNothing(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        AtomicBoolean nodeReady = new AtomicBoolean();
        Path node = temp.resolve("node.exe");
        PlaywrightSetupService.NodeOwner nodeOwner = new PlaywrightSetupService.NodeOwner() {
            @Override public SetupReadiness readiness() {
                return nodeReady.get() ? SetupReadiness.READY : SetupReadiness.MISSING;
            }
            @Override public void install(SetupAction action) throws java.io.IOException {
                Files.writeString(node, "node");
                nodeReady.set(true);
            }
            @Override public Path executable() { return node; }
        };
        PlaywrightSetupService service = new PlaywrightSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, nodeOwner, action -> {
                    if (action.version().contains("chromium-headless-shell")) throw new java.io.IOException("offline");
                    return Files.writeString(temp.resolve(action.version().replace(':', '-') + ".zip"), "archive");
                }, (nodePath, destination) -> { throw new AssertionError("driver extraction must not start"); },
                (nodePath, driverRoot, browserRoot, archives, log, timeout) -> {
                    throw new AssertionError("browser installation must not start");
                });
        SetupPlan plan = PlaywrightSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);

        SetupExecutionException failure = assertThrows(SetupExecutionException.class,
                () -> service.install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of())));

        assertEquals(SetupTarget.PLAYWRIGHT_CHROMIUM, failure.failedAction().target());
        assertEquals(List.of(SetupTarget.NODE), failure.partialReceipt().completedActions().stream()
                .map(SetupAction::target).toList());
        assertFalse(Files.exists(service.browserRoot()));
        assertFalse(Files.exists(paths.receipts().resolve("playwright.json")));
        Path browserParent = service.browserRoot().getParent();
        if (Files.exists(browserParent)) {
            try (var siblings = Files.list(browserParent)) {
                assertTrue(siblings.noneMatch(path -> path.getFileName().toString().contains("staging")));
            }
        }
    }

    @Test
    void driverExtractionFailureReturnsPartialReceiptAndCleansTransaction(@TempDir Path temp) throws Exception {
        assertPostFetchFailure(temp, true);
    }

    @Test
    void browserInstallerFailureReturnsPartialReceiptAndCleansTransaction(@TempDir Path temp) throws Exception {
        assertPostFetchFailure(temp, false);
    }

    @Test
    void receiptPrepublicationFailureNeverPublishesBrowsers(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path node = Files.writeString(temp.resolve("node.exe"), "node");
        PlaywrightSetupService service = new PlaywrightSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, readyNode(node), action -> Files.writeString(
                temp.resolve(action.version().replace(':', '-') + ".zip"), action.version()),
                (nodePath, destination) -> {
                    Path cli = destination.resolve("package/cli.js");
                    Files.createDirectories(cli.getParent());
                    Files.writeString(cli, "cli");
                },
                (nodePath, driverRoot, browserRoot, archives, log, timeout) ->
                        createReadyWindowsLayout(browserRoot));
        Files.createDirectories(paths.receipts().getParent());
        Files.writeString(paths.receipts(), "not-a-directory");
        SetupPlan plan = PlaywrightSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);

        assertThrows(SetupExecutionException.class,
                () -> service.install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of())));

        assertFalse(Files.exists(service.browserRoot()));
        assertTrue(Files.isRegularFile(paths.receipts()));
    }

    @Test
    void recoversWhenBrowserPublishedBeforeReceipt(@TempDir Path temp) throws Exception {
        assertInterruptedPublicationRecovery(temp, false);
    }

    @Test
    void recoversWhenPublishedPairStillHasOldQuarantines(@TempDir Path temp) throws Exception {
        assertInterruptedPublicationRecovery(temp, true);
    }

    @Test
    void installerReceivesOnlyTheRemainingSharedTransactionBudget(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path node = Files.writeString(temp.resolve("node.exe"), "node");
        java.util.concurrent.atomic.AtomicLong now = new java.util.concurrent.atomic.AtomicLong();
        java.util.concurrent.atomic.AtomicReference<Duration> installerBudget = new java.util.concurrent.atomic.AtomicReference<>();
        PlaywrightSetupService service = new PlaywrightSetupService(paths, PlaywrightHostPlatform.WIN64,
                SetupArchitecture.X64, readyNode(node), (action, timeout) -> {
                    now.addAndGet(Duration.ofSeconds(1).toNanos());
                    return Files.writeString(temp.resolve(action.version().replace(':', '-') + ".zip"), action.version());
                }, (nodePath, destination) -> {
                    now.addAndGet(Duration.ofSeconds(2).toNanos());
                    Path cli = destination.resolve("package/cli.js");
                    Files.createDirectories(cli.getParent());
                    Files.writeString(cli, "cli");
                }, (nodePath, driverRoot, browserRoot, archives, log, timeout) -> {
                    installerBudget.set(timeout);
                    createReadyWindowsLayout(browserRoot);
                }, now::get, Duration.ofSeconds(20));
        SetupPlan plan = PlaywrightSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);

        service.install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()));

        assertEquals(Duration.ofSeconds(12), installerBudget.get());
    }

    private static void assertPostFetchFailure(Path temp, boolean failDriver) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path node = Files.writeString(temp.resolve("node.exe"), "node");
        AtomicInteger fetched = new AtomicInteger();
        PlaywrightSetupService service = new PlaywrightSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, readyNode(node),
                action -> Files.writeString(temp.resolve("archive-" + fetched.incrementAndGet()), action.version()),
                (nodePath, destination) -> {
                    if (failDriver) throw new java.io.IOException("driver failed");
                    Path cli = destination.resolve("package/cli.js");
                    Files.createDirectories(cli.getParent());
                    Files.writeString(cli, "cli");
                },
                (nodePath, driverRoot, browserRoot, archives, log, timeout) -> {
                    throw new java.io.IOException("installer failed");
                });
        SetupPlan plan = PlaywrightSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);

        SetupExecutionException failure = assertThrows(SetupExecutionException.class,
                () -> service.install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of())));

        assertEquals(SetupTarget.PLAYWRIGHT_CHROMIUM, failure.failedAction().target());
        assertEquals(List.of(SetupTarget.NODE), failure.partialReceipt().completedActions().stream()
                .map(SetupAction::target).toList());
        assertEquals(6, fetched.get());
        assertFalse(Files.exists(service.browserRoot()));
        assertFalse(Files.exists(paths.receipts().resolve("playwright.json")));
        Path browserParent = service.browserRoot().getParent();
        if (Files.exists(browserParent)) {
            try (var siblings = Files.list(browserParent)) {
                assertTrue(siblings.noneMatch(path -> path.getFileName().toString().contains("staging")
                        || path.getFileName().toString().startsWith(".driver-")));
            }
        }
    }

    private static void assertInterruptedPublicationRecovery(Path temp, boolean receiptPublished) throws Exception {
        ShaftCachePaths paths = paths(temp);
        Path node = Files.writeString(temp.resolve("node.exe"), "node");
        AtomicInteger installs = new AtomicInteger();
        PlaywrightSetupService service = new PlaywrightSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, readyNode(node), action -> Files.writeString(
                temp.resolve(action.version().replace(':', '-') + ".zip"), action.version()),
                (nodePath, destination) -> {
                    Path cli = destination.resolve("package/cli.js");
                    Files.createDirectories(cli.getParent());
                    Files.writeString(cli, "cli");
                },
                (nodePath, driverRoot, browserRoot, archives, log, timeout) -> {
                    installs.incrementAndGet();
                    createReadyWindowsLayout(browserRoot);
                });
        SetupPlan plan = PlaywrightSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.EPOCH, Set.of());
        service.install(plan, approval);

        Path browser = service.browserRoot();
        Path browserQuarantine = browser.resolveSibling(browser.getFileName() + ".quarantine");
        Path receipt = paths.receipts().resolve("playwright.json");
        Path receiptQuarantine = receipt.resolveSibling(receipt.getFileName() + ".quarantine");
        Files.move(browser, browserQuarantine);
        Files.move(receipt, receiptQuarantine);
        createReadyWindowsLayout(browser);
        Files.writeString(browser.resolve("SHAFT_PLAYWRIGHT_VERSION"), PlaywrightSetupPlanner.PLAYWRIGHT_VERSION);
        if (receiptPublished) Files.copy(receiptQuarantine, receipt);

        SetupReceipt recovered = service.install(plan, approval);

        assertEquals(plan.digest(), recovered.planDigest());
        assertEquals(1, installs.get());
        assertEquals(SetupReadiness.READY, service.status().readiness());
        assertFalse(Files.exists(browserQuarantine));
        assertFalse(Files.exists(receiptQuarantine));
    }

    private static PlaywrightSetupService.NodeOwner readyNode(Path node) {
        return new PlaywrightSetupService.NodeOwner() {
            @Override public SetupReadiness readiness() { return SetupReadiness.READY; }
            @Override public void install(SetupAction action) { throw new AssertionError(); }
            @Override public Path executable() { return node; }
        };
    }

    private static void createReadyWindowsLayout(Path root) throws java.io.IOException {
        for (String marker : List.of("chromium-1234/INSTALLATION_COMPLETE",
                "chromium_headless_shell-1234/INSTALLATION_COMPLETE", "firefox-1538/INSTALLATION_COMPLETE",
                "webkit-2336/INSTALLATION_COMPLETE", "ffmpeg-1011/INSTALLATION_COMPLETE",
                "winldd-1007/INSTALLATION_COMPLETE")) {
            Path file = root.resolve(marker);
            Files.createDirectories(file.getParent());
            Files.writeString(file, "");
        }
        for (String executable : List.of("chromium-1234/chrome-win64/chrome.exe",
                "chromium_headless_shell-1234/chrome-headless-shell-win64/chrome-headless-shell.exe",
                "firefox-1538/firefox/firefox.exe", "webkit-2336/Playwright.exe",
                "ffmpeg-1011/ffmpeg-win64.exe", "winldd-1007/PrintDeps.exe")) {
            Path file = root.resolve(executable);
            Files.createDirectories(file.getParent());
            Files.writeString(file, "binary");
        }
    }

    private static void createReadyUbuntuLayout(Path root) throws java.io.IOException {
        for (String marker : List.of("chromium-1234/INSTALLATION_COMPLETE",
                "chromium_headless_shell-1234/INSTALLATION_COMPLETE", "firefox-1538/INSTALLATION_COMPLETE",
                "webkit-2336/INSTALLATION_COMPLETE", "ffmpeg-1011/INSTALLATION_COMPLETE")) {
            Path file = root.resolve(marker);
            Files.createDirectories(file.getParent());
            Files.writeString(file, "");
        }
        for (String executable : List.of("chromium-1234/chrome-linux64/chrome",
                "chromium_headless_shell-1234/chrome-headless-shell-linux64/chrome-headless-shell",
                "firefox-1538/firefox/firefox", "webkit-2336/pw_run.sh", "ffmpeg-1011/ffmpeg-linux")) {
            Path file = root.resolve(executable);
            Files.createDirectories(file.getParent());
            Files.writeString(file, "binary");
            assertTrue(file.toFile().setExecutable(true));
        }
    }

    private static ShaftCachePaths paths(Path temp) {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }

    private static String read(Process process) throws java.io.IOException {
        return new String(process.getInputStream().readAllBytes());
    }
}
