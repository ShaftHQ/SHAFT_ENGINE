package com.shaft.infrastructure;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Instant;
import java.util.List;
import java.util.Set;

/** Child-JVM probe used only by the setup convergence test. */
final class PlaywrightSetupProcessProbe {
    private PlaywrightSetupProcessProbe() { }

    public static void main(String[] args) throws Exception {
        Path root = Path.of(args[0]).toAbsolutePath().normalize();
        ShaftCachePaths paths = paths(root);
        Files.createDirectories(root);
        Path node = Files.writeString(root.resolve("node.exe"), "node",
                StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
        PlaywrightSetupService service = new PlaywrightSetupService(paths, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, readyNode(node), action -> {
                    Path archive = root.resolve("archive-" + action.version().replace(':', '-'));
                    return Files.writeString(archive, action.version(), StandardOpenOption.CREATE,
                            StandardOpenOption.TRUNCATE_EXISTING);
                }, (nodePath, destination) -> {
                    Path cli = destination.resolve("package/cli.js");
                    Files.createDirectories(cli.getParent());
                    Files.writeString(cli, "cli");
                }, (nodePath, driverRoot, browserRoot, archives, log, timeout) -> {
                    Files.writeString(root.resolve("install-count.txt"), "installed\n",
                            StandardOpenOption.CREATE, StandardOpenOption.APPEND);
                    try {
                        Thread.sleep(500);
                    } catch (InterruptedException interrupted) {
                        Thread.currentThread().interrupt();
                        throw new java.io.IOException("probe interrupted", interrupted);
                    }
                    createReadyLayout(browserRoot);
                });
        SetupPlan plan = PlaywrightSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);
        SetupReceipt receipt = service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()));
        System.out.println(receipt.planDigest());
    }

    static ShaftCachePaths paths(Path root) {
        Path cache = root.resolve("cache");
        Path data = root.resolve("data");
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }

    private static PlaywrightSetupService.NodeOwner readyNode(Path node) {
        return new PlaywrightSetupService.NodeOwner() {
            @Override public SetupReadiness readiness() { return SetupReadiness.READY; }
            @Override public void install(SetupAction action) { throw new AssertionError(); }
            @Override public Path executable() { return node; }
        };
    }

    private static void createReadyLayout(Path root) throws java.io.IOException {
        for (String relative : List.of("chromium-1234/INSTALLATION_COMPLETE",
                "chromium-1234/chrome-win64/chrome.exe",
                "chromium_headless_shell-1234/INSTALLATION_COMPLETE",
                "chromium_headless_shell-1234/chrome-headless-shell-win64/chrome-headless-shell.exe",
                "firefox-1538/INSTALLATION_COMPLETE", "firefox-1538/firefox/firefox.exe",
                "webkit-2336/INSTALLATION_COMPLETE", "webkit-2336/Playwright.exe",
                "ffmpeg-1011/INSTALLATION_COMPLETE", "ffmpeg-1011/ffmpeg-win64.exe",
                "winldd-1007/INSTALLATION_COMPLETE", "winldd-1007/PrintDeps.exe")) {
            Path file = root.resolve(relative);
            Files.createDirectories(file.getParent());
            Files.writeString(file, "ready");
        }
    }
}
