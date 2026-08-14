package com.shaft.infrastructure;

import java.io.IOException;
import java.util.List;

final class PlaywrightSetupProvider implements SetupProvider {
    @Override
    public SetupProfile profile() {
        return SetupProfile.PLAYWRIGHT;
    }

    @Override
    public SetupPlan plan(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        return PlaywrightSetupPlanner.plan(platform, architecture, options.effectiveMode());
    }

    @Override
    public SetupReport status(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        if (options.effectiveMode() == SetupMode.EXTERNAL) {
            String detail = "External mode is diagnostic-only and does not execute managed tools.";
            List<SetupStatus> targets = PlaywrightSetupPlanner.plan(platform, architecture, SetupMode.EXTERNAL)
                    .actions().stream()
                    .map(action -> new SetupStatus(action.target(), SetupReadiness.MISSING, "", detail))
                    .toList();
            return SetupReport.from(new SetupProfileStatus(1, profile(), SetupReadiness.MISSING, targets));
        }
        return SetupReport.from(service(options, platform, architecture).status());
    }

    @Override
    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        SetupPlan providerPlan = PlaywrightSetupPlanner.plan(plan.platform(), plan.architecture(), plan.mode());
        SetupReceipt receipt = service(options, plan.platform(), plan.architecture()).install(providerPlan,
                new SetupApproval(providerPlan.digest(), approval.approvedAt(), approval.acceptedLicenses()));
        return new SetupReceipt(plan.digest(), receipt.completedAt(), receipt.completedActions());
    }

    private static PlaywrightSetupService service(SetupOptions options, SetupPlatform platform,
                                                  SetupArchitecture architecture) {
        ReportingSetupService nodeService = new ReportingSetupService(options.paths(), platform, architecture,
                options.offline());
        PlaywrightHostPlatform hostPlatform = PlaywrightHostPlatform.current(platform, architecture);
        PathOwner nodeOwner = new PathOwner(nodeService, nodeExecutable(options.paths(), platform, architecture));
        PlaywrightBrowserInstaller browserInstaller = new PlaywrightBrowserInstaller(
                (command, log, timeout, environment, removed) -> ReportingSetupService.runProcess(
                        command, log, timeout, options.paths().cacheRoot(),
                        nodeRoot(options.paths(), platform, architecture), null, environment, removed, null));
        return new PlaywrightSetupService(options.paths(), hostPlatform, architecture, nodeOwner,
                (action, timeout) -> new VerifiedArtifactStore(options.paths().downloads())
                        .fetch(action, options.offline(), timeout), PlaywrightDriverExtractor::extract,
                (node, driver, browsers, archives, log, timeout) -> browserInstaller.install(
                        node, driver, browsers, archives, log, timeout, hostPlatform),
                System::nanoTime, java.time.Duration.ofMinutes(10));
    }

    private static java.nio.file.Path nodeRoot(ShaftCachePaths paths, SetupPlatform platform,
                                               SetupArchitecture architecture) {
        return paths.tools().resolve("node").resolve(ReportingSetupPlanner.NODE_VERSION)
                .resolve(platform.name().toLowerCase() + '-' + architecture.artifactName());
    }

    private static java.nio.file.Path nodeExecutable(ShaftCachePaths paths, SetupPlatform platform,
                                                     SetupArchitecture architecture) {
        java.nio.file.Path root = nodeRoot(paths, platform, architecture);
        return platform == SetupPlatform.WINDOWS ? root.resolve("node.exe") : root.resolve("bin/node");
    }

    private record PathOwner(ReportingSetupService service, java.nio.file.Path executable)
            implements PlaywrightSetupService.NodeOwner {
        private PathOwner {
            java.util.Objects.requireNonNull(service, "service");
            java.util.Objects.requireNonNull(executable, "executable");
        }

        @Override public SetupReadiness readiness() { return service.nodeStatus().readiness(); }

        @Override public void install(SetupAction action) throws IOException { service.installNodeAction(action); }
    }
}
