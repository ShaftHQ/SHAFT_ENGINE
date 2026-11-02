package com.shaft.infrastructure;

import java.io.IOException;
import java.util.List;

final class LighthouseSetupProvider implements SetupProvider {
    @Override
    public SetupProfile profile() {
        return SetupProfile.LIGHTHOUSE;
    }

    @Override
    public SetupPlan plan(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        return LighthouseSetupPlanner.plan(platform, architecture, options.effectiveMode());
    }

    @Override
    public SetupReport status(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        if (options.effectiveMode() == SetupMode.EXTERNAL) {
            String detail = "External mode is diagnostic-only and does not execute managed tools.";
            return SetupReport.from(new SetupProfileStatus(1, SetupProfile.LIGHTHOUSE, SetupReadiness.MISSING,
                    List.of(new SetupStatus(SetupTarget.NODE, SetupReadiness.MISSING, "", detail),
                            new SetupStatus(SetupTarget.LIGHTHOUSE, SetupReadiness.MISSING, "", detail))));
        }
        return SetupReport.from(service(options, platform, architecture).status());
    }

    @Override
    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        SetupPlan providerPlan = LighthouseSetupPlanner.plan(plan.platform(), plan.architecture(), plan.mode());
        SetupReceipt receipt = service(options, plan.platform(), plan.architecture()).install(providerPlan,
                new SetupApproval(providerPlan.digest(), approval.approvedAt(), approval.acceptedLicenses()));
        return new SetupReceipt(plan.digest(), receipt.completedAt(), receipt.completedActions());
    }

    static LighthouseSetupService service(SetupOptions options, SetupPlatform platform,
                                          SetupArchitecture architecture) {
        return new LighthouseSetupService(options.paths(), platform, architecture,
                action -> new VerifiedArtifactStore(options.paths().downloads()).fetch(action, options.offline()),
                (command, log, timeout) -> ReportingSetupService.runProcess(command, log, timeout,
                        options.paths().cacheRoot(), options.paths().tools().resolve("node")
                                .resolve(ReportingSetupPlanner.NODE_VERSION)
                                .resolve(platform.name().toLowerCase() + '-' + architecture.artifactName())),
                options.offline());
    }

}
