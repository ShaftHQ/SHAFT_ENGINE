package com.shaft.infrastructure;

import java.io.IOException;
import java.nio.file.Path;

final class ReportingSetupProvider implements SetupProvider {
    @Override
    public SetupProfile profile() {
        return SetupProfile.REPORTING;
    }

    @Override
    public SetupPlan plan(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        return ReportingSetupPlanner.plan(platform, architecture, options.effectiveMode());
    }

    @Override
    public SetupReport status(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        return SetupReport.from(service(options, platform, architecture).status());
    }

    @Override
    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        SetupPlan providerPlan = ReportingSetupPlanner.plan(plan.platform(), plan.architecture(), plan.mode());
        SetupReceipt receipt = service(options, plan.platform(), plan.architecture()).install(providerPlan,
                new SetupApproval(providerPlan.digest(), approval.approvedAt(), approval.acceptedLicenses()));
        return new SetupReceipt(plan.digest(), receipt.completedAt(), receipt.completedActions());
    }

    @Override
    public String logs(SetupOptions options, SetupSelection selection, SetupPlatform platform,
                       SetupArchitecture architecture) throws IOException {
        if (!selection.components().isEmpty()) {
            throw new IllegalArgumentException("Profile REPORTING does not accept component selection.");
        }
        Path log = service(options, platform, architecture).logFile();
        return OwnedLogReader.read("Reporting", log);
    }

    private static ReportingSetupService service(SetupOptions options, SetupPlatform platform,
                                                 SetupArchitecture architecture) {
        return new ReportingSetupService(options.paths(), platform, architecture, options.offline());
    }
}
