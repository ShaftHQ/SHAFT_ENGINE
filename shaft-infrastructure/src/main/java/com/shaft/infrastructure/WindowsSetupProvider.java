package com.shaft.infrastructure;

import java.io.IOException;

/** Built-in provider for pinned Appium Windows support and an existing WinAppDriver host. */
final class WindowsSetupProvider implements SetupProvider {
    private final DesktopMobileOperationsFactory operationsFactory;

    WindowsSetupProvider() {
        this(DefaultDesktopMobileToolchainOperations::new);
    }

    WindowsSetupProvider(DesktopMobileOperationsFactory operationsFactory) {
        this.operationsFactory = java.util.Objects.requireNonNull(operationsFactory, "operationsFactory");
    }

    @Override public SetupProfile profile() { return SetupProfile.MOBILE_WINDOWS; }

    @Override
    public SetupPlan plan(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        return plan(options, SetupSelection.defaults(), platform, architecture);
    }

    @Override
    public SetupPlan plan(SetupOptions options, SetupSelection selection,
                          SetupPlatform platform, SetupArchitecture architecture) {
        return DesktopMobileSetupPlanner.windows(platform, architecture, options.effectiveMode(), selection);
    }

    @Override
    public SetupSelection selectionFromPlan(SetupPlan plan) {
        return DesktopMobileSetupPlanner.selectionFromPlan(plan);
    }

    @Override
    public SetupReport status(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        return status(options, SetupSelection.defaults(), platform, architecture);
    }

    @Override
    public SetupReport status(SetupOptions options, SetupSelection selection,
                              SetupPlatform platform, SetupArchitecture architecture) {
        SetupPlan plan = plan(options, selection, platform, architecture);
        if (options.effectiveMode() == SetupMode.EXTERNAL) return externalStatus(plan);
        DesktopMobileToolchainOperations operations = operationsFactory.create(options.paths(), plan,
                options.offline());
        return SetupReport.from(new DesktopMobileSetupService(options.paths(), plan, operations, options.offline())
                .status());
    }

    @Override
    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        SetupExecutor.validate(plan, approval);
        SetupPlan providerPlan = canonicalPlan(plan, options);
        DesktopMobileToolchainOperations operations = operationsFactory.create(options.paths(), providerPlan,
                options.offline());
        SetupReceipt receipt = new DesktopMobileSetupService(options.paths(), providerPlan, operations,
                options.offline()).install(providerPlan,
                new SetupApproval(providerPlan.digest(), approval.approvedAt(), approval.acceptedLicenses()));
        return new SetupReceipt(plan.digest(), receipt.completedAt(), receipt.completedActions());
    }

    private SetupPlan canonicalPlan(SetupPlan plan, SetupOptions options) {
        if (options.profile() != profile()) {
            throw new IllegalArgumentException("Options do not select Windows desktop setup.");
        }
        SetupPlan canonical = DesktopMobileSetupPlanner.windows(plan.platform(), plan.architecture(), plan.mode(),
                DesktopMobileSetupPlanner.selectionFromPlan(plan));
        if (!SetupPlan.bind(canonical, options.policyDigest()).equals(plan)) {
            throw new IllegalArgumentException("Plan does not match the Windows manifest shipped with this release.");
        }
        return canonical;
    }

    private SetupReport externalStatus(SetupPlan plan) {
        return SetupReport.from(new SetupProfileStatus(1, profile(), SetupReadiness.MISSING,
                plan.actions().stream().map(action -> new SetupStatus(action.target(), SetupReadiness.MISSING, "",
                        "External mode is diagnostic-only and does not execute managed tools.")).toList()));
    }
}
