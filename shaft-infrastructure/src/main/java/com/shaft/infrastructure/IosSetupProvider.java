package com.shaft.infrastructure;

import java.io.IOException;

/** Built-in provider for a pinned Appium XCUITest toolchain and an existing iOS Simulator. */
final class IosSetupProvider implements SetupProvider {
    private final DesktopMobileOperationsFactory operationsFactory;
    private final DesktopMobileLifecycleFactory lifecycleFactory;

    IosSetupProvider() {
        this(DefaultDesktopMobileToolchainOperations::new, DesktopMobileRuntimeManager::systemLifecycle);
    }

    IosSetupProvider(DesktopMobileOperationsFactory operationsFactory) {
        this(operationsFactory, DesktopMobileRuntimeManager::systemLifecycle);
    }

    IosSetupProvider(DesktopMobileOperationsFactory operationsFactory,
                     DesktopMobileLifecycleFactory lifecycleFactory) {
        this.operationsFactory = java.util.Objects.requireNonNull(operationsFactory, "operationsFactory");
        this.lifecycleFactory = java.util.Objects.requireNonNull(lifecycleFactory, "lifecycleFactory");
    }

    @Override public SetupProfile profile() { return SetupProfile.MOBILE_IOS; }

    @Override
    public SetupPlan plan(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        return plan(options, SetupSelection.defaults(), platform, architecture);
    }

    @Override
    public SetupPlan plan(SetupOptions options, SetupSelection selection,
                          SetupPlatform platform, SetupArchitecture architecture) {
        return DesktopMobileSetupPlanner.ios(platform, architecture, options.effectiveMode(), selection);
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

    @Override
    public ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        SetupPlan providerPlan = canonicalPlan(plan, options);
        DesktopMobileToolchainOperations operations = operationsFactory.create(options.paths(), providerPlan,
                options.offline());
        ManagedEnvironment inner = lifecycleFactory.create(options.paths(), providerPlan, operations)
                .start(providerPlan, new SetupApproval(providerPlan.digest(), approval.approvedAt(),
                        approval.acceptedLicenses()), options);
        SetupReceipt receipt = new SetupReceipt(plan.digest(), inner.receipt().completedAt(),
                inner.receipt().completedActions());
        return new ManagedEnvironment(profile(), receipt, inner.endpoint(), inner.connectionProperties(),
                inner::close);
    }

    @Override
    public boolean stop(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        SetupExecutor.validate(plan, approval);
        SetupPlan providerPlan = canonicalPlan(plan, options);
        DesktopMobileToolchainOperations operations = operationsFactory.create(options.paths(), providerPlan,
                options.offline());
        return lifecycleFactory.create(options.paths(), providerPlan, operations).stop(options.shutdownTimeout());
    }

    @Override
    public String logs(SetupOptions options, SetupSelection selection, SetupPlatform platform,
                       SetupArchitecture architecture) throws IOException {
        SetupPlan plan = plan(options, selection, platform, architecture);
        DesktopMobileToolchainOperations operations = operationsFactory.create(options.paths(), plan,
                options.offline());
        return lifecycleFactory.create(options.paths(), plan, operations).logs();
    }

    private SetupPlan canonicalPlan(SetupPlan plan, SetupOptions options) {
        if (options.profile() != profile()) throw new IllegalArgumentException("Options do not select iOS setup.");
        SetupPlan canonical = DesktopMobileSetupPlanner.ios(plan.platform(), plan.architecture(), plan.mode(),
                DesktopMobileSetupPlanner.selectionFromPlan(plan));
        if (!SetupPlan.bind(canonical, options.policyDigest()).equals(plan)) {
            throw new IllegalArgumentException("Plan does not match the iOS manifest shipped with this release.");
        }
        return canonical;
    }

    private SetupReport externalStatus(SetupPlan plan) {
        return SetupReport.from(new SetupProfileStatus(1, profile(), SetupReadiness.MISSING,
                plan.actions().stream().map(action -> new SetupStatus(action.target(), SetupReadiness.MISSING, "",
                        "External mode is diagnostic-only and does not execute managed tools.")).toList()));
    }
}
