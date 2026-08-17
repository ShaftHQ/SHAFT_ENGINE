package com.shaft.infrastructure;

import java.io.IOException;

/** Built-in provider for a pinned, dev-only ReportPortal compose project. */
final class ReportPortalSetupProvider implements SetupProvider {
    private final ReportPortalOperationsFactory operationsFactory;
    private final ReportPortalLifecycleFactory lifecycleFactory;

    ReportPortalSetupProvider() {
        this(DefaultReportPortalToolchainOperations::new, ReportPortalRuntimeManager::systemLifecycle);
    }

    ReportPortalSetupProvider(ReportPortalOperationsFactory operationsFactory) {
        this(operationsFactory, ReportPortalRuntimeManager::systemLifecycle);
    }

    ReportPortalSetupProvider(ReportPortalOperationsFactory operationsFactory,
                              ReportPortalLifecycleFactory lifecycleFactory) {
        this.operationsFactory = java.util.Objects.requireNonNull(operationsFactory, "operationsFactory");
        this.lifecycleFactory = java.util.Objects.requireNonNull(lifecycleFactory, "lifecycleFactory");
    }

    @Override
    public SetupProfile profile() {
        return SetupProfile.REPORT_PORTAL;
    }

    @Override
    public SetupPlan plan(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        return plan(options, SetupSelection.defaults(), platform, architecture);
    }

    @Override
    public SetupPlan plan(SetupOptions options, SetupSelection selection,
                          SetupPlatform platform, SetupArchitecture architecture) {
        return ReportPortalSetupPlanner.plan(platform, architecture, options.effectiveMode(), selection);
    }

    @Override
    public SetupSelection selectionFromPlan(SetupPlan plan) {
        return ReportPortalSetupPlanner.selectionFromPlan(plan);
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
        ReportPortalToolchainOperations operations = operationsFactory.create(options.paths(), plan, options.offline());
        return SetupReport.from(new ReportPortalSetupService(options.paths(), plan, operations, options.offline())
                .status());
    }

    @Override
    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        SetupExecutor.validate(plan, approval);
        SetupPlan providerPlan = canonicalPlan(plan, options);
        ReportPortalToolchainOperations operations = operationsFactory.create(options.paths(), providerPlan,
                options.offline());
        SetupReceipt receipt = new ReportPortalSetupService(options.paths(), providerPlan, operations,
                options.offline()).install(providerPlan,
                new SetupApproval(providerPlan.digest(), approval.approvedAt(), approval.acceptedLicenses()));
        return new SetupReceipt(plan.digest(), receipt.completedAt(), receipt.completedActions());
    }

    @Override
    public ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        SetupPlan providerPlan = canonicalPlan(plan, options);
        ReportPortalToolchainOperations operations = operationsFactory.create(options.paths(), providerPlan,
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
        ReportPortalToolchainOperations operations = operationsFactory.create(options.paths(), providerPlan,
                options.offline());
        return lifecycleFactory.create(options.paths(), providerPlan, operations).stop(options.shutdownTimeout());
    }

    @Override
    public String logs(SetupOptions options, SetupSelection selection, SetupPlatform platform,
                       SetupArchitecture architecture) throws IOException {
        SetupPlan plan = plan(options, selection, platform, architecture);
        ReportPortalToolchainOperations operations = operationsFactory.create(options.paths(), plan,
                options.offline());
        return lifecycleFactory.create(options.paths(), plan, operations).logs();
    }

    private SetupPlan canonicalPlan(SetupPlan plan, SetupOptions options) {
        if (options.profile() != profile()) {
            throw new IllegalArgumentException("Options do not select ReportPortal setup.");
        }
        SetupPlan canonical = ReportPortalSetupPlanner.plan(plan.platform(), plan.architecture(), plan.mode(),
                ReportPortalSetupPlanner.selectionFromPlan(plan));
        if (!SetupPlan.bind(canonical, options.policyDigest()).equals(plan)) {
            throw new IllegalArgumentException(
                    "Plan does not match the ReportPortal manifest shipped with this release.");
        }
        return canonical;
    }

    private SetupReport externalStatus(SetupPlan plan) {
        return SetupReport.from(new SetupProfileStatus(1, profile(), SetupReadiness.MISSING,
                plan.actions().stream().map(action -> new SetupStatus(action.target(), SetupReadiness.MISSING, "",
                        "External mode is diagnostic-only and does not execute managed tools.")).toList()));
    }
}
