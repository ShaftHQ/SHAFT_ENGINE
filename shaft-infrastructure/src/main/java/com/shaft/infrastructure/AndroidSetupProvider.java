package com.shaft.infrastructure;

import java.io.IOException;
import java.util.List;

/** Built-in provider for the release-pinned Appium and Android emulator toolchain. */
final class AndroidSetupProvider implements SetupProvider {
    @Override
    public SetupProfile profile() {
        return SetupProfile.MOBILE_ANDROID;
    }

    @Override
    public SetupPlan plan(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        return AndroidSetupPlanner.plan(platform, architecture, options.effectiveMode());
    }

    @Override
    public SetupPlan plan(SetupOptions options, SetupSelection selection,
                          SetupPlatform platform, SetupArchitecture architecture) {
        return AndroidSetupPlanner.plan(platform, architecture, options.effectiveMode(),
                AndroidSetupRequest.fromSelection(selection));
    }

    @Override
    public SetupReport status(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        return status(options, SetupSelection.defaults(), platform, architecture);
    }

    @Override
    public SetupReport status(SetupOptions options, SetupSelection selection,
                              SetupPlatform platform, SetupArchitecture architecture) {
        AndroidSetupRequest request = AndroidSetupRequest.fromSelection(selection);
        String detail = options.effectiveMode() == SetupMode.EXTERNAL
                ? "External mode is diagnostic-only and does not execute managed tools."
                : "The managed Android toolchain is not installed.";
        if (options.effectiveMode() == SetupMode.EXTERNAL) {
            List<SetupStatus> targets = AndroidSetupPlanner.plan(platform, architecture, SetupMode.EXTERNAL, request)
                    .actions().stream().map(action -> new SetupStatus(action.target(), SetupReadiness.MISSING, "", detail))
                    .toList();
            return SetupReport.from(new SetupProfileStatus(1, profile(), SetupReadiness.MISSING, targets));
        }
        AndroidToolchainOperations operations = new DefaultAndroidToolchainOperations(options.paths(), platform,
                architecture, request, options.offline());
        return SetupReport.from(new AndroidSetupService(options.paths(), platform, architecture, request,
                operations, options.offline()).status());
    }

    @Override
    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        AndroidSetupRequest request = AndroidSetupRequest.fromPlan(plan);
        AndroidSetupService service = new AndroidSetupService(options.paths(), plan.platform(), plan.architecture(),
                request, new DefaultAndroidToolchainOperations(options.paths(), plan.platform(),
                plan.architecture(), request, options.offline()), options.offline());
        SetupPlan providerPlan = AndroidSetupPlanner.plan(plan.platform(), plan.architecture(), plan.mode(), request);
        SetupReceipt receipt = service.install(providerPlan,
                new SetupApproval(providerPlan.digest(), approval.approvedAt(), approval.acceptedLicenses()));
        return new SetupReceipt(plan.digest(), receipt.completedAt(), receipt.completedActions());
    }

    @Override
    public ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        AndroidSetupRequest request = AndroidSetupRequest.fromPlan(plan);
        SetupPlan providerPlan = AndroidSetupPlanner.plan(plan.platform(), plan.architecture(), plan.mode(), request);
        SetupApproval providerApproval = new SetupApproval(providerPlan.digest(), approval.approvedAt(),
                approval.acceptedLicenses());
        AndroidToolchainOperations operations = new DefaultAndroidToolchainOperations(options.paths(),
                plan.platform(), plan.architecture(), request, options.offline());
        AndroidLifecycleService lifecycle = new AndroidLifecycleService(options.paths(), plan.platform(),
                plan.architecture(), request, operations, new SystemAndroidRuntimeController(),
                new SystemAndroidRuntimeHealth(options.paths(), plan.platform(), plan.architecture()));
        ManagedEnvironment inner = lifecycle.start(providerPlan, providerApproval, options);
        SetupReceipt receipt = new SetupReceipt(plan.digest(), inner.receipt().completedAt(),
                inner.receipt().completedActions());
        return new ManagedEnvironment(profile(), receipt, inner.endpoint(), inner.connectionProperties(), inner::close);
    }

    @Override
    public boolean stop(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        SetupExecutor.validate(plan, approval);
        return AndroidRuntimeManager.stop(options.paths(), plan.platform(), plan.architecture(),
                AndroidSetupRequest.fromPlan(plan), options.shutdownTimeout());
    }

    @Override
    public String logs(SetupOptions options, SetupSelection selection, SetupPlatform platform,
                       SetupArchitecture architecture) throws IOException {
        return AndroidRuntimeManager.logs(options.paths(), platform, architecture,
                AndroidSetupRequest.fromSelection(selection));
    }

}
