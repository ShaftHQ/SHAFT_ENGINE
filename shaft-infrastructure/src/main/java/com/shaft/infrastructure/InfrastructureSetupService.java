package com.shaft.infrastructure;

import java.io.IOException;
import java.util.List;
import java.util.Objects;
import java.util.ServiceLoader;

/** Shared provider-neutral setup orchestration used by every public adapter. */
public final class InfrastructureSetupService {
    private final SetupProviderRegistry providers;
    private final SetupPlatform platform;
    private final SetupArchitecture architecture;

    public InfrastructureSetupService(SetupProviderRegistry providers, SetupPlatform platform,
                                      SetupArchitecture architecture) {
        this.providers = Objects.requireNonNull(providers, "providers");
        this.platform = Objects.requireNonNull(platform, "platform");
        this.architecture = Objects.requireNonNull(architecture, "architecture");
    }

    public static InfrastructureSetupService builtIn() {
        return builtIn(SetupPlatform.current(), SetupArchitecture.current());
    }

    public static InfrastructureSetupService builtIn(SetupPlatform platform, SetupArchitecture architecture) {
        List<SetupProvider> providers = new java.util.ArrayList<>(List.of(
                new ReportingSetupProvider(), new OcrSetupProvider(), new LighthouseSetupProvider(),
                new PlaywrightSetupProvider(), new AndroidSetupProvider()));
        ClassLoader contextLoader = Thread.currentThread().getContextClassLoader();
        ClassLoader loader = contextLoader == null ? InfrastructureSetupService.class.getClassLoader() : contextLoader;
        ServiceLoader.load(SetupProvider.class, loader).forEach(providers::add);
        return new InfrastructureSetupService(new SetupProviderRegistry(providers), platform, architecture);
    }

    public SetupCatalog catalog() {
        return SetupCatalog.builtIn();
    }

    /**
     * Returns whether this coordinator has an executable provider for the profile.
     *
     * @param profile profile to query
     * @return {@code true} when an executable provider is registered
     */
    public boolean supports(SetupProfile profile) {
        return providers.profiles().contains(Objects.requireNonNull(profile, "profile"));
    }

    public SetupPlan plan(SetupOptions options) {
        return plan(options, SetupSelection.defaults());
    }

    public SetupPlan plan(SetupOptions options, SetupSelection selection) {
        SetupOptions value = Objects.requireNonNull(options, "options");
        SetupPlan plan = providers.require(value.profile()).plan(value, Objects.requireNonNull(selection, "selection"),
                platform, architecture);
        requirePlanIdentity(plan, value);
        return SetupPlan.bind(plan, value.policyDigest());
    }

    /** Plans one typed Android emulator request without exposing provider encoding details. */
    public SetupPlan plan(SetupOptions options, AndroidSetupRequest request) {
        requireAndroidProfile(options);
        return plan(options, Objects.requireNonNull(request, "request").toSelection());
    }

    public SetupReport doctor(SetupOptions options) {
        return status(options);
    }

    public SetupReport status(SetupOptions options) {
        return status(options, SetupSelection.defaults());
    }

    public SetupReport status(SetupOptions options, SetupSelection selection) {
        SetupOptions value = Objects.requireNonNull(options, "options");
        SetupReport report = providers.require(value.profile()).status(value,
                Objects.requireNonNull(selection, "selection"), platform, architecture);
        if (report.profile() != value.profile()) {
            throw new IllegalStateException("Setup provider returned a report for " + report.profile()
                    + " instead of " + value.profile() + '.');
        }
        return report;
    }

    /** Reports readiness for one exact typed Android emulator request. */
    public SetupReport status(SetupOptions options, AndroidSetupRequest request) {
        requireAndroidProfile(options);
        return status(options, Objects.requireNonNull(request, "request").toSelection());
    }

    public SetupReport verify(SetupOptions options) {
        return status(options);
    }

    public SetupReport verify(SetupOptions options, SetupSelection selection) {
        return status(options, selection);
    }

    /** Verifies one exact typed Android emulator request. */
    public SetupReport verify(SetupOptions options, AndroidSetupRequest request) {
        return status(options, request);
    }

    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        return install(plan, approval, options, SetupSelection.defaults());
    }

    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                SetupSelection selection) throws IOException {
        SetupProvider provider = authorize(plan, approval, options, selection, "mutate the host");
        SetupReceipt receipt = provider.install(plan, approval, options);
        requireReceiptIdentity(receipt, plan);
        return receipt;
    }

    /** Installs one exact typed Android emulator request after approval. */
    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                AndroidSetupRequest request) throws IOException {
        requireAndroidProfile(options);
        return install(plan, approval, options, Objects.requireNonNull(request, "request").toSelection());
    }

    public ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options)
            throws IOException {
        return start(plan, approval, options, SetupSelection.defaults());
    }

    public ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                    SetupSelection selection) throws IOException {
        SetupProvider provider = authorize(plan, approval, options, selection, "start a managed service");
        ManagedEnvironment environment = provider.start(plan, approval, options);
        try {
            if (environment.profile() != plan.profile()) {
                throw new IllegalStateException("Setup provider returned a mismatched managed environment.");
            }
            requireReceiptIdentity(environment.receipt(), plan);
        } catch (IllegalStateException mismatch) {
            try {
                environment.close();
            } catch (RuntimeException cleanupFailure) {
                mismatch.addSuppressed(cleanupFailure);
            }
            throw mismatch;
        }
        return environment;
    }

    /** Starts one exact typed Android emulator request from its verified install receipt. */
    public ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                    AndroidSetupRequest request) throws IOException {
        requireAndroidProfile(options);
        return start(plan, approval, options, Objects.requireNonNull(request, "request").toSelection());
    }

    private SetupProvider authorize(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                    SetupSelection selection, String operation) {
        Objects.requireNonNull(plan, "plan");
        Objects.requireNonNull(options, "options");
        if (options.effectiveMode() == SetupMode.EXTERNAL) {
            throw new IllegalArgumentException("External setup is diagnostic and cannot " + operation + '.');
        }
        SetupExecutor.validate(plan, Objects.requireNonNull(approval, "approval"));
        if (plan.actions().stream().anyMatch(SetupAction::privileged)) {
            throw new IllegalArgumentException("Privileged host prerequisites cannot be performed by the Java API.");
        }
        if (plan.profile() != options.profile() || plan.mode() != options.effectiveMode()) {
            throw new IllegalArgumentException("Plan does not match the requested setup options.");
        }
        SetupProvider provider = providers.require(options.profile());
        SetupPlan expected = provider.plan(options, Objects.requireNonNull(selection, "selection"), platform, architecture);
        requirePlanIdentity(expected, options);
        if (!SetupPlan.bind(expected, options.policyDigest()).equals(plan)) {
            throw new IllegalArgumentException("Plan does not match the provider manifest shipped with this release.");
        }
        return provider;
    }

    private void requirePlanIdentity(SetupPlan plan, SetupOptions options) {
        if (plan.profile() != options.profile() || plan.platform() != platform
                || plan.architecture() != architecture || plan.mode() != options.effectiveMode()) {
            throw new IllegalStateException("Setup provider returned a plan with mismatched identity.");
        }
    }

    private static void requireReceiptIdentity(SetupReceipt receipt, SetupPlan plan) {
        Objects.requireNonNull(receipt, "receipt");
        if (!receipt.planDigest().equals(plan.digest()) || !receipt.completedActions().equals(plan.actions())) {
            throw new IllegalStateException("Setup provider returned a mismatched receipt.");
        }
    }

    private static void requireAndroidProfile(SetupOptions options) {
        if (Objects.requireNonNull(options, "options").profile() != SetupProfile.MOBILE_ANDROID) {
            throw new IllegalArgumentException("Android setup requests require profile MOBILE_ANDROID.");
        }
    }
}
