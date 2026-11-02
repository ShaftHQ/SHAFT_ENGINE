package com.shaft.infrastructure;

import java.io.IOException;
import java.util.List;
import java.util.Objects;
import java.util.ServiceLoader;
import java.util.function.Consumer;

/** Shared provider-neutral setup orchestration used by every public adapter. */
public final class InfrastructureSetupService {
    private final SetupProviderRegistry providers;
    private final SetupPlatform platform;
    private final SetupArchitecture architecture;

    /**
     * Creates a setup service over the given providers for the given platform and architecture.
     */
    public InfrastructureSetupService(SetupProviderRegistry providers, SetupPlatform platform,
                                      SetupArchitecture architecture) {
        this.providers = Objects.requireNonNull(providers, "providers");
        this.platform = Objects.requireNonNull(platform, "platform");
        this.architecture = Objects.requireNonNull(architecture, "architecture");
    }

    /**
     * Creates a setup service with the built-in providers for the current platform.
     */
    public static InfrastructureSetupService builtIn() {
        return builtIn(SetupPlatform.current(), SetupArchitecture.current());
    }

    /**
     * Creates a setup service with the built-in providers for the given platform and architecture.
     */
    public static InfrastructureSetupService builtIn(SetupPlatform platform, SetupArchitecture architecture) {
        List<SetupProvider> providers = new java.util.ArrayList<>(List.of(
                new ReportingSetupProvider(), new OcrSetupProvider(), new LighthouseSetupProvider(),
                new PlaywrightSetupProvider(), new AndroidSetupProvider(), new SeleniumGridSetupProvider(),
                new HealeniumSetupProvider(), new ReportPortalSetupProvider(),
                new BrowserStackLocalSetupProvider(), new AgentToolsSetupProvider()));
        if (platform == SetupPlatform.MACOS) providers.add(new IosSetupProvider());
        if (platform == SetupPlatform.WINDOWS) providers.add(new WindowsSetupProvider());
        ClassLoader contextLoader = Thread.currentThread().getContextClassLoader();
        ClassLoader loader = contextLoader == null ? InfrastructureSetupService.class.getClassLoader() : contextLoader;
        ServiceLoader.load(SetupProvider.class, loader).forEach(providers::add);
        return new InfrastructureSetupService(new SetupProviderRegistry(providers), platform, architecture);
    }

    /**
     * Returns the catalog of setup providers this service can manage.
     */
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

    /** Reconstructs the canonical selection embedded in an approved plan. */
    public SetupSelection selectionFromPlan(SetupPlan plan) {
        return providers.require(Objects.requireNonNull(plan, "plan").profile()).selectionFromPlan(plan);
    }

    /**
     * Plans the changes needed for the given options without applying them.
     */
    public SetupPlan plan(SetupOptions options) {
        return plan(options, SetupSelection.defaults());
    }

    /**
     * Plans the changes needed for the selected components without applying them.
     */
    public SetupPlan plan(SetupOptions options, SetupSelection selection) {
        return plan(options, selection, SetupOperation.INSTALL);
    }

    /**
     * Plans the given operation (such as install or remove) for the selected components without
     * applying it.
     */
    public SetupPlan plan(SetupOptions options, SetupSelection selection, SetupOperation operation) {
        SetupOptions value = Objects.requireNonNull(options, "options");
        SetupPlan plan = providers.require(value.profile()).plan(value, Objects.requireNonNull(selection, "selection"),
                Objects.requireNonNull(operation, "operation"), platform, architecture);
        requirePlanIdentity(plan, value);
        return SetupPlan.bind(plan, value.policyDigest());
    }

    /** Plans one typed Android emulator request without exposing provider encoding details. */
    public SetupPlan plan(SetupOptions options, AndroidSetupRequest request) {
        requireAndroidProfile(options);
        return plan(options, Objects.requireNonNull(request, "request").toSelection());
    }

    /**
     * Checks the setup for the given options and reports problems without changing anything.
     */
    public SetupReport doctor(SetupOptions options) {
        return status(options);
    }

    /**
     * Reports the current state of the setup for the given options.
     */
    public SetupReport status(SetupOptions options) {
        return status(options, SetupSelection.defaults());
    }

    /**
     * Reports the current state of the selected components.
     */
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

    /**
     * Verifies that the setup for the given options is installed and healthy.
     */
    public SetupReport verify(SetupOptions options) {
        return status(options);
    }

    /**
     * Verifies that the selected components are installed and healthy.
     */
    public SetupReport verify(SetupOptions options, SetupSelection selection) {
        return status(options, selection);
    }

    /** Verifies one exact typed Android emulator request. */
    public SetupReport verify(SetupOptions options, AndroidSetupRequest request) {
        return status(options, request);
    }

    /**
     * Installs an approved plan.
     *
     * @throws IOException when an artifact cannot be fetched or written
     */
    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        return install(plan, approval, options,
                authorizedSelectionFromPlan(plan, approval, options, "mutate the host"));
    }

    /**
     * Installs an approved plan, reporting progress as it runs.
     *
     * @throws IOException when an artifact cannot be fetched or written
     */
    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                Consumer<SetupProgress> progress) throws IOException {
        return install(plan, approval, options,
                authorizedSelectionFromPlan(plan, approval, options, "mutate the host"), progress);
    }

    /**
     * Installs the selected components of an approved plan.
     *
     * @throws IOException when an artifact cannot be fetched or written
     */
    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                SetupSelection selection) throws IOException {
        return install(plan, approval, options, selection, ignored -> { });
    }

    /**
     * Installs the selected components of an approved plan, reporting progress as it runs.
     *
     * @throws IOException when an artifact cannot be fetched or written
     */
    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                SetupSelection selection, Consumer<SetupProgress> progress) throws IOException {
        SetupProvider provider = authorize(plan, approval, options, selection, "mutate the host");
        SetupReceipt receipt = provider.install(plan, approval, options,
                Objects.requireNonNull(progress, "progress"));
        requireReceiptIdentity(receipt, plan);
        return receipt;
    }

    /** Installs one exact typed Android emulator request after approval. */
    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                AndroidSetupRequest request) throws IOException {
        requireAndroidProfile(options);
        return install(plan, approval, options, Objects.requireNonNull(request, "request").toSelection());
    }

    /**
     * Installs an approved plan if needed and starts the services it owns.
     *
     * @throws IOException when an artifact cannot be fetched or written
     */
    public ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options)
            throws IOException {
        return start(plan, approval, options,
                authorizedSelectionFromPlan(plan, approval, options, "start a managed service"));
    }

    /**
     * Installs the selected components of an approved plan if needed and starts their services.
     *
     * @throws IOException when an artifact cannot be fetched or written
     */
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

    /** Stops the exact SHAFT-owned service represented by an approved plan. */
    public boolean stop(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        return stop(plan, approval, options,
                authorizedSelectionFromPlan(plan, approval, options, "stop a managed service"));
    }

    /** Stops the exact selected SHAFT-owned service represented by an approved plan. */
    public boolean stop(SetupPlan plan, SetupApproval approval, SetupOptions options,
                        SetupSelection selection) throws IOException {
        SetupProvider provider = authorize(plan, approval, options, selection, "stop a managed service");
        return provider.stop(plan, approval, options);
    }

    /** Reads bounded logs for the default selection of one setup profile. */
    public String logs(SetupOptions options) throws IOException {
        return logs(options, SetupSelection.defaults());
    }

    /** Reads bounded logs owned by one exact provider selection without mutating the host. */
    public String logs(SetupOptions options, SetupSelection selection) throws IOException {
        SetupOptions value = Objects.requireNonNull(options, "options");
        String logs = providers.require(value.profile()).logs(value,
                Objects.requireNonNull(selection, "selection"), platform, architecture);
        return Objects.requireNonNull(logs, "Setup provider returned null logs.");
    }

    private SetupProvider authorize(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                    SetupSelection selection, String operation) {
        SetupProvider provider = preauthorize(plan, approval, options, operation);
        SetupPlan expected = provider.plan(options, Objects.requireNonNull(selection, "selection"),
                SetupOperation.fromPlan(plan), platform, architecture);
        requirePlanIdentity(expected, options);
        if (!SetupPlan.bind(expected, options.policyDigest()).equals(plan)) {
            throw new IllegalArgumentException("Plan does not match the provider manifest shipped with this release.");
        }
        return provider;
    }

    private SetupProvider preauthorize(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                       String operation) {
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
        return providers.require(options.profile());
    }

    private SetupSelection authorizedSelectionFromPlan(SetupPlan plan, SetupApproval approval,
                                                       SetupOptions options, String operation) {
        return preauthorize(plan, approval, options, operation).selectionFromPlan(plan);
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
