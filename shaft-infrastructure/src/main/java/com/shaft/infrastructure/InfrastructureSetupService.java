package com.shaft.infrastructure;

import java.io.IOException;
import java.util.List;
import java.util.Objects;

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
        return new InfrastructureSetupService(new SetupProviderRegistry(List.of(new ReportingSetupProvider())),
                platform, architecture);
    }

    public SetupCatalog catalog() {
        return SetupCatalog.builtIn();
    }

    public SetupPlan plan(SetupOptions options) {
        SetupOptions value = Objects.requireNonNull(options, "options");
        SetupPlan plan = providers.require(value.profile()).plan(value, platform, architecture);
        requirePlanIdentity(plan, value);
        return SetupPlan.bind(plan, value.policyDigest());
    }

    public SetupReport doctor(SetupOptions options) {
        return status(options);
    }

    public SetupReport status(SetupOptions options) {
        SetupOptions value = Objects.requireNonNull(options, "options");
        SetupReport report = providers.require(value.profile()).status(value, platform, architecture);
        if (report.profile() != value.profile()) {
            throw new IllegalStateException("Setup provider returned a report for " + report.profile()
                    + " instead of " + value.profile() + '.');
        }
        return report;
    }

    public SetupReport verify(SetupOptions options) {
        return status(options);
    }

    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        SetupProvider provider = authorize(plan, approval, options, "mutate the host");
        SetupReceipt receipt = provider.install(plan, approval, options);
        requireReceiptIdentity(receipt, plan);
        return receipt;
    }

    public ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options)
            throws IOException {
        SetupProvider provider = authorize(plan, approval, options, "start a managed service");
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

    private SetupProvider authorize(SetupPlan plan, SetupApproval approval, SetupOptions options, String operation) {
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
        SetupPlan expected = provider.plan(options, platform, architecture);
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
}
