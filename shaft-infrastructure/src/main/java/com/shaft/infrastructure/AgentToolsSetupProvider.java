package com.shaft.infrastructure;

import java.io.IOException;

/** Built-in diagnostic provider for host agent tools. */
final class AgentToolsSetupProvider implements SetupProvider {
    private final AgentToolsOperationsFactory operationsFactory;

    AgentToolsSetupProvider() {
        this(DefaultAgentToolsToolchainOperations::new);
    }

    AgentToolsSetupProvider(AgentToolsOperationsFactory operationsFactory) {
        this.operationsFactory = java.util.Objects.requireNonNull(operationsFactory, "operationsFactory");
    }

    @Override
    public SetupProfile profile() {
        return SetupProfile.AGENT_TOOLS;
    }

    @Override
    public SetupPlan plan(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        return AgentToolsSetupPlanner.plan(platform, architecture, options.effectiveMode());
    }

    @Override
    public SetupReport status(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        SetupPlan plan = plan(options, platform, architecture);
        AgentToolsToolchainOperations operations = operationsFactory.create(options.paths(), plan, options.offline());
        return SetupReport.from(new AgentToolsSetupService(options.paths(), plan, operations, options.offline())
                .status());
    }

    @Override
    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        SetupExecutor.validate(plan, approval);
        SetupPlan providerPlan = canonicalPlan(plan, options);
        AgentToolsToolchainOperations operations = operationsFactory.create(options.paths(), providerPlan,
                options.offline());
        SetupReceipt receipt = new AgentToolsSetupService(options.paths(), providerPlan, operations,
                options.offline()).install(providerPlan,
                new SetupApproval(providerPlan.digest(), approval.approvedAt(), approval.acceptedLicenses()));
        return new SetupReceipt(plan.digest(), receipt.completedAt(), receipt.completedActions());
    }

    private SetupPlan canonicalPlan(SetupPlan plan, SetupOptions options) {
        if (options.profile() != profile()) {
            throw new IllegalArgumentException("Options do not select agent-tools setup.");
        }
        SetupPlan canonical = AgentToolsSetupPlanner.plan(plan.platform(), plan.architecture(), plan.mode());
        if (!SetupPlan.bind(canonical, options.policyDigest()).equals(plan)) {
            throw new IllegalArgumentException(
                    "Plan does not match the agent-tools manifest shipped with this release.");
        }
        return canonical;
    }
}
