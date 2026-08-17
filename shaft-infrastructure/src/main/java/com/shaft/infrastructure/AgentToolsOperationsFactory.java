package com.shaft.infrastructure;

@FunctionalInterface
interface AgentToolsOperationsFactory {
    AgentToolsToolchainOperations create(ShaftCachePaths paths, SetupPlan plan, boolean offline);
}
