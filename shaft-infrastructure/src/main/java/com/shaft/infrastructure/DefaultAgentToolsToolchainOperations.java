package com.shaft.infrastructure;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Set;

/** Host PATH probes and catalog write for agent tools. */
final class DefaultAgentToolsToolchainOperations implements AgentToolsToolchainOperations {
    private final ShaftCachePaths paths;
    private final SetupPlan plan;
    private final AndroidCommandRunner runner;
    private final boolean offline;

    DefaultAgentToolsToolchainOperations(ShaftCachePaths paths, SetupPlan plan, boolean offline) {
        this(paths, plan, AndroidCommandRunner.system(paths, plan.platform(), plan.architecture()), offline);
    }

    DefaultAgentToolsToolchainOperations(ShaftCachePaths paths, SetupPlan plan, AndroidCommandRunner runner,
                                         boolean offline) {
        this.paths = java.util.Objects.requireNonNull(paths, "paths");
        this.plan = java.util.Objects.requireNonNull(plan, "plan");
        this.runner = java.util.Objects.requireNonNull(runner, "runner");
        this.offline = offline;
    }

    @Override
    public void hostPreflight(List<SetupAction> actions) {
        java.util.Objects.requireNonNull(actions, "actions");
        for (SetupAction action : actions) {
            if (action.target() != SetupTarget.AGENT_CLI) {
                java.util.Objects.requireNonNull(status(action), "status");
            }
        }
    }

    @Override
    public void lockedPreflight(List<SetupAction> actions, boolean requireOffline) throws IOException {
        java.util.Objects.requireNonNull(actions, "actions");
        VerifiedArtifactStore.requireUnlinkedAncestors(catalogFile());
        if ((offline || requireOffline) && !Files.isRegularFile(catalogFile(), LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("Offline agent-tools setup requires a staged client catalog.");
        }
    }

    @Override
    public void install(SetupAction action) throws IOException {
        if (action.target() != SetupTarget.AGENT_CLI) return;
        if (!AgentToolsSetupPlanner.sha256(AgentToolsSetupPlanner.CLIENTS_JSON)
                .equalsIgnoreCase(action.checksum())) {
            throw new IOException("Agent client catalog does not match the approved plan.");
        }
        Path destination = catalogFile();
        Files.createDirectories(destination.getParent());
        Path temporary = Files.createTempFile(destination.getParent(), "agent-clients", ".tmp");
        try {
            Files.writeString(temporary, AgentToolsSetupPlanner.CLIENTS_JSON);
            VerifiedArtifactStore.move(temporary, destination);
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    @Override
    public SetupStatus status(SetupAction action) {
        return switch (action.target()) {
            case JAVA, MAVEN, PYTHON, NODE -> probe(action, probeCommand(action.target(), plan.platform()));
            case AGENT_CLI -> catalogStatus(action);
            default -> new SetupStatus(action.target(), SetupReadiness.MISSING, "",
                    "Unsupported agent-tools target.");
        };
    }

    private SetupStatus catalogStatus(SetupAction action) {
        try {
            Path catalog = catalogFile();
            VerifiedArtifactStore.requireUnlinkedAncestors(catalog);
            if (!Files.isRegularFile(catalog, LinkOption.NOFOLLOW_LINKS)) {
                return new SetupStatus(action.target(), SetupReadiness.MISSING, "",
                        "Managed agent client catalog is missing.");
            }
            String actual = AgentToolsSetupPlanner.sha256(Files.readString(catalog, StandardCharsets.UTF_8));
            if (!actual.equalsIgnoreCase(action.checksum())) {
                return new SetupStatus(action.target(), SetupReadiness.DEGRADED, "",
                        "Staged agent client catalog does not match the approved plan.");
            }
            return new SetupStatus(action.target(), SetupReadiness.READY, action.version(),
                    "Staged agent client catalog matches the reviewed plan.");
        } catch (IOException failure) {
            return new SetupStatus(action.target(), SetupReadiness.DEGRADED, "", failure.getMessage());
        }
    }

    private SetupStatus probe(SetupAction action, List<String> command) {
        try {
            ReportingSetupService.ProcessResult result = runner.run(command, null, Map.of(),
                    Set.of(), null, null, Duration.ofSeconds(15));
            if (result.exitCode() != 0) {
                return new SetupStatus(action.target(), SetupReadiness.MISSING, "",
                        action.target() + " is not available on PATH.");
            }
            return new SetupStatus(action.target(), SetupReadiness.READY, action.version(),
                    result.output().strip());
        } catch (IOException failure) {
            return new SetupStatus(action.target(), SetupReadiness.MISSING, "", failure.getMessage());
        }
    }

    private Path catalogFile() {
        return paths.tools().resolve("agent-tools").resolve("agent-clients.json");
    }

    static List<String> probeCommand(SetupTarget target, SetupPlatform platform) {
        boolean windows = platform == SetupPlatform.WINDOWS;
        return switch (target) {
            case JAVA -> List.of(windows ? "java.exe" : "java", "-version");
            case MAVEN -> List.of(windows ? "mvn.cmd" : "mvn", "-version");
            case PYTHON -> windows ? List.of("py", "-3", "--version") : List.of("python3", "--version");
            case NODE -> List.of(windows ? "node.exe" : "node", "--version");
            default -> throw new IllegalArgumentException("No host probe for " + target);
        };
    }
}
