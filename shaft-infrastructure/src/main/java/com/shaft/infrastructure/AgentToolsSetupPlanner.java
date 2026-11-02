package com.shaft.infrastructure;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.List;
import java.util.Set;

/** Release-coupled diagnostic plans for host agent tools. */
final class AgentToolsSetupPlanner {
    static final String JAVA_VERSION = "25+";
    static final String MAVEN_VERSION = "3.9.0+";
    static final String PYTHON_VERSION = "3.10+";
    static final String NODE_VERSION = "20+";
    static final String CLIENTS_VERSION = "1";
    static final String CLIENTS_JSON = """
            {
              "schemaVersion": 1,
              "clients": [
                {"id": "gh", "argv": ["gh", "--version"]}
              ]
            }
            """;

    private AgentToolsSetupPlanner() {
        throw new IllegalStateException("Utility class");
    }

    static SetupPlan plan(SetupPlatform platform, SetupArchitecture architecture, SetupMode mode) {
        SetupActionKind cliKind = mode == SetupMode.EXTERNAL ? SetupActionKind.DIAGNOSE : SetupActionKind.INSTALL;
        return SetupPlan.create(SetupProfile.AGENT_TOOLS, platform, architecture, mode, List.of(
                diagnose(SetupTarget.JAVA, JAVA_VERSION),
                diagnose(SetupTarget.MAVEN, MAVEN_VERSION),
                diagnose(SetupTarget.PYTHON, PYTHON_VERSION),
                diagnose(SetupTarget.NODE, NODE_VERSION),
                new SetupAction(SetupTarget.AGENT_CLI, cliKind, CLIENTS_VERSION,
                        URI.create("urn:shaft:agent-tools:clients:" + CLIENTS_VERSION),
                        sha256(CLIENTS_JSON), CLIENTS_JSON.getBytes(StandardCharsets.UTF_8).length, false, Set.of())));
    }

    private static SetupAction diagnose(SetupTarget target, String version) {
        return new SetupAction(target, SetupActionKind.DIAGNOSE, version,
                URI.create("urn:shaft:host:" + target.name().toLowerCase()),
                sha256(target.name() + version), false, Set.of());
    }

    static String sha256(String value) {
        try {
            return "sha256:" + HexFormat.of().formatHex(
                    MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8)));
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException("SHA-256 is required by the Java platform.", impossible);
        }
    }
}
