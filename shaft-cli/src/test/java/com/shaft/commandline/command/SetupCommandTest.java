package com.shaft.commandline.command;

import com.shaft.commandline.ShaftCli;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import picocli.CommandLine;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SetupCommandTest {
    private static final JsonMapper JSON = JsonMapper.builder().build();

    @Test
    void reportingPlanIsPersistedAndPrintedWithExactArtifacts(@TempDir Path temp) throws Exception {
        Path planFile = temp.resolve("reporting-plan.json");
        StringWriter stdout = new StringWriter();
        StringWriter stderr = new StringWriter();
        CommandLine cli = new CommandLine(new ShaftCli())
                .setOut(new PrintWriter(stdout, true))
                .setErr(new PrintWriter(stderr, true));

        int exitCode = cli.execute("setup", "plan", "--profile", "REPORTING", "--mode", "MANAGED",
                "--output", planFile.toString(), "--json");

        assertEquals(0, exitCode, stderr.toString());
        assertTrue(Files.isRegularFile(planFile));
        JsonNode printed = JSON.readTree(stdout.toString());
        JsonNode persisted = JSON.readTree(Files.readString(planFile));
        assertEquals(persisted, printed);
        assertEquals(3, persisted.get("schemaVersion").asInt());
        assertTrue(persisted.get("executionPolicyDigest").asText().matches("sha256:[0-9a-f]{64}"));
        assertEquals("REPORTING", persisted.get("profile").asText());
        assertEquals("MANAGED", persisted.get("mode").asText());
        assertTrue(persisted.get("digest").asText().matches("sha256:[0-9a-f]{64}"));
        assertEquals("NODE", persisted.get("actions").get(0).get("target").asText());
        assertEquals("ALLURE", persisted.get("actions").get(1).get("target").asText());
        persisted.get("actions").forEach(action -> {
            assertTrue(action.hasNonNull("version"));
            assertTrue(action.hasNonNull("source"));
            assertTrue(action.get("checksum").asText().matches("sha256:[0-9a-f]{64}"));
            assertTrue(action.hasNonNull("dependencyLockChecksum"));
            assertTrue(action.has("requiredLicenses"));
            assertTrue(action.has("privileged"));
        });
    }

    @Test
    void staleInstallApprovalFailsBeforeCreatingManagedState(@TempDir Path temp) throws Exception {
        Path planFile = temp.resolve("reporting-plan.json");
        CommandResult planned = execute("setup", "plan", "--profile", "REPORTING", "--mode", "MANAGED",
                "--output", planFile.toString(), "--json");
        assertEquals(0, planned.exitCode(), planned.stderr());
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();

        CommandResult installed = execute("setup", "install", "--plan", planFile.toString(),
                "--approve", "sha256:" + "0".repeat(64), "--cache-root", cache.toString(),
                "--data-root", data.toString(), "--json");

        assertEquals(2, installed.exitCode());
        assertTrue(installed.stderr().contains("approval is stale"), installed.stderr());
        assertTrue(Files.notExists(cache));
        assertTrue(Files.notExists(data));
    }

    @Test
    void missingReportingToolsAreReportedAsMissing(@TempDir Path temp) throws Exception {
        CommandResult result = execute("setup", "status", "--profile", "REPORTING",
                "--cache-root", temp.resolve("cache").toAbsolutePath().toString(),
                "--data-root", temp.resolve("data").toAbsolutePath().toString(), "--json");

        assertEquals(3, result.exitCode());
        JsonNode status = JSON.readTree(result.stdout());
        assertEquals(1, status.get("schemaVersion").asInt());
        assertEquals("MISSING", status.get("readiness").asText());
        assertEquals("NODE", status.get("targets").get(0).get("target").asText());
        assertEquals("ALLURE", status.get("targets").get(1).get("target").asText());
    }

    @Test
    void setupHelpExposesOnlyTheApprovedVersionOneTree() {
        CommandResult result = execute("setup", "--help");

        assertEquals(0, result.exitCode(), result.stderr());
        for (String command : java.util.List.of("catalog", "profiles", "doctor", "status", "plan", "install",
                "apply", "update", "verify", "start", "stop", "logs")) {
            assertTrue(result.stdout().contains(command), command);
        }
        for (String deferred : java.util.List.of("uninstall", "cache-clean")) {
            assertTrue(!result.stdout().contains(deferred), deferred);
        }
    }

    @Test
    void relativeMutationRootsAreRejectedBeforeStateCreation(@TempDir Path temp) throws Exception {
        Path planFile = temp.resolve("plan.json");
        CommandResult planned = execute("setup", "plan", "--profile", "REPORTING", "--mode", "MANAGED",
                "--output", planFile.toString(), "--json");
        JsonNode plan = JSON.readTree(planned.stdout());

        CommandResult result = execute("setup", "install", "--plan", planFile.toString(),
                "--approve", plan.get("digest").asText(), "--cache-root", "relative-cache",
                "--data-root", "relative-data");

        assertEquals(2, result.exitCode());
        assertTrue(result.stderr().contains("must be absolute"));
    }

    @Test
    void foreignPlatformPlanIsRejectedBeforeStateCreation(@TempDir Path temp) throws Exception {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        com.shaft.infrastructure.ShaftCachePaths paths = new com.shaft.infrastructure.ShaftCachePaths(
                cache, data, cache.resolve("downloads"), data.resolve("tools"), data.resolve("state"),
                data.resolve("receipts"));
        com.shaft.infrastructure.SetupPlatform current = com.shaft.infrastructure.SetupPlatform.current();
        com.shaft.infrastructure.SetupPlatform foreign = current == com.shaft.infrastructure.SetupPlatform.LINUX
                ? com.shaft.infrastructure.SetupPlatform.WINDOWS : com.shaft.infrastructure.SetupPlatform.LINUX;
        com.shaft.infrastructure.SetupOptions options = com.shaft.infrastructure.SetupOptions.defaults(
                com.shaft.infrastructure.SetupProfile.REPORTING, paths)
                .withMode(com.shaft.infrastructure.SetupMode.MANAGED);
        com.shaft.infrastructure.SetupPlan plan = com.shaft.infrastructure.InfrastructureSetupService.builtIn(
                foreign, com.shaft.infrastructure.SetupArchitecture.current()).plan(options);
        Path planFile = temp.resolve("foreign-plan.json");
        com.shaft.infrastructure.SetupPlanStore.write(planFile, plan);

        CommandResult result = execute("setup", "install", "--plan", planFile.toString(),
                "--approve", plan.digest(), "--cache-root", cache.toString(), "--data-root", data.toString());

        assertEquals(2, result.exitCode());
        assertTrue(Files.notExists(cache));
        assertTrue(Files.notExists(data));
    }

    private static CommandResult execute(String... arguments) {
        StringWriter stdout = new StringWriter();
        StringWriter stderr = new StringWriter();
        int exitCode = new CommandLine(new ShaftCli())
                .setOut(new PrintWriter(stdout, true))
                .setErr(new PrintWriter(stderr, true))
                .execute(arguments);
        return new CommandResult(exitCode, stdout.toString(), stderr.toString());
    }

    private record CommandResult(int exitCode, String stdout, String stderr) { }
}
