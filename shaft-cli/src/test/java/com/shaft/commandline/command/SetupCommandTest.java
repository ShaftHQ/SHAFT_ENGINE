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
    void lighthouseProfileUsesTheRegisteredProvider(@TempDir Path temp) throws Exception {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        CommandResult status = execute("setup", "status", "--profile", "LIGHTHOUSE",
                "--cache-root", cache.toString(), "--data-root", data.toString(), "--json");
        Path planFile = temp.resolve("lighthouse-plan.json").toAbsolutePath();
        CommandResult plan = execute("setup", "plan", "--profile", "LIGHTHOUSE", "--mode", "MANAGED",
                "--output", planFile.toString(), "--cache-root", cache.toString(),
                "--data-root", data.toString(), "--json");

        assertEquals(3, status.exitCode(), status.stderr());
        assertEquals("LIGHTHOUSE", JSON.readTree(status.stdout()).get("profile").asText());
        assertEquals(0, plan.exitCode(), plan.stderr());
        assertEquals("LIGHTHOUSE", JSON.readTree(plan.stdout()).get("profile").asText());
    }

    @Test
    void ocrProfileHasProviderBackedStatusAndPlan(@TempDir Path temp) throws Exception {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        CommandResult status = execute("setup", "status", "--profile", "OCR",
                "--cache-root", cache.toString(), "--data-root", data.toString(), "--json");
        assertEquals(3, status.exitCode(), status.stderr());
        assertEquals("OCR", JSON.readTree(status.stdout()).get("profile").asText());

        Path planFile = temp.resolve("ocr-plan.json").toAbsolutePath();
        CommandResult planned = execute("setup", "plan", "--profile", "OCR", "--mode", "MANAGED",
                "--output", planFile.toString(), "--cache-root", cache.toString(),
                "--data-root", data.toString(), "--json");
        assertEquals(0, planned.exitCode(), planned.stderr());
        JsonNode plan = JSON.readTree(planned.stdout());
        assertEquals("OCR", plan.get("profile").asText());
        assertEquals(2, plan.get("actions").size());
        assertEquals("OCR_TESSDATA", plan.get("actions").get(0).get("target").asText());

        Path languagePlan = temp.resolve("ocr-language-plan.json").toAbsolutePath();
        CommandResult selected = execute("setup", "plan", "--profile", "OCR", "--mode", "MANAGED",
                "--language", "fra", "--language", "deu", "--output", languagePlan.toString(),
                "--cache-root", cache.toString(), "--data-root", data.toString(), "--offline", "--json");
        assertEquals(0, selected.exitCode(), selected.stderr());
        JsonNode selectedPlan = JSON.readTree(selected.stdout());
        assertEquals(2, selectedPlan.get("actions").size());
        assertTrue(selectedPlan.get("actions").get(0).get("version").asText().endsWith(":deu"));
        assertTrue(selectedPlan.get("actions").get(1).get("version").asText().endsWith(":fra"));

        CommandResult installWithoutRepeatedLanguages = execute("setup", "install", "--plan",
                languagePlan.toString(), "--approve", selectedPlan.get("digest").asText(),
                "--cache-root", cache.toString(), "--data-root", data.toString(), "--offline");
        assertEquals(5, installWithoutRepeatedLanguages.exitCode());
        assertTrue(installWithoutRepeatedLanguages.stderr().contains("offline cache"),
                installWithoutRepeatedLanguages.stderr());
    }

    @Test
    void androidRequestIsBoundIntoPlanAndRecoveredByInstallWithoutRepeatedSelectors(@TempDir Path temp)
            throws Exception {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        Path planFile = temp.resolve("android-plan.json").toAbsolutePath();

        CommandResult planned = execute("setup", "plan", "--profile", "MOBILE_ANDROID", "--mode", "MANAGED",
                "--output", planFile.toString(), "--cache-root", cache.toString(), "--data-root", data.toString(),
                "--offline", "--avd-name", "cli_avd", "--ram-mb", "6144", "--cores", "4",
                "--port", "4823", "--json");

        assertEquals(0, planned.exitCode(), planned.stderr());
        JsonNode plan = JSON.readTree(planned.stdout());
        assertEquals("MOBILE_ANDROID", plan.get("profile").asText());
        assertEquals(6, plan.get("actions").size());
        String request = plan.get("actions").get(5).get("version").asText();
        assertTrue(request.contains("avd=cli_avd"));
        assertTrue(request.contains("ramMb=6144"));
        assertTrue(request.contains("cores=4"));
        assertTrue(request.contains("port=4823"));
        assertTrue(plan.get("actions").get(4).get("requiredLicenses").toString()
                .contains("android-sdk-license"));

        CommandResult install = execute("setup", "install", "--plan", planFile.toString(),
                "--approve", plan.get("digest").asText(), "--accept-license", "android-sdk-license",
                "--cache-root", cache.toString(), "--data-root", data.toString(), "--offline");
        assertEquals(5, install.exitCode(), install.stderr());
        assertTrue(install.stderr().contains("complete verified installation"), install.stderr());
        assertTrue(Files.notExists(cache));
        assertTrue(Files.notExists(data));

        CommandResult start = execute("setup", "start", "--plan", planFile.toString(),
                "--approve", plan.get("digest").asText(), "--accept-license", "android-sdk-license",
                "--cache-root", cache.toString(), "--data-root", data.toString(), "--offline");
        assertEquals(5, start.exitCode(), start.stderr());
        assertTrue(start.stderr().contains("install receipt"), start.stderr());
        assertTrue(Files.notExists(cache));
        assertTrue(Files.notExists(data));
    }

    @Test
    void androidStopAndLogsReportMissingWithoutCreatingRuntimeState(@TempDir Path temp) {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        CommandResult stop = execute("setup", "stop", "--profile", "MOBILE_ANDROID",
                "--cache-root", cache.toString(), "--data-root", data.toString());
        CommandResult logs = execute("setup", "logs", "--profile", "MOBILE_ANDROID",
                "--cache-root", cache.toString(), "--data-root", data.toString());

        assertEquals(3, stop.exitCode(), stop.stderr());
        assertEquals(3, logs.exitCode(), logs.stderr());
        assertTrue(Files.notExists(cache));
        assertTrue(Files.notExists(data));
    }

    @Test
    void androidSelectorsAreRejectedForUnrelatedProfiles(@TempDir Path temp) {
        CommandResult result = execute("setup", "status", "--profile", "REPORTING", "--port", "4823",
                "--cache-root", temp.resolve("cache").toAbsolutePath().toString(),
                "--data-root", temp.resolve("data").toAbsolutePath().toString());

        assertEquals(2, result.exitCode());
        assertTrue(result.stderr().contains("only for profile MOBILE_ANDROID"), result.stderr());
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
    void invalidAndNonOcrLanguageSelectorsReturnInvalidInputWithoutStackTrace(@TempDir Path temp) {
        String cache = temp.resolve("cache").toAbsolutePath().toString();
        String data = temp.resolve("data").toAbsolutePath().toString();
        CommandResult unknown = execute("setup", "status", "--profile", "OCR", "--language", "zzz",
                "--cache-root", cache, "--data-root", data);
        CommandResult nonOcr = execute("setup", "status", "--profile", "REPORTING", "--language", "eng",
                "--cache-root", cache, "--data-root", data);

        assertEquals(2, unknown.exitCode());
        assertTrue(unknown.stderr().contains("Unsupported OCR language code"));
        assertTrue(!unknown.stderr().contains("\tat "));
        assertEquals(2, nonOcr.exitCode());
        assertTrue(nonOcr.stderr().contains("only for profile OCR"));
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
    void incompletePlanRootsReturnInvalidInputWithoutWritingPlan(@TempDir Path temp) {
        Path planFile = temp.resolve("plan.json").toAbsolutePath();

        CommandResult result = execute("setup", "plan", "--profile", "REPORTING", "--mode", "MANAGED",
                "--output", planFile.toString(), "--cache-root", temp.resolve("cache").toAbsolutePath().toString());

        assertEquals(2, result.exitCode());
        assertTrue(result.stderr().contains("must be supplied together"), result.stderr());
        assertTrue(Files.notExists(planFile));
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

    @Test
    void cliPlanBindsTheSameNonDefaultPolicyAsJavaApi(@TempDir Path temp) throws Exception {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        Path planFile = temp.resolve("policy-plan.json").toAbsolutePath();
        CommandResult result = execute("setup", "plan", "--profile", "REPORTING", "--mode", "MANAGED",
                "--output", planFile.toString(), "--cache-root", cache.toString(), "--data-root", data.toString(),
                "--offline", "--auto-start", "--prefer-system-tools=false", "--reuse-owned-processes=false",
                "--startup-timeout", "PT45S", "--shutdown-timeout", "PT10S", "--json");

        assertEquals(0, result.exitCode(), result.stderr());
        com.shaft.infrastructure.SetupPlan plan = com.shaft.infrastructure.SetupPlanStore.read(planFile);
        com.shaft.infrastructure.ShaftCachePaths paths = new com.shaft.infrastructure.ShaftCachePaths(cache, data,
                cache.resolve("downloads"), data.resolve("tools"), data.resolve("state"), data.resolve("receipts"));
        com.shaft.infrastructure.SetupOptions expected = com.shaft.infrastructure.SetupOptions.defaults(
                com.shaft.infrastructure.SetupProfile.REPORTING, paths)
                .withMode(com.shaft.infrastructure.SetupMode.MANAGED).withOffline(true).withAutoStart(true)
                .withPreferSystemTools(false).withReuseOwnedProcesses(false)
                .withTimeouts(java.time.Duration.ofSeconds(45), java.time.Duration.ofSeconds(10));
        assertEquals(expected.policyDigest(), plan.executionPolicyDigest());
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
