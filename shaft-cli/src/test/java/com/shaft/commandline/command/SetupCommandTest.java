package com.shaft.commandline.command;

import com.shaft.commandline.ShaftCli;
import com.shaft.infrastructure.InfrastructureSetupService;
import com.shaft.infrastructure.SetupMode;
import com.shaft.infrastructure.SetupOptions;
import com.shaft.infrastructure.SetupPlan;
import com.shaft.infrastructure.SetupPlanStore;
import com.shaft.infrastructure.SetupProfile;
import com.shaft.infrastructure.SetupSelection;
import com.shaft.infrastructure.ShaftCachePaths;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import picocli.CommandLine;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SetupCommandTest {
    @Test
    void failureDetailsPreserveOuterActionAndNestedCause() {
        RuntimeException failure = new RuntimeException("Setup action failed",
                new IllegalStateException("Appium manifest is invalid", new java.io.IOException("lock mismatch")));

        assertEquals("Setup action failed: Appium manifest is invalid: lock mismatch",
                SetupCommand.failureDetails(failure));
    }

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
        assertEquals(4, persisted.get("schemaVersion").asInt());
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
            assertEquals(0L, action.get("artifactBytes").asLong());
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
    void playwrightProfileUsesTheRegisteredProviderWithoutMutatingOnStatus(@TempDir Path temp) throws Exception {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        CommandResult status = execute("setup", "status", "--profile", "PLAYWRIGHT",
                "--cache-root", cache.toString(), "--data-root", data.toString(), "--json");
        Path planFile = temp.resolve("playwright-plan.json").toAbsolutePath();
        CommandResult plan = execute("setup", "plan", "--profile", "PLAYWRIGHT", "--mode", "MANAGED",
                "--output", planFile.toString(), "--cache-root", cache.toString(),
                "--data-root", data.toString(), "--json");

        if (status.exitCode() == 3) {
            JsonNode report = JSON.readTree(status.stdout());
            assertEquals("PLAYWRIGHT", report.get("profile").asText());
            assertEquals(5, report.get("targets").size());
        } else {
            assertEquals(2, status.exitCode(), status.stderr());
            assertTrue(status.stderr().contains("Unsupported Playwright host"), status.stderr());
        }
        if (plan.exitCode() == 0) {
            JsonNode planned = JSON.readTree(plan.stdout());
            assertEquals("PLAYWRIGHT", planned.get("profile").asText());
            assertEquals(5, planned.get("actions").size());
        } else {
            assertEquals(2, plan.exitCode(), plan.stderr());
            assertTrue(plan.stderr().contains("Unsupported Playwright host"), plan.stderr());
        }
        assertTrue(Files.notExists(cache));
        assertTrue(Files.notExists(data));
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
                "--data-root", data.toString(), "--offline", "--json");
        assertEquals(0, planned.exitCode(), planned.stderr());
        JsonNode plan = JSON.readTree(planned.stdout());
        assertEquals("OCR", plan.get("profile").asText());
        assertEquals(2, plan.get("actions").size());
        assertEquals("OCR_TESSDATA", plan.get("actions").get(0).get("target").asText());
        assertTrue(plan.get("actions").get(0).get("version").asText().endsWith(":ara"));
        assertTrue(plan.get("actions").get(1).get("version").asText().endsWith(":eng"));

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

        CommandResult installBaselineWithoutLanguages = execute("setup", "install", "--plan",
                planFile.toString(), "--approve", plan.get("digest").asText(),
                "--cache-root", cache.toString(), "--data-root", data.toString(), "--offline");
        assertEquals(5, installBaselineWithoutLanguages.exitCode(), installBaselineWithoutLanguages.stderr());
        assertFalse(installBaselineWithoutLanguages.stderr().contains(
                "Plan does not match the provider manifest shipped with this release."));
        assertTrue(installBaselineWithoutLanguages.stderr().contains("offline cache"),
                installBaselineWithoutLanguages.stderr());
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
    void androidReadinessCommandsCanInspectManagedState(@TempDir Path temp) throws Exception {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();

        for (String command : java.util.List.of("doctor", "status", "verify")) {
            CommandResult result = execute("setup", command, "--profile", "MOBILE_ANDROID",
                    "--mode", "MANAGED", "--cache-root", cache.toString(), "--data-root", data.toString(),
                    "--json");

            assertEquals(3, result.exitCode(), result.stderr());
            JsonNode report = JSON.readTree(result.stdout());
            assertEquals("MOBILE_ANDROID", report.get("profile").asText());
            report.get("targets").forEach(target -> assertTrue(
                    !target.get("detail").asText().contains("External mode is diagnostic-only"),
                    command + " must inspect managed readiness instead of forcing EXTERNAL mode."));
        }
        assertTrue(Files.notExists(cache));
        assertTrue(Files.notExists(data));
    }

    @Test
    void androidStopRequiresAnExactPlanAndLogsReportMissingWithoutCreatingRuntimeState(@TempDir Path temp)
            throws Exception {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        Path planFile = temp.resolve("android-stop-plan.json");
        CommandResult planned = execute("setup", "plan", "--profile", "MOBILE_ANDROID", "--mode", "MANAGED",
                "--output", planFile.toString(), "--cache-root", cache.toString(), "--data-root", data.toString(),
                "--json");
        JsonNode plan = JSON.readTree(planned.stdout());
        CommandResult stop = execute("setup", "stop", "--plan", planFile.toString(),
                "--approve", plan.get("digest").asText(), "--accept-license", "android-sdk-license",
                "--cache-root", cache.toString(), "--data-root", data.toString());
        CommandResult logs = execute("setup", "logs", "--profile", "MOBILE_ANDROID",
                "--cache-root", cache.toString(), "--data-root", data.toString());
        CommandResult unsafeCompatibility = execute("setup", "stop", "--profile", "MOBILE_ANDROID",
                "--cache-root", cache.toString(), "--data-root", data.toString());

        assertEquals(0, planned.exitCode(), planned.stderr());
        assertEquals(3, stop.exitCode(), stop.stderr());
        assertEquals(3, logs.exitCode(), logs.stderr());
        assertEquals(2, unsafeCompatibility.exitCode(), unsafeCompatibility.stderr());
        assertTrue(unsafeCompatibility.stderr().contains("--plan"), unsafeCompatibility.stderr());
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

    @Test
    void localAiStatusPlanAndRejectedOfflineInstallUseTheSharedProvider(@TempDir Path temp) throws Exception {
        Path cache = temp.resolve("managed-ai-cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        Path planFile = temp.resolve("local-ai-plan.json").toAbsolutePath();
        Files.createDirectories(cache);
        com.shaft.driver.SHAFT.Properties.managedLocalAi.set().enabled(true)
                .model("qwen3-0.6b-q8_0").cacheDirectory(cache.toString());
        try {
            var lifecycle = new com.shaft.ai.local.ManagedLocalAiService().inspect();
            CommandResult status = execute("setup", "status", "--profile", "LOCAL_AI", "--mode", "MANAGED",
                    "--cache-root", cache.toString(), "--data-root", data.toString(), "--json");
            assertEquals(3, status.exitCode(), status.stderr());
            assertTrue(status.stdout().contains("MANAGED_LOCAL_AI_RUNTIME"));
            assertTrue(status.stdout().contains("MANAGED_LOCAL_AI_MODEL"));
            assertDirectoryEmpty(cache);
            assertFalse(Files.exists(data));

            CommandResult planned = execute("setup", "plan", "--profile", "LOCAL_AI", "--mode", "MANAGED",
                    "--output", planFile.toString(), "--cache-root", cache.toString(),
                    "--data-root", data.toString(), "--offline", "--json");
            if (lifecycle.selectedModelId() == null) {
                assertEquals(5, planned.exitCode(), planned.stderr());
                assertTrue(planned.stderr().contains("cannot select a reviewed model"));
                assertDirectoryEmpty(cache);
                assertFalse(Files.exists(data));
                return;
            }
            assertEquals(0, planned.exitCode(), planned.stderr());
            com.shaft.infrastructure.SetupPlan plan = com.shaft.infrastructure.SetupPlanStore.read(planFile);
            assertEquals(java.util.List.of(com.shaft.infrastructure.SetupTarget.MANAGED_LOCAL_AI_RUNTIME,
                            com.shaft.infrastructure.SetupTarget.MANAGED_LOCAL_AI_MODEL),
                    plan.actions().stream().map(com.shaft.infrastructure.SetupAction::target).toList());
            assertDirectoryEmpty(cache);
            assertFalse(Files.exists(data));

            CommandResult install = execute("setup", "install", "--plan", planFile.toString(),
                    "--approve", plan.digest(), "--accept-license", "MIT", "--accept-license", "Apache-2.0",
                    "--cache-root", cache.toString(), "--data-root", data.toString(), "--offline");
            assertEquals(5, install.exitCode(), install.stderr());
            assertTrue(install.stderr().contains("offline setup cannot download"));
            assertDirectoryEmpty(cache);
            assertFalse(Files.exists(data));
        } finally {
            com.shaft.properties.internal.Properties.clearForCurrentThread();
        }
    }

    @Test
    void unsupportedMaintenanceOperationIsRejectedByTheProviderInsteadOfTheCliParser(@TempDir Path temp) {
        Path planFile = temp.resolve("reporting-clean-plan.json").toAbsolutePath();

        CommandResult result = execute("setup", "plan", "--profile", "REPORTING", "--mode", "MANAGED",
                "--operation", "CLEAN", "--output", planFile.toString(), "--json");

        assertEquals(2, result.exitCode());
        assertTrue(result.stderr().contains("REPORTING does not support CLEAN"), result.stderr());
        assertFalse(Files.exists(planFile));
    }

    @Test
    void externalCleanIsRejectedBeforeWritingAPlan(@TempDir Path temp) {
        Path planFile = temp.resolve("local-ai-clean-plan.json").toAbsolutePath();

        CommandResult result = execute("setup", "plan", "--profile", "LOCAL_AI",
                "--operation", "CLEAN", "--output", planFile.toString(), "--json");

        assertEquals(2, result.exitCode());
        assertTrue(result.stderr().contains("CLEAN requires MANAGED mode"), result.stderr());
        assertFalse(Files.exists(planFile));
    }

    @Test
    void localAiRollbackReachesTheProviderAndFailsClosedWithoutAReviewedCandidate(@TempDir Path temp)
            throws Exception {
        Path cache = temp.resolve("managed-ai-cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        Path planFile = temp.resolve("local-ai-rollback-plan.json").toAbsolutePath();
        Files.createDirectories(cache);
        com.shaft.driver.SHAFT.Properties.managedLocalAi.set().enabled(true)
                .model("qwen3-0.6b-q8_0").cacheDirectory(cache.toString());
        try {
            CommandResult result = execute("setup", "plan", "--profile", "LOCAL_AI", "--mode", "MANAGED",
                    "--operation", "ROLLBACK", "--output", planFile.toString(),
                    "--cache-root", cache.toString(), "--data-root", data.toString(), "--json");

            assertEquals(5, result.exitCode(), result.stderr());
            assertTrue(result.stderr().contains("No reviewed managed local AI rollback candidate"), result.stderr());
            assertFalse(Files.exists(planFile));
            assertDirectoryEmpty(cache);
            assertFalse(Files.exists(data));
        } finally {
            com.shaft.properties.internal.Properties.clearForCurrentThread();
        }
    }

    @Test
    void bareSetupPrintsUsage() {
        CommandResult result = execute("setup");

        assertEquals(0, result.exitCode(), result.stderr());
        assertTrue(result.stdout().contains("catalog"), result.stdout());
        assertTrue(result.stdout().contains("Usage") || result.stdout().contains("usage"), result.stdout());
    }

    @Test
    void catalogPrintsProfilesAsTextAndJson() throws Exception {
        CommandResult text = execute("setup", "catalog");
        CommandResult json = execute("setup", "profiles", "--json");

        assertEquals(0, text.exitCode(), text.stderr());
        assertTrue(text.stdout().contains("WEB_LOCAL\t"), text.stdout());
        assertTrue(text.stdout().contains("REPORTING\t"), text.stdout());
        assertEquals(0, json.exitCode(), json.stderr());
        JsonNode catalog = JSON.readTree(json.stdout());
        assertTrue(catalog.toString().contains("WEB_LOCAL"), catalog.toString());
    }

    @Test
    void planWithoutJsonPrintsPathAndDigest(@TempDir Path temp) throws Exception {
        Path planFile = temp.resolve("text-plan.json").toAbsolutePath();

        CommandResult result = execute("setup", "plan", "--profile", "REPORTING", "--mode", "MANAGED",
                "--output", planFile.toString());

        assertEquals(0, result.exitCode(), result.stderr());
        assertTrue(result.stdout().contains("Plan:"), result.stdout());
        assertTrue(result.stdout().contains("Digest:"), result.stdout());
        assertTrue(result.stdout().contains("sha256:"), result.stdout());
        assertTrue(Files.isRegularFile(planFile));
    }

    @Test
    void relativePlanOutputIsRejectedBeforeWriting() throws Exception {
        Path relative = Path.of("shaft-cli-relative-plan-7836.json");
        try {
            CommandResult result = execute("setup", "plan", "--profile", "REPORTING", "--mode", "MANAGED",
                    "--output", relative.toString());

            assertEquals(2, result.exitCode(), result.stderr());
            assertTrue(result.stderr().contains("absolute path"), result.stderr());
            assertTrue(Files.notExists(relative));
        } finally {
            Files.deleteIfExists(relative);
        }
    }

    @Test
    void reportingStatusWithoutJsonPrintsTargetRows(@TempDir Path temp) {
        CommandResult result = execute("setup", "status", "--profile", "REPORTING",
                "--cache-root", temp.resolve("cache").toAbsolutePath().toString(),
                "--data-root", temp.resolve("data").toAbsolutePath().toString());

        assertEquals(3, result.exitCode(), result.stderr());
        assertTrue(result.stdout().contains("NODE\t"), result.stdout());
        assertTrue(result.stdout().contains("ALLURE\t"), result.stdout());
    }

    @Test
    void unsupportedProfileFailsClosedForPlanStatusAndLogs(@TempDir Path temp) {
        String cache = temp.resolve("cache").toAbsolutePath().toString();
        String data = temp.resolve("data").toAbsolutePath().toString();

        CommandResult plan = execute("setup", "plan", "--profile", "WEB_LOCAL", "--output",
                temp.resolve("web-local.json").toAbsolutePath().toString());
        CommandResult status = execute("setup", "status", "--profile", "WEB_LOCAL",
                "--cache-root", cache, "--data-root", data);
        CommandResult logs = execute("setup", "logs", "--profile", "WEB_LOCAL",
                "--cache-root", cache, "--data-root", data);

        assertEquals(4, plan.exitCode(), plan.stderr());
        assertTrue(plan.stderr().contains("No setup provider is available for profile WEB_LOCAL"), plan.stderr());
        assertEquals(4, status.exitCode(), status.stderr());
        assertTrue(status.stderr().contains("No lifecycle provider is available for profile WEB_LOCAL"),
                status.stderr());
        assertEquals(4, logs.exitCode(), logs.stderr());
        assertTrue(logs.stderr().contains("No lifecycle provider is available for profile WEB_LOCAL"), logs.stderr());
        assertTrue(Files.notExists(temp.resolve("web-local.json")));
    }

    @Test
    void ocrLogsAreNotExposedByTheProvider(@TempDir Path temp) {
        CommandResult logs = execute("setup", "logs", "--profile", "OCR",
                "--cache-root", temp.resolve("cache").toAbsolutePath().toString(),
                "--data-root", temp.resolve("data").toAbsolutePath().toString());

        assertEquals(4, logs.exitCode(), logs.stderr());
        assertTrue(logs.stderr().contains("No lifecycle provider is available for profile OCR"), logs.stderr());
    }

    @Test
    void invalidStartupTimeoutIsRejectedBeforeAPlanIsWritten(@TempDir Path temp) {
        Path planFile = temp.resolve("bad-timeout.json").toAbsolutePath();

        CommandResult result = execute("setup", "plan", "--profile", "REPORTING", "--mode", "MANAGED",
                "--output", planFile.toString(), "--startup-timeout", "nope");

        assertEquals(2, result.exitCode(), result.stderr());
        assertTrue(result.stderr().contains("ISO-8601"), result.stderr());
        assertTrue(Files.notExists(planFile));
    }

    @Test
    void nonOcrLanguageOnInstallAndStartIsRejected(@TempDir Path temp) throws Exception {
        Path planFile = temp.resolve("reporting-plan.json").toAbsolutePath();
        CommandResult planned = execute("setup", "plan", "--profile", "REPORTING", "--mode", "MANAGED",
                "--output", planFile.toString(), "--json");
        assertEquals(0, planned.exitCode(), planned.stderr());
        String digest = JSON.readTree(planned.stdout()).get("digest").asText();

        CommandResult install = execute("setup", "install", "--plan", planFile.toString(),
                "--approve", digest, "--language", "eng");
        CommandResult start = execute("setup", "start", "--plan", planFile.toString(),
                "--approve", digest, "--language", "eng");

        assertEquals(2, install.exitCode(), install.stderr());
        assertTrue(install.stderr().contains("only for profile OCR"), install.stderr());
        assertEquals(2, start.exitCode(), start.stderr());
        assertTrue(start.stderr().contains("only for profile OCR"), start.stderr());
    }

    @Test
    void ocrInstallRejectsLanguagesThatAreNotInTheReviewedPlan(@TempDir Path temp) throws Exception {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        Path planFile = temp.resolve("ocr-plan.json").toAbsolutePath();
        CommandResult planned = execute("setup", "plan", "--profile", "OCR", "--mode", "MANAGED",
                "--language", "fra", "--output", planFile.toString(), "--cache-root", cache.toString(),
                "--data-root", data.toString(), "--offline", "--json");
        assertEquals(0, planned.exitCode(), planned.stderr());
        String digest = JSON.readTree(planned.stdout()).get("digest").asText();

        CommandResult mismatch = execute("setup", "install", "--plan", planFile.toString(),
                "--approve", digest, "--language", "eng", "--cache-root", cache.toString(),
                "--data-root", data.toString(), "--offline");
        assertEquals(2, mismatch.exitCode(), mismatch.stderr());
        assertTrue(mismatch.stderr().contains("does not match the reviewed OCR plan"), mismatch.stderr());
        assertTrue(Files.notExists(cache));
        assertTrue(Files.notExists(data));
    }

    @Test
    void androidInstallRejectsSelectorsThatDifferFromTheReviewedPlan(@TempDir Path temp) throws Exception {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        Path planFile = temp.resolve("android-plan.json").toAbsolutePath();
        CommandResult planned = execute("setup", "plan", "--profile", "MOBILE_ANDROID", "--mode", "MANAGED",
                "--output", planFile.toString(), "--cache-root", cache.toString(), "--data-root", data.toString(),
                "--offline", "--avd-name", "cli_avd", "--ram-mb", "6144", "--cores", "4", "--port", "4823", "--json");
        assertEquals(0, planned.exitCode(), planned.stderr());
        String digest = JSON.readTree(planned.stdout()).get("digest").asText();

        CommandResult install = execute("setup", "install", "--plan", planFile.toString(),
                "--approve", digest, "--avd-name", "other_avd", "--cache-root", cache.toString(),
                "--data-root", data.toString(), "--offline");

        assertEquals(2, install.exitCode(), install.stderr());
        assertTrue(install.stderr().contains("do not match the reviewed plan"), install.stderr());
        assertTrue(Files.notExists(cache));
        assertTrue(Files.notExists(data));
    }

    @Test
    void healeniumSelectionIsRebuiltBeforeAStaleApprovalIsRejected(@TempDir Path temp) throws Exception {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        SetupSelection chosen = new SetupSelection(List.of("backend_8888", "imitate_9000"));
        SetupOptions options = SetupOptions.defaults(SetupProfile.HEALENIUM, new ShaftCachePaths(
                cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"))).withMode(SetupMode.MANAGED);
        SetupPlan plan = InfrastructureSetupService.builtIn().plan(options, chosen);
        SetupSelection rebuilt = selectionFromPlan(plan);
        assertEquals(chosen, rebuilt);
        assertNotEquals(SetupSelection.defaults(), rebuilt);

        Path planFile = temp.resolve("healenium-plan.json").toAbsolutePath();
        SetupPlanStore.write(planFile, plan);
        CommandResult install = execute("setup", "install", "--plan", planFile.toString(),
                "--approve", "sha256:" + "0".repeat(64), "--cache-root", cache.toString(),
                "--data-root", data.toString(), "--offline");

        assertEquals(2, install.exitCode(), install.stderr());
        assertTrue(install.stderr().contains("approval is stale"), install.stderr());
        assertTrue(Files.notExists(cache));
        assertTrue(Files.notExists(data));
    }

    private static SetupSelection selectionFromPlan(SetupPlan plan) throws Exception {
        Method method = SetupCommand.class.getDeclaredMethod("selectionFromPlan",
                SetupPlan.class, List.class, SetupCommand.AndroidOptions.class);
        method.setAccessible(true);
        return (SetupSelection) method.invoke(null, plan, List.of(), new SetupCommand.AndroidOptions());
    }

    @Test
    void stopOfAnUnreadablePlanFailsClosed(@TempDir Path temp) throws Exception {
        Path planDir = temp.resolve("plan-dir");
        Files.createDirectories(planDir);

        CommandResult stop = execute("setup", "stop", "--plan", planDir.toString(),
                "--approve", "sha256:" + "a".repeat(64));

        assertEquals(5, stop.exitCode(), stop.stderr());
        assertTrue(stop.stderr().contains("Is a directory") || stop.stderr().contains("plan-dir"),
                stop.stderr());
        assertFalse(stop.stderr().contains("\tat "));
    }

    @Test
    void directUnsupportedLifecycleReportsTheProfile() {
        StringWriter err = new StringWriter();
        int exit = new CommandLine(new SetupCommand.UnsupportedLifecycle())
                .setErr(new PrintWriter(err, true))
                .execute("--profile", "WEB_LOCAL");

        assertEquals(4, exit);
        assertTrue(err.toString().contains("No lifecycle provider is available for profile WEB_LOCAL"), err.toString());
    }

    @Test
    void failureDetailsUsesTheTypeNameWhenEveryMessageIsBlank() {
        assertEquals("NullPointerException", SetupCommand.failureDetails(new NullPointerException()));
        assertEquals("IllegalStateException", SetupCommand.failureDetails(new IllegalStateException(" ")));
        assertEquals("RuntimeException",
                SetupCommand.failureDetails(new RuntimeException("RuntimeException", new IllegalStateException(" "))));
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

    private static void assertDirectoryEmpty(Path directory) throws Exception {
        try (var entries = Files.list(directory)) {
            assertTrue(entries.findAny().isEmpty());
        }
    }

    private record CommandResult(int exitCode, String stdout, String stderr) { }
}
