package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

class AssistantCodegenWorkflowCoordinatorTest {
    private static final String COORDINATOR =
            "com.shaft.intellij.ui.AssistantCodegenWorkflowCoordinator";

    @TempDir
    Path projectRoot;

    @Test
    void scenarioUrlWinsOverConfiguredBaseUrlAndStartsReadOnlyRecord() throws Exception {
        writeConfiguredBaseUrl("https://configured.example");
        Object coordinator = coordinator();
        AssistantCommand.Invocation invocation = route(
                coordinator,
                "url-precedence",
                "/codegen visit https://scenario.example/login and sign in",
                codegen("/codegen visit https://scenario.example/login and sign in"));

        assertAll(
                () -> assertEquals("RECORD", phase(coordinator, "url-precedence")),
                () -> assertEquals("autobot_local_agent_run", invocation.toolName()),
                () -> assertFalse(invocation.arguments().get("allowSourceMutation").getAsBoolean()),
                () -> assertFalse(invocation.arguments().has("targetUrl"),
                        "plugin must pass the URL in AutoBot state, not own browser orchestration"),
                () -> assertTrue(invocation.arguments().get("prompt").getAsString()
                        .contains("\"targetUrl\":\"https://scenario.example/login\"")),
                () -> assertFalse(invocation.arguments().get("prompt").getAsString()
                        .contains("\"targetUrl\":\"https://configured.example\"")),
                () -> assertTrue(invocation.arguments().get("prompt").getAsString()
                        .contains("SHAFT_CODEGEN_PROPOSAL")));
    }

    @Test
    void codexRecordLaunchAllowsOnlyBrowserCaptureMcpToolsInReadOnlySandbox() throws Exception {
        Object coordinator = coordinator();
        AssistantCommand.Invocation invocation = route(
                coordinator,
                "codex-record-tools",
                "/codegen visit https://example.com and submit the form",
                codegen("/codegen visit https://example.com and submit the form"));

        List<String> command = AssistantLocalAgentRunner.commandFor(invocation.arguments());
        String enabledTools = command.stream()
                .filter(value -> value.startsWith("mcp_servers.shaft-mcp.enabled_tools="))
                .findFirst()
                .orElse("");

        assertAll(
                () -> assertTrue(command.contains("read-only"), command.toString()),
                () -> assertTrue(command.contains("mcp_servers.shaft-mcp.default_tools_approval_mode=\"approve\""),
                        command.toString()),
                () -> assertTrue(enabledTools.contains("capture_start"), enabledTools),
                () -> assertTrue(enabledTools.contains("capture_checkpoint"), enabledTools),
                () -> assertTrue(enabledTools.contains("capture_set_mode"), enabledTools),
                () -> assertTrue(enabledTools.contains("capture_stop"), enabledTools),
                () -> assertTrue(enabledTools.contains("element_click"), enabledTools),
                () -> assertFalse(enabledTools.contains("capture_generate_replay"), enabledTools));
    }

    @Test
    void recordForcesAgentModeEvenWhenPanelWasInAskMode() throws Exception {
        String request = "/codegen visit https://example.com";
        AssistantCommand.Invocation askMode = AssistantCommand.fromPrompt(
                request,
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "ASK",
                projectRoot.toString(),
                "",
                false);

        AssistantCommand.Invocation invocation = route(
                coordinator(), "ask-mode-record", request, askMode);

        assertAll(
                () -> assertEquals("AGENT", invocation.arguments().get("mode").getAsString()),
                () -> assertFalse(invocation.arguments().get("allowSourceMutation").getAsBoolean()),
                () -> assertTrue(AssistantLocalAgentRunner.commandFor(invocation.arguments()).contains("read-only")));
    }

    @Test
    void panelAskSelectionLaunchesCoordinatorForcedAgentInvocationWithoutOrphaningRecord() throws Exception {
        ShaftSettingsState.Settings settings = blankMcpSettings();
        settings.assistantFamily = "CODEX";
        settings.defaultAutobotClient = "CODEX";
        settings.defaultAutobotMode = "ASK";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings);
        CountDownLatch launchAttempted = new CountDownLatch(1);
        setField(panel, "localAgentProcessLauncher",
                (AssistantLocalAgentRunner.ProcessLauncher) (command, directory, environment) -> {
                    launchAttempted.countDown();
                    throw new java.io.IOException("test launcher");
                });
        setField(panel, "requireLocalAgentCommandAvailable", false);
        ((javax.swing.JTextArea) fieldValue(panel, "prompt"))
                .setText("/codegen visit https://example.com");

        Method send = ShaftAssistantPanel.class.getDeclaredMethod("send", com.intellij.openapi.project.Project.class);
        send.setAccessible(true); // NOPMD - invokes the real panel send path for this integration test
        send.invoke(panel, new Object[]{null});

        Object panelCoordinator = fieldValue(panel, "codegenCoordinator");
        String sessionId = ((ShaftAssistantChatState) fieldValue(panel, "chatState")).activeSession().id;
        assertAll(
                () -> assertTrue(launchAttempted.await(2, TimeUnit.SECONDS),
                        "the coordinator-forced AGENT invocation must pass the panel's MCP preflight"),
                () -> assertEquals("ASK", ((javax.swing.JComboBox<?>) fieldValue(panel, "mode")).getSelectedItem()),
                () -> assertEquals("RECORD", phase(panelCoordinator, sessionId)),
                () -> assertNotNull(fieldValue(panel, "currentInvocation"),
                        "accepted RECORD work must retain a cancellable invocation"));
        panel.dispose();
    }

    @Test
    void clientWithoutEnforceableRecordToolSandboxFailsClosed() throws Exception {
        String request = "/codegen visit https://example.com";
        AssistantCommand.Invocation unsupported = AssistantCommand.fromPrompt(
                request,
                AssistantCommand.Selection.local("GROK", "CLI"),
                "AGENT",
                projectRoot.toString(),
                "",
                false);
        Object coordinator = coordinator();

        AssistantCommand.Invocation invocation = route(
                coordinator, "unsupported-record-client", request, unsupported);

        assertAll(
                () -> assertTrue(invocation.isLocal()),
                () -> assertTrue(invocation.localResponse().contains("cannot enforce")),
                () -> assertEquals("IDLE", phase(coordinator, "unsupported-record-client")));
    }

    @Test
    void claudeRecordClientFailsClosedWithActionableCodexGuidance() throws Exception {
        String request = "/codegen visit https://example.com";
        AssistantCommand.Invocation claude = AssistantCommand.fromPrompt(
                request, "CLAUDE_CODE", "AGENT", projectRoot.toString(), "", false);
        Object coordinator = coordinator();

        AssistantCommand.Invocation invocation = route(
                coordinator, "claude-record-client", request, claude);

        assertAll(
                () -> assertTrue(invocation.isLocal()),
                () -> assertTrue(invocation.localResponse().contains("Codex CLI"), invocation.localResponse()),
                () -> assertTrue(invocation.localResponse().contains("cannot enforce"), invocation.localResponse()),
                () -> assertEquals("IDLE", phase(coordinator, "claude-record-client")));
    }

    @Test
    void recordToolPolicyBlocksGenerationAndReplayBeforeConsent() throws Exception {
        Object coordinator = coordinator();
        route(coordinator, "record-tool-policy", "/codegen visit https://example.com",
                codegen("/codegen visit https://example.com"));

        boolean captureAllowed = (boolean) invoke(
                coordinator,
                "allowsRecordMcpTool",
                new Class<?>[]{String.class, String.class},
                "record-tool-policy",
                "mcp__shaft-mcp__capture_start");
        boolean clickAllowed = (boolean) invoke(
                coordinator,
                "allowsRecordMcpTool",
                new Class<?>[]{String.class, String.class},
                "record-tool-policy",
                "mcp__shaft-mcp__element_click");
        boolean generateAllowed = (boolean) invoke(
                coordinator,
                "allowsRecordMcpTool",
                new Class<?>[]{String.class, String.class},
                "record-tool-policy",
                "mcp__shaft-mcp__capture_generate_replay");

        assertAll(
                () -> assertTrue(captureAllowed),
                () -> assertTrue(clickAllowed),
                () -> assertFalse(generateAllowed));
    }

    @Test
    void configuredBaseUrlStartsRecordWhenScenarioHasNoUrl() throws Exception {
        writeConfiguredBaseUrl("https://configured.example/base");
        Object coordinator = coordinator();
        AssistantCommand.Invocation invocation = route(
                coordinator,
                "configured-url",
                "/codegen sign in and verify the dashboard",
                codegen("/codegen sign in and verify the dashboard"));

        assertAll(
                () -> assertEquals("RECORD", phase(coordinator, "configured-url")),
                () -> assertTrue(invocation.arguments().get("prompt").getAsString()
                        .contains("\"targetUrl\":\"https://configured.example/base\"")),
                () -> assertFalse(invocation.arguments().get("allowSourceMutation").getAsBoolean()));
    }

    @Test
    void missingUrlAsksOnceThenEitherStartsOrTerminates() throws Exception {
        Object startsAfterAnswer = coordinator();
        AssistantCommand.Invocation question = route(
                startsAfterAnswer,
                "url-answer",
                "/codegen sign in and verify the dashboard",
                codegen("/codegen sign in and verify the dashboard"));
        AssistantCommand.Invocation record = route(
                startsAfterAnswer,
                "url-answer",
                "https://answer.example/login",
                null);

        Object rejectsRepeatedMissing = coordinator();
        route(rejectsRepeatedMissing, "url-missing", "/codegen verify dashboard",
                codegen("/codegen verify dashboard"));
        AssistantCommand.Invocation terminal = route(
                rejectsRepeatedMissing,
                "url-missing",
                "use the normal environment",
                null);
        AssistantCommand.Invocation lateAnswer = route(
                rejectsRepeatedMissing,
                "url-missing",
                "https://too-late.example",
                null);

        assertAll(
                () -> assertEquals("What URL should AutoBot record?", question.localResponse()),
                () -> assertEquals("RECORD", phase(startsAfterAnswer, "url-answer")),
                () -> assertEquals("autobot_local_agent_run", record.toolName()),
                () -> assertTrue(record.arguments().get("prompt").getAsString()
                        .contains("\"targetUrl\":\"https://answer.example/login\"")),
                () -> assertEquals("FAILED", phase(rejectsRepeatedMissing, "url-missing")),
                () -> assertTrue(terminal.isLocal()),
                () -> assertFalse(terminal.localResponse().contains("What URL"),
                        "the second missing answer must terminate instead of asking indefinitely"),
                () -> assertTrue(lateAnswer.isLocal()),
                () -> assertFalse(lateAnswer.arguments().has("allowSourceMutation")));
    }

    @Test
    void proposalRequiresExplicitApprovalBeforeMutableSecondRun() throws Exception {
        Object coordinator = coordinator();
        startAndCompleteProposal(coordinator, "approval", "/codegen visit https://example.com and log in");

        AssistantCommand.Invocation unrelated = route(coordinator, "approval", "looks reasonable", null);
        AssistantCommand.Invocation approved = route(coordinator, "approval", "approve edits", null);

        assertAll(
                () -> assertTrue(unrelated.isLocal(), "non-explicit consent must not launch a process"),
                () -> assertEquals("GENERATE", phase(coordinator, "approval")),
                () -> assertEquals("autobot_local_agent_run", approved.toolName()),
                () -> assertTrue(approved.arguments().get("allowSourceMutation").getAsBoolean()),
                () -> assertEquals(1, approved.arguments().get("healRetryAllowance").getAsInt()),
                () -> assertTrue(approved.arguments().get("prompt").getAsString()
                        .contains("\"recordingPath\":\"recordings/intellij-capture.json\"")),
                () -> assertTrue(approved.arguments().get("prompt").getAsString()
                        .contains("\"healRetryAllowance\":1")));
    }

    @Test
    void launchedRecordAndMutablePromptsRetainTheShaftCodeGenerationContract() throws Exception {
        Object coordinator = coordinator();
        String sessionId = "launched-prompt-contract";
        String request = "/codegen visit https://example.com and log in";

        AssistantCommand.Invocation record = route(coordinator, sessionId, request, codegen(request));
        writeRecording();
        complete(coordinator, sessionId, ShaftMcpToolResult.success(
                "SHAFT_CODEGEN_PROPOSAL " + proposalJson()));
        AssistantCommand.Invocation mutable = route(coordinator, sessionId, "approve edits", null);

        String recordPrompt = record.arguments().get("prompt").getAsString();
        String mutablePrompt = mutable.arguments().get("prompt").getAsString();
        assertAll(
                () -> assertTrue(recordPrompt.contains("$shaft-test-recording"), recordPrompt),
                () -> assertTrue(recordPrompt.contains("capture_checkpoint"), recordPrompt),
                () -> assertTrue(recordPrompt.contains("SHAFT MCP/CLI"), recordPrompt),
                () -> assertTrue(recordPrompt.contains("existing test classes and page objects"), recordPrompt),
                () -> assertTrue(mutablePrompt.contains("$shaft-automated-test-authoring"), mutablePrompt),
                () -> assertTrue(mutablePrompt.contains("$shaft-page-objects"), mutablePrompt),
                () -> assertTrue(mutablePrompt.contains("shaft_coding_partner_plan"), mutablePrompt),
                () -> assertTrue(mutablePrompt.contains("Page Object Model"), mutablePrompt),
                () -> assertTrue(mutablePrompt.contains("stable author-written IDs first"), mutablePrompt),
                () -> assertTrue(mutablePrompt.contains("existing project locator conventions"), mutablePrompt),
                () -> assertTrue(mutablePrompt.contains("Create a new owner only when none fits"), mutablePrompt),
                () -> assertTrue(mutablePrompt.contains("Replay once"), mutablePrompt),
                () -> assertTrue(mutablePrompt.contains("healRetryAllowance"), mutablePrompt));
    }

    @Test
    void mutableRunProgressAllowsTheSingleHealReplayLoopButRejectsOtherBacktracking() throws Exception {
        Object coordinator = coordinator();
        startAndCompleteProposal(coordinator, "progress", "/codegen visit https://example.com");
        route(coordinator, "progress", "approve", null);

        invoke(coordinator, "progress", new Class<?>[]{String.class, String.class},
                "progress", "SHAFT_CODEGEN_PROGRESS {\"phase\":\"REPLAY\"}");
        String replay = phase(coordinator, "progress");
        invoke(coordinator, "progress", new Class<?>[]{String.class, String.class},
                "progress", "SHAFT_CODEGEN_PROGRESS {\"phase\":\"HEAL\"}");
        String heal = phase(coordinator, "progress");
        invoke(coordinator, "progress", new Class<?>[]{String.class, String.class},
                "progress", "SHAFT_CODEGEN_PROGRESS {\"phase\":\"REPLAY\"}");
        String replayAfterHeal = phase(coordinator, "progress");
        invoke(coordinator, "progress", new Class<?>[]{String.class, String.class},
                "progress", "SHAFT_CODEGEN_PROGRESS {\"phase\":\"GENERATE\"}");
        invoke(coordinator, "progress", new Class<?>[]{String.class, String.class},
                "progress", "SHAFT_CODEGEN_PROGRESS {\"phase\":\"HEAL\"}");

        assertAll(
                () -> assertEquals("REPLAY", replay),
                () -> assertEquals("HEAL", heal),
                () -> assertEquals("REPLAY", replayAfterHeal),
                () -> assertEquals("REPLAY", phase(coordinator, "progress"),
                        "the default allowance must reject a second heal/replay loop"));
    }

    @Test
    void denialEndsAtProposalWithoutMutableInvocation() throws Exception {
        Object coordinator = coordinator();
        startAndCompleteProposal(coordinator, "denial", "/codegen visit https://example.com");

        AssistantCommand.Invocation denied = route(coordinator, "denial", "deny", null);
        AssistantCommand.Invocation laterApproval = route(coordinator, "denial", "approve", null);

        assertAll(
                () -> assertEquals("DENIED", phase(coordinator, "denial")),
                () -> assertTrue(denied.isLocal()),
                () -> assertFalse(denied.arguments().has("allowSourceMutation")),
                () -> assertTrue(laterApproval.isLocal()),
                () -> assertFalse(laterApproval.arguments().has("allowSourceMutation")));
    }

    @Test
    void explicitHealOverrideIsNarrowAndDoesNotInheritHealerDefault() throws Exception {
        Object defaultCoordinator = coordinator();
        startAndCompleteProposal(defaultCoordinator, "default-retry",
                "/codegen visit https://example.com and retry failed steps");
        AssistantCommand.Invocation defaultMutable = route(defaultCoordinator, "default-retry", "approve", null);

        Object overrideCoordinator = coordinator();
        startAndCompleteProposal(overrideCoordinator, "override-retry",
                "/codegen visit https://example.com; allow 3 heal retries");
        AssistantCommand.Invocation overrideMutable = route(overrideCoordinator, "override-retry", "approve", null);
        invoke(overrideCoordinator, "progress", new Class<?>[]{String.class, String.class},
                "override-retry", "SHAFT_CODEGEN_PROGRESS {\"phase\":\"REPLAY\"}");
        invoke(overrideCoordinator, "progress", new Class<?>[]{String.class, String.class},
                "override-retry", "SHAFT_CODEGEN_PROGRESS {\"phase\":\"HEAL\"}");
        invoke(overrideCoordinator, "progress", new Class<?>[]{String.class, String.class},
                "override-retry", "SHAFT_CODEGEN_PROGRESS {\"phase\":\"REPLAY\"}");
        invoke(overrideCoordinator, "progress", new Class<?>[]{String.class, String.class},
                "override-retry", "SHAFT_CODEGEN_PROGRESS {\"phase\":\"HEAL\"}");

        assertAll(
                () -> assertEquals(1, defaultMutable.arguments().get("healRetryAllowance").getAsInt()),
                () -> assertEquals(3, overrideMutable.arguments().get("healRetryAllowance").getAsInt()),
                () -> assertEquals("HEAL", phase(overrideCoordinator, "override-retry"),
                        "an explicit allowance must permit the requested additional loop"));
    }

    @Test
    void cancelAndPanelDisposalMakeWorkflowTerminal() throws Exception {
        Object cancelledCoordinator = coordinator();
        route(cancelledCoordinator, "cancelled", "/codegen visit https://example.com",
                codegen("/codegen visit https://example.com"));
        invoke(cancelledCoordinator, "cancel", new Class<?>[]{String.class}, "cancelled");
        AssistantCommand.Invocation afterCancel = route(cancelledCoordinator, "cancelled", "approve", null);

        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        Field coordinatorField = findField(ShaftAssistantPanel.class, "codegenCoordinator");
        assertNotNull(coordinatorField, "panel must retain its workflow coordinator");
        coordinatorField.setAccessible(true); // NOPMD - test-only inspection of the panel-owned coordinator
        Object panelCoordinator = coordinatorField.get(panel);
        route(panelCoordinator, "panel-session", "/codegen visit https://example.com",
                codegen("/codegen visit https://example.com"));
        Field sessionField = findField(ShaftAssistantPanel.class, "activeCodegenSessionId");
        assertNotNull(sessionField, "panel must identify the workflow session it owns");
        sessionField.setAccessible(true); // NOPMD - test-only setup of the panel-owned workflow session
        sessionField.set(panel, "panel-session");
        panel.dispose();

        assertAll(
                () -> assertEquals("CANCELLED", phase(cancelledCoordinator, "cancelled")),
                () -> assertTrue(afterCancel.isLocal()),
                () -> assertFalse(afterCancel.arguments().has("allowSourceMutation")),
                () -> assertEquals("DISPOSED", phase(panelCoordinator, "panel-session")));
    }

    @Test
    void terminalSummaryUsesStructuredOutcomesAndOnlyLinksARealReport() throws Exception {
        Object withReport = coordinator();
        startAndCompleteProposal(withReport, "with-report", "/codegen visit https://example.com");
        route(withReport, "with-report", "approve", null);
        Path report = projectRoot.resolve("allure-report/AllureReport.html");
        Files.createDirectories(report.getParent());
        Files.writeString(report, "report");
        ShaftMcpToolResult linked = complete(withReport, "with-report", terminalResult(true));

        Files.delete(report);
        Object withoutReport = coordinator();
        startAndCompleteProposal(withoutReport, "without-report", "/codegen visit https://example.com");
        route(withoutReport, "without-report", "approve", null);
        ShaftMcpToolResult unlinked = complete(withoutReport, "without-report", terminalResult(true));

        assertAll(
                () -> assertEquals("SUCCEEDED", phase(withReport, "with-report")),
                () -> assertTrue(linked.output().contains("RECORD: passed")),
                () -> assertTrue(linked.output().contains("Changed classes: `LoginPage`")),
                () -> assertTrue(linked.output().contains("Created classes: `LoginTest`")),
                () -> assertTrue(linked.output().contains("allure-report/AllureReport.html")),
                () -> assertEquals("SUCCEEDED", phase(withoutReport, "without-report")),
                () -> assertFalse(unlinked.output().contains("AllureReport.html"),
                        "a model claim must never fabricate a report link when the file is absent"));
    }

    @Test
    void malformedProposalFailsClosedAtStructuredBoundary() throws Exception {
        Object coordinator = coordinator();
        route(coordinator, "bad-proposal", "/codegen visit https://example.com",
                codegen("/codegen visit https://example.com"));

        ShaftMcpToolResult completed = complete(
                coordinator,
                "bad-proposal",
                ShaftMcpToolResult.success("recorded, trust me"));

        assertAll(
                () -> assertEquals("FAILED", phase(coordinator, "bad-proposal")),
                () -> assertFalse(completed.success()),
                () -> assertTrue(completed.output().contains("structured proposal")));
    }

    @Test
    void proposalBoundaryMustBeTheFinalNonBlankLine() throws Exception {
        Object coordinator = coordinator();
        String request = "/codegen visit https://example.com";
        route(coordinator, "proposal-boundary", request, codegen(request));

        ShaftMcpToolResult completed = complete(
                coordinator,
                "proposal-boundary",
                ShaftMcpToolResult.success("SHAFT_CODEGEN_PROPOSAL " + proposalJson() + "\nmore prose"));

        assertAll(
                () -> assertFalse(completed.success()),
                () -> assertEquals("FAILED", phase(coordinator, "proposal-boundary")));
    }

    @Test
    void runnerComposedProposalMetadataStillReachesEditConfirmation() throws Exception {
        Object coordinator = coordinator();
        String sessionId = "runner-composed-proposal";
        String request = "/codegen visit https://example.com";
        route(coordinator, sessionId, request, codegen(request));

        Path recording = projectRoot.resolve("recordings/intellij-capture.json");
        Files.createDirectories(recording.getParent());
        Files.writeString(recording, "{}");
        AssistantLocalAgentRunner.StructuredStreamParser parser =
                new AssistantLocalAgentRunner.StructuredStreamParser(
                        AssistantLocalAgentRunner.StructuredStreamParser.Format.CODEX);
        JsonObject usage = new JsonObject();
        usage.addProperty("input_tokens", 7);
        usage.addProperty("output_tokens", 3);
        JsonObject agentMessage = new JsonObject();
        agentMessage.addProperty("type", "item.completed");
        JsonObject agentMessageItem = new JsonObject();
        agentMessageItem.addProperty("type", "agent_message");
        agentMessageItem.addProperty("text", "Recording saved.\nSHAFT_CODEGEN_PROPOSAL " + proposalJson());
        agentMessage.add("item", agentMessageItem);
        parser.accept(agentMessage.toString(), ignored -> { });
        JsonObject terminalEvent = new JsonObject();
        terminalEvent.addProperty("type", "turn.completed");
        terminalEvent.add("usage", usage);
        parser.accept(terminalEvent.toString(), ignored -> { });

        ShaftMcpToolResult completed = complete(
                coordinator, sessionId, ShaftMcpToolResult.success(parser.finalOutput()));

        assertAll(
                () -> assertTrue(completed.success(), completed.output()),
                () -> assertEquals("AWAITING_EDIT_CONFIRMATION", phase(coordinator, sessionId)),
                () -> assertTrue(completed.output().contains("Reuse `LoginPage`")));
    }

    @Test
    void runnerComposedTerminalResultWithActivityReachesSucceeded() throws Exception {
        Object coordinator = coordinator();
        String sessionId = "runner-composed-result";
        String request = "/codegen visit https://example.com";
        startAndCompleteProposal(coordinator, sessionId, request);
        route(coordinator, sessionId, "approve", null);

        AssistantLocalAgentRunner.StructuredStreamParser parser =
                new AssistantLocalAgentRunner.StructuredStreamParser(
                        AssistantLocalAgentRunner.StructuredStreamParser.Format.CODEX);
        parser.accept("{\"type\":\"item.completed\",\"item\":{\"type\":\"file_change\","
                        + "\"status\":\"completed\",\"changes\":[{\"path\":\"src/test/java/LoginTest.java\","
                        + "\"kind\":\"add\"}]}}",
                ignored -> { });
        JsonObject usage = new JsonObject();
        usage.addProperty("input_tokens", 11);
        usage.addProperty("output_tokens", 5);
        JsonObject terminalEvent = new JsonObject();
        terminalEvent.addProperty("type", "turn.completed");
        terminalEvent.addProperty("last_agent_message", terminalResult(true).output());
        terminalEvent.add("usage", usage);
        parser.accept(terminalEvent.toString(), ignored -> { });

        ShaftMcpToolResult completed = complete(
                coordinator, sessionId, ShaftMcpToolResult.success(parser.finalOutput()));

        assertAll(
                () -> assertTrue(completed.success(), completed.output()),
                () -> assertEquals("SUCCEEDED", phase(coordinator, sessionId)),
                () -> assertTrue(completed.output().contains("Created classes: `LoginTest`")));
    }

    @Test
    void proposalRejectsCanonicalPathWhenRecordingFileIsMissing() throws Exception {
        Object coordinator = coordinator();
        String sessionId = "missing-recording-file";
        String request = "/codegen visit https://example.com";
        route(coordinator, sessionId, request, codegen(request));

        ShaftMcpToolResult completed = complete(coordinator, sessionId, ShaftMcpToolResult.success(
                "SHAFT_CODEGEN_PROPOSAL " + proposalJson()));

        assertAll(
                () -> assertFalse(completed.success()),
                () -> assertEquals("FAILED", phase(coordinator, sessionId)),
                () -> assertTrue(completed.output().contains("recording file"), completed.output()));
    }

    @Test
    void proposalAndPassedResultRequireMandatoryPhaseEvidence() throws Exception {
        writeRecording();
        Object proposalCoordinator = coordinator();
        String request = "/codegen visit https://example.com";
        route(proposalCoordinator, "empty-record-phase", request, codegen(request));
        ShaftMcpToolResult emptyRecordPhase = complete(
                proposalCoordinator,
                "empty-record-phase",
                ShaftMcpToolResult.success("SHAFT_CODEGEN_PROPOSAL "
                        + "{\"recordingPath\":\"recordings/intellij-capture.json\","
                        + "\"proposalMarkdown\":\"Reuse LoginPage\",\"phaseOutcomes\":{}}"));

        Object terminalCoordinator = coordinator();
        startAndCompleteProposal(terminalCoordinator, "missing-mutable-phases", request);
        route(terminalCoordinator, "missing-mutable-phases", "approve", null);
        ShaftMcpToolResult missingMutablePhases = complete(
                terminalCoordinator,
                "missing-mutable-phases",
                terminalResult("passed", "{\"GENERATE\":\"passed\"}", "[\"LoginPage\"]", "[]"));

        assertAll(
                () -> assertFalse(emptyRecordPhase.success()),
                () -> assertEquals("FAILED", phase(proposalCoordinator, "empty-record-phase")),
                () -> assertFalse(missingMutablePhases.success()),
                () -> assertEquals("FAILED", phase(terminalCoordinator, "missing-mutable-phases")));
    }

    @Test
    void passedResultRequiresAtLeastOneChangedOrCreatedClass() throws Exception {
        Object coordinator = coordinator();
        String sessionId = "empty-class-evidence";
        String request = "/codegen visit https://example.com";
        startAndCompleteProposal(coordinator, sessionId, request);
        route(coordinator, sessionId, "approve", null);

        ShaftMcpToolResult completed = complete(
                coordinator,
                sessionId,
                terminalResult(
                        "passed",
                        "{\"GENERATE\":\"passed\",\"REPLAY\":\"passed\",\"HEAL\":\"not needed\"}",
                        "[]",
                        "[]"));

        assertAll(
                () -> assertFalse(completed.success()),
                () -> assertEquals("FAILED", phase(coordinator, sessionId)));
    }

    @Test
    void forgedRecordingPathAndTerminalStatusFailClosed() throws Exception {
        writeRecording();
        Object pathCoordinator = coordinator();
        String request = "/codegen visit https://example.com";
        route(pathCoordinator, "forged-path", request, codegen(request));
        ShaftMcpToolResult forgedPath = complete(
                pathCoordinator,
                "forged-path",
                ShaftMcpToolResult.success("SHAFT_CODEGEN_PROPOSAL "
                        + proposalJson().replace("recordings/intellij-capture.json", "../outside.json")));

        Object statusCoordinator = coordinator();
        startAndCompleteProposal(statusCoordinator, "forged-status", request);
        route(statusCoordinator, "forged-status", "approve", null);
        ShaftMcpToolResult forgedStatus = complete(
                statusCoordinator,
                "forged-status",
                terminalResult(
                        "success",
                        "{\"GENERATE\":\"passed\",\"REPLAY\":\"passed\",\"HEAL\":\"not needed\"}",
                        "[\"LoginPage\"]",
                        "[]"));

        assertAll(
                () -> assertFalse(forgedPath.success()),
                () -> assertEquals("FAILED", phase(pathCoordinator, "forged-path")),
                () -> assertFalse(forgedStatus.success()),
                () -> assertEquals("FAILED", phase(statusCoordinator, "forged-status")));
    }

    @Test
    void failedTerminalRejectsEveryMalformedOrUnknownPhaseOutcome() {
        assertAll(
                () -> assertInvalidTerminal("failed-object-phase", "failed",
                        "{\"RECORD\":{\"detail\":\"saved\"}}", "[]", "[]"),
                () -> assertInvalidTerminal("failed-null-phase", "failed",
                        "{\"RECORD\":null}", "[]", "[]"),
                () -> assertInvalidTerminal("failed-blank-phase", "failed",
                        "{\"RECORD\":\"   \"}", "[]", "[]"),
                () -> assertInvalidTerminal("failed-unknown-phase", "failed",
                        "{\"UNKNOWN\":\"failed\"}", "[]", "[]"));
    }

    @Test
    void passedTerminalRejectsExtraMalformedOrUnknownPhaseOutcome() {
        assertAll(
                () -> assertInvalidTerminal("passed-object-phase", "passed",
                        "{\"GENERATE\":\"passed\",\"REPLAY\":\"passed\",\"HEAL\":\"not needed\","
                                + "\"RECORD\":{\"detail\":\"saved\"}}",
                        "[\"LoginPage\"]", "[]"),
                () -> assertInvalidTerminal("passed-unknown-phase", "passed",
                        "{\"GENERATE\":\"passed\",\"REPLAY\":\"passed\",\"HEAL\":\"not needed\","
                                + "\"UNKNOWN\":\"passed\"}",
                        "[\"LoginPage\"]", "[]"));
    }

    @Test
    void validPartialFailedTerminalStillRendersItsAvailablePhaseOutcome() throws Exception {
        Object coordinator = mutableCoordinator("partial-failed-terminal");

        ShaftMcpToolResult completed = complete(
                coordinator,
                "partial-failed-terminal",
                terminalResult("failed", "{\"RECORD\":\"recorded\"}", "[]", "[]"));

        assertAll(
                () -> assertFalse(completed.success(), completed.output()),
                () -> assertEquals("FAILED", phase(coordinator, "partial-failed-terminal")),
                () -> assertTrue(completed.output().contains("Status: **failed**"), completed.output()),
                () -> assertTrue(completed.output().contains("- RECORD: recorded"), completed.output()));
    }

    @Test
    void malformedTerminalCompletesPanelWithoutLeavingRunningState() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        String sessionId = "panel-malformed-terminal";
        try {
            Object coordinator = fieldValue(panel, "codegenCoordinator");
            startAndCompleteProposal(coordinator, sessionId, "/codegen visit https://example.com");
            route(coordinator, sessionId, "approve", null);
            setField(panel, "activeCodegenSessionId", sessionId);
            panel.setRunning(true, "Running AutoBot codegen");

            assertAll(
                    () -> assertDoesNotThrow(() -> showAgentResult(
                            panel,
                            terminalResult("failed", "{\"RECORD\":{\"detail\":\"saved\"}}", "[]", "[]"))),
                    () -> assertFalse((boolean) fieldValue(panel, "running")),
                    () -> assertEquals("FAILED", phase(coordinator, sessionId)));
        } finally {
            panel.dispose();
        }
    }

    private void assertInvalidTerminal(
            String sessionId, String status, String phaseOutcomes, String changedClasses, String createdClasses)
            throws Exception {
        Object coordinator = mutableCoordinator(sessionId);
        ShaftMcpToolResult completed = complete(
                coordinator, sessionId, terminalResult(status, phaseOutcomes, changedClasses, createdClasses));

        assertAll(
                () -> assertFalse(completed.success(), completed.output()),
                () -> assertEquals("FAILED", phase(coordinator, sessionId)),
                () -> assertTrue(completed.output().contains("valid structured terminal summary"), completed.output()));
    }

    private Object mutableCoordinator(String sessionId) throws Exception {
        Object coordinator = coordinator();
        String request = "/codegen visit https://example.com";
        startAndCompleteProposal(coordinator, sessionId, request);
        route(coordinator, sessionId, "approve", null);
        return coordinator;
    }

    private static void showAgentResult(ShaftAssistantPanel panel, ShaftMcpToolResult result) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod(
                "showAgentResult", ShaftMcpToolResult.class, Throwable.class);
        method.setAccessible(true); // NOPMD - invokes the real asynchronous completion target in this integration test
        method.invoke(panel, result, null);
    }

    private void startAndCompleteProposal(Object coordinator, String sessionId, String request) throws Exception {
        writeRecording();
        AssistantCommand.Invocation first = route(coordinator, sessionId, request, codegen(request));
        assertFalse(first.arguments().get("allowSourceMutation").getAsBoolean());
        ShaftMcpToolResult proposal = complete(coordinator, sessionId, ShaftMcpToolResult.success(
                "Recording saved.\nSHAFT_CODEGEN_PROPOSAL " + proposalJson()));
        assertAll(
                () -> assertTrue(proposal.success()),
                () -> assertEquals("AWAITING_EDIT_CONFIRMATION", phase(coordinator, sessionId)),
                () -> assertTrue(proposal.output().contains("Reuse `LoginPage`")));
    }

    private AssistantCommand.Invocation codegen(String prompt) {
        return AssistantCommand.fromPrompt(
                prompt,
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "AGENT",
                projectRoot.toString(),
                "",
                false);
    }

    private AssistantCommand.Invocation route(
            Object coordinator,
            String sessionId,
            String userText,
            AssistantCommand.Invocation baseInvocation) throws Exception {
        return (AssistantCommand.Invocation) invoke(
                coordinator,
                "route",
                new Class<?>[]{String.class, String.class, AssistantCommand.Invocation.class, String.class},
                sessionId,
                userText,
                baseInvocation,
                projectRoot.toString());
    }

    private ShaftMcpToolResult complete(Object coordinator, String sessionId, ShaftMcpToolResult result)
            throws Exception {
        return (ShaftMcpToolResult) invoke(
                coordinator,
                "complete",
                new Class<?>[]{String.class, ShaftMcpToolResult.class, String.class},
                sessionId,
                result,
                projectRoot.toString());
    }

    private String phase(Object coordinator, String sessionId) throws Exception {
        return String.valueOf(invoke(
                coordinator,
                "phase",
                new Class<?>[]{String.class},
                sessionId));
    }

    private static Object coordinator() throws Exception {
        Class<?> type;
        try {
            type = Class.forName(COORDINATOR);
        } catch (ClassNotFoundException error) {
            fail("Observable codegen coordinator is missing", error);
            return null;
        }
        Constructor<?> constructor = type.getDeclaredConstructor();
        constructor.setAccessible(true); // NOPMD - test-only construction of the package-private coordinator seam
        return constructor.newInstance();
    }

    private static Object invoke(Object target, String name, Class<?>[] parameterTypes, Object... arguments)
            throws Exception {
        Method method = target.getClass().getDeclaredMethod(name, parameterTypes);
        method.setAccessible(true); // NOPMD - test-only invocation of package-private coordinator behavior
        return method.invoke(target, arguments);
    }

    private static Field findField(Class<?> type, String name) {
        try {
            return type.getDeclaredField(name);
        } catch (NoSuchFieldException error) {
            return null;
        }
    }

    private static Object fieldValue(Object target, String name) throws Exception {
        Field field = target.getClass().getDeclaredField(name);
        field.setAccessible(true); // NOPMD - test-only field inspection through the established helper
        return field.get(target);
    }

    private static void setField(Object target, String name, Object value) throws Exception {
        Field field = target.getClass().getDeclaredField(name);
        field.setAccessible(true); // NOPMD - test-only field injection through the established helper
        field.set(target, value);
    }

    private void writeConfiguredBaseUrl(String value) throws Exception {
        Path properties = projectRoot.resolve("src/main/resources/properties/custom.properties");
        Files.createDirectories(properties.getParent());
        Files.writeString(properties, "baseURL=" + value + System.lineSeparator());
    }

    private void writeRecording() throws Exception {
        Path recording = projectRoot.resolve("recordings/intellij-capture.json");
        Files.createDirectories(recording.getParent());
        Files.writeString(recording, "{}");
    }

    private static String proposalJson() {
        return "{\"recordingPath\":\"recordings/intellij-capture.json\","
                + "\"proposalMarkdown\":\"Reuse `LoginPage`; create `LoginTest`.\","
                + "\"phaseOutcomes\":{\"RECORD\":\"passed\"}}";
    }

    private static ShaftMcpToolResult terminalResult(boolean success) {
        String json = "{\"status\":\"" + (success ? "passed" : "failed") + "\","
                + "\"phaseOutcomes\":{\"RECORD\":\"passed\",\"GENERATE\":\"passed\","
                + "\"REPLAY\":\"passed\",\"HEAL\":\"not needed\"},"
                + "\"changedClasses\":[\"LoginPage\"],\"createdClasses\":[\"LoginTest\"],"
                + "\"reportPath\":\"allure-report/AllureReport.html\"}";
        return ShaftMcpToolResult.success("Finished.\nSHAFT_CODEGEN_RESULT " + json);
    }

    private static ShaftMcpToolResult terminalResult(
            String status, String phaseOutcomes, String changedClasses, String createdClasses) {
        String json = "{\"status\":\"" + status + "\",\"phaseOutcomes\":" + phaseOutcomes
                + ",\"changedClasses\":" + changedClasses + ",\"createdClasses\":" + createdClasses
                + ",\"reportPath\":\"\"}";
        return ShaftMcpToolResult.success("SHAFT_CODEGEN_RESULT " + json);
    }

    private static ShaftSettingsState.Settings blankMcpSettings() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "";
        return settings;
    }
}
