package com.shaft.intellij.ui;

import java.util.List;

/**
 * Curated SHAFT MCP templates shown in the IntelliJ tool window.
 */
final class ToolTemplates {
    private ToolTemplates() {
        throw new IllegalStateException("Utility class");
    }

    static List<ToolCategory> categories() {
        return List.of(
                new ToolCategory("Recorder", recorder()),
                new ToolCategory("Playback", playback()),
                new ToolCategory("Doctor", doctor()),
                new ToolCategory("Healer", healer()),
                new ToolCategory("Inspector", inspector()),
                new ToolCategory("Projects", projects()),
                new ToolCategory("MCP", mcp()),
                new ToolCategory("Guide", guide()));
    }

    static List<ToolCategory> categories(String toolsListOutput) {
        return ToolCatalog.mergeDiscoveredTools(categories(), ToolCatalog.parseToolsList(toolsListOutput));
    }

    static List<ToolTemplate> recorder() {
        return List.of(
                template("Start WebDriver Recording", "capture_start",
                        """
                        {
                          "targetUrl": "",
                          "browser": "Chrome",
                          "outputPath": "recordings/intellij-capture.json",
                          "headless": false
                        }
                        """),
                template("Recording Status", "capture_status", "{}"),
                template("Checkpoint", "capture_checkpoint",
                        """
                        {
                          "description": "Important flow checkpoint",
                          "kind": "USER_MARKER"
                        }
                        """),
                template("Stop Recording", "capture_stop",
                        """
                        {
                          "discard": false
                        }
                        """),
                template("Generate Code Blocks", "capture_code_blocks",
                        """
                        {
                          "sessionPath": "recordings/intellij-capture.json",
                          "outputDirectory": ".",
                          "packageName": "tests.generated",
                          "className": "RecordedFlowTest",
                          "overwrite": false,
                          "driverVariableName": "driver"
                        }
                        """),
                template("Find Java Recorder Targets", "capture_target_candidates",
                        """
                        {
                          "repositoryPath": ".",
                          "maxResults": 10
                        }
                        """,
                        "Finds Java test and page-object anchors for record-at-target insertion.",
                        false),
                template("Generate Playwright Code Blocks", "capture_code_blocks",
                        """
                        {
                          "sessionPath": "recordings/playwright-recording.json",
                          "outputDirectory": ".",
                          "packageName": "tests.generated",
                          "className": "RecordedFlowTest",
                          "overwrite": false,
                          "driverVariableName": "driver",
                          "backend": "playwright"
                        }
                        """,
                        "Generates Playwright-friendly code blocks from a Playwright recording or Capture session"
                                + " (sniffs the session file; backend=playwright covers both).",
                        false),
                template("Record Into Java Target", "capture_record_at_target_code_blocks",
                        """
                        {
                          "sessionPath": "recordings/intellij-capture.json",
                          "outputDirectory": ".",
                          "packageName": "tests.generated",
                          "className": "RecordedFlowTest",
                          "overwrite": false,
                          "targetSourcePath": "src/test/java/example/Test.java",
                          "insertAfter": "testMethod",
                          "driverVariableName": "driver"
                        }
                        """,
                        "Record into an existing test: review generated code blocks, inspect the patch preview, then apply and verify.",
                        true),
                template("Compare WebDriver And Playwright Codegen", "capture_backend_comparison",
                        """
                        {
                          "sessionPath": "recordings/intellij-capture.json",
                          "outputDirectory": ".",
                          "packageName": "tests.generated",
                          "className": "ComparedFlowTest",
                          "overwrite": false,
                          "driverVariableName": "driver"
                        }
                        """,
                        "Generates side-by-side WebDriver and Playwright code block summaries. Review output paths first.",
                        true),
                template("Create Recorder Evidence Pack", "capture_evidence_pack",
                        """
                        {
                          "sourcePath": "src/test/java/tests/generated/RecordedFlowTest.java",
                          "reportPath": "target/shaft-capture/generation-report.json",
                          "reviewPath": "target/shaft-capture/capture-review.json",
                          "screenshotPaths": []
                        }
                        """,
                        "Builds a local manifest of generated source, reports, and screenshot evidence.",
                        false));
    }

    static List<ToolTemplate> playback() {
        return List.of(
                template("Generate And Replay WebDriver", "capture_generate_replay",
                        """
                        {
                          "sessionPath": "recordings/intellij-capture.json",
                          "outputDirectory": ".",
                          "packageName": "tests.generated",
                          "className": "RecordedFlowTest",
                          "overwrite": false,
                          "replay": true,
                          "useAi": false,
                          "allowLocalAi": false,
                          "allowRemoteAi": false,
                          "driverVariableName": "driver"
                        }
                        """),
                template("Generate And Replay Playwright", "capture_generate_replay",
                        """
                        {
                          "sessionPath": "recordings/playwright-recording.json",
                          "outputDirectory": ".",
                          "packageName": "tests.generated",
                          "className": "RecordedFlowTest",
                          "overwrite": false,
                          "replay": false,
                          "useAi": false,
                          "allowLocalAi": false,
                          "allowRemoteAi": false,
                          "driverVariableName": "driver",
                          "backend": "playwright"
                        }
                        """,
                        "Generates Playwright tests from a recording session. Set replay=true only after review.",
                        true),
                template("Replay Mobile Recording", "capture_generate_replay",
                        """
                        {
                          "sessionPath": "recordings/mobile-recording.json",
                          "outputDirectory": ".",
                          "packageName": "tests.generated",
                          "className": "RecordedFlowTest",
                          "overwrite": false,
                          "replay": true,
                          "useAi": false,
                          "allowLocalAi": false,
                          "allowRemoteAi": false,
                          "driverVariableName": "driver",
                          "backend": "mobile"
                        }
                        """,
                        "Replays a mobile recording against the active SHAFT mobile driver.",
                        true));
    }

    static List<ToolTemplate> doctor() {
        return List.of(
                template("Read Trace", "trace_read",
                        """
                        {
                          "tracePath": "target/shaft-traces",
                          "maxCharacters": 12000
                        }
                        """,
                        "Reads trace data with bounded output for evidence review.",
                        false),
                template("Summarize Trace", "trace_summarize",
                        """
                        {
                          "tracePath": "target/shaft-traces"
                        }
                        """,
                        "Returns a deterministic summary for a persisted SHAFT trace.",
                        false),
                template("Analyze Failed Allure", "doctor_analyze_failed_allure",
                        """
                        {
                          "allureResultPaths": ["allure-results"],
                          "historicalBundlePaths": [],
                          "outputDirectory": "target/shaft-doctor",
                          "includeScreenshots": true,
                          "includePageSnapshots": true,
                          "minimumAllureResults": 1,
                          "repositoryRoot": ".",
                          "allowedSourcePaths": [],
                          "useAi": false,
                          "allowLocalAi": false,
                          "allowRemoteAi": false,
                          "driverVariableName": "driver"
                        }
                        """),
                template("Suggest Fix From Report", "doctor_suggest_fix",
                        """
                        {
                          "jsonReportPath": "target/shaft-doctor/doctor-report.json",
                          "repositoryRoot": ".",
                          "allowedSourcePaths": [],
                          "useAi": false,
                          "allowLocalAi": false,
                          "allowRemoteAi": false,
                          "driverVariableName": "driver"
                        }
                        """),
                template("Latest Traces", "trace_latest",
                        """
                        {
                          "maxResults": 5
                        }
                        """),
                template("Analyze Trace", "doctor_analyze_trace",
                        """
                        {
                          "tracePath": "target/shaft-traces",
                          "backend": "webdriver"
                        }
                        """));
    }

    static List<ToolTemplate> healer() {
        return List.of(
                template("Run Failed Test Healer", "healer_run_failed_test",
                        """
                        {
                          "repositoryRoot": ".",
                          "testCommand": ["mvn", "-q", "-Dtest=ExampleTest", "test"],
                          "outputDirectory": "target/shaft-healer",
                          "maxAttempts": 1,
                          "includeScreenshots": true,
                          "includePageSnapshots": true,
                          "allowedSourcePaths": [],
                          "networkValidationApproved": false,
                          "useConfiguredAi": false,
                          "allowLocalAi": false,
                          "allowRemoteAi": false,
                          "driverVariableName": "driver"
                        }
                        """,
                        "Runs a focused failed-test repair attempt. Review testCommand and allowedSourcePaths.",
                        true),
                template("Propose Healed Locator", "doctor_propose_healed_locator",
                        """
                        {
                          "repositoryRoot": ".",
                          "healingReportPath": "target/shaft-heal/healing-report.json",
                          "sourcePath": "src/test/java/example/Page.java",
                          "sourcePatchConsent": true,
                          "outputDirectory": "target/shaft-doctor/healing-proposals"
                        }
                        """,
                        "Creates a locator patch proposal. Confirm the sourcePath before running.",
                        true));
    }

    static List<ToolTemplate> inspector() {
        return List.of(
                template("Mobile Toolchain Status", "mobile_toolchain_status",
                        """
                        {
                          "platformName": "Android"
                        }
                        """),
                template("Open Intent", "browser_open_intent",
                        """
                        {
                          "targetUrl": "https://example.com",
                          "userIntent": "Sign in",
                          "maxCharacters": 12000,
                          "maxElements": 10
                        }
                        """,
                        "Opens the URL and returns DOM plus locator candidates for the requested intent.",
                        true),
                template("Get Page DOM", "browser_get_page_dom",
                        """
                        {
                          "maxCharacters": 12000
                        }
                        """,
                        "Returns bounded DOM for locator inspection.",
                        false),
                template("Take Screenshot", "browser_take_screenshot",
                        """
                        {
                          "outputPath": "target/shaft-browser/screenshot.png",
                          "includeBase64": false
                        }
                        """,
                        "Captures a viewport screenshot before risky manual actions.",
                        true),
                template("Start Mobile Inspector Recording", "mobile_inspector_record_start",
                        """
                        {
                          "platformName": "Android",
                          "outputPath": "recordings/mobile-inspector.json",
                          "includeSensitiveValues": false,
                          "app": "",
                          "appPackage": "",
                          "appActivity": "",
                          "bundleId": "",
                          "udid": "",
                          "deviceName": "",
                          "platformVersion": "",
                          "selectedAndroidAvdName": "",
                          "androidApiLevel": 0,
                          "androidDeviceProfile": "pixel_6",
                          "androidImageTag": "google_apis",
                          "androidAbi": "x86_64",
                          "androidRamMb": 2048,
                          "androidCores": 2,
                          "provisionAndroidEmulator": false,
                          "openInspector": false
                        }
                        """,
                        "Prepares and starts a wrapped Appium Inspector recording session in one call.",
                        true),
                template("Inspector Status", "mobile_inspector_record_status", "{}"),
                template("Get Accessibility Tree", "mobile_get_accessibility_tree",
                        """
                        {
                          "maxCharacters": 8000
                        }
                        """,
                        "Returns bounded native accessibility XML or mobile web source.",
                        false),
                template("Get Mobile Contexts", "mobile_get_contexts",
                        """
                        {
                          "maxCharacters": 8000
                        }
                        """,
                        "Lists Appium contexts and bounded current source.",
                        false),
                template("Switch Mobile Context", "mobile_switch_context",
                        """
                        {
                          "contextName": "NATIVE_APP"
                        }
                        """,
                        "Switches native, hybrid, or mobile web Appium context.",
                        true),
                template("Take Mobile Screenshot", "browser_take_screenshot",
                        """
                        {
                          "outputPath": "target/shaft-mobile/screenshot.png",
                          "includeBase64": false
                        }
                        """,
                        "Writes mobile screenshot evidence without inline base64 by default"
                                + " (dispatches to the active mobile engine).",
                        true));
    }

    static List<ToolTemplate> mcp() {
        return List.of(
                template("Plan Coding Partner Work", "shaft_coding_partner_plan",
                        """
                        {
                          "repositoryPath": ".",
                          "intent": "",
                          "backend": "WebDriver",
                          "currentSourcePath": "",
                          "selectedText": "",
                          "artifactPaths": [],
                          "maxResults": 10
                        }
                        """,
                        "Creates a preview-only SHAFT coding partner plan from the current source, selected text, reuse candidates, evidence, and verification needs.",
                        false),
                template("Automation Scenarios", "test_automation_scenarios",
                        """
                        {
                          "area": "all",
                          "intent": "",
                          "maxResults": 20
                        }
                        """),
                template("Guardrail Check", "test_code_guardrails_check",
                        """
                        {
                          "language": "java",
                          "code": ""
                        }
                        """),
                template("Assistant Clients", "autobot_local_agent_clients", "{}"));
    }

    static List<ToolTemplate> guide() {
        return List.of(
                template("Search User Guide", "shaft_guide_search",
                        """
                        {
                          "query": "page objects locators",
                          "maxResults": 5
                        }
                        """));
    }

    static List<ToolTemplate> projects() {
        return List.of(
                template("Create SHAFT Project", "shaft_project_create",
                        """
                        {
                          "outputDirectory": "shaft-web-testng",
                          "runner": "TestNG",
                          "platform": "web",
                          "groupId": "io.github.yourUsername",
                          "artifactId": "shaft-web-testng",
                          "version": "1.0.0",
                          "optionalModules": [],
                          "includeGithubActions": true,
                          "includeDependabot": true,
                          "overwrite": false
                        }
                        """,
                        "Creates a new SHAFT project directory. Confirm the output path before running.",
                        true),
                template("Preview Current Project Upgrade", "shaft_project_upgrade",
                        """
                        {
                          "projectRoot": ".",
                          "upgradeType": "basic",
                          "dryRun": true,
                          "approve": false,
                          "shaftVersion": "",
                          "compileCommand": "",
                          "compileTimeout": 900,
                          "skipBaselineCompile": false,
                          "allowAiRepair": false
                        }
                        """),
                template("Apply Current Project Upgrade", "shaft_project_upgrade",
                        """
                        {
                          "projectRoot": ".",
                          "upgradeType": "basic",
                          "dryRun": false,
                          "approve": true,
                          "shaftVersion": "",
                          "compileCommand": "",
                          "compileTimeout": 900,
                          "skipBaselineCompile": false,
                          "allowAiRepair": false
                        }
                        """,
                        "Applies a SHAFT project upgrade. Run the preview template first.",
                        true));
    }

    private static ToolTemplate template(String label, String toolName, String arguments) {
        return new ToolTemplate(label, toolName, arguments.stripIndent().trim());
    }

    private static ToolTemplate template(
            String label,
            String toolName,
            String arguments,
            String description,
            boolean confirmationRequired) {
        return new ToolTemplate(label, toolName, arguments.stripIndent().trim(), description, confirmationRequired);
    }
}
