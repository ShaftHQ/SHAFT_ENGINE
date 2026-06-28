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
                template("Generate Record At Target", "capture_record_at_target_code_blocks",
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
                        """));
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
                template("Replay Playwright Recording", "playwright_replay_recording",
                        """
                        {
                          "recordingPath": "recordings/playwright-recording.json",
                          "driverVariableName": "driver"
                        }
                        """),
                template("Replay Mobile Recording", "mobile_replay_recording",
                        """
                        {
                          "recordingPath": "recordings/mobile-recording.json",
                          "driverVariableName": "driver"
                        }
                        """));
    }

    static List<ToolTemplate> doctor() {
        return List.of(
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
                        """),
                template("Propose Healed Locator", "doctor_propose_healed_locator",
                        """
                        {
                          "repositoryRoot": ".",
                          "healingReportPath": "target/shaft-heal/healing-report.json",
                          "sourcePath": "src/test/java/example/Page.java",
                          "sourcePatchConsent": true,
                          "outputDirectory": "target/shaft-doctor/healing-proposals"
                        }
                        """));
    }

    static List<ToolTemplate> inspector() {
        return List.of(
                template("Mobile Toolchain Status", "mobile_toolchain_status",
                        """
                        {
                          "platformName": "Android"
                        }
                        """),
                template("Prepare Mobile Inspector Recording", "mobile_inspector_record_prepare",
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
                          "provisionAndroidEmulator": false
                        }
                        """),
                template("Inspector Status", "mobile_inspector_record_status", "{}"),
                template("Get Accessibility Tree", "mobile_get_accessibility_tree",
                        """
                        {
                          "maxCharacters": 12000
                        }
                        """),
                template("Take Mobile Screenshot", "mobile_take_screenshot",
                        """
                        {
                          "outputPath": "target/shaft-mobile/screenshot.png"
                        }
                        """));
    }

    static List<ToolTemplate> mcp() {
        return List.of(
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
                        """),
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
                        """));
    }

    private static ToolTemplate template(String label, String toolName, String arguments) {
        return new ToolTemplate(label, toolName, arguments.stripIndent().trim());
    }
}
