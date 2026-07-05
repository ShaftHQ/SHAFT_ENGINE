package com.shaft.mcp;

import java.util.ArrayList;
import java.util.List;

/**
 * Shared Java snippet generation for mobile MCP tools.
 */
final class McpMobileCode {
    private McpMobileCode() {
        throw new IllegalStateException("Utility class");
    }

    static McpCodeBlock nativeSetupBlock(
            String platform,
            String deviceName,
            String server,
            String automation,
            String platformVersion,
            String udid,
            String app,
            String appPackage,
            String appActivity,
            String bundleId) {
        StringBuilder code = new StringBuilder();
        code.append("SHAFT.Properties.platform.set()\n");
        code.append("        .targetPlatform(").append(java(platform)).append(")\n");
        code.append("        .executionAddress(").append(java(server)).append(");\n");
        code.append("SHAFT.Properties.web.set().isMobileEmulation(false);\n");
        code.append("SHAFT.Properties.mobile.set()\n");
        code.append("        .platformName(").append(java(platform)).append(")\n");
        code.append("        .automationName(").append(java(automation)).append(")\n");
        code.append("        .deviceName(").append(java(deviceName)).append(")\n");
        code.append("        .platformVersion(").append(java(platformVersion)).append(")\n");
        code.append("        .udid(").append(java(udid)).append(")\n");
        code.append("        .browserName(\"\")\n");
        code.append("        .app(").append(java(app)).append(")\n");
        code.append("        .appPackage(").append(java(appPackage)).append(")\n");
        code.append("        .appActivity(").append(java(appActivity)).append(")\n");
        code.append("        .bundleId(").append(java(bundleId)).append(");\n");
        code.append("SHAFT.GUI.WebDriver driver = new SHAFT.GUI.WebDriver();\n");
        return new McpCodeBlock(
                "mobile-native-setup",
                "Native mobile Appium setup",
                McpCodeBlock.Kind.SETUP,
                "java",
                List.of("com.shaft.driver.SHAFT"),
                code.toString(),
                "Paste into setup before native mobile actions.",
                true,
                List.of(),
                nativeWarnings(platform, app, appPackage, appActivity, bundleId));
    }

    static McpCodeBlock actionBlock(String action, String javaCode) {
        return new McpCodeBlock(
                "mobile-action-" + text(action),
                "Mobile action snippet",
                McpCodeBlock.Kind.ACTION,
                "java",
                javaCode.contains("By.")
                        ? List.of("com.shaft.driver.SHAFT", "org.openqa.selenium.By")
                        : List.of("com.shaft.driver.SHAFT"),
                javaCode + System.lineSeparator(),
                "Paste inside a method that already owns a SHAFT.GUI.WebDriver named driver.",
                true,
                List.of(),
                List.of());
    }

    static String locatorCode(locatorStrategy strategy, String value) {
        String literal = java(value);
        if (strategy == null) {
            return "By.xpath(" + literal + ")";
        }
        return switch (strategy) {
            case ID -> "SHAFT.GUI.Locator.id(" + literal + ")";
            case CSSSELECTOR, CSS, SELECTOR -> "SHAFT.GUI.Locator.cssSelector(" + literal + ")";
            case XPATH -> "By.xpath(" + literal + ")";
            case NAME -> "SHAFT.GUI.Locator.name(" + literal + ")";
            case TAGNAME -> "SHAFT.GUI.Locator.tagName(" + literal + ")";
            case CLASSNAME -> "SHAFT.GUI.Locator.className(" + literal + ")";
            case ACCESSIBILITY_ID -> "SHAFT.GUI.Locator.accessibilityId(" + literal + ")";
            case ANDROID_UIAUTOMATOR -> "SHAFT.GUI.Locator.androidUiAutomator(" + literal + ")";
            case IOS_PREDICATE -> "SHAFT.GUI.Locator.iosPredicateString(" + literal + ")";
            case IOS_CLASS_CHAIN -> "SHAFT.GUI.Locator.iosClassChain(" + literal + ")";
        };
    }

    static String tapCoordinatesCode(int x, int y) {
        return "driver.element().touch().tapByCoordinates(%d, %d);".formatted(x, y);
    }

    static String swipeCoordinatesCode(int startX, int startY, int endX, int endY, int durationMillis) {
        return "driver.element().touch().swipeByCoordinates(%d, %d, %d, %d, %d);"
                .formatted(startX, startY, endX, endY, Math.max(durationMillis, 100));
    }

    static List<String> nativeWarnings(String platform, String app, String appPackage, String appActivity,
            String bundleId) {
        List<String> warnings = new ArrayList<>();
        if ("Android".equals(platform) && text(app).isBlank()
                && (text(appPackage).isBlank() || text(appActivity).isBlank())) {
            warnings.add("For installed Android apps, provide appPackage and appActivity when app is blank.");
        }
        if ("iOS".equals(platform) && text(app).isBlank() && text(bundleId).isBlank()) {
            warnings.add("For installed iOS apps, provide bundleId when app is blank.");
        }
        return List.copyOf(warnings);
    }

    static String java(String value) {
        String text = value == null ? "" : value;
        return "\"" + text
                .replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t") + "\"";
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
