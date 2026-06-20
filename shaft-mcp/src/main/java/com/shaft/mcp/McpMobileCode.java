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
                List.of(
                        "com.shaft.driver.SHAFT",
                        "com.shaft.gui.element.TouchActions",
                        "io.appium.java_client.AppiumBy",
                        "org.openqa.selenium.By",
                        "org.openqa.selenium.ScreenOrientation",
                        "org.openqa.selenium.interactions.Pause",
                        "org.openqa.selenium.interactions.PointerInput",
                        "org.openqa.selenium.interactions.Sequence",
                        "org.openqa.selenium.remote.RemoteWebDriver",
                        "java.time.Duration",
                        "java.util.List"),
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
            case ID -> "SHAFT.GUI.Locator.hasAnyTagName().hasId(" + literal + ").build()";
            case CSSSELECTOR, CSS, SELECTOR -> "By.cssSelector(" + literal + ")";
            case XPATH -> "By.xpath(" + literal + ")";
            case NAME -> "SHAFT.GUI.Locator.hasAnyTagName().hasAttribute(\"name\", " + literal + ").build()";
            case TAGNAME -> "SHAFT.GUI.Locator.hasTagName(" + literal + ").build()";
            case CLASSNAME -> "SHAFT.GUI.Locator.hasAnyTagName().hasAttribute(\"class\", " + literal + ").build()";
            case ACCESSIBILITY_ID -> "AppiumBy.accessibilityId(" + literal + ")";
            case ANDROID_UIAUTOMATOR -> "AppiumBy.androidUIAutomator(" + literal + ")";
            case IOS_PREDICATE -> "AppiumBy.iOSNsPredicateString(" + literal + ")";
            case IOS_CLASS_CHAIN -> "AppiumBy.iOSClassChain(" + literal + ")";
        };
    }

    static String tapCoordinatesCode(int x, int y) {
        return """
                PointerInput finger = new PointerInput(PointerInput.Kind.TOUCH, "finger");
                Sequence tap = new Sequence(finger, 0);
                tap.addAction(finger.createPointerMove(Duration.ZERO, PointerInput.Origin.viewport(), %d, %d));
                tap.addAction(finger.createPointerDown(PointerInput.MouseButton.LEFT.asArg()));
                tap.addAction(new Pause(finger, Duration.ofMillis(100)));
                tap.addAction(finger.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));
                ((RemoteWebDriver) driver.getDriver()).perform(List.of(tap));""".formatted(x, y);
    }

    static String swipeCoordinatesCode(int startX, int startY, int endX, int endY, int durationMillis) {
        return """
                PointerInput finger = new PointerInput(PointerInput.Kind.TOUCH, "finger");
                Sequence swipe = new Sequence(finger, 0);
                swipe.addAction(finger.createPointerMove(Duration.ZERO, PointerInput.Origin.viewport(), %d, %d));
                swipe.addAction(finger.createPointerDown(PointerInput.MouseButton.LEFT.asArg()));
                swipe.addAction(finger.createPointerMove(Duration.ofMillis(%d), PointerInput.Origin.viewport(), %d, %d));
                swipe.addAction(finger.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));
                ((RemoteWebDriver) driver.getDriver()).perform(List.of(swipe));"""
                .formatted(startX, startY, Math.max(durationMillis, 100), endX, endY);
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
