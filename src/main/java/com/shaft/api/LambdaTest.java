package com.shaft.api;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import io.github.shafthq.shaft.tools.io.ReportManagerHelper;
import org.openqa.selenium.MutableCapabilities;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public class LambdaTest {
    private static final String hubUrl = "@hub.lambdatest.com/wd/hub";


    public static MutableCapabilities setupDesktopWebExecution() {
        ReportManager.logDiscrete("Setting up BrowserStack configuration for desktop web execution...");
        String username = SHAFT.Properties.LambdaTest.username();
        String password = SHAFT.Properties.LambdaTest.accessKey();
        String os = SHAFT.Properties.platform.targetPlatform();
        String osVersion = SHAFT.Properties.LambdaTest.osVersion();

        String testData = "Username: " + username + ", Password: " + password + ", Operating System: " + os + ", Operating System Version: " + osVersion;
        // set properties
        SHAFT.Properties.platform.set().executionAddress(username + ":" + password + "@" + hubUrl);
        System.setProperty("browserName", SHAFT.Properties.web.targetBrowserName());

        MutableCapabilities lambdaTestCapabilities = new MutableCapabilities();
        var browserVersion = SHAFT.Properties.LambdaTest.browserVersion();
        if (browserVersion != null && !"".equals(browserVersion.trim())) {
            lambdaTestCapabilities.setCapability("browserVersion", SHAFT.Properties.LambdaTest.browserVersion());
        }
        HashMap<String, Object> lambdaTestOptions = new HashMap<>();
        if (os.toLowerCase().contains("mac")) {
            lambdaTestOptions.put("platform", "OS X");
        } else if (os.toLowerCase().contains("windows")) {
            lambdaTestOptions.put("platform", "Windows");
        }
        lambdaTestOptions.put("version", osVersion);
        lambdaTestOptions.put("tunnel", SHAFT.Properties.LambdaTest.tunnel());
        lambdaTestOptions.put("selenium_version", SHAFT.Properties.LambdaTest.selenium_version());
        String geoLocation = SHAFT.Properties.LambdaTest.geoLocation();
        if (geoLocation != null && !"".equals(geoLocation)) {
            lambdaTestOptions.put("geoLocation", SHAFT.Properties.LambdaTest.geoLocation());
        }
        lambdaTestCapabilities.setCapability("LT:Options", lambdaTestOptions);

        passAction(testData);
        return lambdaTestCapabilities;
    }

    private static void passAction(String testData) {
        reportActionResult(Thread.currentThread().getStackTrace()[2].getMethodName(), testData, true);
    }

    private static String reportActionResult(String actionName, String testData, Boolean passFailStatus, Throwable... rootCauseException) {
        actionName = actionName.substring(0, 1).toUpperCase() + actionName.substring(1);
        String message;
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = "LambdaTest API Action \"" + actionName + "\" successfully performed.";
        } else {
            message = "LambdaTest API Action \"" + actionName + "\" failed.";
        }
        if (testData != null && !testData.isEmpty()) {
            message = message + " With the following test data \"" + testData + "\".";
        }

        if (rootCauseException != null && rootCauseException.length >= 1) {
            List<List<Object>> attachments = new ArrayList<>();
            List<Object> actualValueAttachment = Arrays.asList("LambdaTest Action Exception - " + actionName,
                    "Stacktrace", ReportManagerHelper.formatStackTraceToLogEntry(rootCauseException[0]));
            attachments.add(actualValueAttachment);
            ReportManagerHelper.log(message, attachments);
        } else {
            ReportManager.logDiscrete(message);
        }

        return message;
    }
}
