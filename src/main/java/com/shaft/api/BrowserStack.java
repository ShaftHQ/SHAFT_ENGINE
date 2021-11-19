package com.shaft.api;

import com.shaft.cli.FileActions;
import com.shaft.tools.io.ReportManager;
import org.openqa.selenium.MutableCapabilities;
import org.testng.Assert;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class BrowserStack {
    private static final String hubUrl = "hub-cloud.browserstack.com";
    private static final String serviceUri = "https://api-cloud.browserstack.com/";
    private static final String appUploadServiceName = "app-automate/upload";

    private BrowserStack() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Use this method to setup all the needed capabilities to be able to upload and test the latest version of your native application.
     * You can refer to the getting started guide for BrowserStack App Automate to get all the needed information here https://app-automate.browserstack.com/dashboard/v2/getting-started
     *
     * @param username              Your BrowserStack username
     * @param password              Your BrowserStack password
     * @param deviceName            Name of the Target device
     * @param osVersion             Version of the Target operating system
     * @param relativePathToAppFile Relative path to your app file inside the project directory
     * @param appName               Name of your APK (excluding version number). This will be used as your CustomID so that you can keep uploading new versions of the same app and run your tests against them.
     * @return appURL for the newly uploaded app file on BrowserStack to be used for future tests
     */
    public static MutableCapabilities setupNativeAppExecution(String username, String password, String deviceName, String osVersion, String relativePathToAppFile, String appName) {
        System.setProperty("apiSocketTimeout", "600"); //increasing socket timeout to 10 minutes to upload a new app file
        ReportManager.logDiscrete("Setting up BrowserStack configuration for new native app version...");
        String testData = "Username: " + username + ", Password: " + "•".repeat(password.length()) + ", Device Name: " + deviceName + ", OS Version: " + osVersion + ", Relative Path to App File: " + relativePathToAppFile + ", App Name: " + appName;

        // upload app to browserstack api
        List<Object> apkFile = new ArrayList<>();
        apkFile.add("file");
        String appPath = FileActions.getAbsolutePath(relativePathToAppFile);
        apkFile.add(new File(appPath));
        ReportManager.logDiscrete("BrowserStack appPath: " + appPath);

        List<Object> customID = new ArrayList<>();
        customID.add("custom_id");
        String custom_id = "SHAFT_Engine_" + appName.replaceAll(" ", "_");
        customID.add(custom_id);
        ReportManager.logDiscrete("BrowserStack custom_id: " + custom_id);

        List<List<Object>> parameters = new ArrayList<>();
        parameters.add(apkFile);
        parameters.add(customID);
        var appUrl = "";
        try {
            appUrl = Objects.requireNonNull(RestActions.getResponseJSONValue(new RestActions(serviceUri).buildNewRequest(appUploadServiceName, RestActions.RequestType.POST)
                            .setParameters(parameters, RestActions.ParametersType.FORM)
                            .setAuthentication(username, password, RequestBuilder.AuthenticationType.BASIC)
                            .performRequest(),
                    "app_url"));
            ReportManager.logDiscrete("BrowserStack app_url: " + appUrl);
        } catch (NullPointerException exception) {
            failAction(testData, exception);
        }
        // set properties
        MutableCapabilities browserStackCapabilities = setBrowserStackProperties(username, password, deviceName, osVersion, appUrl);
        testData = testData + ", App URL: " + appUrl;
        passAction(testData);
        return browserStackCapabilities;
    }

    /**
     * Use this method to setup all the needed capabilities to be able to test an already uploaded version of your native application.
     * You can refer to the getting started guide for BrowserStack App Automate to get all the needed information here https://app-automate.browserstack.com/dashboard/v2/getting-started
     *
     * @param username   Your BrowserStack username
     * @param password   Your BrowserStack password
     * @param deviceName Name of the Target device
     * @param osVersion  Version of the Target operating system
     * @param appUrl     Url of the target app that was previously uploaded to be tested via BrowserStack
     */
    public static MutableCapabilities setupNativeAppExecution(String username, String password, String deviceName, String osVersion, String appUrl) {
        ReportManager.logDiscrete("Setting up BrowserStack configuration for existing native app version...");
        String testData = "Username: " + username + ", Password: " + password + ", Device Name: " + deviceName + ", OS Version: " + osVersion + ", App URL: " + appUrl;
        // set properties
        MutableCapabilities browserStackCapabilities = setBrowserStackProperties(username, password, deviceName, osVersion, appUrl);
        passAction(testData);
        return browserStackCapabilities;
    }

    private static MutableCapabilities setBrowserStackProperties(String username, String password, String deviceName, String osVersion, String appUrl) {
        System.setProperty("executionAddress", username+":"+password+"@"+hubUrl);
        System.setProperty("mobile_deviceName", deviceName);
        System.setProperty("mobile_platformVersion", osVersion);
        System.setProperty("mobile_app", appUrl);

        MutableCapabilities browserStackCapabilities = new MutableCapabilities();
        HashMap<String, Object> browserstackOptions = new HashMap<>();
        browserstackOptions.put("appiumVersion", System.getProperty("browserStack.appiumVersion"));
        browserstackOptions.put("acceptInsecureCerts", System.getProperty("browserStack.acceptInsecureCerts"));
        browserstackOptions.put("debug", System.getProperty("browserStack.debug"));
        browserstackOptions.put("networkLogs", System.getProperty("browserStack.networkLogs"));
        browserStackCapabilities.setCapability("bstack:options", browserstackOptions);
        return browserStackCapabilities;
    }
  
    private static void passAction(String testData) {
        reportActionResult(Thread.currentThread().getStackTrace()[2].getMethodName(), testData, true);
    }

    private static void failAction(String testData, Throwable... rootCauseException) {
        String message = reportActionResult(Thread.currentThread().getStackTrace()[2].getMethodName(), testData, false);
        if (rootCauseException != null && rootCauseException.length >= 1) {
            Assert.fail(message, rootCauseException[0]);
        } else {
            Assert.fail(message);
        }
    }

    private static String reportActionResult(String actionName, String testData, Boolean passFailStatus) {
        actionName = actionName.substring(0, 1).toUpperCase() + actionName.substring(1);
        String message;
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = "BrowserStack API Action [" + actionName + "] successfully performed.";
        } else {
            message = "BrowserStack API Action [" + actionName + "] failed.";
        }
        if (testData != null && !testData.isEmpty()) {
            message = message + " With the following test data [" + testData + "].";
        }
        ReportManager.log(message);
        return message;
    }
}
