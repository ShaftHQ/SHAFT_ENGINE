package com.shaft.api;

import com.shaft.cli.FileActions;
import org.testng.Assert;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class BrowserStack {
    private static final String hubUrl = "hub.browserstack.com";
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
     */
    public static void setupNativeAppExecution(String username, String password, String deviceName, String osVersion, String relativePathToAppFile, String appName) {
        String testData = "Username: " + username + ", Password: " + password + ", Device Name: " + deviceName + ", OS Version: " + osVersion + ", Relative Path to App File: " + relativePathToAppFile + ", App Name: " + appName;
        // set initial properties
        System.setProperty("executionAddress", hubUrl);
        System.setProperty("mobile_browserstack.user", username);
        System.setProperty("mobile_browserstack.key", password);
        System.setProperty("mobile_device", deviceName);
        System.setProperty("mobile_os_version", osVersion);

        // upload app to browserstack api
        List<Object> apkFile = new ArrayList<>();
        apkFile.add("file");
        apkFile.add(new File(FileActions.getAbsolutePath(relativePathToAppFile)));

        List<Object> customID = new ArrayList<>();
        customID.add("custom_id");
        customID.add("SHAFT_Engine_" + appName.replaceAll(" ", "_"));

        List<List<Object>> parameters = new ArrayList<>();
        parameters.add(apkFile);
        parameters.add(customID);

        try {
            System.setProperty("mobile_app", Objects.requireNonNull(RestActions.getResponseJSONValue(new RestActions(serviceUri).buildNewRequest(appUploadServiceName, RestActions.RequestType.POST)
                            .setParameters(parameters)
                            .setParametersType(RestActions.ParametersType.FORM)
                            .setAuthentication(username, password, RequestBuilder.AuthenticationType.BASIC)
                            .performRequest(),
                    "app_url")));
        } catch (NullPointerException exception) {
            failAction(testData, exception);
        }
        passAction(testData);
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
        String message;
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = "API Action [" + actionName + "] successfully performed.";
        } else {
            message = "API Action [" + actionName + "] failed.";
        }
        if (testData != null && !testData.isEmpty()) {
            message = message + " With the following test data [" + testData + "].";
        }
        return message;
    }
}
