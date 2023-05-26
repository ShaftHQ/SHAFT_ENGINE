package com.shaft.api;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.openqa.selenium.MutableCapabilities;

import java.io.File;
import java.util.*;


public class LambdaTest {

    private static final String hubUrl = "hub.lambdatest.com/wd/hub";
    private static final String serviceUri = "https://manual-api.lambdatest.com/";
    private static final String appUploadServiceName = "app/upload/realDevice";

    private LambdaTest() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Use this method to set up all the needed capabilities to be able to upload and test the latest version of your native application.
     * You can refer to the getting started guide for LambdaTest App Automate to get all the needed information here <a href="https://app-automate..com/dashboard/v2/getting-started">BrowserStack: Getting Started</a>
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
        SHAFT.Properties.timeouts.set().apiSocketTimeout(600); //increasing socket timeout to 10 minutes to upload a new app file
        ReportManager.logDiscrete("Setting up BrowserStack configuration for new native app version...");
        String testData = "Username: " + username + ", Password: " + "•".repeat(password.length()) + ", Device Name: " + deviceName + ", OS Version: " + osVersion + ", Relative Path to App File: " + relativePathToAppFile + ", App Name: " + appName;

        // upload app to browserstack api
        List<Object> apkFile = new ArrayList<>();
        apkFile.add("file");
        String appPath = FileActions.getInstance().getAbsolutePath(relativePathToAppFile);
        apkFile.add(new File(appPath));
        ReportManager.logDiscrete("LambdaTest appPath: " + appPath);

        List<Object> customID = new ArrayList<>();
        customID.add("custom_id");
        String userProvidedCustomID = SHAFT.Properties.browserStack.customID();
        String custom_id = "".equals(userProvidedCustomID) ? "SHAFT_Engine_" + appName.replaceAll(" ", "_") : userProvidedCustomID;
        customID.add(custom_id);
        ReportManager.logDiscrete("LambdaTest custom_id: " + custom_id);

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
            ReportManager.logDiscrete("LambdaTest app_url: " + appUrl);
        } catch (NullPointerException exception) {
            failAction(testData, exception);
        }
        // set properties
        MutableCapabilities lambdaTestCapabilities = setLambdaTestProperties(username, password, deviceName, osVersion, appUrl);
        testData = testData + ", App URL: " + appUrl;
        passAction(testData);
        return lambdaTestCapabilities;
    }

    /**
     * Use this method to set up all the needed capabilities to be able to test an already uploaded version of your native application.
     * You can refer to the getting started guide for BrowserStack App Automate to get all the needed information here <a href="https://app-automate.browserstack.com/dashboard/v2/getting-started">BrowserStack: Getting Started</a>
     *
     * @param username   Your BrowserStack username
     * @param password   Your BrowserStack password
     * @param deviceName Name of the Target device
     * @param osVersion  Version of the Target operating system
     * @param appUrl     Url of the target app that was previously uploaded to be tested via BrowserStack
     * @return native app capabilities
     */
    public static MutableCapabilities setupNativeAppExecution(String username, String password, String deviceName, String osVersion, String appUrl) {
        ReportManager.logDiscrete("Setting up BrowserStack configuration for existing native app version...");
        String testData = "Username: " + username + ", Password: " + password + ", Device Name: " + deviceName + ", OS Version: " + osVersion + ", App URL: " + appUrl;
        // set properties
        MutableCapabilities browserStackCapabilities = setLambdaTestProperties(username, password, deviceName, osVersion, appUrl);
        passAction(testData);
        return browserStackCapabilities;
    }

    public static MutableCapabilities setupMobileWebExecution() {
        ReportManager.logDiscrete("Setting up BrowserStack configuration for mobile web execution...");
        String username = SHAFT.Properties.lambdaTest.username();
        String password = SHAFT.Properties.lambdaTest.accessKey();
        String os = SHAFT.Properties.platform.targetPlatform();
        String osVersion = SHAFT.Properties.lambdaTest.osVersion();

        String testData = "Username: " + username + ", Password: " + password + ", Operating System: " + os + ", Operating System Version: " + osVersion;

        // set properties
        SHAFT.Properties.platform.set().executionAddress(username + ":" + password + "@mobile-" + hubUrl);
        SHAFT.Properties.mobile.set().browserName(SHAFT.Properties.web.targetBrowserName());
        MutableCapabilities lambdaTestCapabilities = new MutableCapabilities();
        var browserVersion = SHAFT.Properties.lambdaTest.browserVersion();
        if (browserVersion != null && !"".equals(browserVersion.trim())) {
            lambdaTestCapabilities.setCapability("browserVersion", SHAFT.Properties.lambdaTest.browserVersion());
        }
        lambdaTestCapabilities.setCapability("browserName", SHAFT.Properties.web.targetBrowserName());
        if (SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase("windows")) {
            lambdaTestCapabilities.setCapability("platformName", "Windows " + SHAFT.Properties.lambdaTest.osVersion());
        }else{
            lambdaTestCapabilities.setCapability("platformName", SHAFT.Properties.platform.targetPlatform());
        }
        HashMap<String, Object> lambdaTestOptions = new HashMap<>();
        lambdaTestOptions.put("project", SHAFT.Properties.lambdaTest.project());
        lambdaTestOptions.put("build", SHAFT.Properties.lambdaTest.build());
        lambdaTestOptions.put("w3c", SHAFT.Properties.lambdaTest.w3c());
        lambdaTestOptions.put("deviceName", SHAFT.Properties.lambdaTest.deviceName());
        lambdaTestOptions.put("platformVersion", SHAFT.Properties.lambdaTest.platformVersion());
        lambdaTestOptions.put("selenium_version", SHAFT.Properties.lambdaTest.selenium_version());
        lambdaTestOptions.put("tunnel", SHAFT.Properties.lambdaTest.tunnel());
        lambdaTestOptions.put("tunnelName", SHAFT.Properties.lambdaTest.tunnelName());
        lambdaTestOptions.put("video", SHAFT.Properties.lambdaTest.video());
        lambdaTestOptions.put("visual", SHAFT.Properties.lambdaTest.visual());
        lambdaTestOptions.put("name", SHAFT.Properties.lambdaTest.buildName());
        lambdaTestOptions.put("visual", SHAFT.Properties.lambdaTest.visual());
        lambdaTestOptions.put("deviceOrientation", SHAFT.Properties.lambdaTest.deviceOrientation());
        lambdaTestOptions.put("idleTimeout", SHAFT.Properties.lambdaTest.idleTimeout());
        lambdaTestOptions.put("queueTimeout", SHAFT.Properties.lambdaTest.queueTimeout());
        lambdaTestOptions.put("autoGrantPermissions", SHAFT.Properties.lambdaTest.autoGrantPermissions());
        lambdaTestOptions.put("autoAcceptAlerts", SHAFT.Properties.lambdaTest.autoAcceptAlerts());
        lambdaTestOptions.put("isRealMobile", SHAFT.Properties.lambdaTest.isRealMobile());
        lambdaTestOptions.put("console", SHAFT.Properties.lambdaTest.console());

        String geoLocation = SHAFT.Properties.lambdaTest.geoLocation();
        if (geoLocation != null && !"".equals(geoLocation)) {
            lambdaTestOptions.put("geoLocation", SHAFT.Properties.lambdaTest.geoLocation());
        }
        lambdaTestCapabilities.setCapability("LT:Options", lambdaTestOptions);
        passAction(testData);
        return lambdaTestCapabilities;
    }

    public static MutableCapabilities setupDesktopWebExecution() {
        ReportManager.logDiscrete("Setting up LambdaTest configuration for desktop web execution...");
        String username = SHAFT.Properties.lambdaTest.username();
        String password = SHAFT.Properties.lambdaTest.accessKey();
        String os = SHAFT.Properties.platform.targetPlatform();
        String osVersion = SHAFT.Properties.lambdaTest.osVersion();

        String testData = "Username: " + username + ", Password: " + password + ", Operating System: " + os + ", Operating System Version: " + osVersion;

        // set properties
        SHAFT.Properties.platform.set().executionAddress(username + ":" + password + "@" + hubUrl);
        SHAFT.Properties.mobile.set().browserName(SHAFT.Properties.web.targetBrowserName());
        MutableCapabilities lambdaTestCapabilities = new MutableCapabilities();
        var browserVersion = SHAFT.Properties.lambdaTest.browserVersion();
        if (browserVersion != null && !"".equals(browserVersion.trim())) {
            lambdaTestCapabilities.setCapability("browserVersion", SHAFT.Properties.lambdaTest.browserVersion());
        }
        lambdaTestCapabilities.setCapability("browserName", SHAFT.Properties.web.targetBrowserName());
        lambdaTestCapabilities.setCapability("platformName", "Windows " + SHAFT.Properties.lambdaTest.osVersion());
        HashMap<String, Object> lambdaTestOptions = new HashMap<>();
        lambdaTestOptions.put("project", SHAFT.Properties.lambdaTest.project());
        lambdaTestOptions.put("build", SHAFT.Properties.lambdaTest.build());
        lambdaTestOptions.put("w3c", SHAFT.Properties.lambdaTest.w3c());
        lambdaTestOptions.put("selenium_version", SHAFT.Properties.lambdaTest.selenium_version());
        lambdaTestOptions.put("tunnel", SHAFT.Properties.lambdaTest.tunnel());
        String geoLocation = SHAFT.Properties.lambdaTest.geoLocation();
        if (geoLocation != null && !"".equals(geoLocation)) {
            lambdaTestOptions.put("geoLocation", SHAFT.Properties.lambdaTest.geoLocation());
        }
        lambdaTestCapabilities.setCapability("LT:Options", lambdaTestOptions);
        passAction(testData);
        return lambdaTestCapabilities;
    }

    private static MutableCapabilities setLambdaTestProperties(String username, String password, String deviceName, String osVersion, String appUrl) {
        SHAFT.Properties.platform.set().executionAddress(username + ":" + password + "@" + hubUrl);
        SHAFT.Properties.mobile.set().deviceName(deviceName);
        SHAFT.Properties.mobile.set().platformVersion(osVersion);
        SHAFT.Properties.mobile.set().app(appUrl);
        MutableCapabilities lambdaTestCapabilities = new MutableCapabilities();
        HashMap<String, Object> lambdaTestOptions = new HashMap<>();
        lambdaTestOptions.put("appiumVersion", SHAFT.Properties.lambdaTest.appiumVersion());
        lambdaTestOptions.put("acceptInsecureCerts", SHAFT.Properties.lambdaTest.acceptInsecureCerts());
        lambdaTestOptions.put("debug", SHAFT.Properties.lambdaTest.debug());
        lambdaTestOptions.put("networkLogs", SHAFT.Properties.lambdaTest.networkLogs());
        lambdaTestCapabilities.setCapability("LT:Options", lambdaTestOptions);
        return lambdaTestCapabilities;
    }

    private static void passAction(String testData) {
        reportActionResult(Thread.currentThread().getStackTrace()[2].getMethodName(), testData, true);
    }

    private static void failAction(String testData, Throwable... rootCauseException) {
        String message = reportActionResult(Thread.currentThread().getStackTrace()[2].getMethodName(), testData, false, rootCauseException);
        FailureReporter.fail(BrowserStack.class, message, rootCauseException[0]);
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
