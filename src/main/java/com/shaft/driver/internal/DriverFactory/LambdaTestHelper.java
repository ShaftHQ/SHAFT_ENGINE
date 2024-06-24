package com.shaft.driver.internal.DriverFactory;

import com.shaft.api.RequestBuilder;
import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.Platform;

import java.io.File;
import java.util.*;

public class LambdaTestHelper {
    private static final String hubUrl = "hub.lambdatest.com/wd/hub";
    private static final String serviceUri = "https://manual-api.lambdatest.com/";
    private static final String appUploadServiceName = "app/upload/realDevice";

    /**
     * Creates a new Selenium WebDriver instance using lambdaTest, use this to test Native Mobile apps over lambdaTest
     *
     * @param lambdaTestOptions custom lambdaTest options to be merged with the default in the lambdaTest.properties file
     * @return a new Selenium WebDriver instance using lambdaTest
     */
    public static DriverFactoryHelper getLambdaTestDriver(MutableCapabilities lambdaTestOptions) {
        String appUrl = SHAFT.Properties.lambdaTest.appUrl();
        var helper = new DriverFactoryHelper();
        if ("".equals(appUrl)) {
            // new native app OR web execution
            if ("".equals(SHAFT.Properties.lambdaTest.appRelativeFilePath())) {
                // this means it's a web execution (desktop or mobile)
                if (DriverFactoryHelper.isMobileWebExecution()) {
                    lambdaTestOptions = setupMobileWebExecution().merge(lambdaTestOptions);
                } else {
                    // desktop web
                    lambdaTestOptions = setupDesktopWebExecution().merge(lambdaTestOptions);
                }
                helper.initializeDriver(lambdaTestOptions);
            } else {
                // this is the new native app scenario
                lambdaTestOptions = setupNativeAppExecution(SHAFT.Properties.lambdaTest.username(), SHAFT.Properties.lambdaTest.accessKey(), SHAFT.Properties.lambdaTest.deviceName(), SHAFT.Properties.lambdaTest.platformVersion(), SHAFT.Properties.lambdaTest.appRelativeFilePath(), SHAFT.Properties.lambdaTest.appName()).merge(lambdaTestOptions);
                helper.initializeDriver(DriverFactory.DriverType.APPIUM_MOBILE_NATIVE, lambdaTestOptions);
            }
        } else {
            // this is the existing version from a native app scenario
            lambdaTestOptions = setupNativeAppExecution(SHAFT.Properties.lambdaTest.username(), SHAFT.Properties.lambdaTest.accessKey(), SHAFT.Properties.lambdaTest.deviceName(), SHAFT.Properties.lambdaTest.platformVersion(), appUrl).merge(lambdaTestOptions);
            helper.initializeDriver(DriverFactory.DriverType.APPIUM_MOBILE_NATIVE, lambdaTestOptions);
        }
        return helper;
    }

    /**
     * Use this method to set up all the needed capabilities to be able to upload and test the latest version of your native application.
     * You can refer to the getting started guide for LambdaTest App Automate to get all the needed information here <a href="https://app-automate..com/dashboard/v2/getting-started">LambdaTest: Getting Started</a>
     *
     * @param username              Your LambdaTest username
     * @param password              Your LambdaTest password
     * @param deviceName            Name of the Target device
     * @param osVersion             Version of the Target operating system
     * @param relativePathToAppFile Relative path to your app file inside the project directory
     * @param appName               Name of your APK (excluding version number). This will be used as your CustomID so that you can keep uploading new versions of the same app and run your tests against them.
     * @return appURL for the newly uploaded app file on LambdaTest to be used for future tests
     */
    private static MutableCapabilities setupNativeAppExecution(String username, String password, String deviceName, String osVersion, String relativePathToAppFile, String appName) {
        SHAFT.Properties.timeouts.set().apiSocketTimeout(600); //increasing socket timeout to 10 minutes to upload a new app file
        ReportManager.logDiscrete("Setting up LambdaTest configuration for new native app version...");
        String testData = "Username: " + username + ", Password: " + "•".repeat(password.length()) + ", Device Name: " + deviceName + ", OS Version: " + osVersion + ", Relative Path to App File: " + relativePathToAppFile + ", App Name: " + appName;
        // upload app to LambdaTest api
        List<Object> apkFile = new ArrayList<>();
        apkFile.add("appFile");
        String appPath = FileActions.getInstance(true).getAbsolutePath(relativePathToAppFile);
        apkFile.add(new File(appPath));
        ReportManager.logDiscrete("LambdaTest appPath: " + appPath);

        List<Object> customID = new ArrayList<>();
        customID.add("name");
        String userProvidedCustomID = SHAFT.Properties.lambdaTest.customID();
        String custom_id = "".equals(userProvidedCustomID) ? "SHAFT_Engine_" + appName.replaceAll(" ", "_") : userProvidedCustomID;
        customID.add(custom_id);
        ReportManager.logDiscrete("LambdaTest custom_id: " + custom_id);

        List<List<Object>> parameters = new ArrayList<>();
        parameters.add(apkFile);
        parameters.add(customID);
        var appUrl = "";
        try {
            appUrl = Objects.requireNonNull(RestActions.getResponseJSONValue(new SHAFT.API(serviceUri).post(appUploadServiceName).setContentType("multipart/form-data").setParameters(parameters, RestActions.ParametersType.FORM).setAuthentication(username, password, RequestBuilder.AuthenticationType.BASIC).perform(), "app_url"));
            ReportManager.logDiscrete("LambdaTest app_url: " + appUrl);
        } catch (NullPointerException exception) {
            failAction(testData, exception);
        }
        // set properties
        testData = testData + ", App URL: " + appUrl;
        passAction(testData);
        MutableCapabilities lambdaTestCapabilities = new MutableCapabilities();
        HashMap<String, Object> lambdaTestOptions = setLambdaTestProperties(username, password, deviceName, osVersion, appUrl);
        lambdaTestOptions.put("w3c", SHAFT.Properties.lambdaTest.w3c());
        if (Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
            lambdaTestOptions.put("platformName", Platform.ANDROID);
        } else {
            lambdaTestOptions.put("platformName", Platform.IOS);
        }
        lambdaTestOptions.put("deviceName", deviceName);
        lambdaTestOptions.put("platformVersion", osVersion);
        lambdaTestOptions.put("isRealMobile", SHAFT.Properties.lambdaTest.isRealMobile());
        lambdaTestOptions.put("appProfiling", SHAFT.Properties.lambdaTest.appProfiling());
        lambdaTestOptions.put("app", appUrl);
        
        lambdaTestCapabilities.setCapability("lt:options", lambdaTestOptions);
        return lambdaTestCapabilities;
    }

    /**
     * Use this method to set up all the needed capabilities to be able to test an already uploaded version of your native application.
     * You can refer to the getting started guide for LambdaTest App Automate to get all the needed information here <a href="https://app-automate.LambdaTest.com/dashboard/v2/getting-started">LambdaTest: Getting Started</a>
     *
     * @param username   Your LambdaTest username
     * @param password   Your LambdaTest password
     * @param deviceName Name of the Target device
     * @param osVersion  Version of the Target operating system
     * @param appUrl     Url of the target app that was previously uploaded to be tested via LambdaTest
     * @return native app capabilities
     */
    private static MutableCapabilities setupNativeAppExecution(String username, String password, String deviceName, String osVersion, String appUrl) {
        ReportManager.logDiscrete("Setting up LambdaTest configuration for existing native app version...");
        String testData = "Username: " + username + ", Password: " + password + ", Device Name: " + deviceName + ", OS Version: " + osVersion + ", App URL: " + appUrl;
        // set properties
        passAction(testData);
        
        MutableCapabilities lambdaTestCapabilities = new MutableCapabilities();
        HashMap<String, Object> lambdaTestOptions = setLambdaTestProperties(username, password, deviceName, osVersion, appUrl);
        lambdaTestOptions.put("w3c", SHAFT.Properties.lambdaTest.w3c());
        if (Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
            lambdaTestOptions.put("platformName", Platform.ANDROID);
        } else {
            lambdaTestOptions.put("platformName", Platform.IOS);
        }
        lambdaTestOptions.put("deviceName", deviceName);
        lambdaTestOptions.put("platformVersion", osVersion);
        lambdaTestOptions.put("isRealMobile", SHAFT.Properties.lambdaTest.isRealMobile());
        lambdaTestOptions.put("appProfiling", SHAFT.Properties.lambdaTest.appProfiling());
        lambdaTestOptions.put("app", appUrl);

        lambdaTestCapabilities.setCapability("lt:options", lambdaTestOptions);
        return lambdaTestCapabilities;
    }

    private static MutableCapabilities setupMobileWebExecution() {
        ReportManager.logDiscrete("Setting up LambdaTest configuration for mobile web execution...");
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
        if (browserVersion != null && !browserVersion.trim().isEmpty()) {
            lambdaTestCapabilities.setCapability("browserVersion", SHAFT.Properties.lambdaTest.browserVersion());
        }
        lambdaTestCapabilities.setCapability("browserName", SHAFT.Properties.web.targetBrowserName());
        HashMap<String, Object> lambdaTestOptions = new HashMap<>();
        lambdaTestOptions.put("project", SHAFT.Properties.lambdaTest.project());
        lambdaTestOptions.put("build", SHAFT.Properties.lambdaTest.build());
        lambdaTestOptions.put("w3c", SHAFT.Properties.lambdaTest.w3c());
        if (Platform.ANDROID.toString().equalsIgnoreCase(SHAFT.Properties.platform.targetPlatform())) {
            lambdaTestOptions.put("platformName", Platform.ANDROID);
        } else {
            lambdaTestOptions.put("platformName", Platform.IOS);
        }
        lambdaTestOptions.put("deviceName", SHAFT.Properties.lambdaTest.deviceName());
        lambdaTestOptions.put("platformVersion", SHAFT.Properties.lambdaTest.platformVersion());
        lambdaTestOptions.put("selenium_version", SHAFT.Properties.lambdaTest.selenium_version());
        lambdaTestOptions.put("tunnel", SHAFT.Properties.lambdaTest.tunnel());
        lambdaTestOptions.put("tunnelName", SHAFT.Properties.lambdaTest.tunnelName());
        lambdaTestOptions.put("video", SHAFT.Properties.lambdaTest.video());
        lambdaTestOptions.put("name", SHAFT.Properties.lambdaTest.buildName());
        lambdaTestOptions.put("visual", SHAFT.Properties.lambdaTest.visual());
        lambdaTestOptions.put("autoGrantPermissions", SHAFT.Properties.lambdaTest.autoGrantPermissions());
        lambdaTestOptions.put("autoAcceptAlerts", SHAFT.Properties.lambdaTest.autoAcceptAlerts());
        lambdaTestOptions.put("isRealMobile", SHAFT.Properties.lambdaTest.isRealMobile());
        lambdaTestOptions.put("console", SHAFT.Properties.lambdaTest.console());
        String geoLocation = SHAFT.Properties.lambdaTest.geoLocation();
        if (geoLocation != null && !Objects.equals(geoLocation, "")) {
            lambdaTestOptions.put("geoLocation", SHAFT.Properties.lambdaTest.geoLocation());
        }
        lambdaTestCapabilities.setCapability("lt:options", lambdaTestOptions);
        passAction(testData);
        return lambdaTestCapabilities;
    }

    private static MutableCapabilities setupDesktopWebExecution() {
        ReportManager.logDiscrete("Setting up LambdaTest configuration for desktop web execution...");
        String username = SHAFT.Properties.lambdaTest.username();
        String password = SHAFT.Properties.lambdaTest.accessKey();
        String os = SHAFT.Properties.platform.targetPlatform();
        String osVersion = SHAFT.Properties.lambdaTest.osVersion();

        String testData = "Username: " + username + ", Password: " + password + ", Operating System: " + os + ", Operating System Version: " + osVersion;

        // set properties
        SHAFT.Properties.platform.set().executionAddress(username + ":" + password + "@" + hubUrl);
        MutableCapabilities lambdaTestCapabilities = new MutableCapabilities();
        var browserVersion = SHAFT.Properties.lambdaTest.browserVersion();
        if (browserVersion != null && !browserVersion.trim().isEmpty()) {
            lambdaTestCapabilities.setCapability("browserVersion", SHAFT.Properties.lambdaTest.browserVersion());
        }
        lambdaTestCapabilities.setCapability("browserName", SHAFT.Properties.web.targetBrowserName());
        if (os.toLowerCase().contains("mac")) {
            lambdaTestCapabilities.setCapability("platformName", "MacOS " + SHAFT.Properties.lambdaTest.osVersion());
        } else if (os.toLowerCase().contains("windows")) {
            lambdaTestCapabilities.setCapability("platformName", "Windows " + SHAFT.Properties.lambdaTest.osVersion());
        }
        HashMap<String, Object> lambdaTestOptions = new HashMap<>();
        lambdaTestOptions.put("project", SHAFT.Properties.lambdaTest.project());
        lambdaTestOptions.put("build", SHAFT.Properties.lambdaTest.build());
        lambdaTestOptions.put("w3c", SHAFT.Properties.lambdaTest.w3c());
        lambdaTestOptions.put("selenium_version", SHAFT.Properties.lambdaTest.selenium_version());
        lambdaTestOptions.put("tunnel", SHAFT.Properties.lambdaTest.tunnel());
        lambdaTestOptions.put("tunnelName", SHAFT.Properties.lambdaTest.tunnelName());
        lambdaTestOptions.put("video", SHAFT.Properties.lambdaTest.video());
        lambdaTestOptions.put("visual", SHAFT.Properties.lambdaTest.visual());
        lambdaTestOptions.put("name", SHAFT.Properties.lambdaTest.buildName());
        lambdaTestOptions.put("autoGrantPermissions", SHAFT.Properties.lambdaTest.autoGrantPermissions());
        lambdaTestOptions.put("autoAcceptAlerts", SHAFT.Properties.lambdaTest.autoAcceptAlerts());
        lambdaTestOptions.put("isRealMobile", SHAFT.Properties.lambdaTest.isRealMobile());
        lambdaTestOptions.put("console", SHAFT.Properties.lambdaTest.console());
        String geoLocation = SHAFT.Properties.lambdaTest.geoLocation();
        if (geoLocation != null && !Objects.equals(geoLocation, "")) {
            lambdaTestOptions.put("geoLocation", SHAFT.Properties.lambdaTest.geoLocation());
        }
        lambdaTestCapabilities.setCapability("lt:options", lambdaTestOptions);
        passAction(testData);
        return lambdaTestCapabilities;
    }

    private static HashMap<String, Object> setLambdaTestProperties(String username, String password, String deviceName, String osVersion, String appUrl) {
        SHAFT.Properties.platform.set().executionAddress(username + ":" + password + "@" + hubUrl);
        SHAFT.Properties.mobile.set().deviceName(deviceName);
        SHAFT.Properties.mobile.set().platformVersion(osVersion);
        SHAFT.Properties.mobile.set().app(appUrl);
        HashMap<String, Object> lambdaTestOptions = new HashMap<>();
        lambdaTestOptions.put("appiumVersion", SHAFT.Properties.lambdaTest.appiumVersion());
        lambdaTestOptions.put("acceptInsecureCerts", SHAFT.Properties.lambdaTest.acceptInsecureCerts());
        lambdaTestOptions.put("debug", SHAFT.Properties.lambdaTest.debug());
        lambdaTestOptions.put("networkLogs", SHAFT.Properties.lambdaTest.networkLogs());
        return lambdaTestOptions;
    }

    private static void passAction(String testData) {
        reportActionResult(Thread.currentThread().getStackTrace()[2].getMethodName(), testData, true);
    }

    private static void failAction(String testData, Throwable... rootCauseException) {
        String message = reportActionResult(Thread.currentThread().getStackTrace()[2].getMethodName(), testData, false, rootCauseException);
        FailureReporter.fail(LambdaTestHelper.class, message, rootCauseException[0]);
    }

    private static String
    reportActionResult(String actionName, String testData, Boolean passFailStatus, Throwable... rootCauseException) {
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
            List<Object> actualValueAttachment = Arrays.asList("LambdaTest Action Exception - " + actionName, "Stacktrace", ReportManagerHelper.formatStackTraceToLogEntry(rootCauseException[0]));
            attachments.add(actualValueAttachment);
            ReportManagerHelper.log(message, attachments);
        } else {
            ReportManager.logDiscrete(message);
        }

        return message;
    }
}
