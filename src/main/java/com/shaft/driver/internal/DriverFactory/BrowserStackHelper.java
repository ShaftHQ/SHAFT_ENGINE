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

import java.io.File;
import java.time.LocalDateTime;
import java.util.*;
import java.util.regex.Pattern;

public class BrowserStackHelper {
    private static final String hubUrl = "hub-cloud.browserstack.com";
    private static final String serviceUri = "https://api-cloud.browserstack.com/";
    private static final String appUploadServiceName = "app-automate/upload";

    /**
     * Creates a new Selenium WebDriver instance using BrowserStack, use this to test Native Mobile apps over BrowserStack
     *
     * @param browserStackOptions custom browserstack options to be merged with the default in the browserStack.properties file
     * @return a new Selenium WebDriver instance using BrowserStack
     */
    public static DriverFactoryHelper getBrowserStackDriver(MutableCapabilities browserStackOptions) {
        String appUrl = SHAFT.Properties.browserStack.appUrl();
        var helper = new DriverFactoryHelper();
        if ("".equals(appUrl)) {
            // new native app OR web execution
            if ("".equals(SHAFT.Properties.browserStack.appRelativeFilePath())) {
                // this means it's a web execution (desktop or mobile)
                if (DriverFactoryHelper.isMobileWebExecution()) {
                    browserStackOptions = setupMobileWebExecution().merge(browserStackOptions);
                } else {
                    // desktop web
                    browserStackOptions = setupDesktopWebExecution().merge(browserStackOptions);
                }
                helper.initializeDriver(browserStackOptions);
            } else {
                // this is the new native app scenario
                browserStackOptions = setupNativeAppExecution(SHAFT.Properties.browserStack.username(), SHAFT.Properties.browserStack.accessKey(),
                        SHAFT.Properties.browserStack.deviceName(), SHAFT.Properties.browserStack.platformVersion(), SHAFT.Properties.browserStack.appRelativeFilePath(), SHAFT.Properties.browserStack.appName()).merge(browserStackOptions);
                helper.initializeDriver(DriverFactory.DriverType.APPIUM_MOBILE_NATIVE, browserStackOptions);
            }
        } else {
            // this is the existing version from a native app scenario
            browserStackOptions = setupNativeAppExecution(SHAFT.Properties.browserStack.username(), SHAFT.Properties.browserStack.accessKey(),
                    SHAFT.Properties.browserStack.deviceName(), SHAFT.Properties.browserStack.platformVersion(), appUrl).merge(browserStackOptions);
            helper.initializeDriver(DriverFactory.DriverType.APPIUM_MOBILE_NATIVE, browserStackOptions);
        }
        return helper;
    }

    /**
     * Use this method to set up all the needed capabilities to be able to upload and test the latest version of your native application.
     * You can refer to the getting started guide for BrowserStack App Automate to get all the needed information here <a href="https://app-automate.browserstack.com/dashboard/v2/getting-started">BrowserStack: Getting Started</a>
     *
     * @param username              Your BrowserStack username
     * @param password              Your BrowserStack password
     * @param deviceName            Name of the Target device
     * @param osVersion             Version of the Target operating system
     * @param relativePathToAppFile Relative path to your app file inside the project directory
     * @param appName               Name of your APK (excluding version number). This will be used as your CustomID so that you can keep uploading new versions of the same app and run your tests against them.
     * @return appURL for the newly uploaded app file on BrowserStack to be used for future tests
     */
    private static MutableCapabilities setupNativeAppExecution(String username, String password, String deviceName, String osVersion, String relativePathToAppFile, String appName) {
        SHAFT.Properties.timeouts.set().apiSocketTimeout(600); //increasing socket timeout to 10 minutes to upload a new app file
        ReportManager.logDiscrete("Setting up BrowserStack configuration for new native app version...");
        String testData = "Username: " + username + ", Password: " + "•".repeat(password.length()) + ", Device Name: " + deviceName + ", OS Version: " + osVersion + ", Relative Path to App File: " + relativePathToAppFile + ", App Name: " + appName;

        // upload app to browserstack api
        List<Object> apkFile = new ArrayList<>();
        apkFile.add("file");
        String appPath = FileActions.getInstance(true).getAbsolutePath(relativePathToAppFile);
        apkFile.add(new File(appPath));
        ReportManager.logDiscrete("BrowserStack appPath: " + appPath);

        List<Object> customID = new ArrayList<>();
        customID.add("custom_id");
        String userProvidedCustomID = SHAFT.Properties.browserStack.customID();
        String custom_id = "".equals(userProvidedCustomID) ? "SHAFT_Engine_" + appName.replaceAll(" ", "_") : userProvidedCustomID;
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
    private static MutableCapabilities setupNativeAppExecution(String username, String password, String deviceName, String osVersion, String appUrl) {
        ReportManager.logDiscrete("Setting up BrowserStack configuration for existing native app version...");
        String testData = "Username: " + username + ", Password: " + password + ", Device Name: " + deviceName + ", OS Version: " + osVersion + ", App URL: " + appUrl;
        // set properties
        MutableCapabilities browserStackCapabilities = setBrowserStackProperties(username, password, deviceName, osVersion, appUrl);
        passAction(testData);
        return browserStackCapabilities;
    }

    private static MutableCapabilities setupMobileWebExecution() {
        ReportManager.logDiscrete("Setting up BrowserStack configuration for mobile web execution...");
        String username = SHAFT.Properties.browserStack.username();
        String password = SHAFT.Properties.browserStack.accessKey();
        String os = SHAFT.Properties.platform.targetPlatform();
        String osVersion = SHAFT.Properties.browserStack.osVersion();

        String testData = "Username: " + username + ", Password: " + password + ", Operating System: " + os + ", Operating System Version: " + osVersion;
        // set properties
        SHAFT.Properties.platform.set().executionAddress(username + ":" + password + "@" + hubUrl);

        MutableCapabilities browserStackCapabilities = new MutableCapabilities();
        HashMap<String, Object> browserstackOptions = new HashMap<>();
        browserstackOptions.put("osVersion", osVersion);
        browserstackOptions.put("local", SHAFT.Properties.browserStack.local());
        browserstackOptions.put("appiumVersion", SHAFT.Properties.browserStack.appiumVersion());
        browserstackOptions.put("deviceName", SHAFT.Properties.browserStack.deviceName());

        var pathItems = System.getProperty("user.dir").split(Pattern.quote(File.separator));
        var time = LocalDateTime.now();
        browserstackOptions.put("projectName", ReportManagerHelper.getTestClassName());
        browserstackOptions.put("buildName", pathItems[pathItems.length - 1] + "_" + time.getYear() + time.getMonthValue() + time.getDayOfMonth());

        browserStackCapabilities.setCapability("bstack:options", browserstackOptions);

        passAction(testData);
        return browserStackCapabilities;
    }

    private static MutableCapabilities setupDesktopWebExecution() {
        ReportManager.logDiscrete("Setting up BrowserStack configuration for desktop web execution...");
        String username = SHAFT.Properties.browserStack.username();
        String password = SHAFT.Properties.browserStack.accessKey();
        String os = SHAFT.Properties.platform.targetPlatform();
        String osVersion = SHAFT.Properties.browserStack.osVersion();

        String testData = "Username: " + username + ", Password: " + password + ", Operating System: " + os + ", Operating System Version: " + osVersion;
        // set properties
        SHAFT.Properties.platform.set().executionAddress(username + ":" + password + "@" + hubUrl);
        SHAFT.Properties.mobile.set().browserName(SHAFT.Properties.web.targetBrowserName());
//        System.setProperty("browserName", SHAFT.Properties.web.targetBrowserName());

        MutableCapabilities browserStackCapabilities = new MutableCapabilities();
        var browserVersion = SHAFT.Properties.browserStack.browserVersion();
        if (browserVersion != null && !browserVersion.trim().isEmpty()) {
            browserStackCapabilities.setCapability("browserVersion", SHAFT.Properties.browserStack.browserVersion());
        }
        HashMap<String, Object> browserstackOptions = new HashMap<>();
        if (os.toLowerCase().contains("mac")) {
            browserstackOptions.put("os", "OS X");
        } else if (os.toLowerCase().contains("windows")) {
            browserstackOptions.put("os", "Windows");
        }
        browserstackOptions.put("osVersion", osVersion);
        browserstackOptions.put("local", SHAFT.Properties.browserStack.local());
        browserstackOptions.put("seleniumVersion", SHAFT.Properties.browserStack.seleniumVersion());
        String geoLocation = SHAFT.Properties.browserStack.geoLocation();
        if (geoLocation != null && !geoLocation.isEmpty()) {
            browserstackOptions.put("geoLocation", SHAFT.Properties.browserStack.geoLocation());
        }

        var pathItems = System.getProperty("user.dir").split(Pattern.quote(File.separator));
        var time = LocalDateTime.now();
        browserstackOptions.put("projectName", ReportManagerHelper.getTestClassName());
        browserstackOptions.put("buildName", pathItems[pathItems.length - 1] + "_" + time.getYear() + time.getMonthValue() + time.getDayOfMonth());

        browserStackCapabilities.setCapability("bstack:options", browserstackOptions);

        passAction(testData);
        return browserStackCapabilities;
    }

    private static MutableCapabilities setBrowserStackProperties(String username, String password, String deviceName, String osVersion, String appUrl) {
        SHAFT.Properties.platform.set().executionAddress(username + ":" + password + "@" + hubUrl);
        SHAFT.Properties.mobile.set().deviceName(deviceName);
        SHAFT.Properties.mobile.set().platformVersion(osVersion);
        SHAFT.Properties.mobile.set().app(appUrl);
        MutableCapabilities browserStackCapabilities = new MutableCapabilities();
        HashMap<String, Object> browserstackOptions = new HashMap<>();
        browserstackOptions.put("appiumVersion", SHAFT.Properties.browserStack.appiumVersion());
        browserstackOptions.put("acceptInsecureCerts", SHAFT.Properties.browserStack.acceptInsecureCerts());
        browserstackOptions.put("debug", SHAFT.Properties.browserStack.debug());
        browserstackOptions.put("interactiveDebugging", SHAFT.Properties.browserStack.debug());
        browserstackOptions.put("enableBiometric", SHAFT.Properties.browserStack.enableBiometric());
        browserstackOptions.put("networkLogs", SHAFT.Properties.browserStack.networkLogs());

        var pathItems = System.getProperty("user.dir").split(Pattern.quote(File.separator));
        var time = LocalDateTime.now();
        browserstackOptions.put("projectName", ReportManagerHelper.getTestClassName());
        browserstackOptions.put("buildName", pathItems[pathItems.length - 1] + "_" + time.getYear() + time.getMonthValue() + time.getDayOfMonth());

        browserStackCapabilities.setCapability("bstack:options", browserstackOptions);
        return browserStackCapabilities;
    }

    private static void passAction(String testData) {
        reportActionResult(Thread.currentThread().getStackTrace()[2].getMethodName(), testData, true);
    }

    private static void failAction(String testData, Throwable... rootCauseException) {
        String message = reportActionResult(Thread.currentThread().getStackTrace()[2].getMethodName(), testData, false, rootCauseException);
        FailureReporter.fail(BrowserStackHelper.class, message, rootCauseException[0]);
    }

    private static String reportActionResult(String actionName, String testData, Boolean passFailStatus, Throwable... rootCauseException) {
        actionName = actionName.substring(0, 1).toUpperCase() + actionName.substring(1);
        String message;
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = "BrowserStack API Action \"" + actionName + "\" successfully performed.";
        } else {
            message = "BrowserStack API Action \"" + actionName + "\" failed.";
        }
        if (testData != null && !testData.isEmpty()) {
            message = message + " With the following test data \"" + testData + "\".";
        }

        if (rootCauseException != null && rootCauseException.length >= 1) {
            List<List<Object>> attachments = new ArrayList<>();
            List<Object> actualValueAttachment = Arrays.asList("BrowserStack Action Exception - " + actionName,
                    "Stacktrace", ReportManagerHelper.formatStackTraceToLogEntry(rootCauseException[0]));
            attachments.add(actualValueAttachment);
            ReportManagerHelper.log(message, attachments);
        } else {
            ReportManager.logDiscrete(message);
        }

        return message;
    }
}
