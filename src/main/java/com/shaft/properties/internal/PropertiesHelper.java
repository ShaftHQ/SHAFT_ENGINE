package com.shaft.properties.internal;

import com.shaft.cli.FileActions;
import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.enums.internal.Screenshots;
import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.ConfigFactory;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;
import org.openqa.selenium.remote.Browser;

import java.io.File;
import java.net.URL;
import java.util.Arrays;

public class PropertiesHelper {
    private static final String DEFAULT_PROPERTIES_FOLDER_PATH = "src/main/resources/properties/default";
    private static final String TARGET_PROPERTIES_FOLDER_PATH = DEFAULT_PROPERTIES_FOLDER_PATH.replace("/default", "");

    public static void initialize() {
        //initialize default properties
        initializeDefaultProperties();
        //attach property files
        attachPropertyFiles();
        //load properties
        loadProperties();
        //override key settings for special platforms
        overrideScreenShotTypeForMobileAndMacPlatforms();
        overrideForcedFlagsForMobilePlatforms();
    }

    public static void loadProperties() {
        //load paths as the default properties path is needed for the next step
        Properties.paths = ConfigFactory.create(Paths.class);
        //read custom property files (if any) into system properties
        PropertyFileManager.readCustomPropertyFiles();
        //load property objects
        Properties.paths = ConfigFactory.create(Paths.class); //reload paths in case the user changed something
        Properties.platform = ConfigFactory.create(Platform.class);
        Properties.web = ConfigFactory.create(Web.class);
        Properties.mobile = ConfigFactory.create(Mobile.class);
        Properties.browserStack = ConfigFactory.create(BrowserStack.class);
        Properties.internal = ConfigFactory.create(Internal.class);
        Properties.flags = ConfigFactory.create(Flags.class);
        Properties.cucumber = ConfigFactory.create(Cucumber.class);
        Properties.healenium = ConfigFactory.create(Healenium.class);
        Properties.jira = ConfigFactory.create(Jira.class);
        Properties.pattern = ConfigFactory.create(Pattern.class);
        Properties.reporting = ConfigFactory.create(Reporting.class);
        Properties.allure = ConfigFactory.create(Allure.class);
        Properties.tinkey = ConfigFactory.create(Tinkey.class);
        Properties.testNG = ConfigFactory.create(TestNG.class);
        Properties.log4j = ConfigFactory.create(Log4j.class);
        Properties.visuals = ConfigFactory.create(Visuals.class);
        Properties.timeouts = ConfigFactory.create(Timeouts.class);
        Properties.performance = ConfigFactory.create(Performance.class);
        Properties.lambdaTest = ConfigFactory.create(LambdaTest.class);
    }

    public static void postProcessing() {
        overrideTargetOperatingSystemForLocalExecution();
        overrideScreenScalingFactorForWindows();
        overrideScreenMaximizationForRemoteExecution();
        overridePropertiesForMaximumPerformanceMode();
        setMobilePlatform();
        overrideScreenShotTypeForAnimatedGIF();
        overrideScreenshotTypeForSafariBrowser();
        overrideScreenshotTypeForParallelExecution();
        setExtraAllureProperties();
    }

    private static void overrideScreenshotTypeForParallelExecution() {
        if (!Properties.testNG.parallel().equals("NONE"))
            SHAFT.Properties.visuals.set().screenshotParamsScreenshotType(String.valueOf(Screenshots.VIEWPORT));
    }

    private static void overrideScreenScalingFactorForWindows() {
        if (Properties.platform.targetPlatform().equalsIgnoreCase(org.openqa.selenium.Platform.WINDOWS.toString())) {
            int res = java.awt.Toolkit.getDefaultToolkit().getScreenResolution();
            double scale = (double)res/96;
            Properties.visuals.set().screenshotParamsScalingFactor(scale);
        }
    }

    private static void overrideForcedFlagsForMobilePlatforms() {
        if (Arrays.asList(org.openqa.selenium.Platform.ANDROID.toString().toLowerCase(),
                org.openqa.selenium.Platform.IOS.toString().toLowerCase()).contains(Properties.platform.targetPlatform().toLowerCase())) {
            SHAFT.Properties.flags.set().attemptClearBeforeTypingUsingBackspace(false);
            SHAFT.Properties.flags.set().attemptClearBeforeTyping(false);
            SHAFT.Properties.flags.set().clickUsingJavascriptWhenWebDriverClickFails(false);
            SHAFT.Properties.flags.set().enableTrueNativeMode(true);
            SHAFT.Properties.flags.set().forceCheckTextWasTypedCorrectly(false);
            SHAFT.Properties.flags.set().respectBuiltInWaitsInNativeMode(false);
            SHAFT.Properties.flags.set().handleNonSelectDropDown(false);
            SHAFT.Properties.flags.set().validateSwipeToElement(false);
        }
    }

    private static void setExtraAllureProperties() {
        System.setProperty("allure.testng.hide.configuration.failures", "true");
        System.setProperty("allure.testng.hide.disabled.tests", "true");
    }

    private static void overrideScreenShotTypeForMobileAndMacPlatforms() {
        if (Arrays.asList(org.openqa.selenium.Platform.ANDROID.toString().toLowerCase(), org.openqa.selenium.Platform.MAC.toString().toLowerCase(),
                org.openqa.selenium.Platform.IOS.toString().toLowerCase()).contains(Properties.platform.targetPlatform().toLowerCase())) {
            SHAFT.Properties.visuals.set().screenshotParamsScreenshotType(String.valueOf(Screenshots.VIEWPORT));
        }
    }

    private static void overrideScreenMaximizationForRemoteExecution() {
        if (!SHAFT.Properties.platform.executionAddress().equalsIgnoreCase("local")) {
            SHAFT.Properties.flags.set().autoMaximizeBrowserWindow(false);
        }
    }

    private static void overrideScreenShotTypeForAnimatedGIF() {
        if (SHAFT.Properties.visuals.createAnimatedGif()) {
            SHAFT.Properties.visuals.set().screenshotParamsScreenshotType(String.valueOf(Screenshots.VIEWPORT));
        }
    }

    private static void overrideTargetOperatingSystemForLocalExecution() {
        var executionAddress = Properties.platform.executionAddress();
        if (executionAddress.equals("local")) {
            if (SystemUtils.IS_OS_WINDOWS) {
                Properties.platform.set().targetPlatform(org.openqa.selenium.Platform.WINDOWS.toString());
            } else if (SystemUtils.IS_OS_LINUX) {
                Properties.platform.set().targetPlatform(org.openqa.selenium.Platform.LINUX.toString());
            } else if (SystemUtils.IS_OS_MAC) {
                Properties.platform.set().targetPlatform(org.openqa.selenium.Platform.MAC.toString());
            }
        }
    }

    private static void overrideScreenshotTypeForSafariBrowser() {
        if (SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            SHAFT.Properties.visuals.set().screenshotParamsScreenshotType(String.valueOf(Screenshots.VIEWPORT));
        }
    }

    public static void setMobilePlatform() {
        String targetOperatingSystem = Properties.platform.targetPlatform();
        if (Arrays.asList("android", "ios").contains(targetOperatingSystem.toLowerCase())) {
            Properties.mobile.set().platformName(Properties.platform.targetPlatform().toLowerCase());
        }
    }

    private static void initializeDefaultProperties() {
        //  https://www.selenium.dev/blog/2022/using-java11-httpclient/
        System.setProperty("webdriver.http.factory", "jdk-http-client");

        URL propertiesFolder = PropertyFileManager.class.getResource(DEFAULT_PROPERTIES_FOLDER_PATH.replace("src/main", "") + "/");
        var propertiesFolderPath = "";
        if (propertiesFolder != null) {
            propertiesFolderPath = propertiesFolder.getFile();
        } else {
            propertiesFolderPath = DEFAULT_PROPERTIES_FOLDER_PATH;
        }

        boolean isExternalRun = propertiesFolderPath.contains("file:");

        // always override default properties
        if (isExternalRun) {
            FileActions.getInstance(true).copyFolderFromJar(propertiesFolderPath, DEFAULT_PROPERTIES_FOLDER_PATH);
        } else {
            FileActions.getInstance(true).copyFolder(propertiesFolderPath, DEFAULT_PROPERTIES_FOLDER_PATH);
        }

        // override target properties only if they do not exist
        var finalPropertiesFolderPath = propertiesFolderPath;
        Arrays.asList("/cucumber.properties", "/customWebdriverCapabilities.properties", "/log4j2.properties", "/TestNG.properties")
                .forEach(file -> {
                    if (!FileActions.getInstance(true).doesFileExist(TARGET_PROPERTIES_FOLDER_PATH + file)) {
                        if (isExternalRun) {
                            FileActions.getInstance(true).copyFileFromJar(finalPropertiesFolderPath, TARGET_PROPERTIES_FOLDER_PATH, file.replace("/", ""));
                        } else {
                            FileActions.getInstance(true).copyFile(finalPropertiesFolderPath + file, TARGET_PROPERTIES_FOLDER_PATH + file);
                        }
                    }
                });
    }

    private static void attachPropertyFiles() {
        ReportManager.logDiscrete("Reading properties directory: " + TARGET_PROPERTIES_FOLDER_PATH);
        FileUtils.listFiles(new File(TARGET_PROPERTIES_FOLDER_PATH), new String[]{"properties"},
                false).forEach(propertyFile -> ReportManager.logDiscrete("Loading properties file: " + propertyFile));
    }

    private static void overridePropertiesForMaximumPerformanceMode() {
        int maximumPerformanceMode = SHAFT.Properties.flags.maximumPerformanceMode();
        switch (maximumPerformanceMode) {
            case 1, 2 -> {
                SHAFT.Properties.healenium.set().healEnabled(false);
                SHAFT.Properties.flags.set().autoMaximizeBrowserWindow(false);
                SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("ValidationPointsOnly");
                SHAFT.Properties.visuals.set().screenshotParamsHighlightElements(true);
                SHAFT.Properties.visuals.set().screenshotParamsHighlightMethod("AI");
                SHAFT.Properties.visuals.set().screenshotParamsScreenshotType(String.valueOf(Screenshots.VIEWPORT));
                SHAFT.Properties.visuals.set().screenshotParamsWatermark(true);
                SHAFT.Properties.visuals.set().createAnimatedGif(false);
                SHAFT.Properties.visuals.set().videoParamsRecordVideo(false);
                SHAFT.Properties.reporting.set().debugMode(false);
                SHAFT.Properties.reporting.set().captureElementName(false);
                SHAFT.Properties.web.set().headlessExecution(false);
                if (maximumPerformanceMode == 2 && !DriverFactory.DriverType.SAFARI.getValue().equals(SHAFT.Properties.web.targetBrowserName())) {
                    SHAFT.Properties.web.set().headlessExecution(true);
                }
            }
            case 0 -> {
                // do nothing
            }
        }
    }
}