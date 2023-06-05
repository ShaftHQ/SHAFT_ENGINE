package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings({"SpellCheckingInspection", "unused"})
@Config.Sources({"system:properties", "file:src/main/resources/properties/LambdaTest.properties", "file:src/main/resources/properties/default/LambdaTest.properties", "classpath:LambdaTest.properties",})
public interface LambdaTest extends EngineProperties {
    //Based on LambdaTest capability builder
    // For Web and Mobile Native https://www.lambdatest.com/capabilities-generator/

    //In case of Mobile Native testing:
    //You must set the "targetOperatingSystem" property under "ExecutionPlatform.properties" or programmatically
    //You must set the "mobile_automationName" property under "MobileCapabilities.properties" or programmatically

    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.lambdaTest = ConfigFactory.create(LambdaTest.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    //Below properties are all required
    @Key("LambdaTest.username")
    @DefaultValue("")
    String username();

    @Key("LambdaTest.accessKey")
    @DefaultValue("")
    String accessKey();

    //Below properties are needed for native mobile app testing:
    //Required
    @Key("LambdaTest.platformVersion")
    @DefaultValue("")
    String platformVersion();

    @Key("LambdaTest.deviceName")
    @DefaultValue("")
    String deviceName();

    //Use appUrl to test a previously uploaded app file
    @Key("LambdaTest.appUrl")
    @DefaultValue("")
    String appUrl();

    @Key("LambdaTest.appProfiling")
    @DefaultValue("false")
    boolean appProfiling();

    @Key("LambdaTest.osVersion")
    @DefaultValue("")
    String osVersion();

    @Key("LambdaTest.visual")
    @DefaultValue("false")
    boolean visual();

    @Key("LambdaTest.video")
    @DefaultValue("false")
    boolean video();

    //Use appName and appRelativeFilePath to upload a new app file and test it
    @Key("LambdaTest.appName")
    @DefaultValue("")
    String appName();

    @Key("LambdaTest.appRelativeFilePath")
    @DefaultValue("")
    String appRelativeFilePath();

    @Key("LambdaTest.resolution")
    @DefaultValue("")
    String resolution();

    @Key("LambdaTest.headless")
    @DefaultValue("false")
    boolean headless();

    @Key("LambdaTest.timezone")
    @DefaultValue("")
    String timezone();

    @Key("LambdaTest.project")
    @DefaultValue("")
    String project();

    @Key("LambdaTest.build")
    @DefaultValue("")
    String build();

    @Key("LambdaTest.tunnel")
    @DefaultValue("false")
    boolean tunnel();

    @Key("LambdaTest.tunnelName")
    @DefaultValue("false")
    String tunnelName();

    @Key("LambdaTest.buildName")
    @DefaultValue("")
    String buildName();

    @Key("LambdaTest.selenium_version")
    @DefaultValue("")
    String selenium_version();

    @Key("LambdaTest.driver_version")
    @DefaultValue("")
    String driver_version();

    @Key("LambdaTest.w3c")
    @DefaultValue("true")
    boolean w3c();

    //optional, uses random by default
    @Key("LambdaTest.browserVersion")
    @DefaultValue("")
    String browserVersion();

    //Optional extra settings
    @Key("LambdaTest.geoLocation")
    @DefaultValue("EG")
    String geoLocation();

    @Key("LambdaTest.debug")
    @DefaultValue("false")
    boolean debug();

    @Key("LambdaTest.acceptInsecureCerts")
    @DefaultValue("true")
    boolean acceptInsecureCerts();

    @Key("LambdaTest.networkLogs")
    @DefaultValue("false")
    boolean networkLogs();

    @Key("LambdaTest.appiumVersion")
    @DefaultValue("2.0.0")
    String appiumVersion();

    @Key("LambdaTest.autoGrantPermissions")
    @DefaultValue("true")
    boolean autoGrantPermissions();

    @Key("LambdaTest.autoAcceptAlerts")
    @DefaultValue("true")
    boolean autoAcceptAlerts();

    @Key("LambdaTest.isRealMobile")
    @DefaultValue("true")
    boolean isRealMobile();

    @Key("LambdaTest.console")
    @DefaultValue("false")
    boolean console();

    @Key("LambdaTest.customID")
    @DefaultValue("")
    String customID();

    default LambdaTest.SetProperty set() {
        return new LambdaTest.SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public void username(String value) {
            setProperty("LambdaTest.username", value);
        }

        public void accessKey(String value) {
            setProperty("LambdaTest.accessKey", value);
        }

        public void platformVersion(String value) {
            setProperty("LambdaTest.platformVersion", value);
        }

        public void osVersion(String value) {
            setProperty("LambdaTest.osVersion", value);
        }

        public void appUrl(String value) {
            setProperty("LambdaTest.appUrl", value);
        }

        public void appProfiling(Boolean value) {
            setProperty("LambdaTest.appProfiling", String.valueOf(value));
        }

        public void deviceName(String value) {
            setProperty("LambdaTest.deviceName", value);
        }

        public void visual(boolean value) {
            setProperty("LambdaTest.visual", String.valueOf(value));
        }

        public void video(boolean value) {
            setProperty("browserStack.video", String.valueOf(value));
        }

        public void resolution(String value) {
            setProperty("LambdaTest.platformVersion", value);
        }

        public void headless(boolean value) {
            setProperty("LambdaTest.headless", String.valueOf(value));
        }

        public void timezone(String value) {
            setProperty("LambdaTest.timezone", value);
        }

        public void project(String value) {
            setProperty("LambdaTest.project", value);
        }

        public void build(String value) {
            setProperty("LambdaTest.build", value);
        }


        public void tunnel(boolean value) {
            setProperty("LambdaTest.tunnel", String.valueOf(value));
        }

        public void tunnelName(String value) {
            setProperty("LambdaTest.tunnelName", value);
        }

        public void buildName(String value) {
            setProperty("LambdaTest.buildName", value);
        }

        public void autoGrantPermissions(boolean value) {
            setProperty("LambdaTest.autoGrantPermissions", String.valueOf(value));
        }

        public void autoAcceptAlerts(boolean value) {
            setProperty("LambdaTest.autoAcceptAlerts", String.valueOf(value));
        }

        public void acceptInsecureCerts(boolean value) {
            setProperty("LambdaTest.acceptInsecureCerts", String.valueOf(value));
        }

        public void isRealMobile(boolean value) {
            setProperty("LambdaTest.isRealMobile", String.valueOf(value));
        }

        public void debug(boolean value) {
            setProperty("LambdaTest.debug", String.valueOf(value));
        }

        public void console(boolean value) {
            setProperty("LambdaTest.console", String.valueOf(value));
        }

        public void selenium_version(String value) {
            setProperty("LambdaTest.selenium_version", value);
        }

        public void browserVersion(String value) {
            setProperty("LambdaTest.browserVersion", value);
        }

        public void appiumVersion(String value) {
            setProperty("LambdaTest.appiumVersion", value);
        }

        public void networkLogs(boolean value) {
            setProperty("LambdaTest.networkLogs", String.valueOf(value));
        }

        public void appRelativeFilePath(String value) {
            setProperty("LambdaTest.appRelativeFilePath", value);
        }

        public void appName(String value) {
            setProperty("LambdaTest.appName", value);
        }

        public void driver_version(String value) {
            setProperty("LambdaTest.driver_version", value);
        }

        public void w3c(boolean value) {
            setProperty("LambdaTest.w3c", String.valueOf(value));
        }

        public void geoLocation(String value) {
            setProperty("LambdaTest.geoLocation", value);
        }

        public void customID(String value) {
            setProperty("LambdaTest.customID", value);
        }

    }
}
