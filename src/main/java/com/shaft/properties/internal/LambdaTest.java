package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings({"unused"})
@Config.Sources({"system:properties", "file:src/main/resources/properties/LambdaTest.properties", "file:src/main/resources/properties/default/LambdaTest.properties", "classpath:LambdaTest.properties",})
public interface LambdaTest extends EngineProperties<LambdaTest> {
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
    @DefaultValue("SHAFT_Engine")
    String project();

    @Key("LambdaTest.build")
    @DefaultValue("Build Name")
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
        public SetProperty username(String value) {
            setProperty("LambdaTest.username", value);
            return this;
        }

        public SetProperty accessKey(String value) {
            setProperty("LambdaTest.accessKey", value);
            return this;
        }

        public SetProperty platformVersion(String value) {
            setProperty("LambdaTest.platformVersion", value);
            return this;
        }

        public SetProperty osVersion(String value) {
            setProperty("LambdaTest.osVersion", value);
            return this;
        }

        public SetProperty appUrl(String value) {
            setProperty("LambdaTest.appUrl", value);
            return this;
        }

        public SetProperty appProfiling(Boolean value) {
            setProperty("LambdaTest.appProfiling", String.valueOf(value));
            return this;
        }

        public SetProperty deviceName(String value) {
            setProperty("LambdaTest.deviceName", value);
            return this;
        }

        public SetProperty visual(boolean value) {
            setProperty("LambdaTest.visual", String.valueOf(value));
            return this;
        }

        public SetProperty video(boolean value) {
            setProperty("browserStack.video", String.valueOf(value));
            return this;
        }

        public SetProperty resolution(String value) {
            setProperty("LambdaTest.platformVersion", value);
            return this;
        }

        public SetProperty headless(boolean value) {
            setProperty("LambdaTest.headless", String.valueOf(value));
            return this;
        }

        public SetProperty timezone(String value) {
            setProperty("LambdaTest.timezone", value);
            return this;
        }

        public SetProperty project(String value) {
            setProperty("LambdaTest.project", value);
            return this;
        }

        public SetProperty build(String value) {
            setProperty("LambdaTest.build", value);
            return this;
        }


        public SetProperty tunnel(boolean value) {
            setProperty("LambdaTest.tunnel", String.valueOf(value));
            return this;
        }

        public SetProperty tunnelName(String value) {
            setProperty("LambdaTest.tunnelName", value);
            return this;
        }

        public SetProperty buildName(String value) {
            setProperty("LambdaTest.buildName", value);
            return this;
        }

        public SetProperty autoGrantPermissions(boolean value) {
            setProperty("LambdaTest.autoGrantPermissions", String.valueOf(value));
            return this;
        }

        public SetProperty autoAcceptAlerts(boolean value) {
            setProperty("LambdaTest.autoAcceptAlerts", String.valueOf(value));
            return this;
        }

        public SetProperty acceptInsecureCerts(boolean value) {
            setProperty("LambdaTest.acceptInsecureCerts", String.valueOf(value));
            return this;
        }

        public SetProperty isRealMobile(boolean value) {
            setProperty("LambdaTest.isRealMobile", String.valueOf(value));
            return this;
        }

        public SetProperty debug(boolean value) {
            setProperty("LambdaTest.debug", String.valueOf(value));
            return this;
        }

        public SetProperty console(boolean value) {
            setProperty("LambdaTest.console", String.valueOf(value));
            return this;
        }

        public SetProperty selenium_version(String value) {
            setProperty("LambdaTest.selenium_version", value);
            return this;
        }

        public SetProperty browserVersion(String value) {
            setProperty("LambdaTest.browserVersion", value);
            return this;
        }

        public SetProperty appiumVersion(String value) {
            setProperty("LambdaTest.appiumVersion", value);
            return this;
        }

        public SetProperty networkLogs(boolean value) {
            setProperty("LambdaTest.networkLogs", String.valueOf(value));
            return this;
        }

        public SetProperty appRelativeFilePath(String value) {
            setProperty("LambdaTest.appRelativeFilePath", value);
            return this;
        }

        public SetProperty appName(String value) {
            setProperty("LambdaTest.appName", value);
            return this;
        }

        public SetProperty driver_version(String value) {
            setProperty("LambdaTest.driver_version", value);
            return this;
        }

        public SetProperty w3c(boolean value) {
            setProperty("LambdaTest.w3c", String.valueOf(value));
            return this;
        }

        public SetProperty geoLocation(String value) {
            setProperty("LambdaTest.geoLocation", value);
            return this;
        }

        public SetProperty customID(String value) {
            setProperty("LambdaTest.customID", value);
            return this;
        }

    }
}
