package io.github.shafthq.shaft.properties;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.ConfigFactory;

public interface LambdaTest extends EngineProperties {
    //Based on LambdaTest capability builder
    // For Web and Mobile Native https://www.lambdatest.com/capabilities-generator/

    //In case of Mobile Native testing:
    //You must set the "targetOperatingSystem" property under "ExecutionPlatform.properties" or programmatically
    //You must set the "mobile_automationName" property under "MobileCapabilities.properties" or programmatically

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
    @Key("LambdaTest.OSVersion")
    @DefaultValue("")
    String osVersion();

    @Key("LambdaTest.visual")
    @DefaultValue("")
    boolean visual();

    @Key("LambdaTest.video")
    @DefaultValue("")
    boolean video();

    @Key("LambdaTest.resolution")
    @DefaultValue("")
    String resolution();

    @Key("LambdaTest.headless")
    @DefaultValue("")
    boolean headless();

    @Key("LambdaTest.timezone")
    @DefaultValue("")
    String timezone();

    @Key("LambdaTest.project")
    @DefaultValue("")
    String project();

    @Key("LambdaTest.tunnel")
    @DefaultValue("")
    boolean tunnel();

    @Key("LambdaTest.selenium_version")
    @DefaultValue("")
    String selenium_version();

    @Key("LambdaTest.driver_version")
    @DefaultValue("")
    String driver_version();

    @Key("LambdaTest.w3c")
    @DefaultValue("")
    boolean w3c();

    //optional, uses random by default
    @Key("LambdaTest.browserVersion")
    @DefaultValue("")
    String browserVersion();

    //Optional extra settings
    @Key("LambdaTest.geoLocation")
    @DefaultValue("")
    String geoLocation();

    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.LambdaTest = ConfigFactory.create(LambdaTest.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }


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
            setProperty("LambdaTest.OSVersion", value);
        }

        public void deviceName(String value) {
            setProperty("LambdaTest.deviceName", value);
        }

        public void visual(boolean value) {
            setProperty("LambdaTest.visual", String.valueOf(value));
        }

        public void video(boolean value) {
            setProperty("browserStack.networkLogs", String.valueOf(value));
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

        public void tunnel(boolean value) {
            setProperty("LambdaTest.tunnel", String.valueOf(value));
        }

        public void selenium_version(String value) {
            setProperty("LambdaTest.selenium_version", value);
        }

        public void driver_version(String value) {
            setProperty("LambdaTest.driver_version", value);
        }

        public void w3c(String value) {
            setProperty("LambdaTest.w3c", value);
        }

        public void geoLocation(String value) {
            setProperty("browserStack.geoLocation", value);
        }

    }
}
