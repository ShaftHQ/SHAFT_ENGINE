package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings({"SpellCheckingInspection", "unused"})
@Sources({"system:properties",
        "file:src/main/resources/properties/browserStack.properties",
        "file:src/main/resources/properties/default/browserStack.properties",
        "classpath:browserStack.properties",
})
public interface BrowserStack extends EngineProperties<BrowserStack> {
    //Based on BrowserStack capability builder
    // For Mobile Native https://www.browserstack.com/app-automate/capabilities?tag=w3c
    // For Web https://www.browserstack.com/automate/capabilities?tag=selenium-4

    //In case of Mobile Native testing:
    //You must set the "targetOperatingSystem" property under "ExecutionPlatform.properties" or programmatically
    //You must set the "mobile_automationName" property under "MobileCapabilities.properties" or programmatically

    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.browserStack = ConfigFactory.create(BrowserStack.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    //Below properties are all required
    @Key("browserStack.username")
    @DefaultValue("mohabmohie1")
    String username();

    @Key("browserStack.accessKey")
    @DefaultValue("7E7PgzBtwk4sWLUcF8Y5")
    String accessKey();

    //Below properties are needed for native mobile app testing:
    //Required
    @Key("browserStack.platformVersion")
    @DefaultValue("")
    String platformVersion();

    @Key("browserStack.deviceName")
    @DefaultValue("")
    String deviceName();

    //Use appUrl to test a previously uploaded app file
    @Key("browserStack.appUrl")
    @DefaultValue("")
    String appUrl();

    //Use customID to test the latest uploaded version as the above url expires regularly
    @Key("browserStack.customID")
    @DefaultValue("")
    String customID();

    //Use appName and appRelativeFilePath to upload a new app file and test it
    @Key("browserStack.appName")
    @DefaultValue("")
    String appName();

    @Key("browserStack.appRelativeFilePath")
    @DefaultValue("")
    String appRelativeFilePath();

    //In case of Desktop web testing:
    //You must set the "targetOperatingSystem" property under "ExecutionPlatform.properties" or programmatically
    //You must set the "targetBrowserName" property under "ExecutionPlatform.properties" or programmatically
    //Required
    @Key("browserStack.osVersion")
    @DefaultValue("")
    String osVersion();

    //optional, uses random by default
    @Key("browserStack.browserVersion")
    @DefaultValue("")
    String browserVersion();

    //Do not change these unless you know what you're doing
    @Key("browserStack.local")
    @DefaultValue("false")
    boolean local();

    @Key("browserStack.seleniumVersion")
    @DefaultValue("4.18.1")
    String seleniumVersion();

    @Key("browserStack.acceptInsecureCerts")
    @DefaultValue("true")
    boolean acceptInsecureCerts();

    @Key("browserStack.debug")
    @DefaultValue("false")
    boolean debug();

    @Key("browserStack.enableBiometric")
    @DefaultValue("false")
    boolean enableBiometric();

    @Key("browserStack.networkLogs")
    @DefaultValue("false")
    boolean networkLogs();

    //Optional extra settings
    //Enterprise accounts only: https://www.browserstack.com/ip-geolocation
    @Key("browserStack.geoLocation")
    @DefaultValue("")
    String geoLocation();

    @Key("browserStack.appiumVersion")
    @DefaultValue("2.4.1")
    String appiumVersion();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty username(String value) {
            setProperty("browserStack.username", value);
            return this;
        }

        public SetProperty accessKey(String value) {
            setProperty("browserStack.accessKey", value);
            return this;
        }

        public SetProperty platformVersion(String value) {
            setProperty("browserStack.platformVersion", value);
            return this;
        }

        public SetProperty deviceName(String value) {
            setProperty("browserStack.deviceName", value);
            return this;
        }

        public SetProperty appUrl(String value) {
            setProperty("browserStack.appUrl", value);
            return this;
        }

        public SetProperty customID(String value) {
            setProperty("browserStack.customID", value);
            return this;
        }

        public SetProperty appName(String value) {
            setProperty("browserStack.appName", value);
            return this;
        }

        public SetProperty appRelativeFilePath(String value) {
            setProperty("browserStack.appRelativeFilePath", value);
            return this;
        }

        public SetProperty osVersion(String value) {
            setProperty("browserStack.osVersion", value);
            return this;
        }

        public SetProperty browserVersion(String value) {
            setProperty("browserStack.browserVersion", value);
            return this;
        }

        public SetProperty local(boolean value) {
            setProperty("browserStack.local", String.valueOf(value));
            return this;
        }

        public SetProperty seleniumVersion(String value) {
            setProperty("browserStack.seleniumVersion", value);
            return this;
        }

        public SetProperty appiumVersion(String value) {
            setProperty("browserStack.appiumVersion", value);
            return this;
        }

        public SetProperty acceptInsecureCerts(boolean value) {
            setProperty("browserStack.acceptInsecureCerts", String.valueOf(value));
            return this;
        }

        public SetProperty debug(boolean value) {
            setProperty("browserStack.debug", String.valueOf(value));
            return this;
        }

        public SetProperty enableBiometric(boolean value) {
            setProperty("browserStack.enableBiometric", String.valueOf(value));
            return this;
        }

        public SetProperty networkLogs(boolean value) {
            setProperty("browserStack.networkLogs", String.valueOf(value));
            return this;
        }

        public SetProperty geoLocation(String value) {
            setProperty("browserStack.geoLocation", value);
            return this;
        }

    }
}
