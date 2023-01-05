package io.github.shafthq.shaft.properties;

import org.aeonbits.owner.Config;
import org.aeonbits.owner.ConfigFactory;

@Config.Sources({"system:properties",
        "file:src/main/resources/properties/browserStack.properties",
        "file:src/main/resources/properties/default/browserStack.properties",
        "classpath:browserStack.properties",
})
public interface BrowserStack extends EngineProperties {
    //Based on BrowserStack capability builder
    // For Mobile Native https://www.browserstack.com/app-automate/capabilities?tag=w3c
    // For Web https://www.browserstack.com/automate/capabilities?tag=selenium-4

    //In case of Mobile Native testing:
    //You must set the "targetOperatingSystem" property under "ExecutionPlatform.properties" or programmatically
    //You must set the "mobile_automationName" property under "MobileCapabilities.properties" or programmatically

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
    @DefaultValue("4.1.2")
    String seleniumVersion();

    @Key("browserStack.appiumVersion")
    @DefaultValue("1.22.0")
    String appiumVersion();

    @Key("browserStack.acceptInsecureCerts")
    @DefaultValue("true")
    boolean acceptInsecureCerts();

    @Key("browserStack.debug")
    @DefaultValue("true")
    boolean debug();

    @Key("browserStack.networkLogs")
    @DefaultValue("true")
    boolean networkLogs();

    //Optional extra settings
    //Enterprise accounts only: https://www.browserstack.com/ip-geolocation
    @Key("browserStack.geoLocation")
    @DefaultValue("")
    String geoLocation();

    @Override
    default void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.browserStack = ConfigFactory.create(BrowserStack.class, updatedProps);
    }
}
