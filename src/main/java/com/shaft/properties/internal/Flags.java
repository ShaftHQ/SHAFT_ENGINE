package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/PlatformFlags.properties", "file:src/main/resources/properties/default/PlatformFlags.properties", "classpath:PlatformFlags.properties",})
public interface Flags extends EngineProperties {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.flags = ConfigFactory.create(Flags.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }


    @Key("retryMaximumNumberOfAttempts")
    @DefaultValue("0")
    String retryMaximumNumberOfAttempts();

    @Key("autoMaximizeBrowserWindow")
    @DefaultValue("true")
    boolean autoMaximizeBrowserWindow();

    @Key("forceCheckForElementVisibility")
    @DefaultValue("true")
    boolean forceCheckForElementVisibility();

    @Key("forceCheckElementLocatorIsUnique")
    @DefaultValue("true")
    boolean forceCheckElementLocatorIsUnique();

    @Key("forceCheckTextWasTypedCorrectly")
    @DefaultValue("false")
    boolean forceCheckTextWasTypedCorrectly();

    @Key("attemptClearBeforeTypingUsingBackspace")
    @DefaultValue("false")
    boolean attemptClearBeforeTypingUsingBackspace();

    @Key("forceCheckNavigationWasSuccessful")
    @DefaultValue("true")
    boolean forceCheckNavigationWasSuccessful();

    @Key("respectBuiltInWaitsInNativeMode")
    @DefaultValue("true")
    boolean respectBuiltInWaitsInNativeMode();

    @Key("forceCheckStatusOfRemoteServer")
    @DefaultValue("false")
    boolean forceCheckStatusOfRemoteServer();

    @Key("clickUsingJavascriptWhenWebDriverClickFails")
    @DefaultValue("false")
    boolean clickUsingJavascriptWhenWebDriverClickFails();

    @Key("automaticallyAssertResponseStatusCode")
    @DefaultValue("true")
    boolean automaticallyAssertResponseStatusCode();

    @Key("maximumPerformanceMode")
    @DefaultValue("0")
    String maximumPerformanceMode();

    @Key("skipTestsWithLinkedIssues")
    @DefaultValue("false")
    boolean skipTestsWithLinkedIssues();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public void retryMaximumNumberOfAttempts(String value) {
            setProperty("retryMaximumNumberOfAttempts", value);
        }

        public void autoMaximizeBrowserWindow(boolean value) {
            setProperty("autoMaximizeBrowserWindow", String.valueOf(value));
        }

        public void forceCheckForElementVisibility(boolean value) {
            setProperty("forceCheckForElementVisibility", String.valueOf(value));
        }

        public void forceCheckElementLocatorIsUnique(boolean value) {
            setProperty("forceCheckElementLocatorIsUnique", String.valueOf(value));
        }

        public void forceCheckTextWasTypedCorrectly(boolean value) {
            setProperty("forceCheckTextWasTypedCorrectly", String.valueOf(value));
        }

        public void attemptClearBeforeTypingUsingBackspace(boolean value) {
            setProperty("attemptClearBeforeTypingUsingBackspace", String.valueOf(value));
        }

        public void forceCheckNavigationWasSuccessful(boolean value) {
            setProperty("forceCheckNavigationWasSuccessful", String.valueOf(value));
        }

        public void forceCheckStatusOfRemoteServer(boolean value) {
            setProperty("forceCheckStatusOfRemoteServer", String.valueOf(value));
        }

        public void respectBuiltInWaitsInNativeMode(boolean value) {
            setProperty("respectBuiltInWaitsInNativeMode", String.valueOf(value));
        }

        public void clickUsingJavascriptWhenWebDriverClickFails(boolean value) {
            setProperty("clickUsingJavascriptWhenWebDriverClickFails", String.valueOf(value));
        }

        public void automaticallyAssertResponseStatusCode(boolean value) {
            setProperty("automaticallyAssertResponseStatusCode", String.valueOf(value));
        }

        public void maximumPerformanceMode(String value) {
            setProperty("maximumPerformanceMode", value);
        }

        public void skipTestsWithLinkedIssues(boolean value) {
            setProperty("skipTestsWithLinkedIssues", String.valueOf(value));
        }
    }

}
