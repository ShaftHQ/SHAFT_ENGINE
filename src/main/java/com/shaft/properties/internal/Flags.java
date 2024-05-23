package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/PlatformFlags.properties", "file:src/main/resources/properties/default/PlatformFlags.properties", "classpath:PlatformFlags.properties",})
public interface Flags extends EngineProperties<Flags> {
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
    int retryMaximumNumberOfAttempts();

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

    @Key("attemptClearBeforeTyping")
    @DefaultValue("true")
    boolean attemptClearBeforeTyping();

    @Key("forceCheckNavigationWasSuccessful")
    @DefaultValue("false")
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

    @Key("autoCloseDriverInstance")
    @DefaultValue("true")
    boolean autoCloseDriverInstance();

    @Key("automaticallyAssertResponseStatusCode")
    @DefaultValue("true")
    boolean automaticallyAssertResponseStatusCode();

    @Key("maximumPerformanceMode")
    @DefaultValue("0")
    int maximumPerformanceMode();

    @Key("skipTestsWithLinkedIssues")
    @DefaultValue("false")
    boolean skipTestsWithLinkedIssues();

    @Key("attemptToClickBeforeTyping")
    @DefaultValue("false")
    boolean attemptToClickBeforeTyping();

    @Key("disableCache")
    @DefaultValue("false")
    boolean disableCache();

    @Key("enableTrueNativeMode")
    @DefaultValue("false")
    boolean enableTrueNativeMode();

    @Key("handleNonSelectDropDown")
    @DefaultValue("true")
    boolean handleNonSelectDropDown();

    @Key("validateSwipeToElement")
    @DefaultValue("false")
    boolean validateSwipeToElement();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty retryMaximumNumberOfAttempts(int value) {
            setProperty("retryMaximumNumberOfAttempts", String.valueOf(value));
            return this;
        }

        public SetProperty autoMaximizeBrowserWindow(boolean value) {
            setProperty("autoMaximizeBrowserWindow", String.valueOf(value));
            return this;
        }

        public SetProperty forceCheckForElementVisibility(boolean value) {
            setProperty("forceCheckForElementVisibility", String.valueOf(value));
            return this;
        }

        public SetProperty forceCheckElementLocatorIsUnique(boolean value) {
            setProperty("forceCheckElementLocatorIsUnique", String.valueOf(value));
            return this;
        }

        public SetProperty forceCheckTextWasTypedCorrectly(boolean value) {
            setProperty("forceCheckTextWasTypedCorrectly", String.valueOf(value));
            return this;
        }

        public SetProperty attemptClearBeforeTyping(boolean value) {
            setProperty("attemptClearBeforeTyping", String.valueOf(value));
            return this;
        }

        public SetProperty attemptClearBeforeTypingUsingBackspace(boolean value) {
            setProperty("attemptClearBeforeTypingUsingBackspace", String.valueOf(value));
            return this;
        }

        public SetProperty forceCheckNavigationWasSuccessful(boolean value) {
            setProperty("forceCheckNavigationWasSuccessful", String.valueOf(value));
            return this;
        }

        public SetProperty forceCheckStatusOfRemoteServer(boolean value) {
            setProperty("forceCheckStatusOfRemoteServer", String.valueOf(value));
            return this;
        }

        public SetProperty respectBuiltInWaitsInNativeMode(boolean value) {
            setProperty("respectBuiltInWaitsInNativeMode", String.valueOf(value));
            return this;
        }

        public SetProperty clickUsingJavascriptWhenWebDriverClickFails(boolean value) {
            setProperty("clickUsingJavascriptWhenWebDriverClickFails", String.valueOf(value));
            return this;
        }

        public SetProperty attemptToClickBeforeTyping(boolean value) {
            setProperty("attemptToClickBeforeTyping", String.valueOf(value));
            return this;
        }

        public SetProperty autoCloseDriverInstance(boolean value) {
            setProperty("autoCloseDriverInstance", String.valueOf(value));
            return this;
        }

        public SetProperty automaticallyAssertResponseStatusCode(boolean value) {
            setProperty("automaticallyAssertResponseStatusCode", String.valueOf(value));
            return this;
        }

        public SetProperty maximumPerformanceMode(int value) {
            setProperty("maximumPerformanceMode", String.valueOf(value));
            return this;
        }

        public SetProperty skipTestsWithLinkedIssues(boolean value) {
            setProperty("skipTestsWithLinkedIssues", String.valueOf(value));
            return this;
        }

        public SetProperty disableCache(boolean value) {
            setProperty("disableCache", String.valueOf(value));
            return this;
        }

        public SetProperty enableTrueNativeMode(boolean value) {
            setProperty("enableTrueNativeMode", String.valueOf(value));
            return this;
        }

        public SetProperty handleNonSelectDropDown(boolean value) {
            setProperty("handleNonSelectDropDown", String.valueOf(value));
            return this;
        }

        public SetProperty validateSwipeToElement(boolean value) {
            setProperty("validateSwipeToElement", String.valueOf(value));
            return this;
        }

    }

}
