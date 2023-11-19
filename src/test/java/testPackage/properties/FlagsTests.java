package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class FlagsTests {

    int retryMaximumNumberOfAttempts;
    boolean autoMaximizeBrowserWindow;
    boolean forceCheckForElementVisibility;
    boolean forceCheckElementLocatorIsUnique;
    boolean forceCheckTextWasTypedCorrectly;
    boolean attemptClearBeforeTypingUsingBackspace;
    boolean attemptClearBeforeTyping ;
    boolean forceCheckNavigationWasSuccessful;
    boolean respectBuiltInWaitsInNativeMode;
    boolean forceCheckStatusOfRemoteServer;
    boolean clickUsingJavascriptWhenWebDriverClickFails;
    boolean automaticallyAssertResponseStatusCode;
    int maximumPerformanceMode;
    boolean skipTestsWithLinkedIssues;
    boolean attemptToClickBeforeTyping ;
    boolean disableCache ;
    boolean enableTrueNativeMode ;


    @BeforeClass
    public void beforeClass() {
        retryMaximumNumberOfAttempts = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        autoMaximizeBrowserWindow = SHAFT.Properties.flags.autoMaximizeBrowserWindow();
        forceCheckForElementVisibility = SHAFT.Properties.flags.forceCheckForElementVisibility();
        forceCheckElementLocatorIsUnique = SHAFT.Properties.flags.forceCheckElementLocatorIsUnique();
        forceCheckTextWasTypedCorrectly = SHAFT.Properties.flags.forceCheckTextWasTypedCorrectly();
        attemptClearBeforeTyping = SHAFT.Properties.flags.attemptClearBeforeTyping();
        attemptClearBeforeTypingUsingBackspace = SHAFT.Properties.flags.attemptClearBeforeTypingUsingBackspace();
        forceCheckNavigationWasSuccessful = SHAFT.Properties.flags.forceCheckNavigationWasSuccessful();
        respectBuiltInWaitsInNativeMode = SHAFT.Properties.flags.respectBuiltInWaitsInNativeMode();
        forceCheckStatusOfRemoteServer = SHAFT.Properties.flags.forceCheckStatusOfRemoteServer();
        clickUsingJavascriptWhenWebDriverClickFails = SHAFT.Properties.flags.clickUsingJavascriptWhenWebDriverClickFails();
        automaticallyAssertResponseStatusCode = SHAFT.Properties.flags.automaticallyAssertResponseStatusCode();
        maximumPerformanceMode = SHAFT.Properties.flags.maximumPerformanceMode();
        skipTestsWithLinkedIssues = SHAFT.Properties.flags.skipTestsWithLinkedIssues();
        attemptToClickBeforeTyping = SHAFT.Properties.flags.attemptToClickBeforeTyping();
        disableCache = SHAFT.Properties.flags.disableCache();
        enableTrueNativeMode = SHAFT.Properties.flags.enableTrueNativeMode();

    }

    @Test
    public void test() {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(retryMaximumNumberOfAttempts);
        SHAFT.Properties.flags.set().autoMaximizeBrowserWindow(autoMaximizeBrowserWindow);
        SHAFT.Properties.flags.set().forceCheckForElementVisibility(forceCheckForElementVisibility);
        SHAFT.Properties.flags.set().forceCheckElementLocatorIsUnique(forceCheckElementLocatorIsUnique);
        SHAFT.Properties.flags.set().forceCheckTextWasTypedCorrectly(forceCheckTextWasTypedCorrectly);
        SHAFT.Properties.flags.set().attemptClearBeforeTypingUsingBackspace(attemptClearBeforeTypingUsingBackspace);
        SHAFT.Properties.flags.set().forceCheckNavigationWasSuccessful(forceCheckNavigationWasSuccessful);
        SHAFT.Properties.flags.set().respectBuiltInWaitsInNativeMode(respectBuiltInWaitsInNativeMode);
        SHAFT.Properties.flags.set().forceCheckStatusOfRemoteServer(forceCheckStatusOfRemoteServer);
        SHAFT.Properties.flags.set().clickUsingJavascriptWhenWebDriverClickFails(clickUsingJavascriptWhenWebDriverClickFails);
        SHAFT.Properties.flags.set().automaticallyAssertResponseStatusCode(automaticallyAssertResponseStatusCode);
        SHAFT.Properties.flags.set().maximumPerformanceMode(maximumPerformanceMode);
        SHAFT.Properties.flags.set().skipTestsWithLinkedIssues(skipTestsWithLinkedIssues);
        SHAFT.Properties.flags.set().attemptToClickBeforeTyping(attemptToClickBeforeTyping);
        SHAFT.Properties.flags.set().disableCache(disableCache);
        SHAFT.Properties.flags.set().enableTrueNativeMode(enableTrueNativeMode);
        SHAFT.Properties.flags.set().attemptClearBeforeTyping(attemptClearBeforeTyping);

    }
}
