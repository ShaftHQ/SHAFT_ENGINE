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
    boolean disableSSLCertificateCheck;
    
    // New retry evidence collection properties
    boolean retryEnableVideoRecording;
    boolean retryEnableNetworkLogging;
    boolean retryEnableConsoleLogging;
    boolean retryEnableEnhancedStabilization;


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
        disableSSLCertificateCheck = SHAFT.Properties.flags.disableSslCertificateCheck();
        
        // Initialize new retry evidence collection properties
        retryEnableVideoRecording = SHAFT.Properties.flags.retryEnableVideoRecording();
        retryEnableNetworkLogging = SHAFT.Properties.flags.retryEnableNetworkLogging();
        retryEnableConsoleLogging = SHAFT.Properties.flags.retryEnableConsoleLogging();
        retryEnableEnhancedStabilization = SHAFT.Properties.flags.retryEnableEnhancedStabilization();

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
        SHAFT.Properties.flags.set().disableSslCertificateCheck(disableSSLCertificateCheck);
        
        // Test new retry evidence collection properties
        SHAFT.Properties.flags.set().retryEnableVideoRecording(retryEnableVideoRecording);
        SHAFT.Properties.flags.set().retryEnableNetworkLogging(retryEnableNetworkLogging);
        SHAFT.Properties.flags.set().retryEnableConsoleLogging(retryEnableConsoleLogging);
        SHAFT.Properties.flags.set().retryEnableEnhancedStabilization(retryEnableEnhancedStabilization);

    }

    @Test
    public void testRetryEvidenceCollectionProperties() {
        // Test that retry evidence collection properties have correct default values
        assert SHAFT.Properties.flags.retryEnableVideoRecording() == true : "Retry video recording should be enabled by default";
        assert SHAFT.Properties.flags.retryEnableNetworkLogging() == true : "Retry network logging should be enabled by default";
        assert SHAFT.Properties.flags.retryEnableConsoleLogging() == true : "Retry console logging should be enabled by default";
        assert SHAFT.Properties.flags.retryEnableEnhancedStabilization() == true : "Retry enhanced stabilization should be enabled by default";
        
        // Test property setters
        SHAFT.Properties.flags.set().retryEnableVideoRecording(false);
        assert SHAFT.Properties.flags.retryEnableVideoRecording() == false : "Retry video recording setting should be changeable";
        
        SHAFT.Properties.flags.set().retryEnableNetworkLogging(false);
        assert SHAFT.Properties.flags.retryEnableNetworkLogging() == false : "Retry network logging setting should be changeable";
        
        SHAFT.Properties.flags.set().retryEnableConsoleLogging(false);
        assert SHAFT.Properties.flags.retryEnableConsoleLogging() == false : "Retry console logging setting should be changeable";
        
        SHAFT.Properties.flags.set().retryEnableEnhancedStabilization(false);
        assert SHAFT.Properties.flags.retryEnableEnhancedStabilization() == false : "Retry enhanced stabilization setting should be changeable";
        
        // Reset to original values
        SHAFT.Properties.flags.set().retryEnableVideoRecording(retryEnableVideoRecording);
        SHAFT.Properties.flags.set().retryEnableNetworkLogging(retryEnableNetworkLogging);
        SHAFT.Properties.flags.set().retryEnableConsoleLogging(retryEnableConsoleLogging);
        SHAFT.Properties.flags.set().retryEnableEnhancedStabilization(retryEnableEnhancedStabilization);
    }
}
