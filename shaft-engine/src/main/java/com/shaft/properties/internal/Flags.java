package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for feature flags and behavioral toggles in the SHAFT framework.
 * Controls options such as automatic scrolling, click method selection, and W3C compliance flags.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.flags.set().autoMaximizeBrowserWindow(true);
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/PlatformFlags.properties", "file:src/main/resources/properties/default/PlatformFlags.properties", "classpath:PlatformFlags.properties",})
public interface Flags extends EngineProperties<Flags> {
    private static void setThreadProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.refreshThreadFlags();
        EngineProperties.logPropertyUpdate(key, value);
    }

    private static void setProperty(String key, String value) {
        // Engine-wide flags are updated under a dedicated lock and then safely
        // published through the volatile Properties.baseFlags reference.
        synchronized (Properties.class) {
            ThreadLocalPropertiesManager.setGlobalProperty(key, value);
            Properties.baseFlags = ConfigFactory.create(Flags.class, ThreadLocalPropertiesManager.getGlobalOverrides());
            Properties.flagsVersion++;
        }
        EngineProperties.logPropertyUpdate(key, value);
    }

    /**
     * Automatically add recommended Chrome options for better stability.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code automaticallyAddRecommendedChromeOptions}
     */
    @Key("automaticallyAddRecommendedChromeOptions")
    @DefaultValue("false")
    boolean automaticallyAddRecommendedChromeOptions();

    /**
     * Maximum number of retry attempts after the first failed execution. JUnit retries run after
     * {@literal @}AfterEach cleanup and execute {@literal @}BeforeEach again for isolated retry state.
     *
     * <p>Default: {@code 0}. Possible values: example: 0, 1, 2, 3, 4, ...etc.
     *
     * @return the configured value of {@code retryMaximumNumberOfAttempts}
     */
    @Key("retryMaximumNumberOfAttempts")
    @DefaultValue("0")
    int retryMaximumNumberOfAttempts();

    /**
     * When retrying a failed test, enable GIF/video/log evidence for the retry attempt.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code forceCaptureSupportingEvidenceOnRetry}
     */
    @Key("forceCaptureSupportingEvidenceOnRetry")
    @DefaultValue("true")
    boolean forceCaptureSupportingEvidenceOnRetry();

    /**
     * Automatically maximize browser window on launch.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code autoMaximizeBrowserWindow}
     */
    @Key("autoMaximizeBrowserWindow")
    @DefaultValue("true")
    boolean autoMaximizeBrowserWindow();

    /**
     * Legacy configuration property with no current runtime consumer.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code forceCheckForElementVisibility}
     */
    @Key("forceCheckForElementVisibility")
    @DefaultValue("true")
    boolean forceCheckForElementVisibility();

    /**
     * Force check that element locator returns only one element. This is still enforced on every retry
     * within the identification timeout; only once that timeout is exhausted does SHAFT make one last-
     * resort attempt to auto-resolve to a single displayed-and-enabled match before throwing
     * MultipleElementsFoundException (#4321).
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code forceCheckElementLocatorIsUnique}
     */
    @Key("forceCheckElementLocatorIsUnique")
    @DefaultValue("true")
    boolean forceCheckElementLocatorIsUnique();

    /**
     * In WebDriver element actions, after type, typeSecure, or typeAppend, compare the final element
     * value/text with the expected typed text. SHAFT skips this comparison for special key sequences
     * such as Keys.ENTER.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code forceCheckTextWasTypedCorrectly}
     */
    @Key("forceCheckTextWasTypedCorrectly")
    @DefaultValue("false")
    boolean forceCheckTextWasTypedCorrectly();

    /**
     * Mode for scrolling to elements.
     *
     * <p>Default: {@code javascript}. Possible values: javascript, native.
     *
     * @return the configured value of {@code scrollingMode}
     */
    @Key("scrollingMode")
    @DefaultValue("javascript")
    String scrollingMode();

    /**
     * Replaces both deprecated attemptClearBeforeTyping and attemptClearBeforeTypingUsingBackspace
     * flags. native = clear using native Selenium method, backspace = clear by deleting letter by
     * letter, off = no clearing before typing.
     *
     * <p>Default: {@code native}. Possible values: native, backspace, off.
     *
     * @return the configured value of {@code clearBeforeTypingMode}
     */
    @Key("clearBeforeTypingMode")
    @DefaultValue("native")
    String clearBeforeTypingMode();

    /**
     * Force check that navigation to URL was successful.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code forceCheckNavigationWasSuccessful}
     */
    @Key("forceCheckNavigationWasSuccessful")
    @DefaultValue("false")
    boolean forceCheckNavigationWasSuccessful();

    /**
     * Respect built-in waits when using native mode.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code respectBuiltInWaitsInNativeMode}
     */
    @Key("respectBuiltInWaitsInNativeMode")
    @DefaultValue("true")
    boolean respectBuiltInWaitsInNativeMode();

    /**
     * Force check status of remote server before execution.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code forceCheckStatusOfRemoteServer}
     */
    @Key("forceCheckStatusOfRemoteServer")
    @DefaultValue("false")
    boolean forceCheckStatusOfRemoteServer();

    /**
     * Fallback to JavaScript click when native WebDriver click fails, including invalid-state and
     * intercepted-click failures.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code clickUsingJavascriptWhenWebDriverClickFails}
     */
    @Key("clickUsingJavascriptWhenWebDriverClickFails")
    @DefaultValue("false")
    boolean clickUsingJavascriptWhenWebDriverClickFails();

    /**
     * Automatically close driver instance after test execution.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code autoCloseDriverInstance}
     */
    @Key("autoCloseDriverInstance")
    @DefaultValue("true")
    boolean autoCloseDriverInstance();

    /**
     * Reuse one browser per thread within a test class. {@code quit()} resets state (cookies, storage, extra tabs,
     * about:blank) and the real quit happens when the class finishes.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code reuseBrowserSession}
     */
    @Key("reuseBrowserSession")
    @DefaultValue("false")
    boolean reuseBrowserSession();

    /**
     * Whether an API request without an explicit target status code must return 2xx.
     * An explicit {@code setTargetStatusCode(n)} is asserted regardless of this flag (#6326).
     * Accepts {@code true}/{@code false} in any case; read when each request is performed.
     * <p>Property key: {@code automaticallyAssertResponseStatusCode} - default: {@code true}
     *
     * @return {@code true} to fail non-2xx responses that have no explicit target
     */
    @Key("automaticallyAssertResponseStatusCode")
    @DefaultValue("true")
    boolean automaticallyAssertResponseStatusCode();

    /**
     * 0 → Disabled, 1 → Without Headless Execution, 2 → With Headless Execution Enabling
     * maximumPerformanceMode will disable all complementary features to ensure the fastest execution
     * possible with a 400% calculated performance boost.
     *
     * <p>Default: {@code 0}. Possible values: 0, 1, 2.
     *
     * @return the configured value of {@code maximumPerformanceMode}
     */
    @Key("maximumPerformanceMode")
    @DefaultValue("0")
    int maximumPerformanceMode();

    /**
     * It is recommended to leave this feature disabled unless you explicitly want to skip any tests
     * that have the {@literal @}Issue or {@literal @}Issues annotation.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code skipTestsWithLinkedIssues}
     */
    @Key("skipTestsWithLinkedIssues")
    @DefaultValue("false")
    boolean skipTestsWithLinkedIssues();

    /**
     * Click the element before typing when clearBeforeTypingMode is not native.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code attemptToClickBeforeTyping}
     */
    @Key("attemptToClickBeforeTyping")
    @DefaultValue("false")
    boolean attemptToClickBeforeTyping();

    /**
     * When true, hide the native soft keyboard after a successful mobile-native {@code type()}.
     * Default false preserves historical behavior; iOS hideKeyboard can be unreliable.
     */
    @Key("hideKeyboardAfterTyping")
    @DefaultValue("false")
    boolean hideKeyboardAfterTyping();

    /**
     * To disable the cache in a browser session.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code disableCache}
     */
    @Key("disableCache")
    @DefaultValue("false")
    boolean disableCache();

    /**
     * Enable true native mode for mobile testing.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code enableTrueNativeMode}
     */
    @Key("enableTrueNativeMode")
    @DefaultValue("false")
    boolean enableTrueNativeMode();

    /**
     * Handle non-select dropdown elements.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code handleNonSelectDropDown}
     */
    @Key("handleNonSelectDropDown")
    @DefaultValue("true")
    boolean handleNonSelectDropDown();

    /**
     * Validate swipe to element action on mobile.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code validateSwipeToElement}
     */
    @Key("validateSwipeToElement")
    @DefaultValue("false")
    boolean validateSwipeToElement();

    /**
     * Disable SSL certificate validation.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code disableSslCertificateCheck}
     */
    @Key("disableSslCertificateCheck")
    @DefaultValue("false")
    boolean disableSslCertificateCheck();

    /**
     * Enable telemetry data collection for SHAFT usage analytics.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code telemetry.enabled}
     */
    @Key("telemetry.enabled")
    @DefaultValue("true")
    boolean telemetryEnabled();

    /**
     * Starts a fluent, engine-global override of these flags. The new values are visible to every
     * thread. Use {@link #setForCurrentThread()} in tests that run in parallel.
     *
     * @return a new {@link SetProperty} builder
     */
    default SetProperty set() {
        return new SetProperty(false);
    }

    /**
     * Starts a fluent override of these flags for the current thread only. Values set this way win
     * over engine-global values on this thread and are cleared at the next test-class boundary, so
     * parallel tests that flip a flag cannot race each other.
     *
     * @return a new thread-scoped {@link SetProperty} builder
     */
    default SetProperty setForCurrentThread() {
        return new SetProperty(true);
    }

    class SetProperty implements EngineProperties.SetProperty {
        private final boolean currentThreadOnly;

        SetProperty(boolean currentThreadOnly) {
            this.currentThreadOnly = currentThreadOnly;
        }

        private void setProperty(String key, String value) {
            if (currentThreadOnly) {
                setThreadProperty(key, value);
            } else {
                Flags.setProperty(key, value);
            }
        }

        /**
         * Overrides the {@code automaticallyAddRecommendedChromeOptions} property at runtime.
         * Automatically add recommended Chrome options for better stability.
         *
         * @param value the new value of {@code automaticallyAddRecommendedChromeOptions}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty automaticallyAddRecommendedChromeOptions(boolean value) {
            setProperty("automaticallyAddRecommendedChromeOptions", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code retryMaximumNumberOfAttempts} property at runtime. Maximum number of retry
         * attempts after the first failed execution.
         *
         * @param value the new value of {@code retryMaximumNumberOfAttempts}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty retryMaximumNumberOfAttempts(int value) {
            setProperty("retryMaximumNumberOfAttempts", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code forceCaptureSupportingEvidenceOnRetry} property at runtime. When retrying a
         * failed test, enable GIF/video/log evidence for the retry attempt.
         *
         * @param value the new value of {@code forceCaptureSupportingEvidenceOnRetry}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty forceCaptureSupportingEvidenceOnRetry(boolean value) {
            setProperty("forceCaptureSupportingEvidenceOnRetry", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code autoMaximizeBrowserWindow} property at runtime. Automatically maximize
         * browser window on launch.
         *
         * @param value the new value of {@code autoMaximizeBrowserWindow}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty autoMaximizeBrowserWindow(boolean value) {
            setProperty("autoMaximizeBrowserWindow", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code forceCheckForElementVisibility} property at runtime. Legacy configuration
         * property with no current runtime consumer.
         *
         * @param value the new value of {@code forceCheckForElementVisibility}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty forceCheckForElementVisibility(boolean value) {
            setProperty("forceCheckForElementVisibility", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code forceCheckElementLocatorIsUnique} property at runtime. Force check that
         * element locator returns only one element.
         *
         * @param value the new value of {@code forceCheckElementLocatorIsUnique}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty forceCheckElementLocatorIsUnique(boolean value) {
            setProperty("forceCheckElementLocatorIsUnique", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code forceCheckTextWasTypedCorrectly} property at runtime. In WebDriver element
         * actions, after type, typeSecure, or typeAppend, compare the final element value/text with the
         * expected typed text.
         *
         * @param value the new value of {@code forceCheckTextWasTypedCorrectly}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty forceCheckTextWasTypedCorrectly(boolean value) {
            setProperty("forceCheckTextWasTypedCorrectly", String.valueOf(value));
            return this;
        }

        /**
         * @param value <ul>
         *                       <li><b>native</b> = clear using native Selenium method</li>
         *                       <li><b>backspace</b> = clear by deleting letter by letter</li>
         *                        <li><b>off</b> = no clearing</li>
         *                       </ul>
         */
        public SetProperty clearBeforeTypingMode(String value) {
            setProperty("clearBeforeTypingMode", value);
            return this;
        }

        /**
         * Overrides the {@code scrollingMode} property at runtime. Mode for scrolling to elements.
         *
         * @param value the new value of {@code scrollingMode}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty scrollingMode(String value) {
            setProperty("scrollingMode", value);
            return this;
        }

        /**
         * Overrides the {@code forceCheckNavigationWasSuccessful} property at runtime. Force check that
         * navigation to URL was successful.
         *
         * @param value the new value of {@code forceCheckNavigationWasSuccessful}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty forceCheckNavigationWasSuccessful(boolean value) {
            setProperty("forceCheckNavigationWasSuccessful", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code forceCheckStatusOfRemoteServer} property at runtime. Force check status of
         * remote server before execution.
         *
         * @param value the new value of {@code forceCheckStatusOfRemoteServer}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty forceCheckStatusOfRemoteServer(boolean value) {
            setProperty("forceCheckStatusOfRemoteServer", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code respectBuiltInWaitsInNativeMode} property at runtime. Respect built-in
         * waits when using native mode.
         *
         * @param value the new value of {@code respectBuiltInWaitsInNativeMode}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty respectBuiltInWaitsInNativeMode(boolean value) {
            setProperty("respectBuiltInWaitsInNativeMode", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code clickUsingJavascriptWhenWebDriverClickFails} property at runtime. Fallback
         * to JavaScript click when native WebDriver click fails, including invalid-state and intercepted-
         * click failures.
         *
         * @param value the new value of {@code clickUsingJavascriptWhenWebDriverClickFails}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty clickUsingJavascriptWhenWebDriverClickFails(boolean value) {
            setProperty("clickUsingJavascriptWhenWebDriverClickFails", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code attemptToClickBeforeTyping} property at runtime. Click the element before
         * typing when clearBeforeTypingMode is not native.
         *
         * @param value the new value of {@code attemptToClickBeforeTyping}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty attemptToClickBeforeTyping(boolean value) {
            setProperty("attemptToClickBeforeTyping", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code hideKeyboardAfterTyping} property at runtime. When true, hides the native
         * soft keyboard after a successful mobile-native type().
         *
         * @param value the new value of {@code hideKeyboardAfterTyping}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty hideKeyboardAfterTyping(boolean value) {
            setProperty("hideKeyboardAfterTyping", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code autoCloseDriverInstance} property at runtime. Automatically close driver
         * instance after test execution.
         *
         * @param value the new value of {@code autoCloseDriverInstance}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty autoCloseDriverInstance(boolean value) {
            setProperty("autoCloseDriverInstance", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code reuseBrowserSession} property at runtime. Reuse one browser per thread within a
         * test class.
         *
         * @param value the new value of {@code reuseBrowserSession}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty reuseBrowserSession(boolean value) {
            setProperty("reuseBrowserSession", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code automaticallyAssertResponseStatusCode} property at runtime. Assert the
         * implicit 2xx status for requests with no target status code.
         *
         * @param value the new value of {@code automaticallyAssertResponseStatusCode}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty automaticallyAssertResponseStatusCode(boolean value) {
            setProperty("automaticallyAssertResponseStatusCode", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code maximumPerformanceMode} property at runtime. 0 → Disabled, 1 → Without
         * Headless Execution, 2 → With Headless Execution Enabling maximumPerformanceMode will disable all
         * complementary features to ensure the fastest execution possible with a 400% calculated
         * performance boost.
         *
         * @param value the new value of {@code maximumPerformanceMode}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty maximumPerformanceMode(int value) {
            setProperty("maximumPerformanceMode", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code skipTestsWithLinkedIssues} property at runtime. It is recommended to leave
         * this feature disabled unless you explicitly want to skip any tests that have the {@literal
         * @}Issue or {@literal @}Issues annotation.
         *
         * @param value the new value of {@code skipTestsWithLinkedIssues}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty skipTestsWithLinkedIssues(boolean value) {
            setProperty("skipTestsWithLinkedIssues", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code disableCache} property at runtime. To disable the cache in a browser
         * session.
         *
         * @param value the new value of {@code disableCache}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty disableCache(boolean value) {
            setProperty("disableCache", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code enableTrueNativeMode} property at runtime. Enable true native mode for
         * mobile testing.
         *
         * @param value the new value of {@code enableTrueNativeMode}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty enableTrueNativeMode(boolean value) {
            setProperty("enableTrueNativeMode", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code handleNonSelectDropDown} property at runtime. Handle non-select dropdown
         * elements.
         *
         * @param value the new value of {@code handleNonSelectDropDown}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty handleNonSelectDropDown(boolean value) {
            setProperty("handleNonSelectDropDown", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code validateSwipeToElement} property at runtime. Validate swipe to element
         * action on mobile.
         *
         * @param value the new value of {@code validateSwipeToElement}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty validateSwipeToElement(boolean value) {
            setProperty("validateSwipeToElement", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code disableSslCertificateCheck} property at runtime. Disable SSL certificate
         * validation.
         *
         * @param value the new value of {@code disableSslCertificateCheck}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty disableSslCertificateCheck(boolean value) {
            setProperty("disableSslCertificateCheck", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code telemetry.enabled} property at runtime. Enable telemetry data collection
         * for SHAFT usage analytics.
         *
         * @param value the new value of {@code telemetry.enabled}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty telemetryEnabled(boolean value) {
            setProperty("telemetry.enabled", String.valueOf(value));
            return this;
        }

    }

}
