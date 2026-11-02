package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for timeout settings in the SHAFT framework.
 * Controls wait durations for browser navigation, page load, script execution, API calls,
 * shell commands, database queries, and element identification.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(10);
 * SHAFT.Properties.timeouts.set().waitForUiStateTimeout(600);
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/Timeouts.properties", "file:src/main/resources/properties/default/Timeouts.properties", "classpath:Timeouts.properties"})
public interface Timeouts extends EngineProperties<Timeouts> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.timeoutsOverride.set(ConfigFactory.create(Timeouts.class, ThreadLocalPropertiesManager.getOverrides()));
        EngineProperties.logPropertyUpdate(key, value);
    }

    /**
     * Especially useful for modern/responsive web apps using React, Vue, Angular, ...etc.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code waitForLazyLoading}
     */
    @Key("waitForLazyLoading")
    @DefaultValue("true")
    Boolean waitForLazyLoading();

    /**
     * Timeout in seconds for browser navigation.
     *
     * <p>Default: {@code 30}. Unit: seconds.
     *
     * @return the configured value of {@code browserNavigationTimeout}
     */
    @Key("browserNavigationTimeout")
    @DefaultValue("30")
    int browserNavigationTimeout();

    /**
     * Timeout in seconds for page load.
     *
     * <p>Default: {@code 30}. Unit: seconds.
     *
     * @return the configured value of {@code pageLoadTimeout}
     */
    @Key("pageLoadTimeout")
    @DefaultValue("30")
    int pageLoadTimeout();

    /**
     * Timeout in seconds for script execution.
     *
     * <p>Default: {@code 30}. Unit: seconds.
     *
     * @return the configured value of {@code scriptExecutionTimeout}
     */
    @Key("scriptExecutionTimeout")
    @DefaultValue("30")
    int scriptExecutionTimeout();

    /**
     * Timeout in seconds for SHAFT's browser lazy-loading synchronization.
     */
    @Key("waitForLazyLoadingTimeout")
    @DefaultValue("30")
    int waitForLazyLoadingTimeout();

    /**
     * Initial network observation window in milliseconds when no requests were seen.
     */
    @Key("lazyLoadingNetworkIdleInitialObservationMillis")
    @DefaultValue("200")
    int lazyLoadingNetworkIdleInitialObservationMillis();

    /**
     * Required network quiet window in milliseconds after observed activity.
     */
    @Key("lazyLoadingNetworkIdleQuietWindowMillis")
    @DefaultValue("500")
    int lazyLoadingNetworkIdleQuietWindowMillis();

    /**
     * Polling interval in milliseconds for the browser-readiness {@code fluentWait} loop.
     */
    @Key("lazyLoadingPollingIntervalMillis")
    @DefaultValue("200")
    int lazyLoadingPollingIntervalMillis();

    /**
     * Required DOM-mutation quiet window in milliseconds before the browser-readiness
     * wait treats the DOM as stable. {@code 0} (default) disables DOM-stability folding
     * entirely, preserving today's behavior bit-for-bit: the readiness wait then never
     * reads the DOM-mutation marker as part of its pass condition.
     */
    @Key("lazyLoadingDomStabilityQuietWindowMillis")
    @DefaultValue("0")
    int lazyLoadingDomStabilityQuietWindowMillis();

    /**
     * Required DOM-mutation quiet window in milliseconds applied after navigation
     * ({@code waitForLazyLoadingAfterNavigation} / public {@code BrowserActions.waitForLazyLoading}).
     * Independent of {@link #lazyLoadingDomStabilityQuietWindowMillis()}, which stays {@code 0}
     * so cheap per-action waits do not fold DOM stability by default.
     */
    @Key("lazyLoadingDomStabilityOnNavigationQuietWindowMillis")
    @DefaultValue("300")
    int lazyLoadingDomStabilityOnNavigationQuietWindowMillis();

    /**
     * Maximum number of progressive-scroll steps that {@code BrowserActions.scrollToLoadAll()}
     * will perform while sweeping a page for scroll-triggered lazy content, bounding its
     * worst-case cost regardless of how far the page keeps growing.
     */
    @Key("lazyLoadingScrollSweepMaxSteps")
    @DefaultValue("20")
    int lazyLoadingScrollSweepMaxSteps();

    /**
     * Default timeout in seconds for element identification.
     *
     * <p>Default: {@code 10}. Unit: seconds.
     *
     * @return the configured value of {@code defaultElementIdentificationTimeout}
     */
    @Key("defaultElementIdentificationTimeout")
    @DefaultValue("10")
    double defaultElementIdentificationTimeout();

    /**
     * Timeout in seconds for default UI state waits.
     */
    @Key("waitForUiStateTimeout")
    @DefaultValue("600")
    int waitForUiStateTimeout();

    /**
     * Timeout in seconds for API socket reads. Read per request, per thread; 0 means no timeout,
     * maximum 2147483. Invalid values fail with a Fix: line.
     *
     * <p>Default: {@code 30}. Unit: seconds.
     *
     * @return the configured value of {@code apiSocketTimeout}
     */
    @Key("apiSocketTimeout")
    @DefaultValue("30")
    int apiSocketTimeout();

    /**
     * Timeout in seconds to establish API connections. Read per request, per thread; 0 means no
     * timeout, maximum 2147483. Invalid values fail with a Fix: line.
     *
     * <p>Default: {@code 30}. Unit: seconds.
     *
     * @return the configured value of {@code apiConnectionTimeout}
     */
    @Key("apiConnectionTimeout")
    @DefaultValue("30")
    int apiConnectionTimeout();

    /**
     * Timeout in seconds to acquire a pooled API connection. Read per request, per thread; 0 means no
     * timeout, maximum 2147483. Invalid values fail with a Fix: line.
     *
     * <p>Default: {@code 30}. Unit: seconds.
     *
     * @return the configured value of {@code apiConnectionManagerTimeout}
     */
    @Key("apiConnectionManagerTimeout")
    @DefaultValue("30")
    int apiConnectionManagerTimeout();

    /**
     * Timeout in seconds for shell session.
     *
     * <p>Default: {@code 30}. Unit: seconds.
     *
     * @return the configured value of {@code shellSessionTimeout}
     */
    @Key("shellSessionTimeout")
    @DefaultValue("30")
    long shellSessionTimeout();

    /**
     * JSch {@code ServerAliveInterval} in seconds for remote SSH sessions.
     * Values {@code <= 0} disable keep-alive packets.
     */
    @Key("sshServerAliveInterval")
    @DefaultValue("60")
    int sshServerAliveInterval();

    /**
     * @deprecated Docker-wrapped terminal execution is deprecated for removal.
     */
    @Deprecated(since = "10.2.20260614", forRemoval = true)
    @Key("dockerCommandTimeout")
    @DefaultValue("30")
    int dockerCommandTimeout();

    /**
     * Timeout in seconds retained for compatibility with deprecated Docker-wrapped terminal execution.
     *
     * @return the configured Docker command timeout in seconds
     */
    @Key("dockerCommandTimeout")
    @DefaultValue("30")
    int dockerCommandTimeoutSeconds();

    /**
     * Timeout in seconds for database login.
     *
     * <p>Default: {@code 30}. Unit: seconds.
     *
     * @return the configured value of {@code databaseLoginTimeout}
     */
    @Key("databaseLoginTimeout")
    @DefaultValue("30")
    int databaseLoginTimeout();

    /**
     * Timeout in seconds for database network operations.
     *
     * <p>Default: {@code 30}. Unit: seconds.
     *
     * @return the configured value of {@code databaseNetworkTimeout}
     */
    @Key("databaseNetworkTimeout")
    @DefaultValue("30")
    int databaseNetworkTimeout();

    /**
     * Timeout in seconds for database queries.
     *
     * <p>Default: {@code 30}. Unit: seconds.
     *
     * @return the configured value of {@code databaseQueryTimeout}
     */
    @Key("databaseQueryTimeout")
    @DefaultValue("30")
    int databaseQueryTimeout();

    /**
     * Wait for remote server to be up before execution.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code waitForRemoteServerToBeUp}
     */
    @Key("waitForRemoteServerToBeUp")
    @DefaultValue("false")
    Boolean waitForRemoteServerToBeUp();

    /**
     * Timeout in seconds for remote server to be up.
     *
     * <p>Default: {@code 1}. Unit: seconds.
     *
     * @return the configured value of {@code timeoutForRemoteServerToBeUp}
     */
    @Key("timeoutForRemoteServerToBeUp")
    @DefaultValue("1")
    int timeoutForRemoteServerToBeUp();

    /**
     * Timeout in minutes for the overall remote WebDriver session-creation retry budget: the total
     * time {@code DriverFactoryHelper} keeps retrying failed connection attempts before giving up.
     * Does <strong>not</strong> bound a single HTTP connect/read attempt against the grid -- see
     * {@link #remoteServerConnectionAttemptTimeout()} for that. Prior to introducing that property,
     * this single value simultaneously bounded the per-attempt HTTP client timeout, the outer retry
     * budget, and the progress bar, so one hung attempt could silently consume the entire budget
     * without the retry loop ever getting a chance to retry (issue #3811).
     */
    @Key("remoteServerInstanceCreationTimeout")
    @DefaultValue("5")
    int remoteServerInstanceCreationTimeout();

    /**
     * Timeout in seconds for a single HTTP connect/read attempt made by the underlying HTTP client
     * while creating a remote WebDriver session. Decoupled from
     * {@link #remoteServerInstanceCreationTimeout()}, which bounds only the overall retry budget
     * (in minutes) across all attempts: this property bounds one connection attempt, so an
     * unreachable or saturated grid fails an attempt fast and the retry loop actually gets to retry
     * within the overall budget, instead of one attempt burning the whole window (issue #3811).
     * The default stays deliberately generous because a healthy-but-busy grid holds the new-session
     * POST open while the request waits in its session queue; a timed-out attempt that gets retried
     * enqueues a duplicate request. Lower this (e.g. to 10-30) only where fail-fast matters more
     * than queue tolerance, such as CI with adaptive throttling.
     */
    @Key("remoteServerConnectionAttemptTimeout")
    @DefaultValue("120")
    int remoteServerConnectionAttemptTimeout();

    /**
     * Starts a fluent, thread-local override of these properties for the current test thread.
     *
     * @return a new {@link SetProperty} builder
     */
    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        /**
         * Overrides the {@code waitForLazyLoading} property at runtime. Especially useful for
         * modern/responsive web apps using React, Vue, Angular, ...etc.
         *
         * @param value the new value of {@code waitForLazyLoading}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty waitForLazyLoading(boolean value) {
            setProperty("waitForLazyLoading", String.valueOf(value));
            return this;
        }

        /**
         * @deprecated Use {@link #waitForLazyLoadingTimeout(int)}.
         */
        @Deprecated(since = "10.2.20260627")
        public SetProperty lazyLoadingTimeout(int value) {
            setProperty("lazyLoadingTimeout", String.valueOf(value));
            setProperty("waitForLazyLoadingTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code browserNavigationTimeout} property at runtime. Timeout in seconds for
         * browser navigation.
         *
         * @param value the new value of {@code browserNavigationTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty browserNavigationTimeout(int value) {
            setProperty("browserNavigationTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code pageLoadTimeout} property at runtime. Timeout in seconds for page load.
         *
         * @param value the new value of {@code pageLoadTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty pageLoadTimeout(int value) {
            setProperty("pageLoadTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code scriptExecutionTimeout} property at runtime. Timeout in seconds for script
         * execution.
         *
         * @param value the new value of {@code scriptExecutionTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty scriptExecutionTimeout(int value) {
            setProperty("scriptExecutionTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code waitForLazyLoadingTimeout} property at runtime. Timeout in seconds for
         * SHAFT's browser lazy-loading synchronization.
         *
         * @param value the new value of {@code waitForLazyLoadingTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty waitForLazyLoadingTimeout(int value) {
            setProperty("waitForLazyLoadingTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code lazyLoadingNetworkIdleInitialObservationMillis} property at runtime.
         * Initial network observation window in milliseconds when no requests were seen.
         *
         * @param value the new value of {@code lazyLoadingNetworkIdleInitialObservationMillis}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty lazyLoadingNetworkIdleInitialObservationMillis(int value) {
            setProperty("lazyLoadingNetworkIdleInitialObservationMillis", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code lazyLoadingNetworkIdleQuietWindowMillis} property at runtime. Required
         * network quiet window in milliseconds after observed activity.
         *
         * @param value the new value of {@code lazyLoadingNetworkIdleQuietWindowMillis}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty lazyLoadingNetworkIdleQuietWindowMillis(int value) {
            setProperty("lazyLoadingNetworkIdleQuietWindowMillis", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code lazyLoadingPollingIntervalMillis} property at runtime. Polling interval in
         * milliseconds for the browser-readiness fluentWait loop.
         *
         * @param value the new value of {@code lazyLoadingPollingIntervalMillis}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty lazyLoadingPollingIntervalMillis(int value) {
            setProperty("lazyLoadingPollingIntervalMillis", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code lazyLoadingDomStabilityQuietWindowMillis} property at runtime. Cheap per-
         * action DOM-mutation quiet window.
         *
         * @param value the new value of {@code lazyLoadingDomStabilityQuietWindowMillis}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty lazyLoadingDomStabilityQuietWindowMillis(int value) {
            setProperty("lazyLoadingDomStabilityQuietWindowMillis", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code lazyLoadingDomStabilityOnNavigationQuietWindowMillis} property at runtime.
         * DOM-mutation quiet window applied after navigation and by public
         * driver.browser().waitForLazyLoading().
         *
         * @param value the new value of {@code lazyLoadingDomStabilityOnNavigationQuietWindowMillis}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty lazyLoadingDomStabilityOnNavigationQuietWindowMillis(int value) {
            setProperty("lazyLoadingDomStabilityOnNavigationQuietWindowMillis", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code lazyLoadingScrollSweepMaxSteps} property at runtime. Maximum number of
         * progressive-scroll steps that driver.browser().scrollToLoadAll() performs while sweeping a page
         * for scroll-triggered lazy content.
         *
         * @param value the new value of {@code lazyLoadingScrollSweepMaxSteps}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty lazyLoadingScrollSweepMaxSteps(int value) {
            setProperty("lazyLoadingScrollSweepMaxSteps", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code defaultElementIdentificationTimeout} property at runtime. Default timeout
         * in seconds for element identification.
         *
         * @param value the new value of {@code defaultElementIdentificationTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty defaultElementIdentificationTimeout(double value) {
            setProperty("defaultElementIdentificationTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code waitForUiStateTimeout} property at runtime. Default timeout in seconds for
         * UI state waits such as waitUntil().
         *
         * @param value the new value of {@code waitForUiStateTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty waitForUiStateTimeout(int value) {
            setProperty("waitForUiStateTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code apiSocketTimeout} property at runtime. Timeout in seconds for API socket
         * reads.
         *
         * @param value the new value of {@code apiSocketTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty apiSocketTimeout(int value) {
            setProperty("apiSocketTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code apiConnectionTimeout} property at runtime. Timeout in seconds to establish
         * API connections.
         *
         * @param value the new value of {@code apiConnectionTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty apiConnectionTimeout(int value) {
            setProperty("apiConnectionTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code apiConnectionManagerTimeout} property at runtime. Timeout in seconds to
         * acquire a pooled API connection.
         *
         * @param value the new value of {@code apiConnectionManagerTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty apiConnectionManagerTimeout(int value) {
            setProperty("apiConnectionManagerTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shellSessionTimeout} property at runtime. Timeout in seconds for shell
         * session.
         *
         * @param value the new value of {@code shellSessionTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty shellSessionTimeout(long value) {
            setProperty("shellSessionTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code sshServerAliveInterval} property at runtime. JSch ServerAliveInterval in
         * seconds for remote SSH sessions.
         *
         * @param value the new value of {@code sshServerAliveInterval}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty sshServerAliveInterval(int value) {
            setProperty("sshServerAliveInterval", String.valueOf(value));
            return this;
        }

        /**
         * @deprecated Docker-wrapped terminal execution is deprecated for removal.
         */
        @Deprecated(since = "10.2.20260614", forRemoval = true)
        public SetProperty dockerCommandTimeout(int value) {
            setProperty("dockerCommandTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code dockerCommandTimeout} property at runtime. Deprecated.
         *
         * @param value the new value of {@code dockerCommandTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty dockerCommandTimeoutSeconds(int value) {
            setProperty("dockerCommandTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code databaseLoginTimeout} property at runtime. Timeout in seconds for database
         * login.
         *
         * @param value the new value of {@code databaseLoginTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty databaseLoginTimeout(int value) {
            setProperty("databaseLoginTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code databaseNetworkTimeout} property at runtime. Timeout in seconds for
         * database network operations.
         *
         * @param value the new value of {@code databaseNetworkTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty databaseNetworkTimeout(int value) {
            setProperty("databaseNetworkTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code databaseQueryTimeout} property at runtime. Timeout in seconds for database
         * queries.
         *
         * @param value the new value of {@code databaseQueryTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty databaseQueryTimeout(int value) {
            setProperty("databaseQueryTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code waitForRemoteServerToBeUp} property at runtime. Wait for remote server to
         * be up before execution.
         *
         * @param value the new value of {@code waitForRemoteServerToBeUp}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty waitForRemoteServerToBeUp(boolean value) {
            setProperty("waitForRemoteServerToBeUp", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code timeoutForRemoteServerToBeUp} property at runtime. Timeout in seconds for
         * remote server to be up.
         *
         * @param value the new value of {@code timeoutForRemoteServerToBeUp}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty timeoutForRemoteServerToBeUp(int value) {
            setProperty("timeoutForRemoteServerToBeUp", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code remoteServerInstanceCreationTimeout} property at runtime. Timeout in
         * seconds for remote server instance creation.
         *
         * @param value the new value of {@code remoteServerInstanceCreationTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty remoteServerInstanceCreationTimeout(int value) {
            setProperty("remoteServerInstanceCreationTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code remoteServerConnectionAttemptTimeout} property at runtime. Timeout in
         * seconds for a single HTTP connect/read attempt while creating a remote WebDriver session.
         *
         * @param value the new value of {@code remoteServerConnectionAttemptTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty remoteServerConnectionAttemptTimeout(int value) {
            setProperty("remoteServerConnectionAttemptTimeout", String.valueOf(value));
            return this;
        }

    }

}
