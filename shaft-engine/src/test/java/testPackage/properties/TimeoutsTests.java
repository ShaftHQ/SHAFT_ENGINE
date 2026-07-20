package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class TimeoutsTests {
    Boolean waitForLazyLoading;
    int browserNavigationTimeout;
    int pageLoadTimeout;
    int scriptExecutionTimeout;
    int waitForLazyLoadingTimeout;
    int lazyLoadingNetworkIdleInitialObservationMillis;
    int lazyLoadingNetworkIdleQuietWindowMillis;
    int lazyLoadingPollingIntervalMillis;
    int lazyLoadingDomStabilityQuietWindowMillis;
    int lazyLoadingScrollSweepMaxSteps;
    double defaultElementIdentificationTimeout;
    int waitForUiStateTimeout;
    int apiSocketTimeout;
    int apiConnectionTimeout;
    int apiConnectionManagerTimeout;
    long shellSessionTimeout;
    int sshServerAliveInterval;
    int dockerCommandTimeout;
    int databaseLoginTimeout;
    int databaseNetworkTimeout;
    int databaseQueryTimeout;
    Boolean waitForRemoteServerToBeUp;
    int timeoutForRemoteServerToBeUp;
    int remoteServerInstanceCreationTimeout;
    int remoteServerConnectionAttemptTimeout;


    @BeforeClass
    public void beforeClass() {
        waitForLazyLoading = SHAFT.Properties.timeouts.waitForLazyLoading();
        browserNavigationTimeout = SHAFT.Properties.timeouts.browserNavigationTimeout();
        pageLoadTimeout = SHAFT.Properties.timeouts.pageLoadTimeout();
        scriptExecutionTimeout = SHAFT.Properties.timeouts.scriptExecutionTimeout();
        waitForLazyLoadingTimeout = SHAFT.Properties.timeouts.waitForLazyLoadingTimeout();
        lazyLoadingNetworkIdleInitialObservationMillis = SHAFT.Properties.timeouts.lazyLoadingNetworkIdleInitialObservationMillis();
        lazyLoadingNetworkIdleQuietWindowMillis = SHAFT.Properties.timeouts.lazyLoadingNetworkIdleQuietWindowMillis();
        lazyLoadingPollingIntervalMillis = SHAFT.Properties.timeouts.lazyLoadingPollingIntervalMillis();
        lazyLoadingDomStabilityQuietWindowMillis = SHAFT.Properties.timeouts.lazyLoadingDomStabilityQuietWindowMillis();
        lazyLoadingScrollSweepMaxSteps = SHAFT.Properties.timeouts.lazyLoadingScrollSweepMaxSteps();
        defaultElementIdentificationTimeout = SHAFT.Properties.timeouts.defaultElementIdentificationTimeout();
        waitForUiStateTimeout = SHAFT.Properties.timeouts.waitForUiStateTimeout();
        apiSocketTimeout = SHAFT.Properties.timeouts.apiSocketTimeout();
        apiConnectionTimeout = SHAFT.Properties.timeouts.apiConnectionTimeout();
        apiConnectionManagerTimeout = SHAFT.Properties.timeouts.apiConnectionManagerTimeout();
        shellSessionTimeout = SHAFT.Properties.timeouts.shellSessionTimeout();
        sshServerAliveInterval = SHAFT.Properties.timeouts.sshServerAliveInterval();
        dockerCommandTimeout = SHAFT.Properties.timeouts.dockerCommandTimeout();
        databaseNetworkTimeout = SHAFT.Properties.timeouts.databaseLoginTimeout();
        databaseLoginTimeout = SHAFT.Properties.timeouts.databaseLoginTimeout();
        databaseQueryTimeout = SHAFT.Properties.timeouts.databaseQueryTimeout();
        waitForRemoteServerToBeUp = SHAFT.Properties.timeouts.waitForRemoteServerToBeUp();
        timeoutForRemoteServerToBeUp = SHAFT.Properties.timeouts.timeoutForRemoteServerToBeUp();
        remoteServerInstanceCreationTimeout = SHAFT.Properties.timeouts.remoteServerInstanceCreationTimeout();
        remoteServerConnectionAttemptTimeout = SHAFT.Properties.timeouts.remoteServerConnectionAttemptTimeout();

    }

    @Test
    public void test() {
        SHAFT.Properties.timeouts.set().waitForLazyLoading(waitForLazyLoading);
        SHAFT.Properties.timeouts.set().browserNavigationTimeout(browserNavigationTimeout);
        SHAFT.Properties.timeouts.set().pageLoadTimeout(pageLoadTimeout);
        SHAFT.Properties.timeouts.set().scriptExecutionTimeout(scriptExecutionTimeout);
        SHAFT.Properties.timeouts.set().waitForLazyLoadingTimeout(waitForLazyLoadingTimeout);
        SHAFT.Properties.timeouts.set().lazyLoadingNetworkIdleInitialObservationMillis(lazyLoadingNetworkIdleInitialObservationMillis);
        SHAFT.Properties.timeouts.set().lazyLoadingNetworkIdleQuietWindowMillis(lazyLoadingNetworkIdleQuietWindowMillis);
        SHAFT.Properties.timeouts.set().lazyLoadingPollingIntervalMillis(lazyLoadingPollingIntervalMillis);
        SHAFT.Properties.timeouts.set().lazyLoadingDomStabilityQuietWindowMillis(lazyLoadingDomStabilityQuietWindowMillis);
        SHAFT.Properties.timeouts.set().lazyLoadingScrollSweepMaxSteps(lazyLoadingScrollSweepMaxSteps);
        SHAFT.Properties.timeouts.set().lazyLoadingTimeout(waitForLazyLoadingTimeout + 1);
        SHAFT.Validations.assertThat().object(SHAFT.Properties.timeouts.waitForLazyLoadingTimeout()).isEqualTo(waitForLazyLoadingTimeout + 1).perform();
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(defaultElementIdentificationTimeout);
        SHAFT.Properties.timeouts.set().waitForUiStateTimeout(waitForUiStateTimeout);
        SHAFT.Properties.timeouts.set().apiConnectionTimeout(apiConnectionTimeout);
        SHAFT.Properties.timeouts.set().apiConnectionManagerTimeout(apiConnectionManagerTimeout);
        SHAFT.Properties.timeouts.set().shellSessionTimeout(shellSessionTimeout);
        SHAFT.Properties.timeouts.set().sshServerAliveInterval(sshServerAliveInterval);
        SHAFT.Properties.timeouts.set().dockerCommandTimeout(dockerCommandTimeout);
        SHAFT.Properties.timeouts.set().databaseNetworkTimeout(databaseNetworkTimeout);
        SHAFT.Properties.timeouts.set().databaseLoginTimeout(databaseLoginTimeout);
        SHAFT.Properties.timeouts.set().databaseQueryTimeout(databaseQueryTimeout);
        SHAFT.Properties.timeouts.set().waitForRemoteServerToBeUp(waitForRemoteServerToBeUp);
        SHAFT.Properties.timeouts.set().timeoutForRemoteServerToBeUp(timeoutForRemoteServerToBeUp);
        SHAFT.Properties.timeouts.set().remoteServerInstanceCreationTimeout(remoteServerInstanceCreationTimeout);
        SHAFT.Properties.timeouts.set().remoteServerConnectionAttemptTimeout(remoteServerConnectionAttemptTimeout);

    }

}
