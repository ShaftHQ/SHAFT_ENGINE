package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class TimeoutsTests {
    Boolean waitForLazyLoading;
    int browserNavigationTimeout;
    int pageLoadTimeout;
    int scriptExecutionTimeout;
    double defaultElementIdentificationTimeout;
    int apiSocketTimeout;
    int apiConnectionTimeout;
    int apiConnectionManagerTimeout;
    long shellSessionTimeout;
    int dockerCommandTimeout;
    int databaseLoginTimeout;
    int databaseNetworkTimeout;
    int databaseQueryTimeout;
    Boolean waitForRemoteServerToBeUp;
    int timeoutForRemoteServerToBeUp;
    int remoteServerInstanceCreationTimeout;


    @BeforeClass
    public void beforeClass() {
        waitForLazyLoading = SHAFT.Properties.timeouts.waitForLazyLoading();
        browserNavigationTimeout = SHAFT.Properties.timeouts.browserNavigationTimeout();
        pageLoadTimeout = SHAFT.Properties.timeouts.pageLoadTimeout();
        scriptExecutionTimeout = SHAFT.Properties.timeouts.scriptExecutionTimeout();
        defaultElementIdentificationTimeout = SHAFT.Properties.timeouts.defaultElementIdentificationTimeout();
        apiSocketTimeout = SHAFT.Properties.timeouts.apiSocketTimeout();
        apiConnectionTimeout = SHAFT.Properties.timeouts.apiConnectionTimeout();
        apiConnectionManagerTimeout = SHAFT.Properties.timeouts.apiConnectionManagerTimeout();
        shellSessionTimeout = SHAFT.Properties.timeouts.shellSessionTimeout();
        dockerCommandTimeout = SHAFT.Properties.timeouts.dockerCommandTimeout();
        databaseNetworkTimeout = SHAFT.Properties.timeouts.databaseLoginTimeout();
        databaseLoginTimeout = SHAFT.Properties.timeouts.databaseLoginTimeout();
        databaseQueryTimeout = SHAFT.Properties.timeouts.databaseQueryTimeout();
        waitForRemoteServerToBeUp = SHAFT.Properties.timeouts.waitForRemoteServerToBeUp();
        timeoutForRemoteServerToBeUp = SHAFT.Properties.timeouts.timeoutForRemoteServerToBeUp();
        remoteServerInstanceCreationTimeout = SHAFT.Properties.timeouts.remoteServerInstanceCreationTimeout();

    }

    @Test
    public void test() {
        SHAFT.Properties.timeouts.set().waitForLazyLoading(waitForLazyLoading);
        SHAFT.Properties.timeouts.set().browserNavigationTimeout(browserNavigationTimeout);
        SHAFT.Properties.timeouts.set().pageLoadTimeout(pageLoadTimeout);
        SHAFT.Properties.timeouts.set().scriptExecutionTimeout(scriptExecutionTimeout);
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(defaultElementIdentificationTimeout);
        SHAFT.Properties.timeouts.set().apiConnectionTimeout(apiConnectionTimeout);
        SHAFT.Properties.timeouts.set().apiConnectionManagerTimeout(apiConnectionManagerTimeout);
        SHAFT.Properties.timeouts.set().shellSessionTimeout(shellSessionTimeout);
        SHAFT.Properties.timeouts.set().dockerCommandTimeout(dockerCommandTimeout);
        SHAFT.Properties.timeouts.set().databaseNetworkTimeout(databaseNetworkTimeout);
        SHAFT.Properties.timeouts.set().databaseLoginTimeout(databaseLoginTimeout);
        SHAFT.Properties.timeouts.set().databaseQueryTimeout(databaseQueryTimeout);
        SHAFT.Properties.timeouts.set().waitForRemoteServerToBeUp(waitForRemoteServerToBeUp);
        SHAFT.Properties.timeouts.set().timeoutForRemoteServerToBeUp(timeoutForRemoteServerToBeUp);
        SHAFT.Properties.timeouts.set().remoteServerInstanceCreationTimeout(remoteServerInstanceCreationTimeout);

    }

}
