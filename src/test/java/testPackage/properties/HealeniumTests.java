package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class HealeniumTests {
    int recoveryTries;
    String scoreCap;
    boolean healEnabled;
    String serverHost;
    int serverPort;
    int imitatePort;

    @BeforeClass
    public void beforeClass() {
        recoveryTries = SHAFT.Properties.healenium.recoveryTries();
        scoreCap = SHAFT.Properties.healenium.scoreCap();
        healEnabled = SHAFT.Properties.healenium.healEnabled();
        serverHost = SHAFT.Properties.healenium.serverHost();
        serverPort = SHAFT.Properties.healenium.serverPort();
        imitatePort = SHAFT.Properties.healenium.imitatePort();
    }

    @Test
    public void test() {
        SHAFT.Properties.healenium.set().recoveryTries(recoveryTries);
        SHAFT.Properties.healenium.set().scoreCap(scoreCap);
        SHAFT.Properties.healenium.set().healEnabled(healEnabled);
        SHAFT.Properties.healenium.set().serverHost(serverHost);
        SHAFT.Properties.healenium.set().serverPort(serverPort);
        SHAFT.Properties.healenium.set().imitatePort(imitatePort);
    }
}
