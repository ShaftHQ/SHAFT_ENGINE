package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class PerformanceTests {
    boolean isEnabled;
    int port;

    @BeforeClass
    public void beforeClass() {
        isEnabled = SHAFT.Properties.performance.isEnabled();
        port = SHAFT.Properties.performance.port();
    }

    @Test
    public void test() {
        SHAFT.Properties.performance.set().isEnabled(isEnabled);
        SHAFT.Properties.performance.set().port(port);
    }
}
