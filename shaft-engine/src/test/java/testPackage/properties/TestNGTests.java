package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

public class TestNGTests {
    String parallel;
    double threadCount;
    int verbose;
    boolean preserveOrder;
    boolean groupByInstances;
    int dataProviderThreadCount;

    @Test
    public void test() {
        parallel = SHAFT.Properties.testNG.parallel();
        threadCount = SHAFT.Properties.testNG.threadCount();
        verbose = SHAFT.Properties.testNG.verbose();
        preserveOrder = SHAFT.Properties.testNG.preserveOrder();
        groupByInstances = SHAFT.Properties.testNG.groupByInstances();
        dataProviderThreadCount = SHAFT.Properties.testNG.dataProviderThreadCount();
    }
}
