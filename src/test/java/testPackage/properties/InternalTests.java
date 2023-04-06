package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

public class InternalTests {
    String shaftEngineVersion;
    String watermarkImagePath;
    String allureVersion;

    @Test
    public void test() {
        shaftEngineVersion = SHAFT.Properties.internal.shaftEngineVersion();
        watermarkImagePath = SHAFT.Properties.internal.watermarkImagePath();
        allureVersion = SHAFT.Properties.internal.allureVersion();
    }
}
