package fixture;

import com.shaft.driver.SHAFT;

public class BomConsumer {
    public String versionlessEngineDependencySmokeTest() {
        return SHAFT.class.getName();
    }
}
