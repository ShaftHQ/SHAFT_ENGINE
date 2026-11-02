package fixture;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.image.VisualProcessingProvider;

import java.util.ServiceLoader;

public class BomConsumer {
    public String versionlessEngineDependencySmokeTest() {
        return SHAFT.class.getName();
    }

    public String automaticallyDiscoveredVisualProvider() {
        return ServiceLoader.load(VisualProcessingProvider.class)
                .findFirst()
                .orElseThrow()
                .getClass()
                .getName();
    }

    public static void main(String[] args) {
        String provider = new BomConsumer().automaticallyDiscoveredVisualProvider();
        if (!"com.shaft.gui.internal.image.OpenCvVisualProcessingProvider".equals(provider)) {
            throw new IllegalStateException("Unexpected visual provider: " + provider);
        }
    }

}
