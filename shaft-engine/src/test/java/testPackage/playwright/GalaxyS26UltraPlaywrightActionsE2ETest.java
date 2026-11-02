package testPackage.playwright;

public class GalaxyS26UltraPlaywrightActionsE2ETest extends PlaywrightMobileWebE2ETestBase {
    @Override
    protected String deviceName() {
        return "Galaxy S26 Ultra";
    }

    @Override
    protected String expectedUserAgentText() {
        return "SM-S948U";
    }

    @Override
    protected int expectedViewportWidth() {
        return 412;
    }

    @Override
    protected int expectedViewportHeight() {
        return 891;
    }

    @Override
    protected double expectedDeviceScaleFactor() {
        return 3.5;
    }
}
