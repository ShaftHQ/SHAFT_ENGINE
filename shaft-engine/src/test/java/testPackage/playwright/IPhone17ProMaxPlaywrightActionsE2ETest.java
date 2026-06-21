package testPackage.playwright;

public class IPhone17ProMaxPlaywrightActionsE2ETest extends PlaywrightMobileWebE2ETestBase {
    @Override
    protected String deviceName() {
        return "iPhone 17 Pro Max";
    }

    @Override
    protected String expectedUserAgentText() {
        return "iPhone OS 26_0";
    }

    @Override
    protected int expectedViewportWidth() {
        return 440;
    }

    @Override
    protected int expectedViewportHeight() {
        return 763;
    }

    @Override
    protected double expectedDeviceScaleFactor() {
        return 3.0;
    }
}
