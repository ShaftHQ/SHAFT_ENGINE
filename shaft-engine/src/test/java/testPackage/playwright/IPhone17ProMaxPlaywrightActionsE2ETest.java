package testPackage.playwright;

public class IPhone17ProMaxPlaywrightActionsE2ETest extends PlaywrightMobileWebE2ETestBase {
    @Override
    protected String deviceName() {
        return "iPhone 17 Pro Max";
    }

    @Override
    protected String expectedUserAgentText() {
        // ponytail: keep the UA check resilient to OS version churn. Ceiling: iPhone OS identity only.
        // Upgrade trigger: the flow depends on a specific browser or operating-system version.
        return "iPhone OS";
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
