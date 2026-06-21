package testPackage.playwright;

public class EdgePlaywrightActionsE2ETest extends PlaywrightActionsE2ETestBase {
    @Override
    protected String browserName() {
        return "edge";
    }

    @Override
    protected String channel() {
        return "msedge";
    }
}
