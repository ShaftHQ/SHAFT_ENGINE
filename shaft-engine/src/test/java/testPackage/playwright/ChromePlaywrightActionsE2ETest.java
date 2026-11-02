package testPackage.playwright;

public class ChromePlaywrightActionsE2ETest extends PlaywrightActionsE2ETestBase {
    @Override
    protected String browserName() {
        return "chrome";
    }

    @Override
    protected String channel() {
        return "chrome";
    }
}
