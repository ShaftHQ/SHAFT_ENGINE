package testPackage.playwright;

import org.testng.annotations.Test;
import testPackage.TestPageServer;

public abstract class PlaywrightMobileWebE2ETestBase extends PlaywrightActionsE2ETestBase {
    @Test
    public void shouldApplyMobileDeviceDescriptor() {
        driver().browser().navigateToURL(TestPageServer.url("mobileViewportFixture.html"));

        driver().assertThat().browser().attribute("userAgent")
                .contains(expectedUserAgentText())
                .perform();
        driver().assertThat().browser().attribute("windowWidth")
                .isEqualTo(expectedViewportWidth())
                .perform();
        driver().assertThat().browser().attribute("windowHeight")
                .isEqualTo(expectedViewportHeight())
                .perform();
        driver().assertThat().browser().attribute("devicePixelRatio")
                .isEqualTo(expectedDeviceScaleFactor())
                .perform();
    }

    protected abstract String expectedUserAgentText();

    protected abstract int expectedViewportWidth();

    protected abstract int expectedViewportHeight();

    protected abstract double expectedDeviceScaleFactor();
}
