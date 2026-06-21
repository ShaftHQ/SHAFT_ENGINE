package testPackage.playwright;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;
import testPackage.TestPageServer;

public abstract class PlaywrightMobileWebE2ETestBase extends PlaywrightActionsE2ETestBase {
    @Test
    public void shouldApplyMobileDeviceDescriptor() {
        driver().browser().navigateToURL(TestPageServer.url("mobileViewportFixture.html"));

        SHAFT.Validations.assertThat().object(evaluateString("() => navigator.userAgent"))
                .contains(expectedUserAgentText())
                .perform();
        SHAFT.Validations.assertThat().object(evaluateNumber("() => window.innerWidth"))
                .isEqualTo((double) expectedViewportWidth())
                .perform();
        SHAFT.Validations.assertThat().object(evaluateNumber("() => window.innerHeight"))
                .isEqualTo((double) expectedViewportHeight())
                .perform();
        SHAFT.Validations.assertThat().object(evaluateNumber("() => window.devicePixelRatio"))
                .isEqualTo(expectedDeviceScaleFactor())
                .perform();
    }

    protected abstract String expectedUserAgentText();

    protected abstract int expectedViewportWidth();

    protected abstract int expectedViewportHeight();

    protected abstract double expectedDeviceScaleFactor();

    private String evaluateString(String expression) {
        return String.valueOf(driver().getDriver().evaluate(expression));
    }

    private double evaluateNumber(String expression) {
        return ((Number) driver().getDriver().evaluate(expression)).doubleValue();
    }
}
