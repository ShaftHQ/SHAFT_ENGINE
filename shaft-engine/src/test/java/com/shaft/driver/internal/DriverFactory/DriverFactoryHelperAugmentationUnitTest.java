package com.shaft.driver.internal.DriverFactory;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.chromium.HasCdp;
import org.openqa.selenium.remote.Command;
import org.openqa.selenium.remote.CommandExecutor;
import org.openqa.selenium.remote.DriverCommand;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.remote.Response;
import org.testng.annotations.Test;

import java.util.Map;

/**
 * Regression coverage for issue #3788 Defect A: on any chromium (chrome/edge) remote/grid
 * session, {@code new Augmenter()} already ServiceLoader-registers the driver-specific
 * concrete {@code AddHasCdp} (org.openqa.selenium.chrome.AddHasCdp /
 * org.openqa.selenium.edge.AddHasCdp, both AugmenterProvider<HasCdp>) shipped in the
 * selenium-chrome-driver / selenium-edge-driver jars. Adding a second, custom {@code AddHasCdp}
 * augmentation on top (as DriverFactoryHelper used to) makes Augmenter#augment collect two
 * Augmentation entries for the same HasCdp interface; ByteBuddy's DynamicType.Builder#implement
 * then throws "IllegalStateException: Already implemented interface ... HasCdp" the moment a
 * second .implement(HasCdp.class) is requested on the proxy builder -- killing every chromium
 * remote/grid session (issue #3788 Defect A).
 */
@Test(singleThreaded = true)
public class DriverFactoryHelperAugmentationUnitTest {

    /**
     * Builds a real RemoteWebDriver (not a mock -- Augmenter#augment ByteBuddy-subclasses
     * driver.getClass(), which does not play well with mock-generated classes) backed by a
     * stubbed CommandExecutor so no network/grid call happens. The stubbed NEW_SESSION response
     * mirrors what a real Selenium Grid returns for a chromium session: browserName=chrome plus
     * the se:cdp / se:cdpVersion capabilities that trigger both the built-in and (formerly) the
     * custom AddHasCdp augmentation.
     */
    private static RemoteWebDriver newStubbedChromeRemoteWebDriver() {
        CommandExecutor stubExecutor = (Command command) -> {
            Response response = new Response();
            response.setState("success");
            if (DriverCommand.NEW_SESSION.equals(command.getName())) {
                response.setSessionId("stub-session-id");
                response.setValue(Map.of(
                        "browserName", "chrome",
                        "se:cdp", "ws://localhost:9515/session/stub-session-id/se/cdp",
                        "se:cdpVersion", "120.0.6099.109"
                ));
            } else {
                response.setValue(null);
            }
            return response;
        };
        return new RemoteWebDriver(stubExecutor, new MutableCapabilities(Map.of("browserName", "chrome")));
    }

    @Test
    public void augmentingChromeRemoteSessionShouldNotThrowAndShouldYieldHasCdp() {
        RemoteWebDriver driver = newStubbedChromeRemoteWebDriver();

        var augmented = DriverFactoryHelper.augmentRemoteWebDriver(driver, "chrome", false);

        SHAFT.Validations.assertThat().object(augmented instanceof HasCdp).isEqualTo(true).perform();
    }

    @Test
    public void augmentingChromeRemoteSessionWithBiDiEnabledShouldNotThrowAndShouldYieldHasCdp() {
        RemoteWebDriver driver = newStubbedChromeRemoteWebDriver();

        var augmented = DriverFactoryHelper.augmentRemoteWebDriver(driver, "chrome", true);

        SHAFT.Validations.assertThat().object(augmented instanceof HasCdp).isEqualTo(true).perform();
    }
}
