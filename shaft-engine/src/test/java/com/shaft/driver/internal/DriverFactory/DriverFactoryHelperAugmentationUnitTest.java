package com.shaft.driver.internal.DriverFactory;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.HasAuthentication;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.UsernameAndPassword;
import org.openqa.selenium.chromium.HasCdp;
import org.openqa.selenium.remote.Command;
import org.openqa.selenium.remote.CommandExecutor;
import org.openqa.selenium.remote.DriverCommand;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.remote.Response;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.URI;
import java.util.Map;
import java.util.function.Predicate;

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

        var augmented = DriverFactoryHelper.augmentRemoteWebDriver(driver, false);

        SHAFT.Validations.assertThat().object(augmented instanceof HasCdp).isEqualTo(true).perform();
    }

    @Test
    public void augmentingChromeRemoteSessionWithBiDiEnabledShouldNotThrowAndShouldYieldHasCdp() {
        RemoteWebDriver driver = newStubbedChromeRemoteWebDriver();

        var augmented = DriverFactoryHelper.augmentRemoteWebDriver(driver, true);

        SHAFT.Validations.assertThat().object(augmented instanceof HasCdp).isEqualTo(true).perform();
    }

    /**
     * Regression coverage for issue #4037 (second pass): {@code Augmenter#augment} captures the
     * {@code ExecuteMethod} used by {@code AddHasAuthentication}'s internal
     * {@code instanceof HasDevTools} check BEFORE the final, actually-augmented class exists --
     * that check runs against {@code RemoteExecuteMethod#getWrappedDriver()}, which returns the
     * exact object {@code Augmenter#extractRemoteWebDriver} was given, captured in the closure at
     * augmentation-build time. Pre-registering {@code AddHasAuthentication} via
     * {@code addDriverAugmentation()} in the SAME {@code .augment()} call as everything else (as
     * {@code DriverFactoryHelper} used to) makes {@code getWrappedDriver()} always return the
     * ORIGINAL, pre-augmentation {@code RemoteWebDriver} -- which never implements
     * {@code HasDevTools} -- so {@code register()}'s handler body is skipped and it returns having
     * attempted nothing, no matter what the real session capabilities say. Selenium's own
     * {@code Augmenter#addDependentAugmentations} safety net (which runs a separate, later
     * {@code .augment()} pass using the already-partially-augmented driver) does not have this
     * problem, because by then {@code extractRemoteWebDriver} returns an object that already
     * implements {@code HasDevTools}.
     *
     * <p>Proven here without a live grid or Docker: point {@code se:cdp} at a local TCP port
     * nothing is listening on. If the native handler is genuinely wired to CDP, {@code register()}
     * attempts a connection and fails loudly (connection refused). If it silently no-ops, nothing
     * happens and no exception is thrown.
     */
    @Test
    public void nativeAuthHandlerShouldAttemptCdpConnectionRatherThanSilentlyNoOp() throws IOException {
        int unreachablePort;
        try (ServerSocket unusedSocket = new ServerSocket(0)) {
            unreachablePort = unusedSocket.getLocalPort();
        } // closed immediately -- nothing listens here, so any connection attempt refuses fast

        CommandExecutor stubExecutor = (Command command) -> {
            Response response = new Response();
            response.setState("success");
            if (DriverCommand.NEW_SESSION.equals(command.getName())) {
                response.setSessionId("stub-session-id");
                response.setValue(Map.of(
                        "browserName", "chrome",
                        "se:cdp", "ws://localhost:" + unreachablePort + "/session/stub-session-id/se/cdp",
                        "se:cdpVersion", "120.0.6099.109"
                ));
            } else {
                response.setValue(null);
            }
            return response;
        };
        RemoteWebDriver driver = new RemoteWebDriver(stubExecutor, new MutableCapabilities(Map.of("browserName", "chrome")));

        var augmented = DriverFactoryHelper.augmentRemoteWebDriver(driver, true);

        Predicate<URI> anyUri = uri -> true;
        Assert.assertThrows(Exception.class, () ->
                ((HasAuthentication) augmented).register(anyUri, UsernameAndPassword.of("user", "pass")));
    }
}
