package com.shaft.gui.browser;

import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.HttpResponse;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

/**
 * Unit coverage for Selenium {@link NetworkActions} facade lines (issue #7077).
 * Stubs {@link BrowserActions#performNetworkAction} so no live DevTools session is required.
 */
public class NetworkActionsCoverageUnitTest {

    @Test
    public void networkFacadeShouldDispatchNamedOperations() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        DriverFactoryHelper helper = Mockito.mock(DriverFactoryHelper.class);
        Mockito.when(helper.getDriver()).thenReturn(driver);

        BrowserActions browser = Mockito.spy(new BrowserActions(helper));
        Mockito.doNothing().when(browser).performNetworkAction(Mockito.anyString(), Mockito.any());
        Mockito.doReturn(null).when(browser).networkInterceptRequest();

        NetworkActions network = new NetworkActions(browser);
        HttpResponse mocked = new HttpResponse().setStatus(200).setContent(Contents.utf8String("ok"));

        Assert.assertSame(network.and(), browser);
        Assert.assertSame(network.mock(request -> true, mocked), network);
        Assert.assertSame(network.intercept(request -> true, mocked), network);
        Assert.assertSame(network.clear(), network);
        Assert.assertSame(network.startContractRecording("/tmp/c.json"), network);
        Assert.assertSame(network.assertContract("/tmp/c.json"), network);
        Assert.assertSame(network.verifyContract("/tmp/c.json"), network);
        Assert.assertSame(network.replayContract("/tmp/c.json"), network);
        Assert.assertSame(network.routeFromHar("/tmp/t.har"), network);
        Assert.assertSame(network.offline(), network);
        Assert.assertSame(network.online(), network);
        Assert.assertSame(network.throttle(5, 10, 15), network);
        Assert.assertSame(network.block("*.png"), network);
        Assert.assertNull(network.interceptRequest());

        ArgumentCaptor<String> ops = ArgumentCaptor.forClass(String.class);
        Mockito.verify(browser, Mockito.times(12)).performNetworkAction(ops.capture(), Mockito.any());
        Assert.assertEquals(ops.getAllValues(), List.of(
                "mock",
                "intercept",
                "clear",
                "start-contract-recording",
                "assert-contract",
                "verify-contract",
                "replay-contract",
                "route-from-har",
                "offline",
                "online",
                "throttling",
                "resource-blocking"));
    }

    @Test
    public void constructorShouldRejectNullBrowser() {
        Assert.expectThrows(NullPointerException.class, () -> new NetworkActions(null));
    }
}
