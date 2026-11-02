package com.shaft.gui.driver;

import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Browser;
import com.microsoft.playwright.Page;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.gui.browser.internal.PlaywrightNetworkInterceptor;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import org.testng.Assert;
import org.testng.annotations.Test;
import org.testng.annotations.AfterMethod;
import org.mockito.Mockito;
import org.openqa.selenium.remote.http.HttpResponse;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.chrome.ChromeDriver;
import io.appium.java_client.AppiumDriver;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.function.Predicate;
import java.nio.file.Files;
import java.nio.file.Path;
import java.net.URI;
import java.util.List;
import java.util.Optional;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;

public class BrowserNamespaceTest {

    @AfterMethod
    public void clearContractState() {
        com.shaft.tools.io.internal.HttpContractRecorder.clear();
    }

    @Test
    public void networkNamespaceShouldBeDiscoverableAcrossTheGenericAndConcreteBrowserFacades() throws Exception {
        Method contract = BrowserActionsContract.class.getMethod("network");
        Method selenium = com.shaft.gui.browser.BrowserActions.class.getMethod("network");
        Method playwright = com.shaft.gui.playwright.browser.BrowserActions.class.getMethod("network");

        Assert.assertEquals(contract.getReturnType().getSimpleName(), "NetworkActionsContract");
        Assert.assertEquals(selenium.getReturnType().getSimpleName(), "NetworkActions");
        Assert.assertEquals(playwright.getReturnType().getSimpleName(), "NetworkActions");

        Set<String> methods = Arrays.stream(NetworkActionsContract.class.getDeclaredMethods())
                .map(Method::getName)
                .collect(Collectors.toSet());
        Assert.assertEquals(methods, Set.of("and", "mock", "interceptRequest", "intercept", "clear",
                "startContractRecording", "assertContract", "verifyContract", "replayContract", "routeFromHar",
                "offline", "online", "throttle", "block"));
        Assert.assertEquals(NetworkActionsContract.class.getDeclaredMethods().length, 14);
        Assert.assertEquals(NetworkActionsContract.class
                .getDeclaredMethod("throttle", long.class, long.class, long.class).getReturnType(),
                NetworkActionsContract.class);
        Assert.assertTrue(NetworkActionsContract.class.getDeclaredMethod("block", String[].class).isVarArgs());
    }

    @Test(expectedExceptions = UnsupportedOperationException.class,
            expectedExceptionsMessageRegExp = "(?s).*[Nn]etwork.*not supported.*")
    public void thirdPartyBrowserFacadeShouldFailClosedWithoutImplementingTheNamespace() {
        BrowserActionsContract facade = Mockito.mock(BrowserActionsContract.class, Mockito.CALLS_REAL_METHODS);
        facade.network();
    }

    @Test(expectedExceptions = UnsupportedOperationException.class,
            expectedExceptionsMessageRegExp = ".*offline.*not supported.*")
    public void seleniumNamespaceShouldRejectUnsupportedDriversBeforeDelegating() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        new com.shaft.gui.browser.BrowserActions(driver, true).network().offline();
    }

    @Test(expectedExceptions = UnsupportedOperationException.class,
            expectedExceptionsMessageRegExp = ".*live Playwright session.*")
    public void playwrightNamespaceShouldRejectAbsentSessions() {
        new com.shaft.gui.playwright.browser.BrowserActions(null).network().offline();
    }

    @Test(expectedExceptions = UnsupportedOperationException.class,
            expectedExceptionsMessageRegExp = ".*live Playwright session.*")
    public void playwrightNamespaceShouldRejectClosedPages() {
        PlaywrightSession session = livePlaywrightSession();
        Mockito.when(session.page().isClosed()).thenReturn(true);
        new com.shaft.gui.playwright.browser.BrowserActions(session).network().offline();
    }

    @Test(expectedExceptions = UnsupportedOperationException.class,
            expectedExceptionsMessageRegExp = ".*live Playwright session.*")
    public void playwrightNamespaceShouldRejectDisconnectedBrowsers() {
        PlaywrightSession session = livePlaywrightSession();
        Mockito.when(session.browser().isConnected()).thenReturn(false);
        new com.shaft.gui.playwright.browser.BrowserActions(session).network().offline();
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void playwrightNamespaceShouldRejectMissingContext() {
        PlaywrightSession session = livePlaywrightSession();
        Mockito.when(session.browserContext()).thenReturn(null);
        new com.shaft.gui.playwright.browser.BrowserActions(session).network().offline();
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void playwrightNamespaceShouldRejectMissingPage() {
        PlaywrightSession session = livePlaywrightSession();
        Mockito.when(session.page()).thenReturn(null);
        new com.shaft.gui.playwright.browser.BrowserActions(session).network().offline();
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void playwrightNamespaceShouldRejectMissingBrowser() {
        PlaywrightSession session = livePlaywrightSession();
        Mockito.when(session.browser()).thenReturn(null);
        new com.shaft.gui.playwright.browser.BrowserActions(session).network().offline();
    }

    @Test(expectedExceptions = UnsupportedOperationException.class,
            expectedExceptionsMessageRegExp = ".*not supported.*live Selenium/Appium session.*")
    public void seleniumNamespaceShouldRejectClosedDevToolsSessions() {
        ChromeDriver driver = Mockito.mock(ChromeDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(null);
        new com.shaft.gui.browser.BrowserActions(driver, true).network().offline();
    }

    @Test(expectedExceptions = UnsupportedOperationException.class,
            expectedExceptionsMessageRegExp = ".*not supported.*live Selenium/Appium session.*")
    public void seleniumNamespaceShouldRejectGenericAppiumSessions() {
        AppiumDriver driver = Mockito.mock(AppiumDriver.class);
        new com.shaft.gui.browser.BrowserActions(driver, true).network().offline();
    }

    @Test
    public void seleniumContractModeShouldRemainInactiveWhenObservationCannotStart() {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasDevTools.class, TakesScreenshot.class));
        stubLiveDevTools(driver);
        DriverFactoryHelper helper = Mockito.mock(DriverFactoryHelper.class);
        Mockito.when(helper.getDriver()).thenReturn(driver);
        Mockito.when(helper.startBrowserNetworkObservation()).thenReturn(false);
        var network = new com.shaft.gui.browser.BrowserActions(helper).network();

        Assert.expectThrows(UnsupportedOperationException.class,
                () -> network.startContractRecording("unwritten.json"));
        Assert.assertFalse(com.shaft.tools.io.internal.HttpContractRecorder.isBrowserContractModeActive());
    }

    @Test
    public void retainedFlatSeleniumContractActionShouldNotLeakStateWhenObservationCannotStart() {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasDevTools.class, TakesScreenshot.class));
        stubLiveDevTools(driver);
        DriverFactoryHelper helper = Mockito.mock(DriverFactoryHelper.class);
        Mockito.when(helper.getDriver()).thenReturn(driver);
        Mockito.when(helper.startBrowserNetworkObservation()).thenReturn(false);

        new com.shaft.gui.browser.BrowserActions(helper).startContractRecording("unwritten.json");

        Assert.assertFalse(com.shaft.tools.io.internal.HttpContractRecorder.isBrowserContractModeActive());
    }

    @Test
    public void playwrightContractModeShouldRemainInactiveWhenRouteInstallationFails() {
        PlaywrightSession session = livePlaywrightSession();
        PlaywrightNetworkInterceptor interceptor = Mockito.mock(PlaywrightNetworkInterceptor.class);
        Mockito.when(session.networkInterceptor()).thenReturn(interceptor);
        Mockito.doThrow(new IllegalStateException("route failed")).when(interceptor).startObserving();
        var network = new com.shaft.gui.playwright.browser.BrowserActions(session).network();

        Assert.expectThrows(IllegalStateException.class,
                () -> network.startContractRecording("unwritten.json"));
        Assert.assertFalse(com.shaft.tools.io.internal.HttpContractRecorder.isBrowserContractModeActive());
    }

    @Test
    public void seleniumContractInitializationFailureShouldRollBackObservation() {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasDevTools.class, TakesScreenshot.class));
        stubLiveDevTools(driver);
        DriverFactoryHelper helper = Mockito.mock(DriverFactoryHelper.class);
        Mockito.when(helper.getDriver()).thenReturn(driver);
        Mockito.when(helper.startBrowserNetworkObservation()).thenReturn(true);
        var network = new com.shaft.gui.browser.BrowserActions(helper).network();

        Assert.expectThrows(IllegalStateException.class, () -> network.assertContract("missing-contract.json"));

        Mockito.verify(helper).stopBrowserNetworkObservation();
        Assert.assertFalse(com.shaft.tools.io.internal.HttpContractRecorder.isBrowserContractModeActive());
    }

    @Test
    public void playwrightContractInitializationFailureShouldRollBackObservation() {
        PlaywrightSession session = livePlaywrightSession();
        PlaywrightNetworkInterceptor interceptor = Mockito.mock(PlaywrightNetworkInterceptor.class);
        Mockito.when(session.networkInterceptor()).thenReturn(interceptor);
        var network = new com.shaft.gui.playwright.browser.BrowserActions(session).network();

        Assert.expectThrows(IllegalStateException.class, () -> network.verifyContract("missing-contract.json"));

        Mockito.verify(interceptor).stopObserving();
        Assert.assertFalse(com.shaft.tools.io.internal.HttpContractRecorder.isBrowserContractModeActive());
    }

    @Test
    public void seleniumNetworkNamespaceShouldDelegateAndReturnToTheBrowserFacade() {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasDevTools.class, TakesScreenshot.class));
        stubLiveDevTools(driver);
        com.shaft.gui.browser.BrowserActions browser = Mockito.spy(
                new com.shaft.gui.browser.BrowserActions(driver, true));
        Mockito.doReturn(browser).when(browser).mock(Mockito.any(), Mockito.any());
        Mockito.doReturn(browser).when(browser).goOffline();
        Mockito.doReturn(browser).when(browser).restoreNetwork();
        HttpResponse response = new HttpResponse().setStatus(200);

        var network = browser.network();

        Assert.assertSame(network.mock(request -> true, response).offline().online().and(), browser);
        Mockito.verify(browser).mock(Mockito.any(), Mockito.same(response));
        Mockito.verify(browser).goOffline();
        Mockito.verify(browser).restoreNetwork();
    }

    @Test
    public void playwrightNetworkNamespaceShouldUseNativeContextOfflineEmulationAndDelegateMocks() {
        PlaywrightSession session = livePlaywrightSession();
        com.shaft.gui.playwright.browser.BrowserActions browser = Mockito.spy(
                new com.shaft.gui.playwright.browser.BrowserActions(session));
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Mockito.doReturn(context).when(session).browserContext();
        Mockito.doReturn(browser).when(browser).mock(Mockito.any(), Mockito.any());
        HttpResponse response = new HttpResponse().setStatus(200);

        var network = browser.network();

        Assert.assertSame(network.mock(request -> true, response).offline().online().and(), browser);
        Mockito.verify(browser).mock(Mockito.any(), Mockito.same(response));
        Mockito.verify(context).setOffline(true);
        Mockito.verify(context).setOffline(false);
    }

    @Test
    @SuppressWarnings("unchecked")
    public void seleniumNamespaceShouldForwardEveryLegacyNetworkOperation() throws Exception {
        Path contract = emptyContract();
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasDevTools.class, TakesScreenshot.class));
        stubLiveDevTools(driver);
        DriverFactoryHelper helper = Mockito.mock(DriverFactoryHelper.class);
        Mockito.when(helper.getDriver()).thenReturn(driver);
        Mockito.when(helper.startBrowserNetworkObservation()).thenReturn(true);
        var browser = Mockito.spy(new com.shaft.gui.browser.BrowserActions(helper));
        Predicate<HttpRequest> predicate = request -> true;
        HttpResponse response = new HttpResponse().setStatus(204);
        Mockito.doReturn(browser).when(browser).mock(predicate, response);
        Mockito.doReturn(browser).when(browser).intercept(predicate, response);
        Mockito.doReturn(browser).when(browser).clearNetworkInterceptors();
        Mockito.doReturn(browser).when(browser).replayContract(contract.toString());
        Mockito.doReturn(browser).when(browser).routeFromHar("traffic.har");
        Mockito.doReturn(browser).when(browser).goOffline();
        Mockito.doReturn(browser).when(browser).restoreNetwork();
        Mockito.doReturn(browser).when(browser).throttleNetwork(10, 20, 30);
        Mockito.doReturn(browser).when(browser).blockNetworkResources("*.png", "*.jpg");

        var network = browser.network();
        Assert.assertSame(network.mock(predicate, response), network);
        Assert.assertSame(network.interceptRequest().get().urlContains("/builder").respond().statusCode(201).perform(),
                browser);
        Assert.assertSame(network.intercept(predicate, response).clear()
                .startContractRecording(contract.toString(), "/api")
                .assertContract(contract.toString(), "/api")
                .verifyContract(contract.toString(), "/api")
                .replayContract(contract.toString()).routeFromHar("traffic.har")
                .offline().online().throttle(10, 20, 30).block("*.png", "*.jpg"), network);

        Mockito.verify(browser).mock(predicate, response);
        Mockito.verify(browser).intercept(predicate, response);
        Mockito.verify(browser).clearNetworkInterceptors();
        Mockito.verify(helper, Mockito.times(3)).startBrowserNetworkObservation();
        Mockito.verify(helper).registerBrowserNetworkInterceptionRule(Mockito.any());
        Mockito.verify(browser).replayContract(contract.toString());
        Mockito.verify(browser).routeFromHar("traffic.har");
        Mockito.verify(browser).throttleNetwork(10, 20, 30);
        Mockito.verify(browser).blockNetworkResources("*.png", "*.jpg");
    }

    @Test
    public void legacyFlatNetworkSignaturesShouldRemainAvailableWithCovariantReturns() throws Exception {
        assertFlatNetworkSignatures(com.shaft.gui.browser.BrowserActions.class);
        assertFlatNetworkSignatures(com.shaft.gui.playwright.browser.BrowserActions.class);
    }

    @Test
    public void defaultMethodCollisionBoundaryShouldBeExecutableDocumentation() throws Exception {
        String compatible = """
                import com.shaft.gui.driver.*;
                interface LegacyCompatible { default NetworkActionsContract network() { return null; } }
                interface CompatibleFacade extends BrowserActionsContract, LegacyCompatible {
                    @Override default NetworkActionsContract network() { return BrowserActionsContract.super.network(); }
                }
                """;
        String incompatible = """
                import com.shaft.gui.driver.*;
                interface LegacyIncompatible { default String network() { return \"legacy\"; } }
                interface IncompatibleFacade extends BrowserActionsContract, LegacyIncompatible {}
                """;

        Assert.assertTrue(compiles("CompatibleFacade", compatible));
        Assert.assertFalse(compiles("IncompatibleFacade", incompatible));
    }

    @Test
    public void playwrightNamespaceShouldForwardEverySupportedNetworkOperation() throws Exception {
        Path contract = emptyContract();
        PlaywrightSession session = livePlaywrightSession();
        PlaywrightNetworkInterceptor interceptor = Mockito.mock(PlaywrightNetworkInterceptor.class);
        Mockito.when(session.networkInterceptor()).thenReturn(interceptor);
        var browser = Mockito.spy(new com.shaft.gui.playwright.browser.BrowserActions(session));
        Predicate<HttpRequest> predicate = request -> true;
        HttpResponse response = new HttpResponse().setStatus(202);
        Mockito.doReturn(browser).when(browser).mock(predicate, response);
        Mockito.doReturn(browser).when(browser).intercept(predicate, response);
        Mockito.doReturn(browser).when(browser).clearNetworkInterceptors();
        Mockito.doReturn(browser).when(browser).replayContract(contract.toString());
        Mockito.doReturn(browser).when(browser).routeFromHar("traffic.har");

        var network = browser.network();
        Assert.assertSame(network.mock(predicate, response).intercept(predicate, response).clear()
                .startContractRecording(contract.toString(), "/record")
                .assertContract(contract.toString(), "/assert")
                .verifyContract(contract.toString(), "/verify")
                .replayContract(contract.toString()).routeFromHar("traffic.har"), network);
        Assert.assertSame(network.interceptRequest().post().urlContains("/builder").respond().statusCode(201).perform(),
                browser);
        Assert.expectThrows(UnsupportedOperationException.class, () -> network.block("*.png"));

        Mockito.verify(browser).mock(predicate, response);
        Mockito.verify(browser).intercept(predicate, response);
        Mockito.verify(browser).clearNetworkInterceptors();
        Mockito.verify(browser).replayContract(contract.toString());
        Mockito.verify(browser).routeFromHar("traffic.har");
        Mockito.verify(interceptor, Mockito.times(3)).startObserving();
        Mockito.verify(interceptor).addRule(Mockito.any());
    }

    private void assertFlatNetworkSignatures(Class<?> facade) throws Exception {
        Assert.assertEquals(facade.getMethod("mock", Predicate.class, HttpResponse.class).getReturnType(), facade);
        Assert.assertEquals(facade.getMethod("interceptRequest").getReturnType().getSimpleName(),
                "NetworkInterceptionRequestBuilder");
        Assert.assertEquals(facade.getMethod("intercept", Predicate.class, HttpResponse.class).getReturnType(), facade);
        Assert.assertEquals(facade.getMethod("clearNetworkInterceptors").getReturnType(), facade);
        Assert.assertEquals(facade.getMethod("startContractRecording", String.class, String[].class).getReturnType(), facade);
        Assert.assertEquals(facade.getMethod("assertContract", String.class, String[].class).getReturnType(), facade);
        Assert.assertEquals(facade.getMethod("verifyContract", String.class, String[].class).getReturnType(), facade);
        Assert.assertEquals(facade.getMethod("replayContract", String.class).getReturnType(), facade);
        Assert.assertEquals(facade.getMethod("routeFromHar", String.class).getReturnType(), facade);
        Assert.assertTrue(facade.getMethod("startContractRecording", String.class, String[].class).isVarArgs());
    }

    private PlaywrightSession livePlaywrightSession() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(page.isClosed()).thenReturn(false);
        return session;
    }

    private Path emptyContract() throws Exception {
        Path contract = Files.createTempFile("shaft-network-contract", ".json");
        com.shaft.tools.io.internal.HttpContractRecorder.startRecording(contract.toString());
        com.shaft.tools.io.internal.HttpContractRecorder.stopRecording();
        contract.toFile().deleteOnExit();
        return contract;
    }

    private boolean compiles(String name, String source) throws Exception {
        Path output = Files.createTempDirectory("shaft-api-compat");
        output.toFile().deleteOnExit();
        var compiler = ToolProvider.getSystemJavaCompiler();
        var sourceFile = new SimpleJavaFileObject(URI.create("string:///" + name + ".java"),
                javax.tools.JavaFileObject.Kind.SOURCE) {
            @Override
            public CharSequence getCharContent(boolean ignoreEncodingErrors) {
                return source;
            }
        };
        return Boolean.TRUE.equals(compiler.getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, List.of(sourceFile)).call());
    }

    private void stubLiveDevTools(WebDriver driver) {
        Mockito.when(((HasDevTools) driver).maybeGetDevTools())
                .thenReturn(Optional.of(Mockito.mock(DevTools.class)));
        Mockito.when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn(new byte[0]);
    }
}
