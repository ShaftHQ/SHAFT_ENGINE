package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.properties.internal.Properties;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.openqa.selenium.*;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.devtools.NetworkInterceptor;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.Filter;
import org.openqa.selenium.remote.http.HttpHandler;
import org.openqa.selenium.remote.http.HttpMethod;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@Test(singleThreaded = true)
public class BrowserActionsCoverageUnitTest {
    private WebDriver driver;
    private WebDriver.Options options;
    private WebDriver.Window window;
    private WebDriver.Navigation navigation;
    private WebDriver.TargetLocator targetLocator;
    private BrowserActions browserActions;

    @BeforeMethod(alwaysRun = true)
    public void setUp() {
        SHAFT.Properties.timeouts.set().waitForLazyLoading(false);
        SHAFT.Properties.flags.set().forceCheckNavigationWasSuccessful(false);
        SHAFT.Properties.web.set().baseURL("");
        SHAFT.Properties.platform.set().executionAddress("local");
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("VIEWPORT");
        SHAFT.Properties.visuals.set().screenshotParamsWatermark(false);

        driver = mock(WebDriver.class, Mockito.withSettings().extraInterfaces(JavascriptExecutor.class, TakesScreenshot.class, HasAuthentication.class));
        options = mock(WebDriver.Options.class);
        window = mock(WebDriver.Window.class);
        navigation = mock(WebDriver.Navigation.class);
        targetLocator = mock(WebDriver.TargetLocator.class);

        when(driver.manage()).thenReturn(options);
        when(options.window()).thenReturn(window);
        when(driver.navigate()).thenReturn(navigation);
        when(driver.switchTo()).thenReturn(targetLocator);
        when(targetLocator.newWindow(any(WindowType.class))).thenReturn(driver);
        when(targetLocator.window(anyString())).thenReturn(driver);

        when(driver.getTitle()).thenReturn("Example title");
        when(driver.getPageSource()).thenReturn("<html><body>example</body></html>");
        when(driver.getWindowHandle()).thenReturn("window-1");
        when(driver.getWindowHandles()).thenReturn(Set.of("window-1", "window-2"));

        when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn("img".getBytes());

        when(window.getPosition()).thenReturn(new Point(0, 0));
        when(window.getSize()).thenReturn(new Dimension(1200, 800));

        Cookie cookie = new Cookie("key", "value", "example.com", "/", null);
        when(options.getCookieNamed("key")).thenReturn(cookie);
        when(options.getCookies()).thenReturn(Set.of(cookie));

        doNothing().when(navigation).to(anyString());
        doNothing().when(navigation).back();
        doNothing().when(navigation).forward();
        doNothing().when(navigation).refresh();
        doNothing().when(window).setPosition(any(Point.class));
        doNothing().when(window).setSize(any(Dimension.class));
        doNothing().when(window).fullscreen();

        browserActions = new BrowserActions(driver, true);
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void shouldCoverBasicBrowserStateMethods() {
        Assert.assertSame(browserActions.and(), browserActions);
        Assert.assertNotNull(browserActions.assertThat());
        Assert.assertNotNull(browserActions.verifyThat());

        when(driver.getCurrentUrl()).thenReturn("https://example.com/home");
        Assert.assertEquals(browserActions.getCurrentURL(), "https://example.com/home");
        Assert.assertEquals(browserActions.getCurrentWindowTitle(), "Example title");
        Assert.assertTrue(browserActions.getPageSource().contains("example"));
        Assert.assertEquals(browserActions.getWindowHandle(), "window-1");
        Assert.assertEquals(browserActions.getWindowPosition(), "(0, 0)");
        Assert.assertEquals(browserActions.getWindowSize(), "(1200, 800)");
        Assert.assertEquals(browserActions.getWindowHeight(), "800");
        Assert.assertEquals(browserActions.getWindowWidth(), "1200");
        List<String> windowHandles = browserActions.getWindowHandles();
        Assert.assertEquals(windowHandles.size(), 2);
        Assert.assertTrue(windowHandles.contains("window-1"));
        Assert.assertTrue(windowHandles.contains("window-2"));
        Assert.assertNotNull(browserActions.accessibility());
    }

    @Test
    public void shouldCoverNavigationAndWindowControlMethods() {
        when(driver.getCurrentUrl()).thenReturn("https://example.com/start", "https://example.com/target");
        Mockito.doReturn("window-1", "window-2", "window-2", "window-3").when(driver).getWindowHandle();

        browserActions.navigateToURL("https://example.com/target", "https://example.com/target");
        Assert.assertThrows(RuntimeException.class,
                () -> browserActions.navigateToURL("https://example.com/new-tab", WindowType.TAB));
        Assert.assertThrows(RuntimeException.class,
                () -> browserActions.navigateToURL("https://example.com/new-window", WindowType.WINDOW));

        when(driver.getCurrentUrl()).thenReturn("https://example.com/page2", "https://example.com/page1");
        browserActions.navigateBack();
        when(driver.getCurrentUrl()).thenReturn("https://example.com/page1", "https://example.com/page2");
        browserActions.navigateForward();
        browserActions.refreshCurrentPage();

        browserActions.setWindowSize(1280, 720);
        browserActions.maximizeWindow();
        browserActions.fullScreenWindow();

        browserActions.switchToWindow("window-1");
        browserActions.waitForLazyLoading();
        browserActions.closeCurrentWindow();
    }

    @Test
    public void navigateShouldFailOnBrowserErrorPageWhenForcedNavigationCheckIsEnabled() {
        SHAFT.Properties.flags.set().forceCheckNavigationWasSuccessful(true);
        when(driver.getCurrentUrl()).thenReturn("https://example.com/start", "https://example.com/down");
        when(driver.getPageSource()).thenReturn("This site can’t be reached");

        Assert.assertThrows(RuntimeException.class,
                () -> browserActions.navigateToURL("https://example.com/down", "https://example.com/down"));
    }

    @Test
    public void shouldCoverCookieAndCaptureMethods() {
        browserActions.addCookie("key", "value");
        Assert.assertNotNull(browserActions.getCookie("key"));
        Assert.assertEquals(browserActions.getCookieDomain("key"), "example.com");
        Assert.assertEquals(browserActions.getCookieValue("key"), "value");
        Assert.assertEquals(browserActions.getCookiePath("key"), "/");
        Assert.assertEquals(browserActions.getAllCookies().size(), 1);

        browserActions.deleteCookie("key");
        browserActions.deleteAllCookies();

        browserActions.capturePageSnapshot();
        browserActions.captureSnapshot();
        browserActions.captureScreenshot();
        browserActions.captureScreenshot(com.shaft.enums.internal.Screenshots.VIEWPORT);

        browserActions.navigateToURLWithBasicAuthentication(
                "https://username:password@example.com/secure",
                "username",
                "password",
                "https://example.com/secure");
    }

    @Test
    public void shouldCoverConstructorsAndExtraControlPaths() {
        BrowserActions browserActionsWithDriver = new BrowserActions(driver);
        BrowserActions browserActionsWithHelper = new BrowserActions(new DriverFactoryHelper(driver));

        when(driver.getCurrentUrl()).thenReturn("https://example.com/path/");
        browserActionsWithDriver.navigateToURL("https://example.com/path");
        browserActionsWithHelper.navigateToURL("https://example.com/path");
        SHAFT.Properties.web.set().baseURL("https://example.com");
        when(driver.getCurrentUrl()).thenReturn("https://example.com/start", "https://example.com/relative");
        browserActions.navigateToURL("./relative", "https://example.com/relative");

        SHAFT.Properties.platform.set().executionAddress("remote");
        SHAFT.Properties.web.set().headlessExecution(true);
        Mockito.doReturn(
                new Dimension(1200, 800),
                new Dimension(1920, 1080),
                new Dimension(1920, 1080)).when(window).getSize();
        browserActions.fullScreenWindow();

        SHAFT.Properties.platform.set().executionAddress("remote-grid");
        browserActions.navigateToURLWithBasicAuthentication(
                "https://example.com/secure",
                "user",
                "pass",
                "https://example.com/secure");

        browserActions.generateLightHouseReport();
    }

    @Test
    public void shouldCoverInterceptionAndMobileContextPaths() {
        HttpResponse mockedResponse = new HttpResponse();
        mockedResponse.setStatus(200);
        Assert.assertThrows(RuntimeException.class,
                () -> browserActions.mock(request -> true, mockedResponse));
        Assert.assertThrows(RuntimeException.class,
                () -> browserActions.intercept(request -> true, mockedResponse));
        Assert.assertThrows(RuntimeException.class,
                () -> browserActions.interceptRequest().get().urlContains("/api").respond().statusCode(200).perform());

        AndroidDriver androidDriver = mock(AndroidDriver.class, Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        when(((TakesScreenshot) androidDriver).getScreenshotAs(OutputType.BYTES)).thenReturn("img".getBytes());
        when(androidDriver.getContext()).thenReturn("NATIVE_APP");
        when(androidDriver.getContextHandles()).thenReturn(Set.of("NATIVE_APP", "WEBVIEW_1"));

        BrowserActions mobileBrowserActions = new BrowserActions(androidDriver, true);
        Assert.assertEquals(mobileBrowserActions.getContext(), "NATIVE_APP");
        mobileBrowserActions.setContext("WEBVIEW_1");
        Assert.assertEquals(mobileBrowserActions.getContextHandles().size(), 2);
    }

    @Test
    public void shouldMockMatchingRequestsWithInterceptionBuilder() {
        AtomicReference<Filter> filterReference = new AtomicReference<>();

        try (MockedConstruction<NetworkInterceptor> ignored = Mockito.mockConstruction(NetworkInterceptor.class,
                (mock, context) -> filterReference.set((Filter) context.arguments().get(1)))) {
            BrowserActions interceptingBrowserActions = new BrowserActions(createInterceptableDriver(), true);
            interceptingBrowserActions.interceptRequest()
                    .get()
                    .urlContains("/api/users")
                    .queryParam("role", "admin")
                    .header("X-Test", "yes")
                    .bodyContains("needle")
                    .respond()
                    .statusCode(201)
                    .jsonBody("{\"ok\":true}")
                    .perform();

            HttpRequest matchingRequest = new HttpRequest(HttpMethod.GET, "https://example.com/api/users?role=admin");
            matchingRequest.addHeader("X-Test", "yes");
            matchingRequest.setContent(Contents.utf8String("needle body"));

            HttpHandler fallback = request -> new HttpResponse().setStatus(599);
            HttpResponse mockedResponse = filterReference.get().apply(fallback).execute(matchingRequest);

            Assert.assertEquals(mockedResponse.getStatus(), 201);
            Assert.assertEquals(mockedResponse.getHeader("Content-Type"), "application/json");
            Assert.assertTrue(mockedResponse.contentAsString().contains("\"ok\":true"));

            HttpResponse realResponse = filterReference.get().apply(fallback)
                    .execute(new HttpRequest(HttpMethod.GET, "https://example.com/api/users?role=user"));
            Assert.assertEquals(realResponse.getStatus(), 599);
        }
    }

    @Test
    public void shouldValidateMatchingRealResponsesWithShaftValidations() {
        AtomicReference<Filter> filterReference = new AtomicReference<>();
        AtomicBoolean validationCalled = new AtomicBoolean(false);

        try (MockedConstruction<NetworkInterceptor> ignored = Mockito.mockConstruction(NetworkInterceptor.class,
                (mock, context) -> filterReference.set((Filter) context.arguments().get(1)))) {
            BrowserActions interceptingBrowserActions = new BrowserActions(createInterceptableDriver(), true);
            interceptingBrowserActions.interceptRequest()
                    .get()
                    .pathEquals("/api/status")
                    .assertResponse(response -> {
                        validationCalled.set(true);
                        response.extractedJsonValue("ok").isEqualTo("true").perform();
                    });

            HttpResponse realResponse = new HttpResponse()
                    .setStatus(200)
                    .addHeader("Content-Type", "application/json");
            realResponse.setContent(Contents.utf8String("{\"ok\":true}"));

            HttpResponse returnedResponse = filterReference.get()
                    .apply(request -> realResponse)
                    .execute(new HttpRequest(HttpMethod.GET, "https://example.com/api/status"));

            Assert.assertSame(returnedResponse, realResponse);
            Assert.assertEquals(returnedResponse.contentAsString(), "{\"ok\":true}");
            Assert.assertTrue(validationCalled.get());
        }
    }

    @Test
    public void shouldPreferLatestMatchingRuleAndClearInterceptors() throws Exception {
        List<Filter> filters = new ArrayList<>();

        try (MockedConstruction<NetworkInterceptor> construction = Mockito.mockConstruction(NetworkInterceptor.class,
                (mock, context) -> filters.add((Filter) context.arguments().get(1)))) {
            BrowserActions interceptingBrowserActions = new BrowserActions(createInterceptableDriver(), true);
            HttpResponse legacyResponse = new HttpResponse().setStatus(200);

            interceptingBrowserActions.mock(request -> request.getUri().contains("/api/users"), legacyResponse);
            interceptingBrowserActions.interceptRequest()
                    .urlContains("/api/users")
                    .respond()
                    .statusCode(418)
                    .body("latest")
                    .perform();

            HttpResponse response = filters.get(filters.size() - 1)
                    .apply(request -> new HttpResponse().setStatus(599))
                    .execute(new HttpRequest(HttpMethod.GET, "https://example.com/api/users"));

            Assert.assertEquals(response.getStatus(), 418);
            Assert.assertEquals(response.contentAsString(), "latest");
            Mockito.verify(construction.constructed().get(0)).close();

            interceptingBrowserActions.clearNetworkInterceptors();
            Mockito.verify(construction.constructed().get(1)).close();
        }
    }

    @Test
    public void shouldCoverFailureBranchesWithExpectedExceptions() {
        WebDriver failingDriver = mock(WebDriver.class, Mockito.withSettings().extraInterfaces(JavascriptExecutor.class, TakesScreenshot.class));
        when(((TakesScreenshot) failingDriver).getScreenshotAs(OutputType.BYTES)).thenReturn("img".getBytes());
        when(failingDriver.getCurrentUrl()).thenThrow(new RuntimeException("forced"));
        when(failingDriver.getTitle()).thenThrow(new RuntimeException("forced"));
        when(failingDriver.getWindowHandle()).thenThrow(new RuntimeException("forced"));
        when(failingDriver.manage()).thenThrow(new RuntimeException("forced"));

        BrowserActions failingBrowserActions = new BrowserActions(failingDriver, true);
        Assert.assertThrows(RuntimeException.class, failingBrowserActions::getCurrentURL);
        Assert.assertThrows(RuntimeException.class, failingBrowserActions::getCurrentWindowTitle);
        Assert.assertThrows(RuntimeException.class, failingBrowserActions::getWindowHandle);
        Assert.assertThrows(RuntimeException.class, failingBrowserActions::getWindowPosition);
        Assert.assertThrows(RuntimeException.class, failingBrowserActions::getWindowSize);
        Assert.assertThrows(RuntimeException.class, failingBrowserActions::getWindowHeight);
        Assert.assertThrows(RuntimeException.class, failingBrowserActions::getWindowWidth);

        WebDriver nullCookieDriver = mock(WebDriver.class, Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        WebDriver.Options nullCookieOptions = mock(WebDriver.Options.class);
        when(((TakesScreenshot) nullCookieDriver).getScreenshotAs(OutputType.BYTES)).thenReturn("img".getBytes());
        when(nullCookieDriver.manage()).thenReturn(nullCookieOptions);
        when(nullCookieOptions.getCookieNamed("missing")).thenReturn(null);
        BrowserActions nullCookieBrowserActions = new BrowserActions(nullCookieDriver, true);
        Assert.assertThrows(RuntimeException.class, () -> nullCookieBrowserActions.getCookie("missing"));

        Assert.assertThrows(RuntimeException.class, () -> browserActions.switchToWindow("missing-window-handle"));
    }

    @Test
    public void shouldCoverIOSAndUnsupportedContextBranches() {
        IOSDriver iosDriver = mock(IOSDriver.class, Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        when(((TakesScreenshot) iosDriver).getScreenshotAs(OutputType.BYTES)).thenReturn("img".getBytes());
        when(iosDriver.getContext()).thenReturn("NATIVE_APP");
        when(iosDriver.getContextHandles()).thenReturn(Set.of("NATIVE_APP", "WEBVIEW_1"));

        BrowserActions iosBrowserActions = new BrowserActions(iosDriver, true);
        Assert.assertEquals(iosBrowserActions.getContext(), "NATIVE_APP");
        iosBrowserActions.setContext("WEBVIEW_1");
        Assert.assertEquals(iosBrowserActions.getContextHandles().size(), 2);

        Assert.assertThrows(AssertionError.class, browserActions::getContext);
        Assert.assertThrows(AssertionError.class, () -> browserActions.setContext("WEBVIEW_1"));
        Assert.assertThrows(AssertionError.class, browserActions::getContextHandles);
    }

    private WebDriver createInterceptableDriver() {
        WebDriver interceptableDriver = mock(WebDriver.class, Mockito.withSettings()
                .extraInterfaces(JavascriptExecutor.class, TakesScreenshot.class, HasAuthentication.class, HasDevTools.class));
        when(((TakesScreenshot) interceptableDriver).getScreenshotAs(OutputType.BYTES)).thenReturn("img".getBytes());
        return interceptableDriver;
    }
}
