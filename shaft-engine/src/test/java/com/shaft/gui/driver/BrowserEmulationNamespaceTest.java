package com.shaft.gui.driver;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Playwright;
import com.microsoft.playwright.options.ColorScheme;
import com.microsoft.playwright.options.Geolocation;
import com.microsoft.playwright.options.Media;
import com.microsoft.playwright.options.ReducedMotion;
import com.microsoft.playwright.options.ViewportSize;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.gui.playwright.internal.PlaywrightTraceManager;
import com.shaft.driver.SHAFT;
import com.shaft.gui.capabilities.AutomationCapabilities;
import com.shaft.gui.capabilities.AutomationFeature;
import com.shaft.gui.capabilities.internal.AutomationCapabilityResolver;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.HasCapabilities;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.bidi.BiDi;
import org.openqa.selenium.bidi.Command;
import org.openqa.selenium.bidi.HasBiDi;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.remote.SessionId;
import io.appium.java_client.AppiumDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.Arrays;
import java.util.Set;
import java.lang.reflect.Method;
import java.util.stream.Collectors;

@SuppressWarnings("PMD.AvoidAccessibilityAlteration") // Private constructors are exercised as compatibility fixtures.
public class BrowserEmulationNamespaceTest {
    private final ThreadLocal<TracePolicy> tracePolicy = new ThreadLocal<>();

    @BeforeMethod(alwaysRun = true)
    public void isolateTracePolicy() {
        tracePolicy.set(new TracePolicy(SHAFT.Properties.reporting.traceEnabled(),
                SHAFT.Properties.reporting.traceIncludeNetwork()));
        SHAFT.Properties.reporting.set().traceEnabled(false).traceIncludeNetwork(false);
    }

    @AfterMethod(alwaysRun = true)
    public void restoreTracePolicy() {
        TracePolicy saved = tracePolicy.get();
        tracePolicy.remove();
        if (saved != null) {
            SHAFT.Properties.reporting.set().traceEnabled(saved.enabled()).traceIncludeNetwork(saved.network());
        }
    }

    @Test
    public void tracePolicyIsolationShouldRestoreTheCallingThread() {
        boolean outerEnabled = SHAFT.Properties.reporting.traceEnabled();
        boolean outerNetwork = SHAFT.Properties.reporting.traceIncludeNetwork();
        BrowserEmulationNamespaceTest lifecycle = new BrowserEmulationNamespaceTest();
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
            lifecycle.isolateTracePolicy();
            Assert.assertFalse(SHAFT.Properties.reporting.traceEnabled());
            Assert.assertFalse(SHAFT.Properties.reporting.traceIncludeNetwork());
            lifecycle.restoreTracePolicy();
            Assert.assertTrue(SHAFT.Properties.reporting.traceEnabled());
            Assert.assertTrue(SHAFT.Properties.reporting.traceIncludeNetwork());
        } finally {
            SHAFT.Properties.reporting.set().traceEnabled(outerEnabled).traceIncludeNetwork(outerNetwork);
        }
    }

    @Test
    public void playwrightContextCreationEmulationPropertiesShouldBeDiscoverable() {
        Set<String> propertyGetters = Arrays.stream(com.shaft.properties.internal.Playwright.class.getMethods())
                .filter(method -> method.getParameterCount() == 0)
                .map(method -> method.getName() + "->" + method.getReturnType().getSimpleName())
                .collect(Collectors.toSet());
        Set<String> propertySetters = Arrays.stream(
                        com.shaft.properties.internal.Playwright.PlaywrightSetProperty.class.getMethods())
                .filter(method -> method.getDeclaringClass()
                        == com.shaft.properties.internal.Playwright.PlaywrightSetProperty.class)
                .map(method -> method.getName() + Arrays.toString(method.getParameterTypes()))
                .collect(Collectors.toSet());

        Assert.assertTrue(propertyGetters.containsAll(Set.of(
                "timezoneId->String", "locale->String", "userAgent->String", "javaScriptEnabled->boolean")));
        Assert.assertTrue(propertySetters.containsAll(Set.of(
                "timezoneId[class java.lang.String]", "locale[class java.lang.String]",
                "userAgent[class java.lang.String]", "javaScriptEnabled[boolean]")));
    }

    @Test
    public void emulationCapabilitiesShouldBeGranularEnoughToFailClosedPerOperation() throws Exception {
        Assert.assertTrue(enumValues("com.shaft.gui.capabilities.AutomationFeature").containsAll(Set.of(
                "VIEWPORT_EMULATION", "SCREEN_EMULATION", "GEOLOCATION_EMULATION",
                "TIMEZONE_EMULATION", "LOCALE_EMULATION", "MEDIA_EMULATION",
                "USER_AGENT_EMULATION", "SCRIPTING_EMULATION")));
    }

    @Test
    public void liveCapabilitySnapshotsShouldMatchExecutableEmulationOperations() {
        MutableCapabilities negotiatedBiDi = new MutableCapabilities();
        negotiatedBiDi.setCapability("webSocketUrl", "ws://bidi.example.test/session");
        WebDriver bidiDriver = Mockito.mock(WebDriver.class, Mockito.withSettings()
                .extraInterfaces(HasBiDi.class, HasCapabilities.class));
        BiDi bidi = Mockito.mock(BiDi.class);
        Mockito.when(((HasBiDi) bidiDriver).maybeGetBiDi()).thenReturn(Optional.of(bidi));
        Mockito.when(((HasCapabilities) bidiDriver).getCapabilities()).thenReturn(negotiatedBiDi);
        AutomationCapabilities bidiCapabilities = AutomationCapabilityResolver.forWebDriver(bidiDriver);
        Assert.assertTrue(bidiCapabilities.supports(AutomationFeature.SCREEN_EMULATION));
        Assert.assertTrue(bidiCapabilities.supports(AutomationFeature.GEOLOCATION_EMULATION));
        Assert.assertTrue(bidiCapabilities.supports(AutomationFeature.TIMEZONE_EMULATION));
        Assert.assertTrue(bidiCapabilities.supports(AutomationFeature.LOCALE_EMULATION));
        Assert.assertTrue(bidiCapabilities.supports(AutomationFeature.USER_AGENT_EMULATION));
        Assert.assertTrue(bidiCapabilities.supports(AutomationFeature.SCRIPTING_EMULATION));
        Assert.assertFalse(bidiCapabilities.supports(AutomationFeature.VIEWPORT_EMULATION));
        Assert.assertFalse(bidiCapabilities.supports(AutomationFeature.MEDIA_EMULATION));

        WebDriver cdpDriver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasDevTools.class));
        DevTools devTools = Mockito.mock(DevTools.class);
        Mockito.when(((HasDevTools) cdpDriver).maybeGetDevTools()).thenReturn(Optional.of(devTools));
        AutomationCapabilities cdpCapabilities = AutomationCapabilityResolver.forWebDriver(cdpDriver);
        Assert.assertTrue(cdpCapabilities.supports(AutomationFeature.VIEWPORT_EMULATION));
        Assert.assertTrue(cdpCapabilities.supports(AutomationFeature.MEDIA_EMULATION));
        Assert.assertFalse(cdpCapabilities.supports(AutomationFeature.SCREEN_EMULATION));

        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        PlaywrightSession playwrightSession = Mockito.mock(PlaywrightSession.class);
        Mockito.when(playwrightSession.browser()).thenReturn(browser);
        Mockito.when(playwrightSession.browserContext()).thenReturn(context);
        Mockito.when(playwrightSession.page()).thenReturn(page);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(context.isClosed()).thenReturn(false);
        Mockito.when(page.isClosed()).thenReturn(false);
        AutomationCapabilities playwrightCapabilities = AutomationCapabilityResolver.forPlaywright(playwrightSession);
        Assert.assertTrue(playwrightCapabilities.supports(AutomationFeature.VIEWPORT_EMULATION));
        Assert.assertTrue(playwrightCapabilities.supports(AutomationFeature.MEDIA_EMULATION));
        Assert.assertTrue(playwrightCapabilities.supports(AutomationFeature.GEOLOCATION_EMULATION));
        Assert.assertFalse(playwrightCapabilities.supports(AutomationFeature.TIMEZONE_EMULATION));
        Assert.assertFalse(playwrightCapabilities.supports(AutomationFeature.USER_AGENT_EMULATION));
    }

    @Test
    public void closedRemoteSessionsShouldNeverAdvertiseEmulationAndLiveAppiumBiDiShould() {
        MutableCapabilities negotiated = new MutableCapabilities();
        negotiated.setCapability("webSocketUrl", "ws://bidi.example.test/session");
        BiDi bidi = Mockito.mock(BiDi.class);
        DevTools devTools = Mockito.mock(DevTools.class);
        RemoteWebDriver closed = Mockito.mock(RemoteWebDriver.class,
                Mockito.withSettings().extraInterfaces(HasBiDi.class, HasDevTools.class));
        Mockito.when(closed.getCapabilities()).thenReturn(negotiated);
        Mockito.when(closed.getSessionId()).thenReturn(null);
        Mockito.when(((HasBiDi) closed).maybeGetBiDi()).thenReturn(Optional.of(bidi));
        Mockito.when(((HasDevTools) closed).maybeGetDevTools()).thenReturn(Optional.of(devTools));
        AutomationCapabilities closedCapabilities = AutomationCapabilityResolver.forWebDriver(closed);
        Assert.assertFalse(closedCapabilities.supports(AutomationFeature.SCREEN_EMULATION));
        Assert.assertFalse(closedCapabilities.supports(AutomationFeature.VIEWPORT_EMULATION));
        Assert.assertFalse(closedCapabilities.supports(AutomationFeature.MEDIA_EMULATION));

        AppiumDriver appium = Mockito.mock(AppiumDriver.class);
        Mockito.when(appium.getSessionId()).thenReturn(new SessionId("appium-emulation"));
        Mockito.when(appium.getCapabilities()).thenReturn(negotiated);
        Mockito.when(appium.maybeGetBiDi()).thenReturn(Optional.of(bidi));
        AutomationCapabilities appiumCapabilities = AutomationCapabilityResolver.forWebDriver(appium);
        Assert.assertTrue(appiumCapabilities.supports(AutomationFeature.GEOLOCATION_EMULATION));
        Assert.assertTrue(appiumCapabilities.supports(AutomationFeature.LOCALE_EMULATION));
        Assert.assertFalse(appiumCapabilities.supports(AutomationFeature.VIEWPORT_EMULATION));
    }

    @Test
    public void runtimeActionsShouldRequireTheSameNegotiatedBiDiEvidenceAsCapabilities() {
        MutableCapabilities missingWebSocket = new MutableCapabilities();
        BiDi bidi = Mockito.mock(BiDi.class);
        WebDriver selenium = Mockito.mock(WebDriver.class, Mockito.withSettings()
                .extraInterfaces(HasBiDi.class, HasCapabilities.class));
        Mockito.when(((HasCapabilities) selenium).getCapabilities()).thenReturn(missingWebSocket);
        Mockito.when(((HasBiDi) selenium).maybeGetBiDi()).thenReturn(Optional.of(bidi));
        Mockito.when(((HasBiDi) selenium).getBiDi()).thenReturn(bidi);
        Mockito.when(selenium.getWindowHandle()).thenReturn("context-1");
        Assert.assertFalse(AutomationCapabilityResolver.forWebDriver(selenium)
                .supports(AutomationFeature.GEOLOCATION_EMULATION));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(selenium, true)
                        .emulation().location().clearGeolocation());

        AppiumDriver appium = Mockito.mock(AppiumDriver.class);
        Mockito.when(appium.getSessionId()).thenReturn(new SessionId("appium-without-websocket"));
        Mockito.when(appium.getCapabilities()).thenReturn(missingWebSocket);
        Mockito.when(appium.maybeGetBiDi()).thenReturn(Optional.of(bidi));
        Mockito.when(appium.getBiDi()).thenReturn(bidi);
        Mockito.when(appium.getWindowHandle()).thenReturn("WEBVIEW_1");
        Assert.assertFalse(AutomationCapabilityResolver.forWebDriver(appium)
                .supports(AutomationFeature.GEOLOCATION_EMULATION));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(appium, true)
                        .emulation().location().clearGeolocation());
    }

    @Test
    public void runtimeContractShouldExposeProtocolAccurateScriptingActions() throws Exception {
        Set<String> runtimeDescriptors = descriptors("com.shaft.gui.driver.RuntimeEmulationActionsContract");
        Assert.assertTrue(runtimeDescriptors.contains(
                "disableScripting[]->RuntimeEmulationActionsContract"));
        Assert.assertFalse(runtimeDescriptors.contains(
                "scriptingEnabled[boolean]->RuntimeEmulationActionsContract"));

        Method disableScripting = Arrays.stream(RuntimeEmulationActionsContract.class.getMethods())
                .filter(method -> method.getName().equals("disableScripting") && method.getParameterCount() == 0)
                .findFirst()
                .orElseThrow();
        MutableCapabilities negotiated = new MutableCapabilities();
        negotiated.setCapability("webSocketUrl", "ws://bidi.example.test/session");
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings()
                .extraInterfaces(HasBiDi.class, HasCapabilities.class));
        BiDi bidi = Mockito.mock(BiDi.class);
        Mockito.when(((HasCapabilities) driver).getCapabilities()).thenReturn(negotiated);
        Mockito.when(((HasBiDi) driver).maybeGetBiDi()).thenReturn(Optional.of(bidi));
        Mockito.when(((HasBiDi) driver).getBiDi()).thenReturn(bidi);
        Mockito.when(driver.getWindowHandle()).thenReturn("context-1");

        disableScripting.invoke(new com.shaft.gui.browser.BrowserActions(driver, true).emulation().runtime());

        ArgumentCaptor<Command> command = ArgumentCaptor.forClass(Command.class);
        Mockito.verify(bidi).send(command.capture());
        Assert.assertEquals(command.getValue().getMethod(), "emulation.setScriptingEnabled");
        Assert.assertEquals(command.getValue().getParams().get("enabled"), Boolean.FALSE);
    }

    @Test
    public void emulationShouldValidateBeforeMutationAndFailClosedForUnavailableLiveBackends() {
        WebDriver cdpDriver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasDevTools.class));
        DevTools devTools = Mockito.mock(DevTools.class);
        Mockito.when(((HasDevTools) cdpDriver).maybeGetDevTools()).thenReturn(Optional.of(devTools));
        Mockito.when(((HasDevTools) cdpDriver).getDevTools()).thenReturn(devTools);
        EmulationActionsContract cdp = new com.shaft.gui.browser.BrowserActions(cdpDriver, true).emulation();
        Assert.expectThrows(IllegalArgumentException.class, () -> cdp.screen().viewport(0, 600));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> cdp.media().colorScheme(null));
        Mockito.verify(devTools, Mockito.never()).send(Mockito.any(org.openqa.selenium.devtools.Command.class));

        WebDriver bidiDriver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasBiDi.class));
        BiDi bidi = Mockito.mock(BiDi.class);
        Mockito.when(((HasBiDi) bidiDriver).maybeGetBiDi()).thenReturn(Optional.of(bidi));
        Mockito.when(((HasBiDi) bidiDriver).getBiDi()).thenReturn(bidi);
        Mockito.when(bidiDriver.getWindowHandle()).thenReturn("context-1");
        EmulationActionsContract bidiActions = new com.shaft.gui.browser.BrowserActions(bidiDriver, true).emulation();
        Assert.expectThrows(IllegalArgumentException.class,
                () -> bidiActions.location().geolocation(Double.NaN, 31.2));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> bidiActions.location().timezone("not/a/timezone"));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> bidiActions.location().locale("not_a_locale"));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> bidiActions.runtime().userAgent(" "));
        Mockito.verify(bidi, Mockito.never()).send(Mockito.any(Command.class));

        WebDriver plainDriver = Mockito.mock(WebDriver.class);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(plainDriver, true)
                        .emulation().location().clearGeolocation());

        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(context.isClosed()).thenReturn(false);
        Mockito.when(page.isClosed()).thenReturn(false);
        UnsupportedOperationException contextOnly = Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(session)
                        .emulation().runtime().userAgent("custom-agent"));
        Assert.assertTrue(contextOnly.getMessage().contains("playwright.userAgent"));
        Mockito.when(browser.isConnected()).thenReturn(false);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(session)
                        .emulation().screen().viewport(800, 600));
        UnsupportedOperationException disconnectedContext = Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(session)
                        .emulation().location().clearGeolocation());
        Assert.assertTrue(disconnectedContext.getMessage().startsWith("Operation emulation clear geolocation"));
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(context.isClosed()).thenReturn(true);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(session)
                        .emulation().media().type(EmulatedMediaType.PRINT));
        Mockito.verify(session, Mockito.never()).setViewport(Mockito.any(), Mockito.anyInt(), Mockito.anyInt());
        Mockito.verify(session, Mockito.never()).setMediaType(Mockito.any(), Mockito.any());
    }

    @Test
    public void playwrightContextCreationShouldApplyEmulationProperties() throws Exception {
        try {
            SHAFT.Properties.clearForCurrentThread();
            SHAFT.Properties.playwright.set()
                    .timezoneId("Africa/Cairo")
                    .locale("ar-EG")
                    .userAgent("SHAFT-emulation-test")
                    .javaScriptEnabled(false);
            var createContextOptions = Arrays.stream(
                            com.shaft.gui.playwright.internal.PlaywrightSessionFactory.class.getDeclaredMethods())
                    .filter(method -> method.getName().equals("createContextOptions"))
                    .findFirst()
                    .orElseThrow();
            createContextOptions.setAccessible(true);
            Browser.NewContextOptions options = (Browser.NewContextOptions) createContextOptions.invoke(null,
                    new Object[]{null});

            Assert.assertEquals(options.timezoneId, "Africa/Cairo");
            Assert.assertEquals(options.locale, "ar-EG");
            Assert.assertEquals(options.userAgent, "SHAFT-emulation-test");
            Assert.assertEquals(options.javaScriptEnabled, Boolean.FALSE);
        } finally {
            SHAFT.Properties.clearForCurrentThread();
        }
    }

    @Test
    @SuppressWarnings({"rawtypes", "unchecked"})
    public void seleniumBiDiShouldApplyAndClearEveryStandardizedEmulationOverride() {
        MutableCapabilities negotiated = new MutableCapabilities();
        negotiated.setCapability("webSocketUrl", "ws://bidi.example.test/session");
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings()
                .extraInterfaces(HasBiDi.class, HasCapabilities.class));
        BiDi bidi = Mockito.mock(BiDi.class);
        Mockito.when(((HasCapabilities) driver).getCapabilities()).thenReturn(negotiated);
        Mockito.when(((HasBiDi) driver).maybeGetBiDi()).thenReturn(Optional.of(bidi));
        Mockito.when(((HasBiDi) driver).getBiDi()).thenReturn(bidi);
        Mockito.when(driver.getWindowHandle()).thenReturn("context-1");
        EmulationActionsContract emulation = new com.shaft.gui.browser.BrowserActions(driver, true).emulation();

        try {
            emulation.screen().screenSize(1440, 900).clearScreenSize();
            emulation.location()
                    .geolocation(30.0444, 31.2357, 8).clearGeolocation()
                    .timezone("Africa/Cairo").clearTimezone()
                    .locale("ar-EG").clearLocale();
            emulation.runtime()
                    .userAgent("SHAFT-BiDi").clearUserAgent()
                    .disableScripting().clearScriptingOverride();
        } catch (UnsupportedOperationException exception) {
            Assert.fail("Negotiated BiDi emulation must use the standardized protocol commands.", exception);
        }

        ArgumentCaptor<Command> commands = ArgumentCaptor.forClass(Command.class);
        Mockito.verify(bidi, Mockito.times(12)).send(commands.capture());
        Assert.assertEquals(commands.getAllValues().stream().map(Command::getMethod).toList(), List.of(
                "emulation.setScreenSettingsOverride", "emulation.setScreenSettingsOverride",
                "emulation.setGeolocationOverride", "emulation.setGeolocationOverride",
                "emulation.setTimezoneOverride", "emulation.setTimezoneOverride",
                "emulation.setLocaleOverride", "emulation.setLocaleOverride",
                "emulation.setUserAgentOverride", "emulation.setUserAgentOverride",
                "emulation.setScriptingEnabled", "emulation.setScriptingEnabled"));
        Assert.assertEquals(((java.util.Map<?, ?>) commands.getAllValues().get(0).getParams()
                .get("screenArea")).get("width"), 1440);
        Assert.assertNull(commands.getAllValues().get(1).getParams().get("screenArea"));
        Assert.assertEquals(commands.getAllValues().get(6).getParams().get("locale"), "ar-EG");
        Assert.assertNull(commands.getAllValues().get(7).getParams().get("locale"));
    }

    @Test
    @SuppressWarnings({"rawtypes", "unchecked"})
    public void seleniumDevToolsShouldApplyAndResetViewportAndCumulativeMedia() {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasDevTools.class));
        DevTools devTools = Mockito.mock(DevTools.class);
        Mockito.when(((HasDevTools) driver).maybeGetDevTools()).thenReturn(Optional.of(devTools));
        Mockito.when(((HasDevTools) driver).getDevTools()).thenReturn(devTools);
        EmulationActionsContract emulation = new com.shaft.gui.browser.BrowserActions(driver, true).emulation();

        try {
            emulation.screen().viewport(800, 600).clearViewport();
            emulation.media().type(EmulatedMediaType.PRINT)
                    .colorScheme(EmulatedColorScheme.DARK)
                    .reducedMotion(EmulatedReducedMotion.REDUCE)
                    .reset();
        } catch (UnsupportedOperationException exception) {
            Assert.fail("DevTools-capable Selenium sessions must expose viewport and media emulation.", exception);
        }

        ArgumentCaptor<org.openqa.selenium.devtools.Command> commands =
                ArgumentCaptor.forClass(org.openqa.selenium.devtools.Command.class);
        Mockito.verify(devTools, Mockito.times(6)).send(commands.capture());
        Assert.assertEquals(commands.getAllValues().stream()
                .map(org.openqa.selenium.devtools.Command::getMethod).toList(), List.of(
                "Emulation.setDeviceMetricsOverride", "Emulation.clearDeviceMetricsOverride",
                "Emulation.setEmulatedMedia", "Emulation.setEmulatedMedia",
                "Emulation.setEmulatedMedia", "Emulation.setEmulatedMedia"));
        Assert.assertEquals(commands.getAllValues().getFirst().getParams().get("width"), 800);
        Assert.assertEquals(commands.getAllValues().getFirst().getParams().get("height"), 600);
        Assert.assertEquals(commands.getAllValues().get(2).getParams().get("media"), "print");
        Assert.assertEquals(commands.getAllValues().get(3).getParams().get("media"), "print");
        Assert.assertEquals(commands.getAllValues().get(4).getParams().get("media"), "print");
        Assert.assertEquals(((List<?>) commands.getAllValues().get(3).getParams().get("features")).size(), 1);
        Assert.assertEquals(((List<?>) commands.getAllValues().get(4).getParams().get("features")).size(), 2);
        Assert.assertTrue(commands.getAllValues().getLast().getParams().isEmpty());
    }

    @Test
    @SuppressWarnings({"rawtypes", "unchecked"})
    public void seleniumEmulationStateShouldBeDiscardedAtDriverTeardown() throws Exception {
        var cleanup = Arrays.stream(com.shaft.gui.browser.internal.BrowserEmulationManager.class.getMethods())
                .filter(method -> method.getName().equals("clearAndRemove")
                        && Arrays.equals(method.getParameterTypes(), new Class<?>[]{WebDriver.class}))
                .findFirst()
                .orElse(null);
        Assert.assertNotNull(cleanup, "Driver teardown needs a deterministic emulation-state cleanup hook.");

        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasDevTools.class));
        DevTools devTools = Mockito.mock(DevTools.class);
        Mockito.when(((HasDevTools) driver).maybeGetDevTools()).thenReturn(Optional.of(devTools));
        Mockito.when(((HasDevTools) driver).getDevTools()).thenReturn(devTools);
        EmulationActionsContract emulation = new com.shaft.gui.browser.BrowserActions(driver, true).emulation();
        emulation.media().type(EmulatedMediaType.PRINT);
        cleanup.invoke(null, driver);
        emulation.media().colorScheme(EmulatedColorScheme.DARK);

        ArgumentCaptor<org.openqa.selenium.devtools.Command> commands =
                ArgumentCaptor.forClass(org.openqa.selenium.devtools.Command.class);
        Mockito.verify(devTools, Mockito.times(2)).send(commands.capture());
        Assert.assertFalse(commands.getAllValues().getLast().getParams().containsKey("media"));
    }

    @Test
    public void emulationNamespaceShouldExposeOneCompactCategorizedContract() throws Exception {
        boolean discoverable = Arrays.stream(BrowserActionsContract.class.getMethods())
                .anyMatch(method -> method.getName().equals("emulation")
                        && method.getParameterCount() == 0
                        && method.getReturnType().getSimpleName().equals("EmulationActionsContract"));

        Assert.assertTrue(discoverable);
        Assert.assertTrue(BrowserActionsContract.class.getMethod("emulation").isDefault());
        Assert.assertEquals(Class.forName("com.shaft.gui.browser.BrowserActions")
                .getDeclaredMethod("emulation").getReturnType().getSimpleName(), "EmulationActions");
        Assert.assertEquals(Class.forName("com.shaft.gui.playwright.browser.BrowserActions")
                .getDeclaredMethod("emulation").getReturnType().getSimpleName(), "EmulationActions");
        Assert.assertEquals(descriptors("com.shaft.gui.driver.EmulationActionsContract"), Set.of(
                "and[]->BrowserActionsContract",
                "location[]->LocationEmulationActionsContract",
                "media[]->MediaEmulationActionsContract",
                "runtime[]->RuntimeEmulationActionsContract",
                "screen[]->ScreenEmulationActionsContract"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.ScreenEmulationActionsContract"), Set.of(
                "and[]->EmulationActionsContract",
                "clearScreenSize[]->ScreenEmulationActionsContract",
                "clearViewport[]->ScreenEmulationActionsContract",
                "screenSize[int, int]->ScreenEmulationActionsContract",
                "viewport[int, int]->ScreenEmulationActionsContract"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.LocationEmulationActionsContract"), Set.of(
                "and[]->EmulationActionsContract",
                "clearGeolocation[]->LocationEmulationActionsContract",
                "clearLocale[]->LocationEmulationActionsContract",
                "clearTimezone[]->LocationEmulationActionsContract",
                "geolocation[double, double, double]->LocationEmulationActionsContract",
                "geolocation[double, double]->LocationEmulationActionsContract",
                "locale[class java.lang.String]->LocationEmulationActionsContract",
                "timezone[class java.lang.String]->LocationEmulationActionsContract"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.MediaEmulationActionsContract"), Set.of(
                "and[]->EmulationActionsContract",
                "colorScheme[class com.shaft.gui.driver.EmulatedColorScheme]->MediaEmulationActionsContract",
                "reducedMotion[class com.shaft.gui.driver.EmulatedReducedMotion]->MediaEmulationActionsContract",
                "reset[]->MediaEmulationActionsContract",
                "type[class com.shaft.gui.driver.EmulatedMediaType]->MediaEmulationActionsContract"));
        Assert.assertEquals(descriptors("com.shaft.gui.driver.RuntimeEmulationActionsContract"), Set.of(
                "and[]->EmulationActionsContract",
                "clearScriptingOverride[]->RuntimeEmulationActionsContract",
                "clearUserAgent[]->RuntimeEmulationActionsContract",
                "disableScripting[]->RuntimeEmulationActionsContract",
                "userAgent[class java.lang.String]->RuntimeEmulationActionsContract"));
        Assert.assertEquals(enumValues("com.shaft.gui.driver.EmulatedColorScheme"),
                Set.of("LIGHT", "DARK", "NO_PREFERENCE"));
        Assert.assertEquals(enumValues("com.shaft.gui.driver.EmulatedMediaType"), Set.of("SCREEN", "PRINT"));
        Assert.assertEquals(enumValues("com.shaft.gui.driver.EmulatedReducedMotion"),
                Set.of("REDUCE", "NO_PREFERENCE"));
    }

    @Test
    public void emulationDefaultMethodCollisionBoundaryShouldBeExecutableDocumentation() throws Exception {
        String compatible = """
                import com.shaft.gui.driver.*;
                interface LegacyEmulation { default EmulationActionsContract emulation() { return null; } }
                interface CompatibleEmulationFacade extends BrowserActionsContract, LegacyEmulation {
                    @Override default EmulationActionsContract emulation() {
                        return BrowserActionsContract.super.emulation();
                    }
                }
                """;
        String incompatible = """
                import com.shaft.gui.driver.*;
                interface LegacyEmulation { default String emulation() { return "legacy"; } }
                interface IncompatibleEmulationFacade extends BrowserActionsContract, LegacyEmulation {}
                """;

        Assert.assertTrue(compiles("CompatibleEmulationFacade", compatible));
        Assert.assertFalse(compiles("IncompatibleEmulationFacade", incompatible));
    }

    @Test
    public void playwrightShouldApplyAndResetRuntimeMutableEmulation() throws Exception {
        Playwright playwright = Mockito.mock(Playwright.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Mockito.when(context.pages()).thenReturn(List.of(page));
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(page.isClosed()).thenReturn(false);
        Mockito.when(page.viewportSize()).thenReturn(new ViewportSize(1280, 720));
        var constructor = PlaywrightSession.class.getDeclaredConstructor(Playwright.class, Browser.class,
                BrowserContext.class, Page.class, PlaywrightTraceManager.class);
        constructor.setAccessible(true);
        PlaywrightSession session = constructor.newInstance(playwright, browser, context, page, null);
        EmulationActionsContract emulation = new com.shaft.gui.playwright.browser.BrowserActions(session).emulation();

        ScreenEmulationActionsContract screen = emulation.screen();
        Assert.assertSame(screen.viewport(800, 600).clearViewport().and(), emulation);
        LocationEmulationActionsContract location = emulation.location();
        Assert.assertSame(location.geolocation(41.890221, 12.492348, 5).clearGeolocation().and(), emulation);
        MediaEmulationActionsContract media = emulation.media();
        Assert.assertSame(media.type(EmulatedMediaType.PRINT)
                .colorScheme(EmulatedColorScheme.DARK)
                .reducedMotion(EmulatedReducedMotion.REDUCE)
                .reset()
                .and(), emulation);

        var orderedPage = Mockito.inOrder(page);
        orderedPage.verify(page).setViewportSize(800, 600);
        orderedPage.verify(page).setViewportSize(1280, 720);
        ArgumentCaptor<Geolocation> geolocation = ArgumentCaptor.forClass(Geolocation.class);
        Mockito.verify(context, Mockito.times(2)).setGeolocation(geolocation.capture());
        Assert.assertEquals(geolocation.getAllValues().getFirst().latitude, 41.890221);
        Assert.assertEquals(geolocation.getAllValues().getFirst().longitude, 12.492348);
        Assert.assertEquals(geolocation.getAllValues().getFirst().accuracy, 5.0);
        Assert.assertNull(geolocation.getAllValues().getLast());

        ArgumentCaptor<Page.EmulateMediaOptions> mediaOptions = ArgumentCaptor.forClass(Page.EmulateMediaOptions.class);
        Mockito.verify(page, Mockito.times(4)).emulateMedia(mediaOptions.capture());
        Assert.assertEquals(mediaOptions.getAllValues().get(0).media.orElseThrow(), Media.PRINT);
        Assert.assertEquals(mediaOptions.getAllValues().get(1).media.orElseThrow(), Media.PRINT);
        Assert.assertEquals(mediaOptions.getAllValues().get(1).colorScheme.orElseThrow(), ColorScheme.DARK);
        Assert.assertEquals(mediaOptions.getAllValues().get(2).media.orElseThrow(), Media.PRINT);
        Assert.assertEquals(mediaOptions.getAllValues().get(2).colorScheme.orElseThrow(), ColorScheme.DARK);
        Assert.assertEquals(mediaOptions.getAllValues().get(2).reducedMotion.orElseThrow(), ReducedMotion.REDUCE);
        Assert.assertNull(mediaOptions.getAllValues().get(3).media);
        Assert.assertNull(mediaOptions.getAllValues().get(3).colorScheme);
        Assert.assertNull(mediaOptions.getAllValues().get(3).reducedMotion);
    }

    private static Set<String> descriptors(String className) throws Exception {
        return Arrays.stream(Class.forName(className).getDeclaredMethods())
                .map(method -> method.getName() + Arrays.toString(method.getParameterTypes())
                        + "->" + method.getReturnType().getSimpleName())
                .collect(Collectors.toSet());
    }

    private static Set<String> enumValues(String className) throws Exception {
        return Arrays.stream(Class.forName(className).getEnumConstants())
                .map(String::valueOf)
                .collect(Collectors.toSet());
    }

    private static boolean compiles(String typeName, String source) throws Exception {
        Path output = Files.createTempDirectory("shaft-emulation-api-compat");
        output.toFile().deleteOnExit();
        var compiler = ToolProvider.getSystemJavaCompiler();
        var sourceFile = new SimpleJavaFileObject(URI.create("string:///" + typeName + ".java"),
                javax.tools.JavaFileObject.Kind.SOURCE) {
            @Override
            public CharSequence getCharContent(boolean ignoreEncodingErrors) {
                return source;
            }
        };
        return Boolean.TRUE.equals(compiler.getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"),
                        "-d", output.toString()), null,
                List.of(sourceFile)).call());
    }

    private record TracePolicy(boolean enabled, boolean network) { }
}
