package com.shaft.gui.driver;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.gui.capabilities.AutomationFeature;
import com.shaft.gui.capabilities.internal.AutomationCapabilityResolver;
import com.shaft.gui.BidiTestSupport;
import io.appium.java_client.AppiumDriver;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.bidi.BiDi;
import org.openqa.selenium.bidi.Command;
import org.openqa.selenium.bidi.HasBiDi;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;

public class BrowserPermissionsNamespaceTest {
    @Test
    public void permissionsNamespaceShouldBeDiscoverableWithoutAddingFlatMethods() {
        boolean discoverable = Arrays.stream(BrowserActionsContract.class.getMethods())
                .anyMatch(method -> method.getName().equals("permissions")
                        && method.getParameterCount() == 0
                        && method.getReturnType().getSimpleName().equals("PermissionActionsContract"));

        Assert.assertTrue(discoverable);
        try {
            Assert.assertTrue(BrowserActionsContract.class.getMethod("permissions").isDefault());
            Set<String> descriptors = Arrays.stream(PermissionActionsContract.class.getDeclaredMethods())
                    .map(method -> method.getName() + Arrays.toString(method.getParameterTypes())
                            + "->" + method.getReturnType().getSimpleName())
                    .collect(Collectors.toSet());
            Assert.assertEquals(descriptors, Set.of(
                    "and[]->BrowserActionsContract",
                    "grant[class [Ljava.lang.String;]->PermissionActionsContract",
                    "grantFor[class java.lang.String, class [Ljava.lang.String;]->PermissionActionsContract",
                    "denyFor[class java.lang.String, class [Ljava.lang.String;]->PermissionActionsContract",
                    "promptFor[class java.lang.String, class [Ljava.lang.String;]->PermissionActionsContract",
                    "clear[]->PermissionActionsContract"));
        } catch (NoSuchMethodException exception) {
            throw new AssertionError(exception);
        }
    }

    @Test
    public void playwrightShouldGrantGloballyAndForAnOriginThenClear() {
        PlaywrightSession session = livePlaywrightSession();
        BrowserContext context = session.browserContext();
        PermissionActionsContract permissions = new com.shaft.gui.playwright.browser.BrowserActions(session).permissions();

        permissions.grant("geolocation", "notifications")
                .grantFor("https://example.test/", "camera")
                .grantFor("https://münich.example", "microphone")
                .grantFor("https://[2001:db8::1]:8443", "clipboard-read")
                .clear();

        Mockito.verify(context).grantPermissions(List.of("geolocation", "notifications"));
        ArgumentCaptor<BrowserContext.GrantPermissionsOptions> options =
                ArgumentCaptor.forClass(BrowserContext.GrantPermissionsOptions.class);
        Mockito.verify(context).grantPermissions(Mockito.eq(List.of("camera")), options.capture());
        Assert.assertEquals(options.getValue().origin, "https://example.test");
        Mockito.verify(context).grantPermissions(Mockito.eq(List.of("microphone")), options.capture());
        Assert.assertEquals(options.getValue().origin, "https://xn--mnich-kva.example");
        Mockito.verify(context).grantPermissions(Mockito.eq(List.of("clipboard-read")), options.capture());
        Assert.assertEquals(options.getValue().origin, "https://[2001:db8::1]:8443");
        Mockito.verify(context).clearPermissions();
    }

    @Test
    public void playwrightShouldFailExplicitlyForUnsupportedDenyAndInvalidOrigins() {
        PermissionActionsContract permissions = new com.shaft.gui.playwright.browser.BrowserActions(livePlaywrightSession()).permissions();

        Assert.expectThrows(UnsupportedOperationException.class,
                () -> permissions.denyFor("https://example.test", "camera"));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> permissions.grantFor("https://example.test/path", "camera"));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> permissions.grantFor("https://:443", "camera"));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> permissions.grantFor("https://example.test:99999", "camera"));
    }

    @Test
    public void playwrightPermissionsShouldRequireOnlyALiveContextAndConnectedBrowser() {
        PlaywrightSession disconnected = livePlaywrightSession();
        Mockito.when(disconnected.browser().isConnected()).thenReturn(false);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(disconnected).permissions().grant("camera"));

        PlaywrightSession missingContext = livePlaywrightSession();
        Mockito.when(missingContext.browserContext()).thenReturn(null);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(missingContext).permissions().clear());

        PlaywrightSession closedContext = livePlaywrightSession();
        Mockito.when(closedContext.browserContext().isClosed()).thenReturn(true);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(closedContext).permissions().grant("camera"));

        PlaywrightSession noPage = livePlaywrightSession();
        Mockito.when(noPage.page()).thenReturn(null);
        new com.shaft.gui.playwright.browser.BrowserActions(noPage).permissions().grant("camera");
        Mockito.verify(noPage.browserContext()).grantPermissions(List.of("camera"));
        Assert.assertTrue(AutomationCapabilityResolver.forPlaywright(noPage).supports(AutomationFeature.PERMISSIONS));
        Assert.assertFalse(AutomationCapabilityResolver.forPlaywright(noPage).supports(AutomationFeature.SCRIPT_EXECUTION));

        PlaywrightSession closedPage = livePlaywrightSession();
        Mockito.when(closedPage.page().isClosed()).thenReturn(true);
        new com.shaft.gui.playwright.browser.BrowserActions(closedPage).permissions().clear();
        Mockito.verify(closedPage.browserContext()).clearPermissions();
        Assert.assertTrue(AutomationCapabilityResolver.forPlaywright(closedPage).supports(AutomationFeature.PERMISSIONS));
    }

    @Test
    public void permissionDefaultMethodCollisionBoundaryShouldBeExecutableDocumentation() throws Exception {
        String compatible = """
                import com.shaft.gui.driver.*;
                interface LegacyPermissions { default PermissionActionsContract permissions() { return null; } }
                interface CompatiblePermissionsFacade extends BrowserActionsContract, LegacyPermissions {
                    @Override default PermissionActionsContract permissions() {
                        return BrowserActionsContract.super.permissions();
                    }
                }
                """;
        String incompatible = """
                import com.shaft.gui.driver.*;
                interface LegacyPermissions { default String permissions() { return "legacy"; } }
                interface IncompatiblePermissionsFacade extends BrowserActionsContract, LegacyPermissions {}
                """;

        Assert.assertTrue(compiles("CompatiblePermissionsFacade", compatible));
        Assert.assertFalse(compiles("IncompatiblePermissionsFacade", incompatible));
    }

    @Test
    @SuppressWarnings({"unchecked", "rawtypes"})
    public void seleniumBiDiShouldGrantForOriginAndClearBackToPrompt() {
        RemoteWebDriver driver = Mockito.mock(RemoteWebDriver.class,
                Mockito.withSettings().extraInterfaces(HasBiDi.class));
        HasBiDi hasBiDi = (HasBiDi) driver;
        BiDi bidi = Mockito.mock(BiDi.class);
        MutableCapabilities capabilities = new MutableCapabilities();
        capabilities.setCapability("webSocketUrl", "ws://localhost/session");
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("selenium"));
        Mockito.when(driver.getCapabilities()).thenReturn(capabilities);
        BidiTestSupport.connect(hasBiDi, bidi);
        new com.shaft.gui.browser.BrowserActions(driver, true).permissions()
                .grantFor("https://example.test", "geolocation");
        new com.shaft.gui.browser.BrowserActions(driver, true).permissions().clear();

        ArgumentCaptor<Command> command = ArgumentCaptor.forClass(Command.class);
        Mockito.verify(bidi, Mockito.times(2)).send(command.capture());
        Assert.assertEquals(command.getAllValues().get(0).getMethod(), "permissions.setPermission");
        Assert.assertEquals(command.getAllValues().get(0).getParams().get("state"), "granted");
        Assert.assertEquals(command.getAllValues().get(1).getParams().get("state"), "prompt");
        Assert.assertTrue(AutomationCapabilityResolver.forWebDriver(driver).supports(AutomationFeature.PERMISSIONS));
    }

    @Test
    @SuppressWarnings({"unchecked", "rawtypes"})
    public void seleniumGrantAndClearShouldSerializeProviderAndInventoryTransitions() throws Exception {
        RemoteWebDriver driver = Mockito.mock(RemoteWebDriver.class,
                Mockito.withSettings().extraInterfaces(HasBiDi.class));
        HasBiDi hasBiDi = (HasBiDi) driver;
        BiDi bidi = Mockito.mock(BiDi.class);
        MutableCapabilities capabilities = new MutableCapabilities();
        capabilities.setCapability("webSocketUrl", "ws://localhost/session");
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("serialized"));
        Mockito.when(driver.getCapabilities()).thenReturn(capabilities);
        BidiTestSupport.connect(hasBiDi, bidi);
        AtomicInteger sends = new AtomicInteger();
        CountDownLatch clearEnteredProvider = new CountDownLatch(1);
        CountDownLatch releaseClear = new CountDownLatch(1);
        CountDownLatch concurrentGrantReachedProvider = new CountDownLatch(1);
        Mockito.doAnswer(invocation -> {
            int call = sends.incrementAndGet();
            if (call == 2) {
                clearEnteredProvider.countDown();
                Assert.assertTrue(releaseClear.await(2, TimeUnit.SECONDS));
            } else if (call == 3) {
                concurrentGrantReachedProvider.countDown();
            }
            return null;
        }).when(bidi).send(Mockito.any(Command.class));

        new com.shaft.gui.browser.BrowserActions(driver, true).permissions()
                .grantFor("https://example.test", "camera");
        try (var executor = Executors.newFixedThreadPool(2)) {
            var clear = executor.submit(() -> new com.shaft.gui.browser.BrowserActions(driver, true).permissions().clear());
            Assert.assertTrue(clearEnteredProvider.await(2, TimeUnit.SECONDS));
            var grant = executor.submit(() -> new com.shaft.gui.browser.BrowserActions(driver, true).permissions()
                    .grantFor("https://example.test", "camera"));
            Assert.assertFalse(concurrentGrantReachedProvider.await(250, TimeUnit.MILLISECONDS));
            releaseClear.countDown();
            clear.get(2, TimeUnit.SECONDS);
            grant.get(2, TimeUnit.SECONDS);
        }
        new com.shaft.gui.browser.BrowserActions(driver, true).permissions().clear();
        Assert.assertEquals(sends.get(), 4);
    }

    @Test
    public void seleniumGlobalGrantAndAppiumPermissionsShouldFailClosed() {
        RemoteWebDriver selenium = Mockito.mock(RemoteWebDriver.class);
        Mockito.when(selenium.getSessionId()).thenReturn(new SessionId("selenium"));
        PermissionActionsContract seleniumPermissions = new com.shaft.gui.browser.BrowserActions(selenium, true).permissions();
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> seleniumPermissions.grant("geolocation"));

        AppiumDriver appium = Mockito.mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(HasBiDi.class));
        Mockito.when(appium.getSessionId()).thenReturn(new SessionId("appium"));
        PermissionActionsContract appiumPermissions = new com.shaft.gui.browser.BrowserActions(appium, true).permissions();
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> appiumPermissions.grantFor("https://example.test", "geolocation"));
        Assert.assertFalse(AutomationCapabilityResolver.forWebDriver(appium).supports(AutomationFeature.PERMISSIONS));

        Assert.expectThrows(IllegalArgumentException.class,
                () -> seleniumPermissions.grantFor("https://example.test:notaport", "geolocation"));
    }

    private static PlaywrightSession livePlaywrightSession() {
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

    private static boolean compiles(String name, String source) throws Exception {
        Path output = Files.createTempDirectory("shaft-permissions-api-compat");
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
}
