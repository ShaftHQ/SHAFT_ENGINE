package com.shaft.gui.driver;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.shaft.gui.browser.internal.PlaywrightNetworkInterceptor;
import com.shaft.gui.capabilities.AutomationFeature;
import com.shaft.gui.capabilities.internal.AutomationCapabilityResolver;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import io.appium.java_client.AppiumDriver;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.openqa.selenium.Credentials;
import org.openqa.selenium.HasAuthentication;
import org.openqa.selenium.UsernameAndPassword;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.net.URI;
import java.util.Arrays;
import java.util.Optional;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.nio.file.Files;
import java.nio.file.Path;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class BrowserAuthenticationNamespaceTest {
    @Test
    public void authenticationNamespaceShouldBeDiscoverableWithoutAddingFlatMethods() {
        boolean discoverable = Arrays.stream(BrowserActionsContract.class.getMethods())
                .anyMatch(method -> method.getName().equals("authentication")
                        && method.getParameterCount() == 0
                        && method.getReturnType().getSimpleName().equals("AuthenticationActionsContract"));

        Assert.assertTrue(discoverable);
        try {
            Assert.assertTrue(BrowserActionsContract.class.getMethod("authentication").isDefault());
            Set<String> descriptors = Arrays.stream(AuthenticationActionsContract.class.getDeclaredMethods())
                    .map(method -> method.getName() + Arrays.toString(method.getParameterTypes())
                            + "->" + method.getReturnType().getSimpleName())
                    .collect(Collectors.toSet());
            Assert.assertEquals(descriptors, Set.of(
                    "and[]->BrowserActionsContract",
                    "basic[class java.lang.String, class java.lang.String]->AuthenticationActionsContract",
                    "basicFor[class java.lang.String, class java.lang.String, class java.lang.String]->AuthenticationActionsContract",
                    "navigateTo[class java.lang.String, class java.lang.String, class java.lang.String]->BrowserActionsContract",
                    "clear[]->AuthenticationActionsContract"));
        } catch (NoSuchMethodException exception) {
            throw new AssertionError(exception);
        }
    }

    @Test
    @SuppressWarnings("unchecked")
    public void seleniumShouldRegisterGlobalAndOriginScopedCredentialsWithoutEmbeddingThemInUrls() {
        RemoteWebDriver driver = liveSeleniumAuthenticationDriver();
        Mockito.when(driver.getCurrentUrl()).thenReturn("https://current.test/home");
        HasAuthentication nativeAuthentication = (HasAuthentication) driver;
        var browser = Mockito.spy(new com.shaft.gui.browser.BrowserActions(driver, true));
        String target = "https://münich.example/secure";
        Mockito.doReturn(browser).when(browser).navigateToURL(target);

        browser.authentication().basic("global-user", "global-pass")
                .basicFor("https://example.test:443/", "scoped-user", "scoped-pass")
                .navigateTo(target, "nav-user", "nav-pass");

        ArgumentCaptor<Predicate<URI>> predicates = ArgumentCaptor.forClass(Predicate.class);
        ArgumentCaptor<Supplier<Credentials>> credentials = ArgumentCaptor.forClass(Supplier.class);
        Mockito.verify(nativeAuthentication, Mockito.times(3)).register(predicates.capture(), credentials.capture());
        Assert.assertTrue(predicates.getAllValues().get(0).test(URI.create("https://current.test/path")));
        Assert.assertFalse(predicates.getAllValues().get(0).test(URI.create("https://any.test/path")));
        Assert.assertTrue(predicates.getAllValues().get(1).test(URI.create("https://example.test/other")));
        Assert.assertFalse(predicates.getAllValues().get(1).test(URI.create("https://other.test/")));
        Assert.assertTrue(predicates.getAllValues().get(2).test(URI.create("https://xn--mnich-kva.example/next")));
        UsernameAndPassword scoped = (UsernameAndPassword) credentials.getAllValues().get(1).get();
        Assert.assertEquals(scoped.username(), "scoped-user");
        Assert.assertEquals(scoped.password(), "scoped-pass");
        Mockito.verify(browser).navigateToURL(target);
        Assert.assertTrue(AutomationCapabilityResolver.forWebDriver(driver).supports(AutomationFeature.AUTHENTICATION));
    }

    @Test
    public void seleniumClearAndAppiumAuthenticationShouldFailClosed() {
        RemoteWebDriver selenium = liveSeleniumAuthenticationDriver();
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(selenium, true).authentication().clear());

        AppiumDriver appium = Mockito.mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(HasAuthentication.class, HasDevTools.class));
        Mockito.when(appium.getSessionId()).thenReturn(new SessionId("appium"));
        Mockito.when(((HasDevTools) appium).maybeGetDevTools()).thenReturn(Optional.of(Mockito.mock(DevTools.class)));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(appium, true).authentication()
                        .basic("user", "password"));
        Assert.assertFalse(AutomationCapabilityResolver.forWebDriver(appium).supports(AutomationFeature.AUTHENTICATION));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new com.shaft.gui.browser.BrowserActions(selenium, true).authentication()
                        .basicFor("https://example.test", "invalid:user", "password"));
    }

    @Test
    public void playwrightShouldRegisterNavigateAndClearThroughItsContextInterceptor() {
        PlaywrightSession session = livePlaywrightSession();
        PlaywrightNetworkInterceptor interceptor = session.networkInterceptor();
        String target = "https://example.test/secure";

        var browser = new com.shaft.gui.playwright.browser.BrowserActions(session);
        browser.authentication().basic("global", "secret")
                .basicFor("https://example.test", "scoped", "credential")
                .navigateTo(target, "nav", "password");
        browser.authentication().clear();

        Mockito.verify(interceptor).registerBasicAuthentication("https://current.test", "Basic Z2xvYmFsOnNlY3JldA==");
        Mockito.verify(interceptor).registerBasicAuthentication("https://example.test", "Basic c2NvcGVkOmNyZWRlbnRpYWw=");
        Mockito.verify(interceptor).registerBasicAuthentication("https://example.test", "Basic bmF2OnBhc3N3b3Jk");
        Mockito.verify(session.page()).navigate(target);
        Mockito.verify(interceptor).clearAuthentication();
        Assert.assertTrue(AutomationCapabilityResolver.forPlaywright(session).supports(AutomationFeature.AUTHENTICATION));
    }

    @Test
    public void authenticationShouldUseTheNarrowestLiveBackendBoundary() {
        PlaywrightSession contextOnly = livePlaywrightSession();
        Mockito.when(contextOnly.page()).thenReturn(null);
        new com.shaft.gui.playwright.browser.BrowserActions(contextOnly).authentication()
                .basicFor("https://example.test", "user", "password");
        Assert.assertTrue(AutomationCapabilityResolver.forPlaywright(contextOnly).supports(AutomationFeature.AUTHENTICATION));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.playwright.browser.BrowserActions(contextOnly).authentication()
                        .basic("user", "password"));

        RemoteWebDriver noDevTools = Mockito.mock(RemoteWebDriver.class,
                Mockito.withSettings().extraInterfaces(HasAuthentication.class));
        Mockito.when(noDevTools.getSessionId()).thenReturn(new SessionId("no-devtools"));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(noDevTools, true).authentication()
                        .basicFor("https://example.test", "user", "password"));
        Assert.assertFalse(AutomationCapabilityResolver.forWebDriver(noDevTools)
                .supports(AutomationFeature.AUTHENTICATION));

        RemoteWebDriver closed = liveSeleniumAuthenticationDriver();
        Mockito.when(closed.getSessionId()).thenReturn(null);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(closed, true).authentication()
                        .basicFor("https://example.test", "user", "password"));
        Assert.assertFalse(AutomationCapabilityResolver.forWebDriver(closed)
                .supports(AutomationFeature.AUTHENTICATION));
    }

    @Test
    public void authenticationDefaultMethodCollisionBoundaryShouldBeExecutableDocumentation() throws Exception {
        String compatible = """
                import com.shaft.gui.driver.*;
                interface LegacyAuthentication { default AuthenticationActionsContract authentication() { return null; } }
                interface CompatibleAuthenticationFacade extends BrowserActionsContract, LegacyAuthentication {
                    @Override default AuthenticationActionsContract authentication() {
                        return BrowserActionsContract.super.authentication();
                    }
                }
                """;
        String incompatible = """
                import com.shaft.gui.driver.*;
                interface LegacyAuthentication { default String authentication() { return "legacy"; } }
                interface IncompatibleAuthenticationFacade extends BrowserActionsContract, LegacyAuthentication {}
                """;

        Assert.assertTrue(compiles("CompatibleAuthenticationFacade", compatible));
        Assert.assertFalse(compiles("IncompatibleAuthenticationFacade", incompatible));
    }

    private static RemoteWebDriver liveSeleniumAuthenticationDriver() {
        RemoteWebDriver driver = Mockito.mock(RemoteWebDriver.class,
                Mockito.withSettings().extraInterfaces(HasAuthentication.class, HasDevTools.class));
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("selenium"));
        Mockito.when(((HasDevTools) driver).maybeGetDevTools()).thenReturn(Optional.of(Mockito.mock(DevTools.class)));
        return driver;
    }

    private static PlaywrightSession livePlaywrightSession() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        PlaywrightNetworkInterceptor interceptor = Mockito.mock(PlaywrightNetworkInterceptor.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(session.networkInterceptor()).thenReturn(interceptor);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(page.isClosed()).thenReturn(false);
        Mockito.when(page.url()).thenReturn("https://current.test/home");
        return session;
    }

    private static boolean compiles(String name, String source) throws Exception {
        Path output = Files.createTempDirectory("shaft-authentication-api-compat");
        output.toFile().deleteOnExit();
        var compiler = ToolProvider.getSystemJavaCompiler();
        var sourceFile = new SimpleJavaFileObject(URI.create("string:///" + name + ".java"),
                javax.tools.JavaFileObject.Kind.SOURCE) {
            @Override public CharSequence getCharContent(boolean ignoreEncodingErrors) { return source; }
        };
        return Boolean.TRUE.equals(compiler.getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, List.of(sourceFile)).call());
    }
}
