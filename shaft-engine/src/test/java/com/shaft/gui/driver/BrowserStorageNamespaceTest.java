package com.shaft.gui.driver;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.remote.SupportsContextSwitching;
import org.mockito.Mockito;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class BrowserStorageNamespaceTest {
    @Test
    public void storageNamespaceShouldBeDiscoverableWithoutAddingFlatTopLevelOperations() {
        Assert.assertTrue(Arrays.stream(BrowserActionsContract.class.getMethods())
                .anyMatch(method -> method.getName().equals("storage")
                        && method.getParameterCount() == 0
                        && method.getReturnType().getSimpleName().equals("StorageActionsContract")));
        Assert.assertEquals(descriptors(StorageActionsContract.class), Set.of(
                "and():BrowserActionsContract", "state():StorageStateActionsContract",
                "local():KeyValueStorageActionsContract", "session():KeyValueStorageActionsContract"));
        Assert.assertEquals(descriptors(StorageStateActionsContract.class), Set.of(
                "and():StorageActionsContract", "save(String):StorageStateActionsContract",
                "load(String):StorageStateActionsContract"));
        Assert.assertEquals(descriptors(KeyValueStorageActionsContract.class), Set.of(
                "and():StorageActionsContract", "get(String):String",
                "set(String,String):KeyValueStorageActionsContract",
                "remove(String):KeyValueStorageActionsContract", "clear():KeyValueStorageActionsContract"));
    }

    @Test
    public void seleniumStorageNamespaceShouldProvideStateAndScopedKeyValueConveniences() {
        WebDriver driver = Mockito.mock(WebDriver.class,
                Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        JavascriptExecutor scripts = (JavascriptExecutor) driver;
        Mockito.when(scripts.executeScript("return window[arguments[0]].getItem(arguments[1]);",
                "localStorage", "token")).thenReturn("abc");
        var browser = Mockito.spy(new com.shaft.gui.browser.BrowserActions(driver, true));
        Mockito.doReturn(browser).when(browser).saveStorageState("state.json");
        Mockito.doReturn(browser).when(browser).loadStorageState("state.json");

        var storage = browser.storage();
        Assert.assertSame(storage.state().save("state.json").load("state.json").and(), storage);
        Assert.assertEquals(storage.local().set("token", "abc").get("token"), "abc");
        Assert.assertSame(storage.local().remove("token").clear().and(), storage);
        Assert.assertSame(storage.session().set("tab", "checkout").and().and(), browser);

        Mockito.verify(browser).saveStorageState("state.json");
        Mockito.verify(browser).loadStorageState("state.json");
        Mockito.verify(scripts).executeScript("window[arguments[0]].setItem(arguments[1], arguments[2]);",
                "localStorage", "token", "abc");
        Mockito.verify(scripts).executeScript("window[arguments[0]].removeItem(arguments[1]);",
                "localStorage", "token");
        Mockito.verify(scripts).executeScript("window[arguments[0]].clear();", "localStorage");
        Mockito.verify(scripts).executeScript("window[arguments[0]].setItem(arguments[1], arguments[2]);",
                "sessionStorage", "tab", "checkout");
    }

    @Test
    public void playwrightStorageNamespaceShouldProvideTheSameVocabulary() {
        PlaywrightSession session = livePlaywrightSession();
        Page page = session.page();
        Mockito.when(page.evaluate("([scope, key]) => window[scope].getItem(key)",
                List.of("sessionStorage", "tab"))).thenReturn("checkout");
        var browser = Mockito.spy(new com.shaft.gui.playwright.browser.BrowserActions(session));
        Mockito.doReturn(browser).when(browser).saveStorageState("state.json");
        Mockito.doReturn(browser).when(browser).loadStorageState("state.json");

        var storage = browser.storage();
        Assert.assertSame(storage.state().save("state.json").load("state.json").and(), storage);
        Assert.assertEquals(storage.session().set("tab", "checkout").get("tab"), "checkout");
        Assert.assertSame(storage.local().remove("token").clear().and().and(), browser);

        Mockito.verify(page).evaluate("([scope, key, value]) => window[scope].setItem(key, value)",
                List.of("sessionStorage", "tab", "checkout"));
        Mockito.verify(page).evaluate("([scope, key]) => window[scope].removeItem(key)",
                List.of("localStorage", "token"));
        Mockito.verify(page).evaluate("scope => window[scope].clear()", "localStorage");
    }

    @Test
    public void unsupportedAndInvalidStorageOperationsShouldFailClosed() {
        WebDriver minimal = Mockito.mock(WebDriver.class);
        var storage = new com.shaft.gui.browser.BrowserActions(minimal, true).storage();
        Assert.expectThrows(UnsupportedOperationException.class, () -> storage.local().get("token"));

        WebDriver scriptDriver = Mockito.mock(WebDriver.class,
                Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        var supported = new com.shaft.gui.browser.BrowserActions(scriptDriver, true).storage();
        Assert.expectThrows(NullPointerException.class, () -> supported.local().get(null));
        Assert.expectThrows(NullPointerException.class, () -> supported.session().set("key", null));
    }

    @Test
    public void webStorageShouldAcceptEmptyAndWhitespaceKeysAcrossBackends() {
        WebDriver driver = Mockito.mock(WebDriver.class,
                Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        var selenium = new com.shaft.gui.browser.BrowserActions(driver, true).storage();
        selenium.local().set("", "empty").set(" ", "space").remove("");

        PlaywrightSession session = livePlaywrightSession();
        var playwright = new com.shaft.gui.playwright.browser.BrowserActions(session).storage();
        playwright.session().set("", "empty").set(" ", "space").remove("");

        Mockito.verify((JavascriptExecutor) driver).executeScript(
                "window[arguments[0]].setItem(arguments[1], arguments[2]);", "localStorage", "", "empty");
        Mockito.verify(session.page()).evaluate("([scope, key, value]) => window[scope].setItem(key, value)",
                List.of("sessionStorage", "", "empty"));
    }

    @Test
    public void appiumNativeContextsShouldRejectWebStorageOperations() {
        AppiumDriver driver = Mockito.mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(SupportsContextSwitching.class));
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("appium-session"));
        Mockito.when(((SupportsContextSwitching) driver).getContext()).thenReturn("NATIVE_APP");

        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).storage().local().get("token"));
    }

    @Test
    public void existingFlatStorageStateSignaturesShouldRemainAvailable() throws Exception {
        for (Class<?> facade : List.of(com.shaft.gui.browser.BrowserActions.class,
                com.shaft.gui.playwright.browser.BrowserActions.class)) {
            Assert.assertEquals(facade.getMethod("saveStorageState", String.class).getReturnType(), facade);
            Assert.assertEquals(facade.getMethod("loadStorageState", String.class).getReturnType(), facade);
        }
    }

    private static Set<String> descriptors(Class<?> contract) {
        return Arrays.stream(contract.getDeclaredMethods())
                .map(method -> method.getName() + "(" + Arrays.stream(method.getParameterTypes())
                        .map(Class::getSimpleName).collect(Collectors.joining(",")) + "):"
                        + method.getReturnType().getSimpleName())
                .collect(Collectors.toSet());
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
}
