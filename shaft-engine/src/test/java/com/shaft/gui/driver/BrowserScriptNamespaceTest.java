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

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.List;

public class BrowserScriptNamespaceTest {
    @Test
    public void scriptNamespaceShouldExposeOneBackendNeutralOptionalArgumentContract() {
        boolean discoverable = Arrays.stream(BrowserActionsContract.class.getMethods())
                .anyMatch(method -> method.getName().equals("script")
                        && method.getParameterCount() == 0
                        && method.getReturnType().getSimpleName().equals("ScriptActionsContract"));

        Assert.assertTrue(discoverable);
    }

    @Test
    public void seleniumShouldExecuteZeroAndOneArgumentScriptsWithoutSemanticPacking() {
        WebDriver driver = Mockito.mock(WebDriver.class,
                Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        JavascriptExecutor executor = (JavascriptExecutor) driver;
        Mockito.when(executor.executeScript("return 1")).thenReturn(1L);
        Mockito.when(executor.executeScript("return arguments[0]", List.of(1, 2))).thenReturn(List.of(1, 2));
        Object script = namespace(new com.shaft.gui.browser.BrowserActions(driver, true));

        Assert.assertEquals(invoke(script, "evaluate", new Class<?>[]{String.class}, "return 1"), 1L);
        Assert.assertEquals(invoke(script, "evaluate", new Class<?>[]{String.class, Object.class},
                "return arguments[0]", List.of(1, 2)), List.of(1, 2));
    }

    @Test
    public void playwrightShouldUseItsNativeSingleArgumentAndPromiseAwaitingContract() {
        PlaywrightSession session = livePlaywrightSession();
        Mockito.when(session.page().evaluate("() => 1")).thenReturn(1);
        Mockito.when(session.page().evaluate("value => Promise.resolve(value)", "done")).thenReturn("done");
        Object script = namespace(new com.shaft.gui.playwright.browser.BrowserActions(session));

        Assert.assertEquals(invoke(script, "evaluate", new Class<?>[]{String.class}, "() => 1"), 1);
        Assert.assertEquals(invoke(script, "evaluateAsync", new Class<?>[]{String.class, Object.class},
                "value => Promise.resolve(value)", "done"), "done");
    }

    @Test
    public void nativeAppiumContextShouldFailClosed() {
        AppiumDriver driver = Mockito.mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(SupportsContextSwitching.class));
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("appium"));
        Mockito.when(((SupportsContextSwitching) driver).getContext()).thenReturn("NATIVE_APP");
        Object script = namespace(new com.shaft.gui.browser.BrowserActions(driver, true));

        Assert.expectThrows(UnsupportedOperationException.class,
                () -> invoke(script, "evaluate", new Class<?>[]{String.class}, "return 1"));
    }

    private static Object namespace(Object browser) {
        return invoke(browser, "script", new Class<?>[]{});
    }

    private static Object invoke(Object target, String method, Class<?>[] parameterTypes, Object... arguments) {
        try {
            return target.getClass().getMethod(method, parameterTypes).invoke(target, arguments);
        } catch (InvocationTargetException exception) {
            if (exception.getCause() instanceof RuntimeException runtimeException) {
                throw runtimeException;
            }
            throw new AssertionError(exception.getCause());
        } catch (ReflectiveOperationException exception) {
            throw new AssertionError(exception);
        }
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
