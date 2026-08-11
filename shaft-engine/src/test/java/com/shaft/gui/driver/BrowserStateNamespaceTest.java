package com.shaft.gui.driver;

import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.Test;
import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.remote.SupportsContextSwitching;
import org.openqa.selenium.remote.SessionId;

import java.util.List;
import java.util.Arrays;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;
import java.nio.file.Files;
import java.nio.file.Path;
import java.net.URI;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;

public class BrowserStateNamespaceTest {
    @Test
    public void dialogAndContextNamespacesShouldBeDiscoverableFromTheGenericFacade() throws Exception {
        Set<String> methods = Arrays.stream(BrowserActionsContract.class.getMethods())
                .map(java.lang.reflect.Method::getName).collect(Collectors.toSet());
        Assert.assertTrue(methods.containsAll(Set.of("dialog", "context")), methods.toString());
        Assert.assertEquals(BrowserActionsContract.class.getMethod("dialog").getReturnType(), DialogActionsContract.class);
        Assert.assertEquals(BrowserActionsContract.class.getMethod("context").getReturnType(), ContextActionsContract.class);
        Assert.assertEquals(DialogActionsContract.class.getDeclaredMethods().length, 4);
        Assert.assertEquals(ContextActionsContract.class.getDeclaredMethods().length, 4);
        Assert.assertEquals(DialogObservationContract.class.getDeclaredMethods().length, 3);
        Assert.assertEquals(CurrentDialogActionsContract.class.getDeclaredMethods().length, 6);
        Assert.assertEquals(NextDialogActionsContract.class.getDeclaredMethods().length, 4);
        Assert.assertEquals(DialogActionsContract.class.getDeclaredMethod("observation").getReturnType(),
                DialogObservationContract.class);
        Assert.assertEquals(DialogActionsContract.class.getDeclaredMethod("current").getReturnType(),
                CurrentDialogActionsContract.class);
        Assert.assertEquals(DialogActionsContract.class.getDeclaredMethod("next").getReturnType(),
                NextDialogActionsContract.class);
        Assert.assertEquals(CurrentDialogActionsContract.class.getDeclaredMethod("type", String.class).getReturnType(),
                CurrentDialogActionsContract.class);
        Assert.assertEquals(NextDialogActionsContract.class.getDeclaredMethod("type", String.class).getReturnType(),
                NextDialogActionsContract.class);
        Assert.assertEquals(ContextActionsContract.class.getDeclaredMethod("switchTo", String.class).getReturnType(),
                ContextActionsContract.class);
        Assert.assertEquals(com.shaft.gui.browser.BrowserActions.class.getMethod("dialog").getReturnType(),
                com.shaft.gui.browser.DialogActions.class);
        Assert.assertEquals(com.shaft.gui.playwright.browser.BrowserActions.class.getMethod("context").getReturnType(),
                com.shaft.gui.playwright.browser.ContextActions.class);
        Assert.assertEquals(com.shaft.gui.browser.BrowserActions.class.getMethod("context").getReturnType(),
                com.shaft.gui.browser.ContextActions.class);
        Assert.assertEquals(com.shaft.gui.playwright.browser.BrowserActions.class.getMethod("dialog").getReturnType(),
                com.shaft.gui.playwright.browser.DialogActions.class);
    }

    @Test
    public void stateNamespaceContractsShouldRetainTheirExactMethodDescriptors() {
        Map<Class<?>, Set<String>> expected = Map.of(
                DialogActionsContract.class, Set.of(
                        "and():BrowserActionsContract:false",
                        "observation():DialogObservationContract:false",
                        "current():CurrentDialogActionsContract:false",
                        "next():NextDialogActionsContract:false"),
                DialogObservationContract.class, Set.of(
                        "and():DialogActionsContract:false",
                        "wasSeen():boolean:false",
                        "lastText():String:false"),
                CurrentDialogActionsContract.class, Set.of(
                        "and():DialogActionsContract:false",
                        "isPresent():boolean:false",
                        "text():String:false",
                        "accept():CurrentDialogActionsContract:false",
                        "dismiss():CurrentDialogActionsContract:false",
                        "type(String):CurrentDialogActionsContract:false"),
                NextDialogActionsContract.class, Set.of(
                        "and():DialogActionsContract:false",
                        "accept():NextDialogActionsContract:false",
                        "dismiss():NextDialogActionsContract:false",
                        "type(String):NextDialogActionsContract:false"),
                ContextActionsContract.class, Set.of(
                        "and():BrowserActionsContract:false",
                        "current():String:false",
                        "handles():List:false",
                        "switchTo(String):ContextActionsContract:false"));

        expected.forEach((contract, descriptors) -> Assert.assertEquals(descriptors(contract), descriptors,
                contract.getSimpleName()));
    }

    @Test(dependsOnMethods = "dialogAndContextNamespacesShouldBeDiscoverableFromTheGenericFacade")
    public void seleniumDialogNamespaceShouldUseFamiliarShortVerbs() throws Exception {
        org.openqa.selenium.WebDriver driver = Mockito.mock(org.openqa.selenium.WebDriver.class);
        org.openqa.selenium.WebDriver.TargetLocator target = Mockito.mock(org.openqa.selenium.WebDriver.TargetLocator.class);
        Mockito.when(driver.switchTo()).thenReturn(target);
        Mockito.when(target.alert()).thenReturn(Mockito.mock(org.openqa.selenium.Alert.class));
        var browser = Mockito.spy(new com.shaft.gui.browser.BrowserActions(driver, true));
        Mockito.doReturn("Confirm purchase?").when(browser).getAlertText();
        Mockito.doReturn(browser).when(browser).acceptAlert();
        Mockito.doReturn(browser).when(browser).dismissAlert();
        Mockito.doReturn(browser).when(browser).typeIntoPromptAlert("yes");

        var dialog = browser.dialog();
        var current = dialog.current();

        Assert.assertTrue(current.isPresent());
        Assert.assertEquals(current.text(), "Confirm purchase?");
        Assert.assertSame(current.accept().dismiss().type("yes").and().and(), browser);
        Mockito.verify(browser).acceptAlert();
        Mockito.verify(browser).dismissAlert();
        Mockito.verify(browser).typeIntoPromptAlert("yes");
        Assert.expectThrows(UnsupportedOperationException.class, dialog::observation);
        Assert.expectThrows(UnsupportedOperationException.class, dialog::next);
    }

    @Test(dependsOnMethods = "dialogAndContextNamespacesShouldBeDiscoverableFromTheGenericFacade")
    public void playwrightContextNamespaceShouldExposeCurrentHandlesAndSwitching() throws Exception {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser nativeBrowser = Mockito.mock(Browser.class);
        BrowserContext nativeContext = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Mockito.when(session.browser()).thenReturn(nativeBrowser);
        Mockito.when(session.browserContext()).thenReturn(nativeContext);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(nativeBrowser.isConnected()).thenReturn(true);
        Mockito.when(page.isClosed()).thenReturn(false);
        var browser = Mockito.spy(new com.shaft.gui.playwright.browser.BrowserActions(session));
        Mockito.doReturn("PLAYWRIGHT").when(browser).getContext();
        Mockito.doReturn(List.of("PLAYWRIGHT")).when(browser).getContextHandles();
        Mockito.doReturn(browser).when(browser).setContext("WEB");

        var context = browser.context();

        Assert.assertEquals(context.current(), "PLAYWRIGHT");
        Assert.assertEquals(context.handles(), List.of("PLAYWRIGHT"));
        Assert.assertSame(context.switchTo("WEB").and(), browser);
        Mockito.verify(browser).setContext("WEB");
    }

    @Test
    public void seleniumContextNamespaceShouldSupportAnyAppiumContextDriver() {
        AppiumDriver driver = Mockito.mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(SupportsContextSwitching.class));
        SupportsContextSwitching contexts = (SupportsContextSwitching) driver;
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("appium-session"));
        AtomicReference<String> current = new AtomicReference<>("WEBVIEW_1");
        Mockito.when(contexts.getContext()).thenAnswer(ignored -> current.get());
        Mockito.doAnswer(invocation -> {
            current.set(invocation.getArgument(0));
            return null;
        }).when(contexts).context(Mockito.anyString());
        Mockito.when(contexts.getContextHandles()).thenReturn(java.util.Set.of("NATIVE_APP", "WEBVIEW_1"));
        var browser = new com.shaft.gui.browser.BrowserActions(driver, true);

        Assert.assertEquals(browser.context().current(), "WEBVIEW_1");
        Assert.assertTrue(browser.context().handles().containsAll(List.of("NATIVE_APP", "WEBVIEW_1")));
        Assert.assertSame(browser.context().switchTo("NATIVE_APP").and(), browser);
        Mockito.verify(contexts).context("NATIVE_APP");
    }

    @Test
    public void playwrightDialogNamespaceShouldSeparateObservationFromNextDialogPolicy() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser nativeBrowser = Mockito.mock(Browser.class);
        BrowserContext nativeContext = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Mockito.when(session.browser()).thenReturn(nativeBrowser);
        Mockito.when(session.browserContext()).thenReturn(nativeContext);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(nativeBrowser.isConnected()).thenReturn(true);
        Mockito.when(page.isClosed()).thenReturn(false);
        var browser = Mockito.spy(new com.shaft.gui.playwright.browser.BrowserActions(session));
        Mockito.doReturn(true).when(browser).isAlertPresent();
        Mockito.doReturn("Delete item?").when(browser).getAlertText();
        Mockito.doReturn(browser).when(browser).acceptAlert();
        Mockito.doReturn(browser).when(browser).dismissAlert();
        Mockito.doReturn(browser).when(browser).typeIntoPromptAlert("confirmed");

        var dialog = browser.dialog();

        Assert.assertTrue(dialog.observation().wasSeen());
        Assert.assertEquals(dialog.observation().lastText(), "Delete item?");
        Assert.assertSame(dialog.next().accept().dismiss().type("confirmed").and().and(), browser);
        Assert.expectThrows(UnsupportedOperationException.class, dialog::current);
        Mockito.verify(browser).acceptAlert();
        Mockito.verify(browser).dismissAlert();
        Mockito.verify(browser).typeIntoPromptAlert("confirmed");
    }

    @Test
    public void thirdPartyFacadesShouldFailClosedForNewStateNamespaces() {
        BrowserActionsContract facade = Mockito.mock(BrowserActionsContract.class, Mockito.CALLS_REAL_METHODS);
        Assert.expectThrows(UnsupportedOperationException.class, facade::dialog);
        Assert.expectThrows(UnsupportedOperationException.class, facade::context);
    }

    @Test
    public void stateNamespaceDefaultMethodCollisionBoundaryShouldBeExecutableDocumentation() throws Exception {
        String compatible = """
                import com.shaft.gui.driver.*;
                interface LegacyDialog { default DialogActionsContract dialog() { return null; } }
                interface LegacyContext { default ContextActionsContract context() { return null; } }
                interface CompatibleStateFacade extends BrowserActionsContract, LegacyDialog, LegacyContext {
                    @Override default DialogActionsContract dialog() { return BrowserActionsContract.super.dialog(); }
                    @Override default ContextActionsContract context() { return BrowserActionsContract.super.context(); }
                }
                """;
        String incompatibleDialog = """
                import com.shaft.gui.driver.*;
                interface LegacyDialog { default String dialog() { return "legacy"; } }
                interface IncompatibleDialogFacade extends BrowserActionsContract, LegacyDialog {}
                """;
        String incompatibleContext = """
                import com.shaft.gui.driver.*;
                interface LegacyContext { default String context() { return "legacy"; } }
                interface IncompatibleContextFacade extends BrowserActionsContract, LegacyContext {}
                """;

        Assert.assertTrue(compiles("CompatibleStateFacade", compatible));
        Assert.assertFalse(compiles("IncompatibleDialogFacade", incompatibleDialog));
        Assert.assertFalse(compiles("IncompatibleContextFacade", incompatibleContext));
    }

    @Test(expectedExceptions = UnsupportedOperationException.class,
            expectedExceptionsMessageRegExp = ".*context.*not supported.*")
    public void ordinarySeleniumSessionsShouldRejectAppiumContextActions() {
        org.openqa.selenium.WebDriver driver = Mockito.mock(org.openqa.selenium.WebDriver.class);
        new com.shaft.gui.browser.BrowserActions(driver, true).context().current();
    }

    @Test(expectedExceptions = UnsupportedOperationException.class,
            expectedExceptionsMessageRegExp = ".*context.*not supported.*")
    public void quitAppiumSessionsShouldRejectContextActions() {
        AppiumDriver driver = Mockito.mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(SupportsContextSwitching.class));
        Mockito.when(driver.getSessionId()).thenReturn(null);
        new com.shaft.gui.browser.BrowserActions(driver, true).context().current();
    }

    @Test(expectedExceptions = UnsupportedOperationException.class,
            expectedExceptionsMessageRegExp = ".*live Playwright session.*")
    public void absentPlaywrightSessionsShouldRejectDialogActions() {
        new com.shaft.gui.playwright.browser.BrowserActions(null).dialog().observation().wasSeen();
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void closedPlaywrightPagesShouldRejectContextActions() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(page.isClosed()).thenReturn(true);
        new com.shaft.gui.playwright.browser.BrowserActions(session).context().current();
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void missingPlaywrightContextsShouldRejectStateNamespaces() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        Page page = Mockito.mock(Page.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(browser.isConnected()).thenReturn(true);
        new com.shaft.gui.playwright.browser.BrowserActions(session).dialog().observation().wasSeen();
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void missingPlaywrightPagesShouldRejectStateNamespaces() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Mockito.when(session.browser()).thenReturn(Mockito.mock(Browser.class));
        Mockito.when(session.browserContext()).thenReturn(Mockito.mock(BrowserContext.class));
        new com.shaft.gui.playwright.browser.BrowserActions(session).context().current();
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void missingPlaywrightBrowsersShouldRejectStateNamespaces() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Mockito.when(session.browserContext()).thenReturn(Mockito.mock(BrowserContext.class));
        Mockito.when(session.page()).thenReturn(Mockito.mock(Page.class));
        new com.shaft.gui.playwright.browser.BrowserActions(session).dialog().observation().wasSeen();
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void disconnectedPlaywrightBrowsersShouldRejectStateNamespaces() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(Mockito.mock(BrowserContext.class));
        Mockito.when(session.page()).thenReturn(Mockito.mock(Page.class));
        Mockito.when(browser.isConnected()).thenReturn(false);
        new com.shaft.gui.playwright.browser.BrowserActions(session).context().handles();
    }

    @Test
    public void seleniumCurrentDialogPresenceShouldReturnFalseWhenNoAlertExists() {
        org.openqa.selenium.WebDriver driver = Mockito.mock(org.openqa.selenium.WebDriver.class);
        org.openqa.selenium.WebDriver.TargetLocator target = Mockito.mock(org.openqa.selenium.WebDriver.TargetLocator.class);
        Mockito.when(driver.switchTo()).thenReturn(target);
        Mockito.when(target.alert()).thenThrow(new org.openqa.selenium.NoAlertPresentException());

        Assert.assertFalse(new com.shaft.gui.browser.BrowserActions(driver, true).dialog().current().isPresent());
    }

    @Test
    public void retainedFlatStateSignaturesShouldRemainAvailable() throws Exception {
        for (Class<?> facade : List.of(com.shaft.gui.browser.BrowserActions.class,
                com.shaft.gui.playwright.browser.BrowserActions.class)) {
            Assert.assertEquals(facade.getMethod("isAlertPresent").getReturnType(), boolean.class);
            Assert.assertEquals(facade.getMethod("getAlertText").getReturnType(), String.class);
            Assert.assertEquals(facade.getMethod("acceptAlert").getReturnType(), facade);
            Assert.assertEquals(facade.getMethod("dismissAlert").getReturnType(), facade);
            Assert.assertEquals(facade.getMethod("typeIntoPromptAlert", String.class).getReturnType(), facade);
            Assert.assertEquals(facade.getMethod("getContext").getReturnType(), String.class);
            Assert.assertEquals(facade.getMethod("setContext", String.class).getReturnType(), facade);
            Assert.assertEquals(facade.getMethod("getContextHandles").getReturnType(), List.class);
        }
    }

    private static Set<String> descriptors(Class<?> contract) {
        return Arrays.stream(contract.getDeclaredMethods())
                .map(method -> method.getName() + "(" + Arrays.stream(method.getParameterTypes())
                        .map(Class::getSimpleName)
                        .collect(Collectors.joining(",")) + "):" + method.getReturnType().getSimpleName()
                        + ":" + method.isVarArgs())
                .collect(Collectors.toSet());
    }

    private static boolean compiles(String name, String source) throws Exception {
        Path output = Files.createTempDirectory("shaft-state-api-compat");
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
