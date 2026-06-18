package com.shaft.gui.element.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.internal.image.AnimatedGifManager;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.internal.image.ScreenshotHelper;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.internal.locator.ShadowLocatorBuilder;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.ReportManager;
import io.appium.java_client.AppiumDriver;
import io.qameta.allure.Allure;
import io.qameta.allure.AllureLifecycle;
import io.qameta.allure.model.StepResult;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.StatusDetails;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.By;
import org.openqa.selenium.InvalidElementStateException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.UnsupportedCommandException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Sequence;
import org.openqa.selenium.remote.RemoteWebElement;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.awt.Color;
import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class ActionsCoverageUnitTest {
    private static final By LOCATOR = By.id("target");
    private static final byte[] PNG = new byte[]{(byte) 0x89, 'P', 'N', 'G'};

    @BeforeMethod
    public void configureFastMockFriendlyProperties() {
        SHAFT.Properties.reporting.set().captureElementName(false);
        SHAFT.Properties.flags.set().forceCheckElementLocatorIsUnique(false);
        SHAFT.Properties.flags.set().scrollingMode("legacy");
        SHAFT.Properties.flags.set().clearBeforeTypingMode("off");
        SHAFT.Properties.flags.set().clickUsingJavascriptWhenWebDriverClickFails(true);
        SHAFT.Properties.visuals.set().createAnimatedGif(false);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("ValidationPointsOnly");
        SHAFT.Properties.visuals.set().screenshotParamsWatermark(false);
        SHAFT.Properties.platform.set().targetPlatform(org.openqa.selenium.Platform.LINUX.name());
        SHAFT.Properties.mobile.set().browserName("chrome");
        LocatorBuilder.cleanup();
        ShadowLocatorBuilder.cleanup();
    }

    @AfterMethod(alwaysRun = true)
    public void cleanupThreadLocalState() {
        LocatorBuilder.cleanup();
        ShadowLocatorBuilder.cleanup();
        Properties.clearForCurrentThread();
    }

    @Test
    public void publicWrapperMethodsShouldDelegateExpectedActionTypeLocatorAndData() {
        WebDriver driver = mock(WebDriver.class);
        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            RecordingActions actions = new RecordingActions(driver);

            Assert.assertSame(actions.hover(LOCATOR), actions);
            assertRecorded(actions, Actions.ActionType.HOVER, LOCATOR, null);
            Assert.assertSame(actions.click(LOCATOR), actions);
            assertRecorded(actions, Actions.ActionType.CLICK, LOCATOR, null);
            Assert.assertSame(actions.clickAndHold(LOCATOR), actions);
            assertRecorded(actions, Actions.ActionType.CLICK_AND_HOLD, LOCATOR, null);
            Assert.assertSame(actions.doubleClick(LOCATOR), actions);
            assertRecorded(actions, Actions.ActionType.DOUBLE_CLICK, LOCATOR, null);
            Assert.assertSame(actions.clickUsingJavascript(LOCATOR), actions);
            assertRecorded(actions, Actions.ActionType.JAVASCRIPT_CLICK, LOCATOR, null);
            Assert.assertSame(actions.setValueUsingJavaScript(LOCATOR, "js-value"), actions);
            assertRecorded(actions, Actions.ActionType.JAVASCRIPT_SET_VALUE, LOCATOR, "js-value");
            Assert.assertSame(actions.type(LOCATOR, "typed"), actions);
            assertRecorded(actions, Actions.ActionType.TYPE, LOCATOR, new CharSequence[]{"typed"});
            Assert.assertSame(actions.typeSecure(LOCATOR, "secret"), actions);
            assertRecorded(actions, Actions.ActionType.TYPE_SECURELY, LOCATOR, new CharSequence[]{"secret"});
            Assert.assertSame(actions.typeAppend(LOCATOR, "suffix"), actions);
            assertRecorded(actions, Actions.ActionType.TYPE_APPEND, LOCATOR, new CharSequence[]{"suffix"});
            Assert.assertSame(actions.clear(LOCATOR), actions);
            assertRecorded(actions, Actions.ActionType.CLEAR, LOCATOR, null);
            Assert.assertSame(actions.dropFileToUpload(LOCATOR, "pom.xml"), actions);
            assertRecorded(actions, Actions.ActionType.DROP_FILE_TO_UPLOAD, LOCATOR, "pom.xml");
            Assert.assertSame(actions.scrollToElement(LOCATOR), actions);
            assertRecorded(actions, Actions.ActionType.SCROLL_TO_ELEMENT, LOCATOR, null);
            Assert.assertSame(actions.select(LOCATOR, "option"), actions);
            assertRecorded(actions, Actions.ActionType.SELECT, LOCATOR, "option");
            Assert.assertSame(actions.submitFormUsingJavaScript(LOCATOR), actions);
            assertRecorded(actions, Actions.ActionType.SUBMIT_FORM_USING_JAVASCRIPT, LOCATOR, null);
            Assert.assertSame(actions.switchToIframe(LOCATOR), actions);
            assertRecorded(actions, Actions.ActionType.SWITCH_TO_IFRAME, LOCATOR, null);
            Assert.assertSame(actions.typeFileLocationForUpload(LOCATOR, "pom.xml"), actions);
            assertRecorded(actions, Actions.ActionType.TYPE_FILE_LOCATION_FOR_UPLOAD, LOCATOR, "pom.xml");
            Assert.assertSame(actions.dragAndDrop(LOCATOR, By.id("drop")), actions);
            assertRecorded(actions, Actions.ActionType.DRAG_AND_DROP, LOCATOR, By.id("drop"));
            Assert.assertSame(actions.dragAndDropByOffset(LOCATOR, 5, 7), actions);
            assertRecorded(actions, Actions.ActionType.DRAG_AND_DROP_BY_OFFSET, LOCATOR, new ArrayList<>(List.of(5, 7)));
            Assert.assertSame(actions.and(), actions);
        }
    }

    @Test
    public void silentPassedActionShouldOnlyWriteDebugDiscreteLog() throws Exception {
        WebDriver driver = mock(WebDriver.class);

        try (MockedStatic<JavaScriptWaitManager> javaScriptWaitManager = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class);
             MockedStatic<ReportManager> reportManager = org.mockito.Mockito.mockStatic(ReportManager.class);
             MockedStatic<Allure> allure = org.mockito.Mockito.mockStatic(Allure.class)) {
            Actions actions = new Actions(driver, true);

            invoke(actions, "report", new Class[]{String.class, String.class, Status.class, byte[].class, RuntimeException.class},
                    "GET_TEXT", "By.id: target", Status.PASSED, null, null);

            reportManager.verify(() -> ReportManager.logDiscrete("Get text \"By.id: target\"", Level.DEBUG));
            allure.verifyNoInteractions();
        }
    }

    @Test
    public void typingActionsShouldShowTypedTextAndKeepTargetMetadataInAllureStep() {
        SHAFT.Properties.reporting.set().captureElementName(true);
        WebDriver driver = mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        WebElement element = standardElement();
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));
        when(((JavascriptExecutor) driver).executeScript(anyString(), any(Object[].class))).thenReturn(null);

        try (MockedStatic<JavaScriptWaitManager> ignoredWait = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class);
             MockedStatic<Allure> allure = org.mockito.Mockito.mockStatic(Allure.class)) {
            AllureLifecycle lifecycle = mock(AllureLifecycle.class);
            allure.when(Allure::getLifecycle).thenReturn(lifecycle);
            StepResult step = captureStepUpdates(lifecycle);
            String typedText = "first line\n" + "x".repeat(150);
            String expectedPreview = ("first line " + "x".repeat(150)).substring(0, 117) + "...";

            new Actions(helperFor(driver)).type(LOCATOR, typedText);

            Assert.assertEquals(step.getName(), "Type \"" + expectedPreview + "\"");
            Assert.assertEquals(parameterValue(step, "locator"), "By.id: target");
            Assert.assertEquals(parameterValue(step, "txt"), "first line\\n" + "x".repeat(150));
            Assert.assertEquals(parameterValue(step, "element name"), "accessible target");
            Assert.assertTrue(step.getDescription().contains("locator: By.id: target"));
            Assert.assertTrue(step.getDescription().contains("txt: first line\\n"));
        }
    }

    @Test
    public void secureTypingShouldKeepTypedSecretMaskedInAllureStep() {
        WebDriver driver = mock(WebDriver.class);
        WebElement element = standardElement();
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (MockedStatic<JavaScriptWaitManager> ignoredWait = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class);
             MockedStatic<Allure> allure = org.mockito.Mockito.mockStatic(Allure.class)) {
            AllureLifecycle lifecycle = mock(AllureLifecycle.class);
            allure.when(Allure::getLifecycle).thenReturn(lifecycle);
            StepResult step = captureStepUpdates(lifecycle);

            new Actions(helperFor(driver)).typeSecure(LOCATOR, "secret-value");

            Assert.assertEquals(step.getName(), "Type securely \"********\"");
            Assert.assertEquals(parameterValue(step, "txt"), "********");
            Assert.assertFalse(step.getName().contains("secret-value"));
            Assert.assertFalse(step.getDescription().contains("secret-value"));
        }
    }

    @Test
    public void appendAndJavascriptSetValueShouldUseValueInAllureStepName() {
        WebDriver driver = mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        WebElement element = standardElement();
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));
        when(((JavascriptExecutor) driver).executeScript(anyString(), any(Object[].class))).thenReturn(null);

        try (MockedStatic<JavaScriptWaitManager> ignoredWait = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class);
             MockedStatic<Allure> allure = org.mockito.Mockito.mockStatic(Allure.class)) {
            AllureLifecycle lifecycle = mock(AllureLifecycle.class);
            allure.when(Allure::getLifecycle).thenReturn(lifecycle);
            StepResult step = captureStepUpdates(lifecycle);

            new Actions(helperFor(driver)).typeAppend(LOCATOR, "suffix");
            Assert.assertEquals(step.getName(), "Type append \"suffix\"");
            Assert.assertEquals(parameterValue(step, "txt"), "suffix");

            step = captureStepUpdates(lifecycle);
            new Actions(helperFor(driver)).setValueUsingJavaScript(LOCATOR, "js value");
            Assert.assertTrue(step.getName().contains("\"js value\""));
            Assert.assertFalse(step.getName().contains("By.id: target"));
            Assert.assertEquals(parameterValue(step, "txt"), "js value");
        }
    }

    @Test
    public void smartLocatorWrappersShouldDelegateToSmartLocators() {
        WebDriver driver = mock(WebDriver.class);
        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            RecordingActions actions = new RecordingActions(driver);

            actions.click("Submit");
            Assert.assertEquals(actions.action, Actions.ActionType.CLICK);
            Assert.assertTrue(actions.locator.toString().contains("Submit"));

            actions.type("Email", "user@example.com");
            Assert.assertEquals(actions.action, Actions.ActionType.TYPE);
            Assert.assertTrue(actions.locator.toString().contains("Email"));
            Assert.assertEquals((CharSequence[]) actions.data, new CharSequence[]{"user@example.com"});
        }
    }


    @Test
    public void constructorsAndDefaultWaitUntilShouldCoverConveniencePaths() {
        WebDriver driver = mock(WebDriver.class);
        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            Actions silentActions = new Actions(driver, true);
            Assert.assertSame(silentActions.waitUntil(d -> true), silentActions);
        }
    }

    @Test
    public void getInformationAndClipboardWrappersShouldDelegateExpectedActions() throws Exception {
        WebDriver driver = mock(WebDriver.class);
        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            RecordingActions actions = new RecordingActions(driver);
            actions.returnValue = "true";

            Assert.assertEquals(actions.get().attribute(LOCATOR, "href"), "true");
            assertRecorded(actions, Actions.ActionType.GET_ATTRIBUTE, LOCATOR, "href");
            Assert.assertEquals(actions.get().domAttribute(LOCATOR, "data-id"), "true");
            assertRecorded(actions, Actions.ActionType.GET_DOM_ATTRIBUTE, LOCATOR, "data-id");
            Assert.assertEquals(actions.get().domProperty(LOCATOR, "value"), "true");
            assertRecorded(actions, Actions.ActionType.GET_DOM_PROPERTY, LOCATOR, "value");
            Assert.assertEquals(actions.get().name(LOCATOR), "true");
            assertRecorded(actions, Actions.ActionType.GET_NAME, LOCATOR, null);
            Assert.assertEquals(actions.get().text(LOCATOR), "true");
            assertRecorded(actions, Actions.ActionType.GET_TEXT, LOCATOR, null);
            Assert.assertEquals(actions.get().selectedText(LOCATOR), "true");
            assertRecorded(actions, Actions.ActionType.GET_SELECTED_TEXT, LOCATOR, null);
            Assert.assertEquals(actions.get().cssValue(LOCATOR, "color"), "true");
            assertRecorded(actions, Actions.ActionType.GET_CSS_VALUE, LOCATOR, "color");
            Assert.assertTrue(actions.get().isDisplayed(LOCATOR));
            assertRecorded(actions, Actions.ActionType.GET_IS_DISPLAYED, LOCATOR, null);
            Assert.assertTrue(actions.get().isEnabled(LOCATOR));
            assertRecorded(actions, Actions.ActionType.GET_IS_ENABLED, LOCATOR, null);
            Assert.assertTrue(actions.get().isSelected(LOCATOR));
            assertRecorded(actions, Actions.ActionType.GET_IS_SELECTED, LOCATOR, null);

            Assert.assertSame(actions.clipboard().deleteAll(LOCATOR), actions);
            assertRecorded(actions, Actions.ActionType.CLIPBOARD_DELETE, LOCATOR, null);
            Assert.assertSame(actions.clipboard().copyAll(LOCATOR), actions);
            assertRecorded(actions, Actions.ActionType.CLIPBOARD_COPY, LOCATOR, null);
            Assert.assertSame(actions.clipboard().cutAll(LOCATOR), actions);
            assertRecorded(actions, Actions.ActionType.CLIPBOARD_CUT, LOCATOR, null);
            Assert.assertSame(actions.clipboard().paste(LOCATOR), actions);
            assertRecorded(actions, Actions.ActionType.CLIPBOARD_PASTE, LOCATOR, null);

            var constructor = Actions.ClipboardAction.class.getDeclaredConstructor(Actions.class);
            constructor.setAccessible(true);
            Assert.assertNotNull(constructor.newInstance(actions));
            var parentConstructor = Actions.ClipboardAction.class.getDeclaredConstructor(Actions.class, Actions.class);
            parentConstructor.setAccessible(true);
            Assert.assertNotNull(parentConstructor.newInstance(actions, actions));
        }
    }

    @Test
    public void performActionShouldExerciseClickTypingJavascriptClearTextAndClipboardBranches() {
        WebDriver driver = mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(JavascriptExecutor.class, TakesScreenshot.class));
        WebElement element = standardElement();
        DriverFactoryHelper helper = helperFor(driver);
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));
        when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        when(((JavascriptExecutor) driver).executeScript(anyString(), any(Object[].class))).thenReturn(null);

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            Actions actions = new Actions(helper);

            actions.click(LOCATOR)
                    .clickUsingJavascript(LOCATOR)
                    .setValueUsingJavaScript(LOCATOR, "js")
                    .type(LOCATOR, "alpha")
                    .typeSecure(LOCATOR, "secret")
                    .typeAppend(LOCATOR, "omega")
                    .clear(LOCATOR)
                    .clipboard().copyAll(LOCATOR)
                    .clipboard().cutAll(LOCATOR)
                    .clipboard().paste(LOCATOR)
                    .clipboard().deleteAll(LOCATOR);

            Assert.assertEquals(actions.get().attribute(LOCATOR, "href"), "attr-href");
            Assert.assertEquals(actions.get().domAttribute(LOCATOR, "data"), "dom-data");
            Assert.assertEquals(actions.get().domProperty(LOCATOR, "value"), "");
            Assert.assertEquals(actions.get().name(LOCATOR), "accessible target");
            Assert.assertEquals(actions.get().text(LOCATOR), "visible text");
            Assert.assertEquals(actions.get().cssValue(LOCATOR, "color"), "css-color");
            Assert.assertTrue(actions.get().isDisplayed(LOCATOR));
            Assert.assertTrue(actions.get().isEnabled(LOCATOR));
            Assert.assertFalse(actions.get().isSelected(LOCATOR));

            verify(element, atLeastOnce()).click();
            verify(element).clear();
            verify(element, atLeastOnce()).sendKeys(any(CharSequence[].class));
            verify((JavascriptExecutor) driver, atLeastOnce()).executeScript(anyString(), any(Object[].class));
        }
    }

    @Test
    public void performActionShouldFallbackToJavascriptClickWhenWebDriverClickFails() {
        SHAFT.Properties.flags.set().clickUsingJavascriptWhenWebDriverClickFails(true);
        WebDriver driver = mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(JavascriptExecutor.class, TakesScreenshot.class));
        WebElement element = standardElement();
        doThrow(new InvalidElementStateException("native click blocked")).when(element).click();
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));
        when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        DriverFactoryHelper helper = helperFor(driver);

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helper).click(LOCATOR);
            verify((JavascriptExecutor) driver).executeScript(eq("arguments[0].click();"), eq(element));
        }
    }

    @Test
    public void performActionShouldReportSeleniumExceptionWhenFallbackIsDisabled() {
        SHAFT.Properties.flags.set().clickUsingJavascriptWhenWebDriverClickFails(false);
        WebDriver driver = mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(JavascriptExecutor.class, TakesScreenshot.class));
        WebElement element = standardElement();
        doThrow(new InvalidElementStateException("native click blocked")).when(element).click();
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));
        when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        DriverFactoryHelper helper = helperFor(driver);

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            RuntimeException exception = Assert.expectThrows(RuntimeException.class, () -> new Actions(helper).click(LOCATOR));
            Assert.assertTrue(hasCause(exception, InvalidElementStateException.class));
            verify((JavascriptExecutor) driver, org.mockito.Mockito.never()).executeScript(eq("arguments[0].click();"), eq(element));
        }
    }

    @Test
    public void performActionShouldFallbackFromAccessibleNameExceptionToDomText() {
        SHAFT.Properties.reporting.set().captureElementName(true);
        WebDriver driver = mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(JavascriptExecutor.class, TakesScreenshot.class));
        WebElement element = standardElement();
        when(element.getAccessibleName()).thenThrow(new UnsupportedCommandException("unsupported accessible name"));
        when(element.getDomProperty("text")).thenReturn("fallback name");
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));
        DriverFactoryHelper helper = helperFor(driver);

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helper).typeAppend(LOCATOR, "append");
            verify(element).getDomProperty("text");
        }
    }

    @Test
    public void performActionShouldCoverDragAndDropAndOffsetBranchesWithMockedSeleniumActions() {
        WebDriver driver = mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(JavascriptExecutor.class, TakesScreenshot.class));
        WebElement source = standardElement();
        WebElement destination = standardElement();
        By destinationLocator = By.id("destination");
        when(driver.findElements(LOCATOR)).thenReturn(List.of(source));
        when(driver.findElements(destinationLocator)).thenReturn(List.of(destination));
        DriverFactoryHelper helper = helperFor(driver);

        try (var ignoredWait = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class);
             var constructedActions = org.mockito.Mockito.mockConstruction(org.openqa.selenium.interactions.Actions.class,
                     (mock, context) -> {
                         when(mock.pause(any())).thenReturn(mock);
                         when(mock.dragAndDrop(any(WebElement.class), any(WebElement.class))).thenReturn(mock);
                         when(mock.dragAndDropBy(any(WebElement.class), anyInt(), anyInt())).thenReturn(mock);
                     })) {
            Actions actions = new Actions(helper);
            actions.dragAndDrop(LOCATOR, destinationLocator)
                    .dragAndDropByOffset(LOCATOR, 10, 20);

            Assert.assertFalse(constructedActions.constructed().isEmpty());
            verify(constructedActions.constructed().get(0), atLeastOnce()).perform();
        }
    }

    @Test
    public void performActionShouldCoverMobileClickDoubleClickAndDragBranchesWithMockedAppiumDriver() {
        SHAFT.Properties.platform.set().targetPlatform(org.openqa.selenium.Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().browserName("");
        AppiumDriver appiumDriver = mock(AppiumDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        WebElement hidden = standardElement();
        when(hidden.isDisplayed()).thenReturn(false);
        WebElement displayed = standardElement();
        RemoteWebElement remoteSource = new RemoteWebElement();
        remoteSource.setId("source-id");
        WebElement destination = standardElement();
        By destinationLocator = By.id("mobileDestination");
        when(appiumDriver.findElements(LOCATOR)).thenReturn(List.of(hidden, displayed, remoteSource));
        when(appiumDriver.findElements(destinationLocator)).thenReturn(List.of(destination));
        when(((TakesScreenshot) appiumDriver).getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        DriverFactoryHelper helper = helperFor(appiumDriver);

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            Actions actions = new Actions(helper);
            actions.click(LOCATOR)
                    .doubleClick(LOCATOR)
                    .dragAndDrop(LOCATOR, destinationLocator);

            verify(displayed).click();
            verify(appiumDriver, atLeastOnce()).perform(any(List.class));
        }
    }

    @Test
    public void privateHelpersShouldCoverClearModesTextParsingScreenshotStylesAndMobileDragFallbacks() throws Exception {
        WebDriver driver = mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(JavascriptExecutor.class, TakesScreenshot.class));
        WebElement element = standardElement();
        WebElement textElement = standardElement();
        when(textElement.getText()).thenReturn("");
        when(textElement.getDomProperty("value")).thenReturn("value-text");
        when(element.getText()).thenReturn("plain");
        when(element.getDomProperty("value")).thenReturn("");
        when(element.getDomProperty("textContent")).thenReturn("");
        when(element.getDomProperty("innerHTML")).thenReturn("inner-text");
        DriverFactoryHelper helper = helperFor(driver);

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            Actions actions = new Actions(helper);

            invoke(actions, "executeClearBasedOnClearMode", new Class[]{WebElement.class, String.class}, element, "native");
            invoke(actions, "executeClearBasedOnClearMode", new Class[]{WebElement.class, String.class}, element, "backspace");
            invoke(actions, "executeClearBasedOnClearMode", new Class[]{WebElement.class, String.class}, element, "off");

            Assert.assertEquals(invoke(actions, "parseElementText", new Class[]{WebElement.class}, textElement), "value-text");
            Assert.assertEquals(invoke(actions, "parseElementText", new Class[]{WebElement.class}, element), "plain");
            WebElement innerHtmlElement = standardElement();
            when(innerHtmlElement.getText()).thenReturn("");
            when(innerHtmlElement.getDomProperty("value")).thenReturn("");
            when(innerHtmlElement.getDomProperty("textContent")).thenReturn("");
            when(innerHtmlElement.getDomProperty("innerHTML")).thenReturn("inner-text");
            Assert.assertEquals(invoke(actions, "parseElementText", new Class[]{WebElement.class}, innerHtmlElement), "inner-text");
            Assert.assertTrue(((String) invoke(actions, "setHighlightedElementStyle", new Class[]{boolean.class}, true)).contains("#46aad2"));
            Assert.assertTrue(((String) invoke(actions, "setHighlightedElementStyle", new Class[]{boolean.class}, false)).contains("#FF0000"));
            byte[] rawScreenshot = new byte[]{1, 2, 3};
            Assert.assertEquals((byte[]) invoke(actions, "appendShaftWatermark", new Class[]{byte[].class}, rawScreenshot), rawScreenshot);
        }

        AppiumDriver appiumDriver = mock(AppiumDriver.class);
        RemoteWebElement remoteSource = new RemoteWebElement();
        remoteSource.setId("remote-id");
        WebElement destination = standardElement();
        invokeStatic("executeMobileDragAndDrop", new Class[]{AppiumDriver.class, WebElement.class, WebElement.class},
                appiumDriver, remoteSource, destination);
        verify(appiumDriver).executeScript(eq("mobile: dragGesture"), any(Map.class));

        WebElement source = standardElement();
        invokeStatic("executeMobileDragAndDrop", new Class[]{AppiumDriver.class, WebElement.class, WebElement.class},
                appiumDriver, source, destination);
        verify(appiumDriver).perform(any(List.class));
    }

    @Test
    public void staticHelpersShouldCoverExceptionIgnoringAndStatusTraceBranches() {
        WebElement throwing = mock(WebElement.class);
        when(throwing.isDisplayed()).thenThrow(new WebDriverException("stale-ish"));
        WebElement displayed = mock(WebElement.class);
        when(displayed.isDisplayed()).thenReturn(true);

        Assert.assertSame(Actions.chooseBestEffortDisplayedElement(List.of(throwing, displayed)), displayed);
        Assert.assertTrue(Actions.hasDisplayedElement(List.of(throwing, displayed)));
        Assert.assertSame(Actions.chooseDragAndDropElement(List.of(throwing, displayed), LOCATOR, "source"), displayed);
        WebElement hidden = mock(WebElement.class);
        when(hidden.isDisplayed()).thenReturn(false);
        Assert.assertSame(Actions.chooseBestEffortDisplayedElement(List.of(hidden)), hidden);
        Assert.expectThrows(NoSuchElementException.class, () -> Actions.chooseDragAndDropElement(List.of(hidden), LOCATOR, "source"));
        Assert.expectThrows(NoSuchElementException.class, () -> Actions.chooseBestEffortDisplayedElement(List.of()));
        Assert.expectThrows(NoSuchElementException.class, () -> Actions.chooseDragAndDropElement(null, LOCATOR, "source"));
        Assert.assertFalse(Actions.hasDisplayedElement(null));
        Assert.assertFalse(Actions.hasDisplayedElement(List.of(throwing)));

        Sequence sequence = Actions.buildTouchDragAndDropSequence(new Rectangle(0, 0, 10, 10), new Rectangle(10, 20, 30, 40));
        Assert.assertNotNull(sequence.toJson());
        Assert.assertEquals(Actions.createDragGestureParameters("id", new Rectangle(10, 20, 30, 40)).get("endX"), 30);
        Assert.assertTrue(Actions.createDragAndDropDestinationNotFoundException(LOCATOR, new NoSuchElementException("root"))
                .getMessage().contains("destination element"));

        StatusDetails blankDetails = new StatusDetails();
        Assert.assertTrue(Actions.mergeStatusTrace(blankDetails, new RuntimeException("boom")).contains("boom"));
        StatusDetails existingDetails = new StatusDetails().setTrace("existing");
        Assert.assertTrue(Actions.mergeStatusTrace(existingDetails, new RuntimeException("boom")).startsWith("existing"));
        Assert.assertTrue(Actions.createFailureMessageWithCausedBy(new RuntimeException("wrapper", new IllegalArgumentException("cause")))
                .contains("Caused by: java.lang.IllegalArgumentException: cause"));
    }



    @Test
    public void getTextShouldReturnEmptyStringWhenAllTextFallbacksAreNull() {
        WebDriver driver = mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(JavascriptExecutor.class, TakesScreenshot.class));
        WebElement element = standardElement();
        when(element.getText()).thenReturn(null);
        when(element.getDomProperty(anyString())).thenReturn(null);
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));
        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            Assert.assertEquals(new Actions(helperFor(driver)).get().text(LOCATOR), "");
        }
    }

    @Test
    public void performActionShouldCoverHoverHoldDoubleClickDropFileTextFallbackAndSelectedTextBranches() throws Exception {
        File uploadFile = File.createTempFile("shaft-actions-upload", ".txt");
        uploadFile.deleteOnExit();
        WebDriver driver = mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(JavascriptExecutor.class, TakesScreenshot.class));
        WebElement element = standardElement();
        WebElement uploadInput = standardElement();
        WebElement optionOne = standardElement();
        WebElement optionTwo = standardElement();
        when(element.getText()).thenReturn("");
        when(element.getDomProperty("textContent")).thenReturn("", "content fallback");
        when(element.getDomProperty("value")).thenReturn("", "value fallback");
        when(element.getTagName()).thenReturn("select");
        when(optionOne.isSelected()).thenReturn(true);
        when(optionTwo.isSelected()).thenReturn(false);
        when(optionOne.getText()).thenReturn("one");
        when(optionTwo.getText()).thenReturn("two");
        when(element.findElements(any(By.class))).thenReturn(List.of(optionOne, optionTwo));
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));
        when(((JavascriptExecutor) driver).executeScript(anyString(), any(Object[].class))).thenReturn(uploadInput);
        DriverFactoryHelper helper = helperFor(driver);

        try (var ignoredWait = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class);
             var ignoredSeleniumActions = org.mockito.Mockito.mockConstruction(org.openqa.selenium.interactions.Actions.class,
                     (mock, context) -> {
                         when(mock.pause(any())).thenReturn(mock);
                         when(mock.moveToElement(any(WebElement.class))).thenReturn(mock);
                         when(mock.clickAndHold(any(WebElement.class))).thenReturn(mock);
                         when(mock.doubleClick(any(WebElement.class))).thenReturn(mock);
                         when(mock.scrollToElement(any(WebElement.class))).thenReturn(mock);
                     })) {
            Actions actions = new Actions(helper);
            actions.hover(LOCATOR)
                    .clickAndHold(LOCATOR)
                    .doubleClick(LOCATOR)
                    .dropFileToUpload(LOCATOR, uploadFile.getAbsolutePath());

            Assert.assertNotNull(actions.get().text(LOCATOR));
            Assert.assertEquals(actions.get().selectedText(LOCATOR), "one");
            verify(uploadInput).sendKeys(uploadFile.getAbsolutePath());
            Assert.assertFalse(ignoredSeleniumActions.constructed().isEmpty());
        }
    }

    @Test
    public void waitUntilShouldCoverPassAndBrokenBranches() {
        WebDriver driver = mock(WebDriver.class);
        DriverFactoryHelper helper = helperFor(driver);
        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            Actions actions = new Actions(helper);
            Assert.assertSame(actions.waitUntil(d -> "ready", java.time.Duration.ofMillis(50)), actions);
            RuntimeException exception = Assert.expectThrows(RuntimeException.class,
                    () -> actions.waitUntil(d -> false, java.time.Duration.ofMillis(1)));
            Assert.assertNotNull(exception);
        }
    }

    @Test
    public void findAllElementsShouldCoverShadowDomAndIFrameBranches() throws Exception {
        WebDriver driver = mock(WebDriver.class);
        DriverFactoryHelper helper = helperFor(driver);
        WebElement host = standardElement();
        SearchContext shadowRoot = mock(SearchContext.class);
        WebDriver.TargetLocator targetLocator = mock(WebDriver.TargetLocator.class);
        WebElement frame = standardElement();
        By hostLocator = By.cssSelector("host");
        By cssLocator = By.cssSelector("button");
        By frameLocator = By.id("frame");
        when(driver.switchTo()).thenReturn(targetLocator);
        when(targetLocator.defaultContent()).thenReturn(driver);
        when(targetLocator.frame(frame)).thenReturn(driver);
        when(driver.findElement(hostLocator)).thenReturn(host);
        when(host.getShadowRoot()).thenReturn(shadowRoot);
        when(shadowRoot.findElements(cssLocator)).thenReturn(List.of(host));
        when(driver.findElement(frameLocator)).thenReturn(frame);
        when(driver.findElements(LOCATOR)).thenReturn(List.of(frame));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            Actions actions = new Actions(helper);
            new ShadowLocatorBuilder(hostLocator, cssLocator);
            Assert.assertEquals(invoke(actions, "findAllElements", new Class[]{By.class}, cssLocator), List.of(host));
            ShadowLocatorBuilder.cleanup();

            Field iFrameLocator = LocatorBuilder.class.getDeclaredField("iFrameLocator");
            iFrameLocator.setAccessible(true);
            @SuppressWarnings("unchecked")
            ThreadLocal<By> iFrameThreadLocal = (ThreadLocal<By>) iFrameLocator.get(null);
            iFrameThreadLocal.set(frameLocator);
            Assert.assertEquals(invoke(actions, "findAllElements", new Class[]{By.class}, LOCATOR), List.of(frame));
        }
    }

    @Test
    public void screenshotHelpersShouldCoverFullElementViewportHighlightAndWatermarkBranches() throws Exception {
        WebDriver driver = mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(JavascriptExecutor.class, TakesScreenshot.class));
        WebElement element = standardElement();
        DriverFactoryHelper helper = helperFor(driver);
        when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        when(element.getScreenshotAs(OutputType.BYTES)).thenThrow(new WebDriverException("element screenshot failed"));
        when(element.getDomProperty("style")).thenReturn("display:block;");

        try (var ignoredWait = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class);
             var screenshotHelper = org.mockito.Mockito.mockStatic(ScreenshotHelper.class);
             var animatedGif = org.mockito.Mockito.mockStatic(AnimatedGifManager.class);
             var imageProcessing = org.mockito.Mockito.mockStatic(ImageProcessingActions.class);
             MockedConstruction<ElementActionsHelper> helperConstruction = org.mockito.Mockito.mockConstruction(ElementActionsHelper.class,
                     (mock, context) -> {
                         List<Object> skippedElementInformation = new ArrayList<>(List.of(
                                 1, element, By.xpath("//div[@id='skip']"), "div", "skip", "", "", ""));
                         when(mock.getMatchingElementsInformation(any(WebDriver.class), any(By.class), eq(false)))
                                 .thenReturn(skippedElementInformation);
                     })) {
            screenshotHelper.when(() -> ScreenshotHelper.makeFullScreenshot(any(WebDriver.class))).thenReturn(PNG);
            screenshotHelper.when(() -> ScreenshotHelper.makeFullScreenshot(any(WebDriver.class), any(WebElement[].class))).thenReturn(PNG);
            imageProcessing.when(() -> ImageProcessingActions.highlightElementInScreenshot(any(byte[].class), any(Rectangle.class), any(Color.class))).thenReturn(PNG);
            animatedGif.when(() -> AnimatedGifManager.startOrAppendToAnimatedGif(any(byte[].class))).thenAnswer(inv -> null);
            Actions actions = new Actions(helper);

            SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Always");
            SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("FULL");
            SHAFT.Properties.visuals.set().screenshotParamsSkippedElementsFromScreenshot("//div[@id='skip']");
            Assert.assertNotNull(invoke(actions, "takeActionScreenshot", new Class[]{WebElement.class}, element));
            Assert.assertNotNull(invoke(actions, "takeFailureScreenshot", new Class[]{WebElement.class}, element));
            ElementActionsHelper mockedHelper = helperConstruction.constructed().getFirst();
            verify(mockedHelper, atLeastOnce()).getMatchingElementsInformation(any(WebDriver.class), any(By.class), eq(false));
            verify(mockedHelper, org.mockito.Mockito.never()).getElementsCount(any(WebDriver.class), any(By.class));
            verify(mockedHelper, org.mockito.Mockito.never()).identifyUniqueElementIgnoringVisibility(any(WebDriver.class), any(By.class));

            SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("ELEMENT");
            Assert.assertNotNull(invoke(actions, "captureScreenshot", new Class[]{WebElement.class, boolean.class}, element, true));

            SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("VIEWPORT");
            SHAFT.Properties.visuals.set().screenshotParamsHighlightElements(true);
            SHAFT.Properties.visuals.set().screenshotParamsHighlightMethod("AI");
            Assert.assertNotNull(invoke(actions, "captureScreenshot", new Class[]{WebElement.class, boolean.class}, element, true));
            SHAFT.Properties.visuals.set().screenshotParamsHighlightMethod("JavaScript");
            Assert.assertNotNull(invoke(actions, "captureScreenshot", new Class[]{WebElement.class, boolean.class}, element, false));
            verify((JavascriptExecutor) driver, atLeastOnce()).executeScript(anyString(), any(Object[].class));
        }
    }

    @Test
    public void animatedGifActionScreenshotShouldNotAttachWhenPolicyIsValidationOnly() throws Exception {
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("ValidationPointsOnly");
        SHAFT.Properties.visuals.set().createAnimatedGif(true);
        SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("VIEWPORT");
        SHAFT.Properties.visuals.set().screenshotParamsHighlightElements(false);

        WebDriver driver = mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        WebElement element = standardElement();
        DriverFactoryHelper helper = helperFor(driver);
        when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);

        try (var animatedGif = org.mockito.Mockito.mockStatic(AnimatedGifManager.class)) {
            animatedGif.when(() -> AnimatedGifManager.startOrAppendToAnimatedGif(any(byte[].class), eq(false))).thenAnswer(inv -> null);

            Object actionScreenshot = invoke(new Actions(helper), "takeActionScreenshot", new Class[]{WebElement.class}, element);
            Object failureScreenshot = invoke(new Actions(helper), "takeFailureScreenshot", new Class[]{WebElement.class}, (Object) null);

            Assert.assertNull(actionScreenshot, "GIF capture should not attach per-action screenshots by itself");
            Assert.assertNotNull(failureScreenshot, "failed actions should attach screenshots regardless of policy");
            animatedGif.verify(() -> AnimatedGifManager.startOrAppendToAnimatedGif(any(byte[].class), eq(false)), org.mockito.Mockito.atLeastOnce());
        }
    }

    private DriverFactoryHelper helperFor(WebDriver driver) {
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(driver);
        return helper;
    }

    @SuppressWarnings("unchecked")
    private StepResult captureStepUpdates(AllureLifecycle lifecycle) {
        StepResult step = new StepResult();
        org.mockito.Mockito.doAnswer(invocation -> {
            Consumer<StepResult> consumer = invocation.getArgument(0);
            consumer.accept(step);
            return null;
        }).when(lifecycle).updateStep(org.mockito.ArgumentMatchers.<Consumer<StepResult>>any());
        return step;
    }

    private String parameterValue(StepResult step, String name) {
        return step.getParameters().stream()
                .filter(parameter -> name.equals(parameter.getName()))
                .findFirst()
                .orElseThrow(() -> new AssertionError("Missing parameter: " + name))
                .getValue();
    }

    private boolean hasCause(Throwable throwable, Class<? extends Throwable> expectedCause) {
        Throwable current = throwable;
        while (current != null) {
            if (expectedCause.isInstance(current)) {
                return true;
            }
            current = current.getCause();
        }
        return false;
    }

    private WebElement standardElement() {
        WebElement element = mock(WebElement.class);
        when(element.isDisplayed()).thenReturn(true);
        when(element.isEnabled()).thenReturn(true);
        when(element.isSelected()).thenReturn(false);
        when(element.getAccessibleName()).thenReturn("accessible target");
        when(element.getText()).thenReturn("visible text");
        when(element.getAttribute(anyString())).thenAnswer(invocation -> "attr-" + invocation.getArgument(0));
        when(element.getDomAttribute(anyString())).thenAnswer(invocation -> "dom-" + invocation.getArgument(0));
        when(element.getDomProperty(anyString())).thenAnswer(invocation -> "value".equals(invocation.getArgument(0)) ? "" : "dom-" + invocation.getArgument(0));
        when(element.getCssValue(anyString())).thenAnswer(invocation -> "css-" + invocation.getArgument(0));
        when(element.getRect()).thenReturn(new Rectangle(10, 20, 30, 40));
        when(element.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        return element;
    }

    private void assertRecorded(RecordingActions actions, Actions.ActionType action, By locator, Object data) {
        Assert.assertEquals(actions.action, action);
        Assert.assertEquals(actions.locator, locator);
        if (data instanceof CharSequence[] expected) {
            Assert.assertEquals((CharSequence[]) actions.data, expected);
        } else {
            Assert.assertEquals(actions.data, data);
        }
    }

    private Object invoke(Object target, String methodName, Class<?>[] parameterTypes, Object... args) throws Exception {
        Method method = Actions.class.getDeclaredMethod(methodName, parameterTypes);
        method.setAccessible(true);
        return method.invoke(target, args);
    }

    private Object invokeStatic(String methodName, Class<?>[] parameterTypes, Object... args) throws Exception {
        Method method = Actions.class.getDeclaredMethod(methodName, parameterTypes);
        method.setAccessible(true);
        return method.invoke(null, args);
    }

    private static final class RecordingActions extends Actions {
        private Actions.ActionType action;
        private By locator;
        private Object data;
        private String returnValue = "";

        private RecordingActions(WebDriver driver) {
            super(driver);
        }

        @Override
        protected String performAction(ActionType action, By locator, Object data) {
            this.action = action;
            this.locator = locator;
            this.data = data;
            return returnValue;
        }
    }
}
