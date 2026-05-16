package com.shaft.gui.element.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.properties.internal.Properties;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.InvalidSelectorException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.FluentWait;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@Test(singleThreaded = true)
public class ElementsHelperCoverageUnitTest {
    private ElementActionsHelper helper;
    private WebDriver driver;
    private WebElement element;
    private By locator;

    @BeforeMethod(alwaysRun = true)
    public void setUp() {
        SHAFT.Properties.reporting.set().captureElementName(true);
        SHAFT.Properties.flags.set().forceCheckElementLocatorIsUnique(false);
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(1);

        helper = new ElementActionsHelper(true);
        driver = mock(WebDriver.class, Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        element = mock(WebElement.class);
        locator = mock(By.class);

        when(locator.toString()).thenReturn("By.cssSelector: #sample");
        when(driver.findElement(locator)).thenReturn(element);
        when(driver.findElements(locator)).thenReturn(List.of(element));
        when(((JavascriptExecutor) driver).executeScript(any(String.class), any())).thenReturn(null);
        when(element.getRect()).thenReturn(new Rectangle(1, 2, 3, 4));
        when(element.getDomProperty("outerHTML")).thenReturn("<button id='sample'>Save</button>");
        when(element.getDomProperty("innerHTML")).thenReturn("Save");
        when(element.getAccessibleName()).thenReturn("Save Button");
        when(element.isDisplayed()).thenReturn(true);
        when(element.isEnabled()).thenReturn(true);
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void safeFindElementShouldReturnSingleElementFromDriver() {
        WebElement actual = ElementActionsHelper.safeFindElement(driver, locator);

        Assert.assertSame(actual, element);
    }

    @Test
    public void safeFindElementsShouldReturnEmptyAndMultipleElementResults() {
        when(driver.findElements(locator)).thenReturn(Collections.emptyList(), List.of(element, mock(WebElement.class)));

        Assert.assertTrue(ElementActionsHelper.safeFindElements(driver, locator).isEmpty());
        Assert.assertEquals(ElementActionsHelper.safeFindElements(driver, locator).size(), 2);
    }

    @Test
    public void safeFindElementShouldWrapNonAppiumStackOverflowAsMissingElement() {
        when(driver.findElement(locator)).thenThrow(new StackOverflowError("recursive locator"));

        NoSuchElementException exception = Assert.expectThrows(NoSuchElementException.class,
                () -> ElementActionsHelper.safeFindElement(driver, locator));

        Assert.assertTrue(exception.getMessage().contains("StackOverflowError"));
    }

    @Test
    public void waitForElementPresenceShouldReturnSingleElementMetadata() {
        try (MockedStatic<DriverFactoryHelper> driverFactoryHelper = mockDesktopExecution();
             MockedConstruction<SynchronizationManager> ignored = mockSynchronizationManagerApplyingCondition()) {
            List<Object> information = helper.waitForElementPresence(driver, locator, true);

            Assert.assertEquals(information.get(0), 1);
            Assert.assertSame(information.get(1), element);
            Assert.assertSame(information.get(2), locator);
            Assert.assertEquals(information.get(3), "<button id='sample'>Save</button>");
            Assert.assertEquals(information.get(4), "Save");
            Assert.assertEquals(information.get(5), "Save Button");
            verify((JavascriptExecutor) driver).executeScript(any(String.class), eq(element));
            driverFactoryHelper.verify(DriverFactoryHelper::isMobileNativeExecution, Mockito.atLeastOnce());
            driverFactoryHelper.verify(DriverFactoryHelper::isMobileWebExecution, Mockito.atLeastOnce());
        }
    }

    @Test
    public void waitForElementPresenceShouldReturnMultipleElementCount() {
        WebElement secondElement = mock(WebElement.class);
        when(driver.findElements(locator)).thenReturn(List.of(element, secondElement));

        try (MockedStatic<DriverFactoryHelper> ignoredDriverFactoryHelper = mockDesktopExecution();
             MockedConstruction<SynchronizationManager> ignored = mockSynchronizationManagerApplyingCondition()) {
            List<Object> information = helper.waitForElementPresence(driver, locator, true);

            Assert.assertEquals(information.get(0), 2);
            Assert.assertSame(information.get(1), element);
        }
    }

    @Test
    public void waitForElementPresenceShouldReturnMissingElementPayloadOnTimeout() {
        TimeoutException timeoutException = new TimeoutException("timed out",
                new NoSuchElementException("missing element\nextra remote details"));

        try (MockedStatic<DriverFactoryHelper> ignoredDriverFactoryHelper = mockDesktopExecution();
             MockedConstruction<SynchronizationManager> ignored = mockSynchronizationManagerThrowing(timeoutException)) {
            List<Object> information = helper.waitForElementPresence(driver, locator, true);

            Assert.assertEquals(information.get(0), 0);
            Assert.assertNull(information.get(1));
            Assert.assertSame(information.get(2), timeoutException);
        }
    }

    @Test
    public void waitForElementPresenceShouldReturnInvalidSelectorPayload() {
        InvalidSelectorException invalidSelectorException = new InvalidSelectorException("bad css");

        try (MockedStatic<DriverFactoryHelper> ignoredDriverFactoryHelper = mockDesktopExecution();
             MockedConstruction<SynchronizationManager> ignored = mockSynchronizationManagerThrowing(invalidSelectorException)) {
            List<Object> information = helper.waitForElementPresence(driver, locator, true);

            Assert.assertEquals(information.get(0), 0);
            Assert.assertNull(information.get(1));
            Assert.assertSame(information.get(2), invalidSelectorException);
        }
    }

    @Test
    public void getMatchingElementsInformationShouldHandleNullAndHtmlLocatorsWithoutDriverLookup() {
        List<Object> nullLocatorInformation = helper.getMatchingElementsInformation(driver, null, true);
        List<Object> htmlLocatorInformation = helper.getMatchingElementsInformation(driver, By.tagName("html"), true);

        Assert.assertEquals(nullLocatorInformation.get(0), 0);
        Assert.assertNull(nullLocatorInformation.get(1));
        Assert.assertEquals(htmlLocatorInformation.get(0), 1);
        Assert.assertNull(htmlLocatorInformation.get(1));
    }

    @Test
    public void identifyUniqueElementIgnoringVisibilityShouldReturnSingleMatch() {
        try (MockedStatic<DriverFactoryHelper> ignoredDriverFactoryHelper = mockDesktopExecution();
             MockedConstruction<SynchronizationManager> ignored = mockSynchronizationManagerApplyingCondition()) {
            List<Object> information = helper.identifyUniqueElementIgnoringVisibility(driver, locator);

            Assert.assertEquals(information.get(0), 1);
            Assert.assertSame(information.get(1), element);
        }
    }

    @Test
    public void getElementNameShouldFallbackToFormattedLocatorWhenAccessibleNameFails() {
        when(element.getAccessibleName()).thenThrow(new RuntimeException("computed label unsupported"));

        try (MockedStatic<DriverFactoryHelper> ignoredDriverFactoryHelper = mockDesktopExecution();
             MockedConstruction<SynchronizationManager> ignored = mockSynchronizationManagerApplyingCondition()) {
            String elementName = helper.getElementName(driver, locator);

            Assert.assertEquals(elementName, "By.cssSelector: #sample");
        }
    }

    @Test
    public void waitForElementToBeClickableShouldCheckDisplayedAndEnabled() {
        try (MockedStatic<DriverFactoryHelper> driverFactoryHelper = Mockito.mockStatic(DriverFactoryHelper.class);
             MockedConstruction<SynchronizationManager> ignored = mockSynchronizationManagerApplyingCondition()) {
            driverFactoryHelper.when(DriverFactoryHelper::isMobileNativeExecution).thenReturn(false);

            Assert.assertTrue(helper.waitForElementToBeClickable(driver, locator, ""));
            verify(element).isDisplayed();
            verify(element).isEnabled();
        }
    }

    @Test
    public void waitForElementToBeClickableShouldReturnFalseOnTimeout() {
        try (MockedStatic<DriverFactoryHelper> driverFactoryHelper = Mockito.mockStatic(DriverFactoryHelper.class);
             MockedConstruction<SynchronizationManager> ignored = mockSynchronizationManagerThrowing(new TimeoutException("not clickable"))) {
            driverFactoryHelper.when(DriverFactoryHelper::isMobileNativeExecution).thenReturn(false);

            Assert.assertFalse(helper.waitForElementToBeClickable(driver, locator, ""));
        }
    }


    @Test
    public void waitForElementTextToBeNotShouldReturnTrueWhenTextChangesAndFalseOnTimeout() {
        when(element.getText()).thenReturn("ready");
        try (MockedConstruction<SynchronizationManager> ignored = mockSynchronizationManagerApplyingCondition()) {
            Assert.assertTrue(helper.waitForElementTextToBeNot(driver, locator, "loading"));
        }

        try (MockedConstruction<SynchronizationManager> ignored = mockSynchronizationManagerThrowing(new TimeoutException("same text"))) {
            Assert.assertFalse(helper.waitForElementTextToBeNot(driver, locator, "ready"));
        }
    }

    @Test
    public void scrollToFindElementShouldReturnFoundElementInformation() {
        try (MockedConstruction<SynchronizationManager> ignored = mockSynchronizationManagerApplyingCondition()) {
            List<Object> information = helper.scrollToFindElement(driver, locator);

            Assert.assertEquals(information.get(0), 1);
            Assert.assertSame(information.get(1), element);
        }
    }

    @Test
    public void javaScriptHelpersShouldExecuteOnlyForDesktopSessions() {
        try (MockedStatic<DriverFactoryHelper> driverFactoryHelper = Mockito.mockStatic(DriverFactoryHelper.class);
             MockedConstruction<SynchronizationManager> ignored = mockSynchronizationManagerApplyingCondition()) {
            driverFactoryHelper.when(DriverFactoryHelper::isNotMobileExecution).thenReturn(true);
            driverFactoryHelper.when(DriverFactoryHelper::isMobileNativeExecution).thenReturn(false);
            driverFactoryHelper.when(DriverFactoryHelper::isMobileWebExecution).thenReturn(false);

            helper.executeNativeMobileCommandUsingJavascript(driver, "mobile: scroll", java.util.Map.of("direction", "down"));
            helper.submitFormUsingJavascript(driver, locator);
            helper.changeWebElementVisibilityUsingJavascript(driver, locator, true);
            helper.changeWebElementVisibilityUsingJavascript(driver, locator, false);

            verify((JavascriptExecutor) driver).executeScript(eq("mobile: scroll"), eq(java.util.Map.of("direction", "down")));
            verify((JavascriptExecutor) driver).executeScript(eq("arguments[0].submit();"), eq(element));
            verify((JavascriptExecutor) driver).executeScript(eq("arguments[0].setAttribute('style', 'display:block !important;');"), eq(element));
            verify((JavascriptExecutor) driver).executeScript(eq("arguments[0].setAttribute('style', 'display:none');"), eq(element));
        }
    }

    @Test
    public void takeScreenshotShouldDelegateToScreenshotManagerForPassAndFailureBranches() {
        List<Object> elementScreenshot = List.of("element screenshot");
        List<Object> fullPageScreenshot = List.of("full page screenshot");
        try (MockedConstruction<ScreenshotManager> screenshots = Mockito.mockConstruction(ScreenshotManager.class,
                (manager, context) -> {
                    when(manager.takeScreenshot(driver, locator, "click", true)).thenReturn(elementScreenshot);
                    when(manager.takeScreenshot(driver, null, "click", false)).thenReturn(fullPageScreenshot);
                    when(manager.takeScreenshot(driver, null, "type", true)).thenReturn(fullPageScreenshot);
                })) {
            Assert.assertSame(helper.takeScreenshot(driver, locator, "click", null, true), elementScreenshot);
            Assert.assertSame(helper.takeScreenshot(driver, null, "type", "value", true), fullPageScreenshot);
            Assert.assertSame(helper.takeScreenshot(driver, locator, "click", null, false), fullPageScreenshot);
            Assert.assertTrue(helper.takeScreenshot(driver, null, "switchToDefaultContent", null, true).isEmpty());
            Assert.assertEquals(screenshots.constructed().size(), 3);
        }
    }

    @Test
    public void getElementNameShouldReturnAccessibleNameAndCountShouldUseMatchingInformation() {
        try (MockedStatic<DriverFactoryHelper> ignoredDriverFactoryHelper = mockDesktopExecution();
             MockedConstruction<SynchronizationManager> ignored = mockSynchronizationManagerApplyingCondition()) {
            Assert.assertEquals(helper.getElementName(driver, locator), "Save Button");
            Assert.assertEquals(helper.getElementsCount(driver, locator), 1);
        }
    }

    @Test
    public void stacktraceSearchShouldFindExactClassNameOnly() {
        RuntimeException throwable = new RuntimeException("sample");
        throwable.setStackTrace(new StackTraceElement[]{
                new StackTraceElement(ElementActionsHelper.class.getName(), "method", "ElementActionsHelper.java", 1)
        });

        Assert.assertTrue(helper.isFoundInStacktrace(ElementActionsHelper.class, throwable));
        Assert.assertFalse(helper.isFoundInStacktrace(ElementsHelperCoverageUnitTest.class, throwable));
    }

    @Test
    public void createReportMessageShouldCoverPassFailurePrepositionsAndLongData() {
        Assert.assertEquals(helper.createReportMessage("type", " value ", " input ", true),
                "Type \"value\" into \"input\".");
        Assert.assertEquals(helper.createReportMessage("getText", "ignored", " label ", false),
                "Failed to Get text \"ignored\" from \"label\".");
        Assert.assertEquals(helper.createReportMessage("clipboard action", "", "field", true),
                "Clipboard  action on \"field\".");
        Assert.assertEquals(helper.createReportMessage("hover", "x".repeat(500), "menu", true),
                "Hover over \"menu\".");
    }

    @SuppressWarnings("unchecked")
    private MockedConstruction<SynchronizationManager> mockSynchronizationManagerApplyingCondition() {
        return Mockito.mockConstruction(SynchronizationManager.class, (manager, context) -> {
            FluentWait<WebDriver> wait = mock(FluentWait.class);
            when(manager.fluentWait()).thenReturn(wait);
            when(manager.fluentWait(anyBoolean())).thenReturn(wait);
            when(wait.until(any())).thenAnswer(invocation -> {
                Function<? super WebDriver, ?> condition = invocation.getArgument(0);
                return condition.apply(driver);
            });
        });
    }

    @SuppressWarnings("unchecked")
    private MockedConstruction<SynchronizationManager> mockSynchronizationManagerThrowing(RuntimeException throwable) {
        return Mockito.mockConstruction(SynchronizationManager.class, (manager, context) -> {
            FluentWait<WebDriver> wait = mock(FluentWait.class);
            when(manager.fluentWait()).thenReturn(wait);
            when(manager.fluentWait(anyBoolean())).thenReturn(wait);
            when(wait.until(any())).thenThrow(throwable);
        });
    }

    private MockedStatic<DriverFactoryHelper> mockDesktopExecution() {
        MockedStatic<DriverFactoryHelper> driverFactoryHelper = Mockito.mockStatic(DriverFactoryHelper.class);
        driverFactoryHelper.when(DriverFactoryHelper::isMobileNativeExecution).thenReturn(false);
        driverFactoryHelper.when(DriverFactoryHelper::isMobileWebExecution).thenReturn(false);
        return driverFactoryHelper;
    }
}
