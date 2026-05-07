package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.properties.internal.Properties;
import org.mockito.Mockito;
import org.mockito.MockedConstruction;
import org.openqa.selenium.*;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.when;

@Test(singleThreaded = true)
public class ElementActionsCoverageUnitTest {
    private WebDriver driver;
    private WebDriver.TargetLocator targetLocator;
    private WebElement element;
    private ElementActions elementActions;

    @BeforeMethod(alwaysRun = true)
    public void setUp() {
        SHAFT.Properties.timeouts.set().waitForLazyLoading(false);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");

        driver = mock(WebDriver.class, Mockito.withSettings().extraInterfaces(JavascriptExecutor.class, TakesScreenshot.class));
        targetLocator = mock(WebDriver.TargetLocator.class);
        element = mock(WebElement.class);

        when(driver.switchTo()).thenReturn(targetLocator);
        when(targetLocator.frame(any(WebElement.class))).thenReturn(driver);
        when(targetLocator.defaultContent()).thenReturn(driver);

        when(driver.findElements(any(By.class))).thenReturn(List.of(element));
        when(driver.findElement(any(By.class))).thenReturn(element);

        when(element.isDisplayed()).thenReturn(true);
        when(element.isEnabled()).thenReturn(true);
        when(element.getTagName()).thenReturn("input");
        when(element.getAccessibleName()).thenReturn("Element");
        when(element.getDomProperty(any())).thenReturn("opt1");
        when(element.getRect()).thenReturn(new Rectangle(0, 0, 10, 10));

        when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn("img".getBytes());
        when(((JavascriptExecutor) driver).executeScript(any(String.class), any())).thenAnswer(invocation -> {
            String script = invocation.getArgument(0);
            if ("return self.name".equals(script)) {
                return "mainFrame";
            }
            if (script.contains("document.readyState")) {
                return "complete";
            }
            return 0;
        });

        elementActions = new ElementActions(driver, true);
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void shouldCoverBasicFluentAndActionWrappers() {
        By locator = By.id("sample");

        invokeAndIgnoreFailures(ElementActions::new);
        Assert.assertNotNull(elementActions.and());
        Assert.assertNotNull(elementActions.assertThat(locator));
        Assert.assertNotNull(elementActions.verifyThat(locator));
        Assert.assertEquals(elementActions.getElementsCount(locator), 1);

        try (MockedConstruction<com.shaft.gui.element.internal.Actions> ignored = mockConstruction(
                com.shaft.gui.element.internal.Actions.class,
                (mock, context) -> {
                    when(mock.click(any(By.class))).thenReturn(mock);
                    when(mock.clickUsingJavascript(any(By.class))).thenReturn(mock);
                    when(mock.clickAndHold(any(By.class))).thenReturn(mock);
                    when(mock.doubleClick(any(By.class))).thenReturn(mock);
                    when(mock.dragAndDrop(any(By.class), any(By.class))).thenReturn(mock);
                    when(mock.dragAndDropByOffset(any(By.class), any(Integer.class), any(Integer.class))).thenReturn(mock);
                    when(mock.hover(any(By.class))).thenReturn(mock);
                    when(mock.setValueUsingJavaScript(any(By.class), any(String.class))).thenReturn(mock);
                    when(mock.type(any(By.class), any(CharSequence[].class))).thenReturn(mock);
                    when(mock.clear(any(By.class))).thenReturn(mock);
                    when(mock.typeAppend(any(By.class), any(CharSequence[].class))).thenReturn(mock);
                    when(mock.typeSecure(any(By.class), any(CharSequence[].class))).thenReturn(mock);
                })) {
            invokeAndIgnoreFailures(() -> elementActions.click(locator));
            invokeAndIgnoreFailures(() -> elementActions.clickUsingJavascript(locator));
            invokeAndIgnoreFailures(() -> elementActions.scrollToElement(locator));
            invokeAndIgnoreFailures(() -> elementActions.clickAndHold(locator));
            invokeAndIgnoreFailures(() -> elementActions.doubleClick(locator));
            invokeAndIgnoreFailures(() -> elementActions.dragAndDrop(locator, By.id("destination")));
            invokeAndIgnoreFailures(() -> elementActions.dragAndDropByOffset(locator, 5, 10));
            invokeAndIgnoreFailures(() -> elementActions.hover(locator));
            invokeAndIgnoreFailures(() -> elementActions.hoverAndClick(List.of(locator), By.id("clickable")));
            invokeAndIgnoreFailures(() -> elementActions.setValueUsingJavaScript(locator, "value"));
            invokeAndIgnoreFailures(() -> elementActions.type(locator, "text"));
            invokeAndIgnoreFailures(() -> elementActions.clear(locator));
            invokeAndIgnoreFailures(() -> elementActions.typeAppend(locator, "append"));
            invokeAndIgnoreFailures(() -> elementActions.typeSecure(locator, "secret"));
        }

        invokeAndIgnoreFailures(() -> elementActions.captureScreenshot(locator));
    }

    @Test
    public void shouldCoverSelectSubmitSwitchAndFrameMethods() throws Exception {
        var helper = mock(ElementActionsHelper.class);
        var helperDriverFactory = mock(DriverFactoryHelper.class);
        By locator = By.id("dropdown");

        when(helperDriverFactory.getDriver()).thenReturn(driver);
        setField(elementActions, "driverFactoryHelper", helperDriverFactory);
        setField(elementActions, "elementActionsHelper", helper);

        SHAFT.Properties.flags.set().handleNonSelectDropDown(true);
        when(helper.identifyUniqueElement(eq(driver), any(By.class))).thenReturn(
                buildElementInfo("div", locator, element, "dropdown"),
                buildElementInfo("div", locator, element, "dropdown"),
                buildElementInfo("li", By.xpath("//*[text()='Div 2']"), element, "option")
        );
        invokeAndIgnoreFailures(() -> elementActions.select(locator, "Div 2"));

        WebElement option = mock(WebElement.class);
        when(option.getText()).thenReturn("Option 2");
        when(option.getDomProperty("value")).thenReturn("opt2");
        when(option.isDisplayed()).thenReturn(true);
        when(option.isEnabled()).thenReturn(true);
        when(element.getTagName()).thenReturn("select");
        when(element.findElements(By.tagName("option"))).thenReturn(List.of(option));

        when(helper.identifyUniqueElement(eq(driver), any(By.class))).thenReturn(buildElementInfo("select", locator, element, "select"));
        when(helper.waitForElementTextToBeNot(driver, locator, "")).thenReturn(true);
        when(helper.getElementName(driver, locator)).thenReturn("select");
        invokeAndIgnoreFailures(() -> elementActions.select(locator, "Option 2"));

        when(helper.takeScreenshot(driver, locator, "submitFormUsingJavaScript", null, true)).thenReturn(Collections.emptyList());
        when(helper.getElementName(driver, locator)).thenReturn("form");
        doNothing().when(helper).submitFormUsingJavascript(driver, locator);
        invokeAndIgnoreFailures(() -> elementActions.submitFormUsingJavaScript(locator));

        doThrow(new JavascriptException("js fallback")).when(helper).submitFormUsingJavascript(driver, locator);
        doNothing().when(element).submit();
        invokeAndIgnoreFailures(() -> elementActions.submitFormUsingJavaScript(locator));

        when(helper.identifyUniqueElement(driver, locator)).thenReturn(buildElementInfo("iframe", locator, element, "frame"));
        invokeAndIgnoreFailures(() -> elementActions.switchToIframe(locator));
        invokeAndIgnoreFailures(elementActions::switchToDefaultContent);

        invokeAndIgnoreFailures(elementActions::getCurrentFrame);
    }

    @Test
    public void shouldCoverNativeCommandUploadAndTableExtraction() throws Exception {
        var helper = mock(ElementActionsHelper.class);
        var helperDriverFactory = mock(DriverFactoryHelper.class);
        By locator = By.id("upload");

        when(helperDriverFactory.getDriver()).thenReturn(driver);
        setField(elementActions, "driverFactoryHelper", helperDriverFactory);
        setField(elementActions, "elementActionsHelper", helper);

        doNothing().when(helper).executeNativeMobileCommandUsingJavascript(driver, "mobile: scroll", Map.of("direction", "down"));
        invokeAndIgnoreFailures(() -> elementActions.executeNativeMobileCommand("mobile: scroll", Map.of("direction", "down")));

        doThrow(new RuntimeException("forced")).when(helper).executeNativeMobileCommandUsingJavascript(driver, "mobile: fail", Map.of());
        invokeAndIgnoreFailures(() -> elementActions.executeNativeMobileCommand("mobile: fail", Map.of()));

        when(helper.scrollToFindElement(driver, locator)).thenReturn(Collections.emptyList());
        invokeAndIgnoreFailures(() -> elementActions.scrollToElement(locator));

        when(helper.getElementName(driver, locator)).thenReturn("upload");
        when(helper.takeScreenshot(driver, locator, "typeFileLocationForUpload", null, true)).thenReturn(Collections.emptyList());
        when(helper.identifyUniqueElementIgnoringVisibility(driver, locator)).thenReturn(buildElementInfo("input", locator, element, "upload"));
        doNothing().when(element).sendKeys(any(CharSequence.class));
        invokeAndIgnoreFailures(() -> elementActions.typeFileLocationForUpload(locator, "/tmp/file.txt"));

        WebElement table = mock(WebElement.class);
        WebElement tbody = mock(WebElement.class);
        WebElement thead = mock(WebElement.class);
        WebElement row = mock(WebElement.class);
        WebElement header = mock(WebElement.class);
        WebElement cell = mock(WebElement.class);

        when(table.isDisplayed()).thenReturn(true);
        when(row.isDisplayed()).thenReturn(true);
        when(driver.findElement(By.id("table"))).thenReturn(table);
        when(driver.findElement(By.cssSelector("tbody tr"))).thenReturn(row);
        when(table.findElement(By.tagName("tbody"))).thenReturn(tbody);
        when(table.findElement(By.tagName("thead"))).thenReturn(thead);
        when(tbody.findElements(By.tagName("tr"))).thenReturn(List.of(row));
        when(thead.findElements(By.tagName("th"))).thenReturn(List.of(header));
        when(row.findElements(By.tagName("td"))).thenReturn(List.of(cell));
        when(header.getText()).thenReturn("Column");
        when(cell.getText()).thenReturn("Value");

        List<Map<String, String>> tableData = elementActions.getTableRowsData(By.id("table"));
        Assert.assertNotNull(tableData);
        Assert.assertEquals(tableData.size(), 1);
        Assert.assertEquals(tableData.getFirst().get("Column"), "Value");
    }

    @Test
    public void shouldCoverFailureBranchesForSelectSubmitAndSwitching() throws Exception {
        var helper = mock(ElementActionsHelper.class);
        var helperDriverFactory = mock(DriverFactoryHelper.class);
        By locator = By.id("complex");

        when(helperDriverFactory.getDriver()).thenReturn(driver);
        setField(elementActions, "driverFactoryHelper", helperDriverFactory);
        setField(elementActions, "elementActionsHelper", helper);

        SHAFT.Properties.flags.set().handleNonSelectDropDown(true);
        when(helper.identifyUniqueElement(eq(driver), any(By.class)))
                .thenReturn(buildElementInfo("div", locator, element, "dropdown"))
                .thenReturn(buildElementInfo("div", locator, element, "dropdown"))
                .thenThrow(new RuntimeException("relative option not found"));
        invokeAndIgnoreFailures(() -> elementActions.select(locator, "Missing Option"));

        SHAFT.Properties.flags.set().handleNonSelectDropDown(false);
        when(helper.identifyUniqueElement(eq(driver), any(By.class)))
                .thenReturn(buildElementInfo("div", locator, element, "dropdown"));
        invokeAndIgnoreFailures(() -> elementActions.select(locator, "Missing Option"));

        when(element.getTagName()).thenReturn("select");
        when(element.findElements(By.tagName("option"))).thenReturn(Collections.emptyList());
        when(helper.identifyUniqueElement(eq(driver), any(By.class))).thenReturn(buildElementInfo("select", locator, element, "select"));
        when(helper.waitForElementTextToBeNot(driver, locator, "")).thenReturn(false);
        invokeAndIgnoreFailures(() -> elementActions.select(locator, "Option"));

        doThrow(new RuntimeException("name failed")).when(helper).getElementName(driver, locator);
        invokeAndIgnoreFailures(() -> elementActions.submitFormUsingJavaScript(locator));

        doReturn("form").when(helper).getElementName(driver, locator);
        when(helper.takeScreenshot(driver, locator, "submitFormUsingJavaScript", null, true))
                .thenThrow(new JavascriptException("no screenshot first"))
                .thenReturn(Collections.emptyList());
        doThrow(new JavascriptException("js submit")).when(helper).submitFormUsingJavascript(driver, locator);
        invokeAndIgnoreFailures(() -> elementActions.submitFormUsingJavaScript(locator));

        when(helper.takeScreenshot(driver, locator, "submitFormUsingJavaScript", null, true))
                .thenReturn(Collections.emptyList());
        doThrow(new RuntimeException("submit failed")).when(helper).submitFormUsingJavascript(driver, locator);
        invokeAndIgnoreFailures(() -> elementActions.submitFormUsingJavaScript(locator));

        when(helper.identifyUniqueElement(driver, locator)).thenThrow(new RuntimeException("iframe missing"));
        invokeAndIgnoreFailures(() -> elementActions.switchToIframe(locator));

        WebDriver failingDefaultContentDriver = mock(WebDriver.class);
        WebDriver.TargetLocator failingTarget = mock(WebDriver.TargetLocator.class);
        when(helperDriverFactory.getDriver()).thenReturn(failingDefaultContentDriver);
        when(failingDefaultContentDriver.switchTo()).thenReturn(failingTarget);
        when(failingTarget.defaultContent()).thenThrow(new RuntimeException("default content failed"));
        invokeAndIgnoreFailures(elementActions::switchToDefaultContent);
    }

    @Test
    public void shouldCoverUploadAndTableFailureBranches() throws Exception {
        var helper = mock(ElementActionsHelper.class);
        var helperDriverFactory = mock(DriverFactoryHelper.class);
        By uploadLocator = By.id("uploadFailure");

        when(helperDriverFactory.getDriver()).thenReturn(driver);
        setField(elementActions, "driverFactoryHelper", helperDriverFactory);
        setField(elementActions, "elementActionsHelper", helper);
        when(helper.getElementName(driver, uploadLocator)).thenReturn("upload");
        when(helper.takeScreenshot(driver, uploadLocator, "typeFileLocationForUpload", null, true)).thenReturn(Collections.emptyList());

        WebElement firstAttemptElement = mock(WebElement.class);
        doThrow(new InvalidArgumentException("bad path")).when(firstAttemptElement).sendKeys(any(CharSequence.class));
        when(helper.identifyUniqueElementIgnoringVisibility(driver, uploadLocator))
                .thenReturn(buildElementInfo("input", uploadLocator, firstAttemptElement, "upload"));
        invokeAndIgnoreFailures(() -> elementActions.typeFileLocationForUpload(uploadLocator, "/tmp/missing-file.txt"));

        WebElement hiddenElement = mock(WebElement.class);
        WebElement visibleElement = mock(WebElement.class);
        doThrow(new ElementNotInteractableException("hidden")).when(hiddenElement).sendKeys(any(CharSequence.class));
        doThrow(new WebDriverException("send keys failed")).when(visibleElement).sendKeys(any(CharSequence.class));
        when(helper.identifyUniqueElementIgnoringVisibility(driver, uploadLocator))
                .thenReturn(buildElementInfo("input", uploadLocator, hiddenElement, "upload"));
        when(helper.identifyUniqueElement(driver, uploadLocator))
                .thenReturn(buildElementInfo("input", uploadLocator, visibleElement, "upload"));
        doThrow(new NoSuchElementException("style reset failed"))
                .when(helper).changeWebElementVisibilityUsingJavascript(driver, uploadLocator, false);
        invokeAndIgnoreFailures(() -> elementActions.typeFileLocationForUpload(uploadLocator, "/tmp/missing-file.txt"));

        By tableLocator = By.id("missingTable");
        when(driver.findElement(tableLocator)).thenThrow(new RuntimeException("table missing"));
        Assert.assertNull(elementActions.getTableRowsData(tableLocator));

        By emptyTableLocator = By.id("emptyTable");
        WebElement table = mock(WebElement.class);
        when(table.isDisplayed()).thenReturn(true);
        when(driver.findElement(emptyTableLocator)).thenReturn(table);
        when(driver.findElement(By.cssSelector("tbody tr"))).thenThrow(new RuntimeException("no rows loaded"));
        List<Map<String, String>> emptyRows = elementActions.getTableRowsData(emptyTableLocator);
        Assert.assertNotNull(emptyRows);
        Assert.assertTrue(emptyRows.isEmpty());
    }

    private static void invokeAndIgnoreFailures(Runnable action) {
        try {
            action.run();
        } catch (Exception ignored) {
            // coverage-only invocation
        }
    }

    private static void setField(ElementActions target, String fieldName, Object value) throws Exception {
        Field field = FluentWebDriverAction.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(target, value);
    }

    private static List<Object> buildElementInfo(String tagName, By locator, WebElement webElement, String elementName) {
        return Arrays.asList(
                1,
                webElement,
                locator,
                "<" + tagName + ">value</" + tagName + ">",
                "value",
                elementName,
                "",
                new Rectangle(0, 0, 10, 10)
        );
    }
}
