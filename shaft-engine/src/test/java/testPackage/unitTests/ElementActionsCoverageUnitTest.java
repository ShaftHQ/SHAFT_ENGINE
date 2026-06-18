package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.internal.Actions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.properties.internal.Properties;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@Test(singleThreaded = true)
public class ElementActionsCoverageUnitTest {
    private static final By LOCATOR = By.id("sample");
    private static final byte[] PNG = new byte[]{(byte) 0x89, 'P', 'N', 'G'};

    private WebDriver driver;
    private ElementActions elementActions;

    @BeforeMethod(alwaysRun = true)
    public void setUp() {
        SHAFT.Properties.timeouts.set().waitForLazyLoading(false);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().screenshotParamsWatermark(false);

        driver = mock(WebDriver.class, Mockito.withSettings().extraInterfaces(JavascriptExecutor.class, TakesScreenshot.class));
        when(((JavascriptExecutor) driver).executeScript(anyString())).thenReturn("complete");
        when(((JavascriptExecutor) driver).executeScript("return self.name")).thenReturn("mainFrame");
        when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        elementActions = new ElementActions(driver, true);
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void actionMethodsShouldDelegateToInternalActionsStack() {
        try (MockedConstruction<Actions> constructedActions = Mockito.mockConstruction(Actions.class,
                (mock, context) -> {
                    when(mock.executeNativeMobileCommand(anyString(), anyMap())).thenReturn(mock);
                    when(mock.click(any(By.class))).thenReturn(mock);
                    when(mock.clickUsingJavascript(any(By.class))).thenReturn(mock);
                    when(mock.scrollToElement(any(By.class))).thenReturn(mock);
                    when(mock.clickAndHold(any(By.class))).thenReturn(mock);
                    when(mock.doubleClick(any(By.class))).thenReturn(mock);
                    when(mock.dragAndDrop(any(By.class), any(By.class))).thenReturn(mock);
                    when(mock.dragAndDropByOffset(any(By.class), any(Integer.class), any(Integer.class))).thenReturn(mock);
                    when(mock.hover(any(By.class))).thenReturn(mock);
                    when(mock.select(any(By.class), anyString())).thenReturn(mock);
                    when(mock.setValueUsingJavaScript(any(By.class), anyString())).thenReturn(mock);
                    when(mock.submitFormUsingJavaScript(any(By.class))).thenReturn(mock);
                    when(mock.switchToIframe(any(By.class))).thenReturn(mock);
                    when(mock.switchToDefaultContent()).thenReturn(mock);
                    when(mock.type(any(By.class), any(CharSequence[].class))).thenReturn(mock);
                    when(mock.clear(any(By.class))).thenReturn(mock);
                    when(mock.typeAppend(any(By.class), any(CharSequence[].class))).thenReturn(mock);
                    when(mock.typeFileLocationForUpload(any(By.class), anyString())).thenReturn(mock);
                    when(mock.typeSecure(any(By.class), any(CharSequence[].class))).thenReturn(mock);
                    when(mock.captureScreenshot(any(By.class))).thenReturn(mock);
                })) {
            Assert.assertNotNull(elementActions.executeNativeMobileCommand("mobile: scroll", Map.of("direction", "down")));
            Mockito.verify(last(constructedActions)).executeNativeMobileCommand("mobile: scroll", Map.of("direction", "down"));

            Assert.assertNotNull(elementActions.click(LOCATOR));
            Mockito.verify(last(constructedActions)).click(LOCATOR);

            Assert.assertNotNull(elementActions.clickUsingJavascript(LOCATOR));
            Mockito.verify(last(constructedActions)).clickUsingJavascript(LOCATOR);

            Assert.assertNotNull(elementActions.scrollToElement(LOCATOR));
            Mockito.verify(last(constructedActions)).scrollToElement(LOCATOR);

            Assert.assertNotNull(elementActions.clickAndHold(LOCATOR));
            Mockito.verify(last(constructedActions)).clickAndHold(LOCATOR);

            Assert.assertNotNull(elementActions.doubleClick(LOCATOR));
            Mockito.verify(last(constructedActions)).doubleClick(LOCATOR);

            Assert.assertNotNull(elementActions.dragAndDrop(LOCATOR, By.id("destination")));
            Mockito.verify(last(constructedActions)).dragAndDrop(LOCATOR, By.id("destination"));

            Assert.assertNotNull(elementActions.dragAndDropByOffset(LOCATOR, 5, 10));
            Mockito.verify(last(constructedActions)).dragAndDropByOffset(LOCATOR, 5, 10);

            Assert.assertNotNull(elementActions.hover(LOCATOR));
            Mockito.verify(last(constructedActions)).hover(LOCATOR);

            Assert.assertNotNull(elementActions.select(LOCATOR, "Option"));
            Mockito.verify(last(constructedActions)).select(LOCATOR, "Option");

            Assert.assertNotNull(elementActions.setValueUsingJavaScript(LOCATOR, "value"));
            Mockito.verify(last(constructedActions)).setValueUsingJavaScript(LOCATOR, "value");

            Assert.assertNotNull(elementActions.submitFormUsingJavaScript(LOCATOR));
            Mockito.verify(last(constructedActions)).submitFormUsingJavaScript(LOCATOR);

            Assert.assertNotNull(elementActions.switchToIframe(LOCATOR));
            Mockito.verify(last(constructedActions)).switchToIframe(LOCATOR);

            Assert.assertNotNull(elementActions.switchToDefaultContent());
            Mockito.verify(last(constructedActions)).switchToDefaultContent();

            Assert.assertNotNull(elementActions.type(LOCATOR, "text"));
            Mockito.verify(last(constructedActions)).type(LOCATOR, "text");

            Assert.assertNotNull(elementActions.clear(LOCATOR));
            Mockito.verify(last(constructedActions)).clear(LOCATOR);

            Assert.assertNotNull(elementActions.typeAppend(LOCATOR, "append"));
            Mockito.verify(last(constructedActions)).typeAppend(LOCATOR, "append");

            Assert.assertNotNull(elementActions.typeFileLocationForUpload(LOCATOR, "/tmp/file.txt"));
            Mockito.verify(last(constructedActions)).typeFileLocationForUpload(LOCATOR, "/tmp/file.txt");

            Assert.assertNotNull(elementActions.typeSecure(LOCATOR, "secret"));
            Mockito.verify(last(constructedActions)).typeSecure(LOCATOR, "secret");

            Assert.assertNotNull(elementActions.captureScreenshot(LOCATOR));
            Mockito.verify(last(constructedActions)).captureScreenshot(LOCATOR);
        }
    }

    @Test
    public void nonActionHelpersShouldRemainLocal() throws Exception {
        ElementActionsHelper helper = mock(ElementActionsHelper.class);
        setField(elementActions, "elementActionsHelper", helper);
        when(helper.getElementsCount(driver, LOCATOR)).thenReturn(3);

        Assert.assertEquals(elementActions.getElementsCount(LOCATOR), 3);
        Assert.assertEquals(elementActions.getCurrentFrame(), "mainFrame");
    }

    @Test
    public void getTableRowsDataShouldExtractSimpleTableRows() {
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

        Assert.assertEquals(tableData.size(), 1);
        Assert.assertEquals(tableData.getFirst().get("Column"), "Value");
    }

    @Test
    public void getTableRowsDataShouldReturnEmptyRowsWhenTableHasNoLoadedRows() {
        WebElement table = mock(WebElement.class);
        when(table.isDisplayed()).thenReturn(true);
        when(driver.findElement(By.id("emptyTable"))).thenReturn(table);
        when(driver.findElement(By.cssSelector("tbody tr"))).thenThrow(new org.openqa.selenium.NoSuchElementException("empty"));

        Assert.assertTrue(elementActions.getTableRowsData(By.id("emptyTable")).isEmpty());
    }

    private static Actions last(MockedConstruction<Actions> constructedActions) {
        List<Actions> actions = constructedActions.constructed();
        return actions.get(actions.size() - 1);
    }

    private static void setField(ElementActions target, String fieldName, Object value) throws Exception {
        Field field = FluentWebDriverAction.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(target, value);
    }
}
