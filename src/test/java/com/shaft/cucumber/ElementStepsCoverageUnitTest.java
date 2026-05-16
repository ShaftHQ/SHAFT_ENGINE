package com.shaft.cucumber;

import com.shaft.driver.SHAFT;
import com.shaft.enums.internal.ClipboardAction;
import com.shaft.gui.element.internal.Actions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@Test(singleThreaded = true)
public class ElementStepsCoverageUnitTest {
    private static final String LOCATOR_VALUE = "target-value";

    private ThreadLocal<SHAFT.GUI.WebDriver> driverHolder;
    private SHAFT.GUI.WebDriver shaftDriver;
    private Actions elementActions;
    private ElementSteps elementSteps;

    @BeforeMethod
    public void setUp() {
        driverHolder = new ThreadLocal<>();
        shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        elementActions = mock(Actions.class);
        when(shaftDriver.element()).thenReturn(elementActions);
        driverHolder.set(shaftDriver);
        elementSteps = new ElementSteps(driverHolder);
    }

    @DataProvider
    public Object[][] locatorAliases() {
        return new Object[][]{
                {"id", By.id(LOCATOR_VALUE)},
                {"tagname", By.tagName(LOCATOR_VALUE)},
                {"tag_name", By.tagName(LOCATOR_VALUE)},
                {"tag name", By.tagName(LOCATOR_VALUE)},
                {"classname", By.className(LOCATOR_VALUE)},
                {"class_name", By.className(LOCATOR_VALUE)},
                {"class name", By.className(LOCATOR_VALUE)},
                {"name", By.name(LOCATOR_VALUE)},
                {"linktext", By.linkText(LOCATOR_VALUE)},
                {"link_text", By.linkText(LOCATOR_VALUE)},
                {"link text", By.linkText(LOCATOR_VALUE)},
                {"partiallinktext", By.partialLinkText(LOCATOR_VALUE)},
                {"partial_link_text", By.partialLinkText(LOCATOR_VALUE)},
                {"partial link text", By.partialLinkText(LOCATOR_VALUE)},
                {"cssselector", By.cssSelector(LOCATOR_VALUE)},
                {"css", By.cssSelector(LOCATOR_VALUE)},
                {"selector", By.cssSelector(LOCATOR_VALUE)},
                {"css_selector", By.cssSelector(LOCATOR_VALUE)},
                {"css selector", By.cssSelector(LOCATOR_VALUE)},
                {"xpath", By.xpath(LOCATOR_VALUE)},
                {"unsupported", By.xpath(LOCATOR_VALUE)}
        };
    }

    @Test(dataProvider = "locatorAliases")
    public void getLocatorFromTypeAndValueShouldResolveAllAliasesAndDefaultToXPath(String locatorType, By expectedLocator) {
        By actualLocator = ElementSteps.getLocatorFromTypeAndValue(locatorType, LOCATOR_VALUE);

        Assert.assertEquals(actualLocator.toString(), expectedLocator.toString());
    }

    @Test
    public void stepMethodsShouldDelegateToElementActionsWithResolvedLocators() {
        By field = By.id("field");
        By source = By.cssSelector("#source");
        By destination = By.xpath("//div[@id='destination']");

        elementSteps.type("typed text", "id", "field");
        elementSteps.keyPress("enter", "id", "field");
        elementSteps.typeSecure("secret", "id", "field");
        elementSteps.typeAppend(" appended", "id", "field");
        elementSteps.typeFileLocationForUpload("/tmp/upload.txt", "id", "field");
        elementSteps.click("id", "field");
        elementSteps.clickAndHold("id", "field");
        elementSteps.doubleClick("id", "field");
        elementSteps.dragAndDrop("css", "#source", "xpath", "//div[@id='destination']");
        elementSteps.dragAndDropByOffset("id", "field", 11, 13);
        elementSteps.hover("id", "field");
        elementSteps.select("Option 1", "id", "field");
        elementSteps.setValueUsingJavaScript("scripted value", "id", "field");
        elementSteps.submitFormUsingJavaScript("id", "field");

        verify(elementActions).type(field, "typed text");
        verify(elementActions).type(field, Keys.ENTER);
        verify(elementActions).typeSecure(field, "secret");
        verify(elementActions).typeAppend(field, " appended");
        verify(elementActions).typeFileLocationForUpload(field, "/tmp/upload.txt");
        verify(elementActions).click(field);
        verify(elementActions).clickAndHold(field);
        verify(elementActions).doubleClick(field);
        verify(elementActions).dragAndDrop(source, destination);
        verify(elementActions).dragAndDropByOffset(field, 11, 13);
        verify(elementActions).hover(field);
        verify(elementActions).select(field, "Option 1");
        verify(elementActions).setValueUsingJavaScript(field, "scripted value");
        verify(elementActions).submitFormUsingJavaScript(field);
    }

    @Test
    public void clipboardActionsShouldConstructHelperAndDelegateWithRequestedAction() {
        WebDriver seleniumDriver = mock(WebDriver.class);
        when(shaftDriver.getDriver()).thenReturn(seleniumDriver);

        try (MockedConstruction<ElementActionsHelper> helperConstruction = Mockito.mockConstruction(
                ElementActionsHelper.class,
                (helper, context) -> Assert.assertEquals(context.arguments(), java.util.List.of(false)))) {
            elementSteps.clipboardActions("COPY", "id", "field");

            Assert.assertEquals(helperConstruction.constructed().size(), 1);
            ElementActionsHelper helper = helperConstruction.constructed().get(0);
            verify(helper).performClipboardActions(seleniumDriver, ClipboardAction.COPY);
        }
    }

    @Test
    public void constructorShouldUseEmptyThreadLocalWhenDriverHolderIsNull() {
        ElementSteps stepsWithFallbackThreadLocal = new ElementSteps(null);

        Assert.assertThrows(NullPointerException.class, () -> stepsWithFallbackThreadLocal.click("id", "field"));
    }
}
