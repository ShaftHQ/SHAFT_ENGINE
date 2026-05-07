package com.shaft.validation.internal;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.internal.BrowserActionsHelper;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.internal.Actions;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.validation.ValidationEnums;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ValidationsHelper2CoverageUnitTest {

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test(description = "Covers hard-assert equality and number validation pass paths")
    public void validateEqualsAndNumberShouldCoverPassAndFailBranches() {
        ValidationsHelper2 hardAssertHelper = new ValidationsHelper2(ValidationEnums.ValidationCategory.HARD_ASSERT);

        hardAssertHelper.validateEquals("same", "same", ValidationEnums.ValidationComparisonType.EQUALS,
                ValidationEnums.ValidationType.POSITIVE);
        hardAssertHelper.validateNumber(10, 10, ValidationEnums.NumbersComparativeRelation.EQUALS,
                ValidationEnums.ValidationType.POSITIVE);

    }

    @Test(description = "Covers browser/element validation branches with mocked dependencies")
    public void validateBrowserAndElementMethodsShouldCoverBranchingPaths() {
        ValidationsHelper2 helper = new ValidationsHelper2(ValidationEnums.ValidationCategory.HARD_ASSERT);
        By locator = By.id("sample");

        WebDriver driver = mock(WebDriver.class, Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        WebElement element = mock(WebElement.class);
        when(driver.findElement(any(By.class))).thenReturn(element);
        when(element.getScreenshotAs(any())).thenReturn(new byte[]{2});
        when(((JavascriptExecutor) driver).executeScript(anyString(), any())).thenReturn("rtl");
        when(((JavascriptExecutor) driver).executeScript(anyString())).thenReturn("rtl");

        try (MockedStatic<ImageProcessingActions> imageProcessingMocked = Mockito.mockStatic(ImageProcessingActions.class);
             MockedConstruction<ScreenshotManager> screenshotManagerMocked = Mockito.mockConstruction(ScreenshotManager.class,
                (mock, context) -> when(mock.takeScreenshot(any(), any(), anyString(), any(Boolean.class)))
                        .thenReturn(List.of("Validation", "Screenshot", "bytes")));
             MockedConstruction<BrowserActionsHelper> browserActionsHelperMocked = Mockito.mockConstruction(BrowserActionsHelper.class,
                     (mock, context) -> when(mock.capturePageSnapshot(any())).thenReturn("<html/>"));
             MockedConstruction<BrowserActions> browserActionsMocked = Mockito.mockConstruction(BrowserActions.class,
                     (mock, context) -> {
                         when(mock.getCurrentURL()).thenReturn("http://example.com");
                         when(mock.getPageSource()).thenReturn("<html/>");
                         when(mock.getCurrentWindowTitle()).thenReturn("Title");
                         when(mock.getWindowHandle()).thenReturn("WINDOW_HANDLE");
                         when(mock.getWindowPosition()).thenReturn("0,0");
                         when(mock.getWindowSize()).thenReturn("1200x800");
                     });
             MockedConstruction<Actions> actionsMocked = Mockito.mockConstruction(Actions.class,
                     (mock, context) -> {
                         Actions.GetElementInformation getInfo = mock(Actions.GetElementInformation.class);
                         when(getInfo.text(any(By.class))).thenReturn("  element text  ");
                         when(getInfo.selectedText(any(By.class))).thenReturn("selected");
                         when(getInfo.attribute(any(By.class), anyString())).thenReturn("attrValue");
                         when(getInfo.domAttribute(any(By.class), anyString())).thenReturn("domAttrValue");
                         when(getInfo.domProperty(any(By.class), anyString())).thenReturn("domPropValue");
                         when(getInfo.cssValue(any(By.class), anyString())).thenReturn("cssValue");
                         when(mock.get()).thenReturn(getInfo);
                     });
             MockedConstruction<ElementActions> elementActionsMocked = Mockito.mockConstruction(ElementActions.class,
                     (mock, context) -> when(mock.getElementsCount(any(By.class))).thenReturn(1))) {
            imageProcessingMocked.when(() -> ImageProcessingActions.getReferenceImage(any(By.class)))
                    .thenReturn(new byte[]{1});
            imageProcessingMocked.when(() -> ImageProcessingActions.compareAgainstBaseline(any(), any(By.class), any(byte[].class), any()))
                    .thenReturn(true);

            helper.validateBrowserAttribute(driver, "url", "http://example.com",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateBrowserAttribute(driver, "source", "<html/>",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateBrowserAttribute(driver, "title", "Title",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateBrowserAttribute(driver, "windowhandle", "WINDOW_HANDLE",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateBrowserAttribute(driver, "windowposition", "0,0",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateBrowserAttribute(driver, "windowsize", "1200x800",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateBrowserAttribute(driver, "textdirection", "rtl",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateBrowserAttribute(driver, "textalignment", "rtl",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateBrowserAttribute(driver, "textorientation", "rtl",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateBrowserAttribute(driver, "textdisplaystyle", "rtl",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);

            helper.validateElementAttribute(driver, locator, "text", "  element text  ",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateElementAttribute(driver, locator, "texttrimmed", "element text",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateElementAttribute(driver, locator, "selectedtext", "selected",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateElementAttribute(driver, locator, "aria-label", "attrValue",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);

            helper.validateElementDomAttribute(driver, locator, "text", "  element text  ",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateElementDomAttribute(driver, locator, "texttrimmed", "element text",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateElementDomAttribute(driver, locator, "selectedtext", "selected",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateElementDomAttribute(driver, locator, "textdirection", "rtl",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateElementDomAttribute(driver, locator, "textalignment", "rtl",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateElementDomAttribute(driver, locator, "textorientation", "rtl",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateElementDomAttribute(driver, locator, "textdisplaystyle", "rtl",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateElementDomAttribute(driver, locator, "data-id", "domAttrValue",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);

            helper.validateElementDomProperty(driver, locator, "value", "domPropValue",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateElementCSSProperty(driver, locator, "color", "cssValue",
                    ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE);
            helper.validateElementExists(driver, locator, ValidationEnums.ValidationType.POSITIVE);
            helper.validateElementMatches(driver, locator, ValidationEnums.VisualValidationEngine.EXACT_OPENCV,
                    ValidationEnums.ValidationType.POSITIVE);
        }
    }

    @Test(description = "Covers private utility methods used by validation reporting")
    public void privateUtilityMethodsShouldReturnExpectedValues() throws Exception {
        Method normalizeDirection = ValidationsHelper2.class.getDeclaredMethod("normalizeDirection", String.class);
        normalizeDirection.setAccessible(true);

        Method setCommonParameters = ValidationsHelper2.class.getDeclaredMethod("setCommonParameters", Object.class, Object.class, String.class);
        setCommonParameters.setAccessible(true);

        Method formatAssertionError = ValidationsHelper2.class.getDeclaredMethod("formatAssertionErrorWithAutoDetectedPackage", AssertionError.class);
        formatAssertionError.setAccessible(true);

        ValidationsHelper2 helper = new ValidationsHelper2(ValidationEnums.ValidationCategory.HARD_ASSERT);

        Assert.assertEquals(normalizeDirection.invoke(helper, "rtl"), "rtl");
        Assert.assertEquals(normalizeDirection.invoke(helper, "anythingElse"), "ltr");

        @SuppressWarnings("unchecked")
        java.util.LinkedHashMap<String, String> parameters =
                (java.util.LinkedHashMap<String, String>) setCommonParameters.invoke(helper, "expected", "actual", "contains");
        Assert.assertEquals(parameters.get("Expected value"), "expected");
        Assert.assertEquals(parameters.get("Actual value"), "actual");
        Assert.assertEquals(parameters.get("Comparison type"), "Contains");

        AssertionError error = new AssertionError("boom");
        error.setStackTrace(new StackTraceElement[]{
                new StackTraceElement("org.testng.SomeClass", "invoke", "SomeClass.java", 10),
                new StackTraceElement("testPackage.unitTests.CustomTest", "sample", "CustomTest.java", 42)
        });

        String formatted = (String) formatAssertionError.invoke(null, error);
        Assert.assertNotNull(formatted);
        Assert.assertTrue(formatted.contains("Assertion Failed"));
        Assert.assertNull(formatAssertionError.invoke(null, (Object) null));
    }
}
