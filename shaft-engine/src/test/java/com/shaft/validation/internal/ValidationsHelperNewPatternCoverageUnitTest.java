package com.shaft.validation.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.internal.BrowserActionsHelper;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.ValidationEnums;
import org.mockito.ArgumentCaptor;
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

public class ValidationsHelperNewPatternCoverageUnitTest {

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
        Properties.clearForCurrentThread();
    }

    @Test(description = "Covers hard-assert equality and number validation pass paths")
    public void validateEqualsAndNumberShouldCoverPassPaths() {
        ValidationsHelper hardAssertHelper = new ValidationsHelper(ValidationEnums.ValidationCategory.HARD_ASSERT);

        hardAssertHelper.validateEquals("same", "same", ValidationEnums.ValidationComparisonType.EQUALS,
                ValidationEnums.ValidationType.POSITIVE);
        hardAssertHelper.validateNumber(10, 10, ValidationEnums.NumbersComparativeRelation.EQUALS,
                ValidationEnums.ValidationType.POSITIVE);

    }

    @Test(description = "Covers browser/element validation branches with mocked dependencies")
    public void validateBrowserAndElementMethodsShouldCoverBranchingPaths() {
        ValidationsHelper helper = new ValidationsHelper(ValidationEnums.ValidationCategory.HARD_ASSERT);
        By locator = By.id("sample");

        WebDriver driver = mock(WebDriver.class, Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        WebElement element = mock(WebElement.class);
        WebElement option = mock(WebElement.class);
        SHAFT.Properties.reporting.set().captureElementName(false);
        SHAFT.Properties.flags.set().forceCheckElementLocatorIsUnique(false);
        SHAFT.Properties.flags.set().scrollingMode("legacy");
        SHAFT.Properties.visuals.set().createAnimatedGif(false);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("ValidationPointsOnly");
        when(driver.findElement(any(By.class))).thenReturn(element);
        when(driver.findElements(any(By.class))).thenReturn(List.of(element));
        when(element.getScreenshotAs(any())).thenReturn(new byte[]{2});
        when(element.getText()).thenReturn("  element text  ");
        when(element.getAttribute("aria-label")).thenReturn("attrValue");
        when(element.getDomAttribute("data-id")).thenReturn("domAttrValue");
        when(element.getDomProperty("value")).thenReturn("domPropValue");
        when(element.getCssValue("color")).thenReturn("cssValue");
        when(element.getTagName()).thenReturn("select");
        when(element.findElements(any(By.class))).thenReturn(List.of(option));
        when(option.isSelected()).thenReturn(true);
        when(option.getText()).thenReturn("selected");
        when(((JavascriptExecutor) driver).executeScript(anyString(), any())).thenReturn("rtl");
        when(((JavascriptExecutor) driver).executeScript(anyString())).thenReturn("rtl");

        try (MockedStatic<JavaScriptWaitManager> javaScriptWaitManagerMocked = Mockito.mockStatic(JavaScriptWaitManager.class);
             MockedStatic<ImageProcessingActions> imageProcessingMocked = Mockito.mockStatic(ImageProcessingActions.class);
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
                     })) {
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

    @Test(description = "Generated element assertion messages should not repeat locator text")
    public void generatedElementAssertionMessageShouldLeaveLocatorForParametersOnly() {
        ValidationsBuilder builder = new ValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT);
        WebDriverElementValidationsBuilder elementBuilder = builder.element(mock(WebDriver.class), By.id("message"));

        Assert.assertEquals(elementBuilder.reportMessageBuilder.toString(), "the element ");
        Assert.assertEquals(elementBuilder.locator, By.id("message"));
        Assert.assertFalse(elementBuilder.reportMessageBuilder.toString().contains("By.id: message"));
    }

    @Test(description = "Visual validation screenshot bytes should be routed as screenshot attachments")
    public void visualValidationImageBytesShouldUseScreenshotAttachmentType() {
        ValidationsHelper helper = new ValidationsHelper(ValidationEnums.ValidationCategory.HARD_ASSERT);
        By locator = By.id("logo");
        WebDriver driver = mock(WebDriver.class);
        WebElement element = mock(WebElement.class);
        byte[] referenceScreenshot = new byte[]{(byte) 0x89, 0x50, 0x4E, 0x47};
        byte[] actualScreenshot = new byte[]{(byte) 0x89, 0x50, 0x4E, 0x47, 1};
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot("Never");
        when(driver.findElement(any(By.class))).thenReturn(element);
        when(element.getScreenshotAs(any())).thenReturn(actualScreenshot);

        try (MockedStatic<ImageProcessingActions> imageProcessingMocked = Mockito.mockStatic(ImageProcessingActions.class);
             MockedStatic<ReportManagerHelper> reportManagerHelperMocked = Mockito.mockStatic(ReportManagerHelper.class)) {
            imageProcessingMocked.when(() -> ImageProcessingActions.getReferenceImage(any(By.class)))
                    .thenReturn(referenceScreenshot);
            imageProcessingMocked.when(() -> ImageProcessingActions.compareAgainstBaseline(any(), any(By.class), any(byte[].class), any()))
                    .thenReturn(true);
            @SuppressWarnings({"rawtypes", "unchecked"})
            ArgumentCaptor<List<List<Object>>> attachmentsCaptor = (ArgumentCaptor) ArgumentCaptor.forClass(List.class);

            helper.validateElementMatches(driver, locator, ValidationEnums.VisualValidationEngine.EXACT_OPENCV,
                    ValidationEnums.ValidationType.POSITIVE);

            reportManagerHelperMocked.verify(() -> ReportManagerHelper.attach(attachmentsCaptor.capture()));
            List<List<Object>> attachments = attachmentsCaptor.getValue();
            Assert.assertTrue(hasAttachment(attachments, "Screenshot", "Reference Screenshot", referenceScreenshot));
            Assert.assertTrue(hasAttachment(attachments, "Screenshot", "Actual Screenshot", actualScreenshot));
            Assert.assertFalse(attachments.stream().anyMatch(attachment ->
                    "Validation Test Data".equals(attachment.get(0)) && attachment.get(1).toString().contains("Screenshot")));
        }
    }

    @Test(description = "Covers private utility methods used by validation reporting")
    public void privateUtilityMethodsShouldReturnExpectedValues() throws Exception {
        Method normalizeDirection = ValidationsHelper.class.getDeclaredMethod("normalizeDirection", String.class);
        normalizeDirection.setAccessible(true);

        Method setCommonParameters = ValidationsHelper.class.getDeclaredMethod("setCommonParameters", Object.class, Object.class, String.class);
        setCommonParameters.setAccessible(true);

        Method formatAssertionError = ValidationsHelper.class.getDeclaredMethod("formatAssertionErrorWithAutoDetectedPackage", AssertionError.class);
        formatAssertionError.setAccessible(true);
        Method performValidation = ValidationsHelper.class.getDeclaredMethod("performValidation", Object.class, Object.class, Object.class, ValidationEnums.ValidationType.class);
        performValidation.setAccessible(true);

        ValidationsHelper helper = new ValidationsHelper(ValidationEnums.ValidationCategory.HARD_ASSERT);

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

        Assert.assertEquals(performValidation.invoke(helper, "a", "b", ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE), false);
        Assert.assertEquals(performValidation.invoke(helper, 3, 3, ValidationEnums.NumbersComparativeRelation.EQUALS, ValidationEnums.ValidationType.POSITIVE), true);
    }

    private boolean hasAttachment(List<List<Object>> attachments, String type, String name, byte[] content) {
        return attachments.stream().anyMatch(attachment ->
                type.equals(attachment.get(0)) && name.equals(attachment.get(1)) && attachment.get(2) == content);
    }
}
