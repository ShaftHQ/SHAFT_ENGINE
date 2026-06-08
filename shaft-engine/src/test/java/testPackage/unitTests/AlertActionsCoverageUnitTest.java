package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.properties.internal.Properties;
import org.mockito.Mockito;
import org.openqa.selenium.Alert;
import org.openqa.selenium.NoAlertPresentException;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Field;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@Test(singleThreaded = true)
public class AlertActionsCoverageUnitTest {
    @BeforeMethod(alwaysRun = true)
    public void setUp() {
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().screenshotParamsScreenshotType("VIEWPORT");
        SHAFT.Properties.visuals.set().screenshotParamsWatermark(false);
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void shouldCoverHappyPathAcrossConstructorsAndMethods() {
        Alert alert = mock(Alert.class);
        when(alert.getText()).thenReturn("expected alert text");

        AlertActions withWebDriver = createAlertActionsWithWebDriverConstructor(alert);
        Assert.assertTrue(withWebDriver.isAlertPresent());
        Assert.assertSame(withWebDriver.acceptAlert(), withWebDriver);
        Assert.assertSame(withWebDriver.dismissAlert(), withWebDriver);
        Assert.assertEquals(withWebDriver.getAlertText(), "expected alert text");
        Assert.assertSame(withWebDriver.typeIntoPromptAlert("prompt input"), withWebDriver);

        AlertActions withHelper = createAlertActionsWithHelperConstructor(alert);
        Assert.assertEquals(withHelper.getAlertText(), "expected alert text");
    }

    @Test
    public void shouldCoverFailureBranchesForCoreAlertMethods() {
        Alert failOnAccept = mock(Alert.class);
        doThrow(new RuntimeException("forced accept failure")).when(failOnAccept).accept();
        Assert.assertThrows(RuntimeException.class,
                () -> createAlertActionsWithHelperConstructor(failOnAccept).acceptAlert());

        Alert failOnDismiss = mock(Alert.class);
        doThrow(new RuntimeException("forced dismiss failure")).when(failOnDismiss).dismiss();
        Assert.assertThrows(RuntimeException.class,
                () -> createAlertActionsWithHelperConstructor(failOnDismiss).dismissAlert());

        Alert failOnGetText = mock(Alert.class);
        when(failOnGetText.getText()).thenThrow(new RuntimeException("forced getText failure"));
        Assert.assertThrows(RuntimeException.class,
                () -> createAlertActionsWithHelperConstructor(failOnGetText).getAlertText());

        Alert failOnSendKeys = mock(Alert.class);
        doThrow(new RuntimeException("forced sendKeys failure")).when(failOnSendKeys).sendKeys(anyString());
        Assert.assertThrows(RuntimeException.class,
                () -> createAlertActionsWithHelperConstructor(failOnSendKeys).typeIntoPromptAlert("prompt"));
    }

    @Test
    public void shouldReturnFalseWhenNoAlertPresentExceptionOccursInIsAlertPresent() throws Exception {
        AlertActionsContext context = createAlertActionsContextWithHelperConstructor(mock(Alert.class));

        ElementActionsHelper mockedHelper = mock(ElementActionsHelper.class);
        doThrow(new NoAlertPresentException("forced no alert")).doNothing()
                .when(mockedHelper).failAction(any(WebDriver.class), isNull(), any(Throwable.class));
        setField(context.alertActions, "elementActionsHelper", mockedHelper);
        when(context.targetLocator.alert()).thenThrow(new RuntimeException("forced wait failure"));

        Assert.assertFalse(context.alertActions.isAlertPresent());
    }

    @Test
    public void shouldReturnFalseWhenGenericExceptionOccursInIsAlertPresent() throws Exception {
        AlertActionsContext context = createAlertActionsContextWithHelperConstructor(mock(Alert.class));

        ElementActionsHelper mockedHelper = mock(ElementActionsHelper.class);
        doThrow(new RuntimeException("forced generic failure")).doNothing()
                .when(mockedHelper).failAction(any(WebDriver.class), isNull(), any(Throwable.class));
        setField(context.alertActions, "elementActionsHelper", mockedHelper);
        when(context.targetLocator.alert()).thenThrow(new RuntimeException("forced wait failure"));

        Assert.assertFalse(context.alertActions.isAlertPresent());
    }

    private AlertActions createAlertActionsWithWebDriverConstructor(Alert alert) {
        return new AlertActions(createDriver(alert));
    }

    private AlertActions createAlertActionsWithHelperConstructor(Alert alert) {
        return new AlertActions(new DriverFactoryHelper(createDriver(alert)));
    }

    private AlertActionsContext createAlertActionsContextWithHelperConstructor(Alert alert) {
        AlertActionsContext context = new AlertActionsContext();
        context.driver = mock(WebDriver.class, Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        context.targetLocator = mock(WebDriver.TargetLocator.class);

        when(((TakesScreenshot) context.driver).getScreenshotAs(OutputType.BYTES)).thenReturn("img".getBytes());
        when(context.driver.switchTo()).thenReturn(context.targetLocator);
        when(context.targetLocator.alert()).thenReturn(alert);

        context.alertActions = new AlertActions(new DriverFactoryHelper(context.driver));
        return context;
    }

    private WebDriver createDriver(Alert alert) {
        WebDriver driver = mock(WebDriver.class, Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        WebDriver.TargetLocator targetLocator = mock(WebDriver.TargetLocator.class);

        when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn("img".getBytes());
        when(driver.switchTo()).thenReturn(targetLocator);
        when(targetLocator.alert()).thenReturn(alert);

        return driver;
    }

    private void setField(Object target, String fieldName, Object value) throws Exception {
        Field field = target.getClass().getSuperclass().getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(target, value);
    }

    private static class AlertActionsContext {
        private AlertActions alertActions;
        private WebDriver driver;
        private WebDriver.TargetLocator targetLocator;
    }
}
