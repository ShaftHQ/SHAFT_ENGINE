package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.driver.DriverFactory;
import com.shaft.driver.internal.WizardHelpers;
import com.shaft.gui.driver.ShaftLocator;
import com.shaft.listeners.internal.RetryAnalyzer;
import com.shaft.gui.driver.Driver;
import com.shaft.gui.driver.DriverAssertions;
import com.shaft.gui.driver.DriverVerifications;
import com.shaft.validation.internal.WebDriverBrowserValidationsBuilder;
import com.shaft.validation.internal.WebDriverElementValidationsBuilder;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

@Test(singleThreaded = true)
public class GUIInterfaceCompatibilityCoverageUnitTest {
    @Test
    public void webDriverShouldImplementDriverInterface() throws NoSuchMethodException {
        Assert.assertTrue(Driver.class.isAssignableFrom(SHAFT.GUI.WebDriver.class));
        Assert.assertTrue(
                com.shaft.gui.driver.BrowserActionsContract.class.isAssignableFrom(shaftGuiBrowserActionsType()));
        Assert.assertTrue(
                com.shaft.gui.driver.ElementActionsContract.class.isAssignableFrom(shaftGuiElementActionsType()));
        Assert.assertTrue(
                com.shaft.gui.driver.AlertActionsContract.class.isAssignableFrom(shaftGuiAlertActionsType()));
        Assert.assertTrue(
                DriverAssertions.class.isAssignableFrom(WizardHelpers.WebDriverAssertions.class));
        Assert.assertTrue(
                DriverVerifications.class.isAssignableFrom(WizardHelpers.WebDriverVerifications.class));
        Assert.assertTrue(
                com.shaft.gui.driver.BrowserAssertions.class.isAssignableFrom(WebDriverBrowserValidationsBuilder.class));
        Assert.assertTrue(
                com.shaft.gui.driver.ElementAssertions.class.isAssignableFrom(WebDriverElementValidationsBuilder.class));

        Method browser = SHAFT.GUI.WebDriver.class.getMethod("browser");
        Method element = SHAFT.GUI.WebDriver.class.getMethod("element");
        Method alert = SHAFT.GUI.WebDriver.class.getMethod("alert");
        Method assertThat = SHAFT.GUI.WebDriver.class.getMethod("assertThat");
        Method verifyThat = SHAFT.GUI.WebDriver.class.getMethod("verifyThat");

        Assert.assertTrue(com.shaft.gui.driver.BrowserActionsContract.class.isAssignableFrom(browser.getReturnType()));
        Assert.assertTrue(com.shaft.gui.driver.ElementActionsContract.class.isAssignableFrom(element.getReturnType()));
        Assert.assertTrue(com.shaft.gui.driver.AlertActionsContract.class.isAssignableFrom(alert.getReturnType()));
        Assert.assertTrue(DriverAssertions.class.isAssignableFrom(assertThat.getReturnType()));
        Assert.assertTrue(DriverVerifications.class.isAssignableFrom(verifyThat.getReturnType()));

        Assert.assertNotNull(SHAFT.GUI.WebDriver.class.getMethod("quit"));
        Assert.assertNotNull(SHAFT.GUI.WebDriver.class.getMethod("act", String.class, Object[].class));
    }

    @Test
    public void playwrightShouldImplementDriverInterfaceWithoutInstantiation() throws NoSuchMethodException {
        Assert.assertTrue(Driver.class.isAssignableFrom(SHAFT.GUI.Playwright.class));

        Assert.assertTrue(com.shaft.gui.driver.BrowserActionsContract.class.isAssignableFrom(
                SHAFT.GUI.Playwright.class.getMethod("browser").getReturnType()));
        Assert.assertTrue(com.shaft.gui.driver.ElementActionsContract.class.isAssignableFrom(
                SHAFT.GUI.Playwright.class.getMethod("element").getReturnType()));
        Assert.assertTrue(com.shaft.gui.driver.AlertActionsContract.class.isAssignableFrom(
                SHAFT.GUI.Playwright.class.getMethod("alert").getReturnType()));
        Assert.assertTrue(DriverAssertions.class.isAssignableFrom(
                SHAFT.GUI.Playwright.class.getMethod("assertThat").getReturnType()));
        Assert.assertTrue(DriverVerifications.class.isAssignableFrom(
                SHAFT.GUI.Playwright.class.getMethod("verifyThat").getReturnType()));
        Assert.assertEquals(SHAFT.GUI.Playwright.class.getMethod("getDriver").getReturnType(),
                com.microsoft.playwright.Page.class);
        Assert.assertEquals(SHAFT.GUI.Playwright.class.getMethod("getNativeContext").getReturnType(),
                com.microsoft.playwright.BrowserContext.class);
    }

    @Test
    public void webDriverPublicConstructorsShouldRemainAvailableWithoutInstantiation() throws NoSuchMethodException {
        assertDeclaredConstructor(SHAFT.GUI.WebDriver.class, new Class<?>[]{});
        assertDeclaredConstructor(SHAFT.GUI.WebDriver.class, new Class<?>[]{DriverFactory.DriverType.class});
        assertDeclaredConstructor(SHAFT.GUI.WebDriver.class,
                new Class<?>[]{DriverFactory.DriverType.class, MutableCapabilities.class});
        assertDeclaredConstructor(SHAFT.GUI.WebDriver.class, new Class<?>[]{WebDriver.class});
    }

    @Test
    public void actionTypesShouldRemainAssignableToContracts() {
        Assert.assertTrue(com.shaft.gui.driver.BrowserActionsContract.class.isAssignableFrom(
                com.shaft.gui.browser.BrowserActions.class));
        Assert.assertTrue(com.shaft.gui.driver.ElementActionsContract.class.isAssignableFrom(
                com.shaft.gui.element.ElementActions.class));
        Assert.assertTrue(com.shaft.gui.driver.AlertActionsContract.class.isAssignableFrom(
                com.shaft.gui.element.AlertActions.class));
    }

    @Test
    public void locatorBuilderShouldExposePortableAndPlaywrightLocatorMethods() throws NoSuchMethodException {
        ShaftLocator locator = SHAFT.GUI.Locator.hasTagName("button").hasId("save").buildPortable();

        Assert.assertEquals(locator.toBy().toString(), "By.xpath: //button[@id=\"save\"]");
        Assert.assertEquals(locator.toPlaywrightSelector(), "xpath=//button[@id=\"save\"]");
        Assert.assertEquals(ShaftLocator.from(By.id("username")).toPlaywrightSelector(), "[id=\"username\"]");
        Assert.assertEquals(ShaftLocator.text("Save \"draft\"").toPlaywrightSelector(), "text=\"Save \\\"draft\\\"\"");
        Assert.assertNotNull(SHAFT.GUI.Locator.hasTagName("input").getClass().getMethod("buildForPlaywright",
                com.microsoft.playwright.Page.class));
    }

    @Test
    public void playwrightPropertiesShouldBeThreadLocalAndSupportRetryTraceToggle() {
        boolean originalForceRetryEvidence = SHAFT.Properties.flags.forceCaptureSupportingEvidenceOnRetry();
        try {
            SHAFT.Properties.clearForCurrentThread();
            Assert.assertFalse(SHAFT.Properties.playwright.tracingEnabled());

            SHAFT.Properties.playwright.set()
                    .connectionMode("connect")
                    .endpoint("ws://localhost:3000")
                    .tracingEnabled(true);
            Assert.assertEquals(SHAFT.Properties.playwright.connectionMode(), "connect");
            Assert.assertEquals(SHAFT.Properties.playwright.endpoint(), "ws://localhost:3000");
            Assert.assertTrue(SHAFT.Properties.playwright.tracingEnabled());

            SHAFT.Properties.clearForCurrentThread();
            SHAFT.Properties.flags.set().forceCaptureSupportingEvidenceOnRetry(true);
            RetryAnalyzer.enableSupportingEvidenceCaptureForRetryAttempt();
            RetryAnalyzer.activateSupportingEvidenceCaptureForRetryAttempt();
            Assert.assertTrue(SHAFT.Properties.playwright.tracingEnabled());
            RetryAnalyzer.restoreSupportingEvidenceCaptureForRetryAttempt();
            Assert.assertFalse(SHAFT.Properties.playwright.tracingEnabled());
        } finally {
            SHAFT.Properties.flags.set().forceCaptureSupportingEvidenceOnRetry(originalForceRetryEvidence);
            SHAFT.Properties.clearForCurrentThread();
        }
    }

    private static void assertDeclaredConstructor(Class<?> type, Class<?>[] parameterTypes) throws NoSuchMethodException {
        Constructor<?> constructor = type.getDeclaredConstructor(parameterTypes);
        Assert.assertNotNull(constructor);
        Assert.assertEquals(constructor.getDeclaringClass(), type);
    }

    private Class<?> shaftGuiBrowserActionsType() {
        return com.shaft.gui.browser.BrowserActions.class;
    }

    private Class<?> shaftGuiElementActionsType() {
        return com.shaft.gui.element.ElementActions.class;
    }

    private Class<?> shaftGuiAlertActionsType() {
        return com.shaft.gui.element.AlertActions.class;
    }
}
