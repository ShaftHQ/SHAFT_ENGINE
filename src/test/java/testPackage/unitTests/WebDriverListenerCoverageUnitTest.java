package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.WebDriverListener;
import org.openqa.selenium.Alert;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@Test(singleThreaded = true)
public class WebDriverListenerCoverageUnitTest {
    private String savedTargetPlatform;
    private String savedMobileBrowserName;
    private boolean savedRespectBuiltInWaitsInNativeMode;

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        if (savedTargetPlatform != null) {
            SHAFT.Properties.platform.set().targetPlatform(savedTargetPlatform);
        }
        if (savedMobileBrowserName != null) {
            SHAFT.Properties.mobile.set().browserName(savedMobileBrowserName);
        }
        SHAFT.Properties.flags.set().respectBuiltInWaitsInNativeMode(savedRespectBuiltInWaitsInNativeMode);
    }

    @Test
    public void listenerShouldCoverDriverElementNavigationAndAlertHooks() throws Exception {
        saveCurrentProperties();

        WebDriverListener listener = new WebDriverListener();
        WebDriver driver = mock(WebDriver.class);
        WebDriver.Navigation navigation = mock(WebDriver.Navigation.class);
        WebDriver.Window window = mock(WebDriver.Window.class);
        WebElement element = mock(WebElement.class);
        Alert alert = mock(Alert.class);

        when(driver.findElement(any(By.class))).thenReturn(element);
        when(element.getAccessibleName()).thenReturn("searchBox");

        Method method = Object.class.getDeclaredMethod("toString");
        listener.afterAnyCall(this, method, new Object[]{}, null);
        listener.onError(this, method, new Object[]{}, new InvocationTargetException(new RuntimeException("forced")));

        listener.afterGet(driver, "https://example.com");
        listener.afterGetCurrentUrl("https://example.com", driver);
        listener.afterGetTitle(driver, "Example");

        SHAFT.Properties.flags.set().respectBuiltInWaitsInNativeMode(true);
        listener.beforeFindElement(driver, By.id("sample"));
        listener.beforeClick(element);

        listener.beforeSubmit(element);
        listener.beforeSendKeys(element, "abc", "123");
        listener.beforeClear(element);
        listener.afterGetAttribute(element, "value", "abc123");
        listener.afterGetText(element, "sample text");

        listener.afterTo(navigation, "https://example.com");
        listener.afterTo(navigation, new URL("https://example.com"));
        listener.afterBack(navigation);
        listener.afterForward(navigation);
        listener.afterRefresh(navigation);

        listener.beforeSendKeys(alert, "yes");
        listener.afterMaximize(window);
        listener.afterClose(driver);
        listener.afterQuit(driver);

        verify(driver, atLeastOnce()).findElement(any(By.class));
    }

    @Test
    public void listenerShouldCoverElementNameFallbackAndMobileNativeBranches() throws Exception {
        saveCurrentProperties();

        WebDriverListener listener = new WebDriverListener();
        WebDriver driver = mock(WebDriver.class);
        WebElement accessibleElement = mock(WebElement.class);
        WebElement blankNameElement = mock(WebElement.class);
        WebElement failingElement = mock(WebElement.class);

        Method getElementName = WebDriverListener.class.getDeclaredMethod("getElementName", WebElement.class);
        getElementName.setAccessible(true);

        SHAFT.Properties.platform.set().targetPlatform(Platform.WINDOWS.name());
        SHAFT.Properties.mobile.set().browserName("chrome");
        when(accessibleElement.getAccessibleName()).thenReturn("CTA");
        when(blankNameElement.getAccessibleName()).thenReturn("");
        when(failingElement.getAccessibleName()).thenThrow(new RuntimeException("forced"));

        SHAFT.Validations.assertThat().object(getElementName.invoke(listener, accessibleElement)).isEqualTo("\"CTA\"").perform();
        SHAFT.Validations.assertThat().object(getElementName.invoke(listener, blankNameElement)).isEqualTo("element").perform();

        listener.afterGet(driver, "https://example.com");
        listener.beforeClick(failingElement);
        listener.beforeSubmit(failingElement);
        listener.beforeSendKeys(failingElement, "fallback");
        listener.beforeClear(failingElement);
        listener.afterGetAttribute(failingElement, "name", "value");
        listener.afterGetText(failingElement, "text");

        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().browserName("");
        SHAFT.Validations.assertThat().object(getElementName.invoke(listener, accessibleElement)).isEqualTo("element").perform();
    }

    private void saveCurrentProperties() {
        savedTargetPlatform = SHAFT.Properties.platform.targetPlatform();
        savedMobileBrowserName = SHAFT.Properties.mobile.browserName();
        savedRespectBuiltInWaitsInNativeMode = SHAFT.Properties.flags.respectBuiltInWaitsInNativeMode();
        SHAFT.Properties.flags.set().respectBuiltInWaitsInNativeMode(false);
    }
}
