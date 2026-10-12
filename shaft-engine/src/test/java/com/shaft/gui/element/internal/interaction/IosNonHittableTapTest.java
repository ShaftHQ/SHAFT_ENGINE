package com.shaft.gui.element.internal.interaction;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.TouchActions;
import io.appium.java_client.ios.IOSDriver;
import org.mockito.ArgumentCaptor;
import org.openqa.selenium.By;
import org.openqa.selenium.interactions.Sequence;
import org.openqa.selenium.remote.RemoteWebElement;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.fail;

/**
 * Drives the shipped iOS element click and {@code touch().tap(locator)} from their public entry points.
 * A non-hittable element whose page source does not change must not be reported as a successful tap.
 */
public class IosNonHittableTapTest {
    private static final By LOGIN = By.xpath("//*[@name='login_iam_loginButton']");
    private static final String UNCHANGED_SOURCE = "<XCUIElementTypeApplication><XCUIElementTypeButton name=\"login_iam_loginButton\" hittable=\"false\"/></XCUIElementTypeApplication>";

    private String platform;
    private String browserName;
    private double identificationTimeout;
    private IOSDriver driver;
    private RemoteWebElement element;
    private DriverFactoryHelper helper;

    @BeforeMethod
    public void rememberProperties() {
        platform = SHAFT.Properties.platform.targetPlatform();
        browserName = SHAFT.Properties.mobile.browserName();
        identificationTimeout = SHAFT.Properties.timeouts.defaultElementIdentificationTimeout();
        SHAFT.Properties.platform.set().targetPlatform("iOS");
        SHAFT.Properties.mobile.set().browserName("");
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(2);
    }

    @AfterMethod(alwaysRun = true)
    public void restoreProperties() {
        if (helper != null) {
            try {
                helper.closeDriver();
            } catch (RuntimeException ignored) {
                // The mock session has nothing to quit; thread-local state still needs clearing.
            }
            helper = null;
        }
        SHAFT.Properties.platform.set().targetPlatform(platform);
        SHAFT.Properties.mobile.set().browserName(browserName);
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(identificationTimeout);
    }

    @Test
    public void elementClickAndTouchTapDoNotSucceedWhenIosElementIsNotHittable() {
        driver = mock(IOSDriver.class);
        element = mock(RemoteWebElement.class);
        when(element.getId()).thenReturn("login_iam_loginButton");
        when(element.getTagName()).thenReturn("XCUIElementTypeButton");
        when(element.isDisplayed()).thenReturn(true);
        when(element.isEnabled()).thenReturn(true);
        when(element.getAttribute(anyString())).thenAnswer(invocation -> attribute(invocation.getArgument(0)));
        when(element.getDomAttribute(anyString())).thenAnswer(invocation -> attribute(invocation.getArgument(0)));
        when(driver.findElements(any())).thenReturn(List.of(element));
        when(driver.findElement(any())).thenReturn(element);
        when(driver.getPageSource()).thenReturn(UNCHANGED_SOURCE);
        doAnswer(invocation -> null).when(element).click();
        doAnswer(invocation -> null).when(driver).executeScript(anyString(), any());

        helper = new DriverFactoryHelper(driver);

        Throwable clickFailure = failureFrom(() -> new ElementActions(helper).click(LOGIN));
        Throwable tapFailure = failureFrom(() -> new TouchActions(helper).tap(LOGIN));

        assertNoEffect(clickFailure);
        assertNoEffect(tapFailure);
        assertAnchoredTapWithoutCoordinates();
    }

    private static Throwable failureFrom(Runnable action) {
        try {
            action.run();
        } catch (RuntimeException failure) {
            return failure;
        }
        fail("action returned success");
        return null;
    }

    private static String attribute(String name) {
        if (name == null) {
            return null;
        }
        return switch (name.toLowerCase()) {
            case "hittable" -> "false";
            case "visible" -> "false";
            case "enabled" -> "true";
            case "name" -> "login_iam_loginButton";
            default -> null;
        };
    }

    private void assertAnchoredTapWithoutCoordinates() {
        @SuppressWarnings("unchecked")
        ArgumentCaptor<Map<String, Object>> params = ArgumentCaptor.forClass(Map.class);
        verify(driver, org.mockito.Mockito.atLeast(2)).executeScript(eq("mobile: tap"), params.capture());
        assertFalse(params.getAllValues().isEmpty());
        for (Map<String, Object> tap : params.getAllValues()) {
            assertEquals(tap.get("elementId"), "login_iam_loginButton");
            assertFalse(tap.containsKey("x"), "coordinate tap key x was sent: " + tap);
            assertFalse(tap.containsKey("y"), "coordinate tap key y was sent: " + tap);
        }
        verify(driver, never()).perform(anyCollection());
    }

    private static Collection<Sequence> anyCollection() {
        return org.mockito.ArgumentMatchers.anyCollection();
    }

    private static void assertNoEffect(Throwable thrown) {
        StringBuilder text = new StringBuilder();
        Throwable current = thrown;
        int depth = 0;
        while (current != null && depth < 8) {
            if (current.getMessage() != null) {
                text.append(current.getMessage()).append('\n');
            }
            current = current.getCause();
            depth++;
        }
        String flat = text.toString().toLowerCase();
        assertTrue(flat.contains("not hittable"), text.toString());
        assertTrue(flat.contains("no effect"), text.toString());
    }
}
