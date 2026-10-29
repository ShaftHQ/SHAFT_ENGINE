package com.shaft.gui.element.internal.interaction;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.element.internal.interaction.IosNonHittableTap.IosTapHadNoEffectException;
import io.appium.java_client.ios.IOSDriver;
import org.mockito.ArgumentCaptor;
import org.openqa.selenium.By;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
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
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.fail;

/**
 * Drives the shipped iOS element click and {@code touch().tap(locator)} from their public entry points.
 * A non-hittable element whose page source does not change must not be reported as a successful tap
 * unless a W3C centre-tap recovery changes the page (#7531 / #6908). Unreadable page source after
 * {@code mobile: tap} fails closed with no further tap (#6913 contract).
 */
// Instance fields (driver, helper, saved properties) are per-test state: methods must not interleave
// under the nightly's -DsetParallel=METHODS (#6931).
@Test(singleThreaded = true)
public class IosNonHittableTapTest {
    private static final By LOGIN = By.xpath("//*[@name='login_iam_loginButton']");
    private static final String UNCHANGED_SOURCE = "<XCUIElementTypeApplication><XCUIElementTypeButton name=\"login_iam_loginButton\" hittable=\"false\"/></XCUIElementTypeApplication>";
    private static final String AFTER_MOBILE_SOURCE = "<XCUIElementTypeApplication><XCUIElementTypeOther name=\"next_screen_entry\"/></XCUIElementTypeApplication>";
    private static final String AFTER_W3C_SOURCE = "<XCUIElementTypeApplication><XCUIElementTypeOther name=\"next_screen_entry\"/></XCUIElementTypeApplication>";
    /** Selenium Rectangle ctor is (x, y, height, width). Button 329×53 at (32,645) → centre (196,671). */
    private static final Rectangle FIXTURE_RECT = new Rectangle(32, 645, 53, 329);

    private String platform;
    private String browserName;
    private double identificationTimeout;
    private IOSDriver driver;
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
        RemoteWebElement element = mock(RemoteWebElement.class);
        when(element.getId()).thenReturn("login_iam_loginButton");
        when(element.getSize()).thenReturn(new Dimension(120, 44));
        when(element.getRect()).thenReturn(FIXTURE_RECT);
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
        doAnswer(invocation -> null).when(driver).perform(any());

        helper = new DriverFactoryHelper(driver);

        Throwable clickFailure = failureFrom(() -> new ElementActions(helper).click(LOGIN));
        Throwable tapFailure = failureFrom(() -> new TouchActions(helper).tap(LOGIN));

        assertNoEffect(clickFailure);
        assertNoEffect(tapFailure);
        assertAnchoredTapAtElementCentre();
        verify(driver, atLeast(2)).perform(anyCollection());
    }

    @Test
    public void w3cCenterTapRecoverySucceedsWhenMobileTapLeavesPageUnchanged() {
        IOSDriver localDriver = mock(IOSDriver.class);
        RemoteWebElement localElement = mock(RemoteWebElement.class);
        when(localElement.getId()).thenReturn("login_iam_loginButton");
        when(localElement.getSize()).thenReturn(new Dimension(329, 53));
        when(localElement.getRect()).thenReturn(FIXTURE_RECT);
        when(localElement.getDomAttribute(anyString())).thenReturn("false");
        when(localElement.getAttribute(anyString())).thenReturn("false");
        // before → after mobile (unchanged) → after W3C (changed)
        when(localDriver.getPageSource()).thenReturn(UNCHANGED_SOURCE, UNCHANGED_SOURCE, AFTER_W3C_SOURCE);
        doAnswer(invocation -> null).when(localDriver).executeScript(anyString(), any());
        doAnswer(invocation -> null).when(localDriver).perform(any());

        IosNonHittableTap.afterReportedClick(localDriver, localElement);

        @SuppressWarnings("unchecked")
        ArgumentCaptor<Map<String, Object>> params = ArgumentCaptor.forClass(Map.class);
        verify(localDriver, times(1)).executeScript(eq("mobile: tap"), params.capture());
        assertEquals(params.getValue().get("elementId"), "login_iam_loginButton");

        @SuppressWarnings("unchecked")
        ArgumentCaptor<Collection<Sequence>> sequences = ArgumentCaptor.forClass(Collection.class);
        verify(localDriver, times(1)).perform(sequences.capture());
        assertW3cCentreCoords(sequences.getValue(), 196, 671);
    }

    @Test
    public void unreadablePageSourceBeforeAnchoredTapDoesNotReportSuccess() {
        IOSDriver localDriver = mock(IOSDriver.class);
        RemoteWebElement localElement = mock(RemoteWebElement.class);
        when(localElement.getId()).thenReturn("login_iam_loginButton");
        when(localElement.getSize()).thenReturn(new Dimension(120, 44));
        when(localElement.getRect()).thenReturn(new Rectangle(10, 20, 44, 120));
        when(localElement.getDomAttribute(anyString())).thenReturn("false");
        when(localElement.getAttribute(anyString())).thenReturn("false");
        when(localDriver.getPageSource()).thenReturn(null, UNCHANGED_SOURCE);
        doAnswer(invocation -> null).when(localDriver).executeScript(anyString(), any());
        doAnswer(invocation -> null).when(localDriver).perform(any());

        Throwable failure = failureFrom(() -> IosNonHittableTap.afterReportedClick(localDriver, localElement));
        assertNoEffect(failure);
        verify(localDriver, times(1)).executeScript(eq("mobile: tap"), any());
        verify(localDriver, never()).perform(anyCollection());
    }

    @Test
    public void unreadablePageSourceAfterAnchoredTapDoesNotReportSuccess() {
        IOSDriver localDriver = mock(IOSDriver.class);
        RemoteWebElement localElement = mock(RemoteWebElement.class);
        when(localElement.getId()).thenReturn("login_iam_loginButton");
        when(localElement.getSize()).thenReturn(new Dimension(120, 44));
        when(localElement.getRect()).thenReturn(new Rectangle(10, 20, 44, 120));
        when(localElement.getDomAttribute(anyString())).thenReturn("false");
        when(localElement.getAttribute(anyString())).thenReturn("false");
        when(localDriver.getPageSource()).thenReturn(UNCHANGED_SOURCE, (String) null);
        doAnswer(invocation -> null).when(localDriver).executeScript(anyString(), any());
        doAnswer(invocation -> null).when(localDriver).perform(any());

        Throwable failure = failureFrom(() -> IosNonHittableTap.afterReportedClick(localDriver, localElement));
        assertNoEffect(failure);
        verify(localDriver, times(1)).executeScript(eq("mobile: tap"), any());
        verify(localDriver, never()).perform(anyCollection());
    }

    @Test
    public void mobileTapRecoverySucceedsWithoutW3cWhenPageSourceChanges() {
        IOSDriver localDriver = mock(IOSDriver.class);
        RemoteWebElement localElement = mock(RemoteWebElement.class);
        when(localElement.getId()).thenReturn("login_iam_loginButton");
        when(localElement.getSize()).thenReturn(new Dimension(329, 53));
        when(localElement.getRect()).thenReturn(FIXTURE_RECT);
        when(localElement.getDomAttribute(anyString())).thenReturn("false");
        when(localElement.getAttribute(anyString())).thenReturn("false");
        when(localDriver.getPageSource()).thenReturn(UNCHANGED_SOURCE, AFTER_MOBILE_SOURCE);
        doAnswer(invocation -> null).when(localDriver).executeScript(anyString(), any());
        doAnswer(invocation -> null).when(localDriver).perform(any());

        IosNonHittableTap.afterReportedClick(localDriver, localElement);

        verify(localDriver, times(1)).executeScript(eq("mobile: tap"), any());
        verify(localDriver, never()).perform(anyCollection());
    }

    @Test
    public void missingElementRectFailsLoudWithoutW3cTap() {
        IOSDriver localDriver = mock(IOSDriver.class);
        RemoteWebElement localElement = mock(RemoteWebElement.class);
        when(localElement.getId()).thenReturn("login_iam_loginButton");
        when(localElement.getSize()).thenReturn(new Dimension(329, 53));
        when(localElement.getRect()).thenReturn(null);
        when(localElement.getDomAttribute(anyString())).thenReturn("false");
        when(localElement.getAttribute(anyString())).thenReturn("false");
        when(localDriver.getPageSource()).thenReturn(UNCHANGED_SOURCE);
        doAnswer(invocation -> null).when(localDriver).executeScript(anyString(), any());
        doAnswer(invocation -> null).when(localDriver).perform(any());

        Throwable failure = failureFrom(() -> IosNonHittableTap.afterReportedClick(localDriver, localElement));
        assertNoEffect(failure);
        verify(localDriver, never()).perform(anyCollection());
    }

    @Test
    public void getRectFailureFailsLoudWithoutW3cTap() {
        IOSDriver localDriver = mock(IOSDriver.class);
        RemoteWebElement localElement = mock(RemoteWebElement.class);
        when(localElement.getId()).thenReturn("login_iam_loginButton");
        when(localElement.getSize()).thenReturn(new Dimension(329, 53));
        when(localElement.getRect()).thenThrow(new RuntimeException("stale element"));
        when(localElement.getDomAttribute(anyString())).thenReturn("false");
        when(localElement.getAttribute(anyString())).thenReturn("false");
        when(localDriver.getPageSource()).thenReturn(UNCHANGED_SOURCE);
        doAnswer(invocation -> null).when(localDriver).executeScript(anyString(), any());
        doAnswer(invocation -> null).when(localDriver).perform(any());

        Throwable failure = failureFrom(() -> IosNonHittableTap.afterReportedClick(localDriver, localElement));
        assertNoEffect(failure);
        verify(localDriver, never()).perform(anyCollection());
    }

    @Test
    public void w3cPerformFailureStillFailsLoud() {
        IOSDriver localDriver = mock(IOSDriver.class);
        RemoteWebElement localElement = mock(RemoteWebElement.class);
        when(localElement.getId()).thenReturn("login_iam_loginButton");
        when(localElement.getSize()).thenReturn(new Dimension(329, 53));
        when(localElement.getRect()).thenReturn(FIXTURE_RECT);
        when(localElement.getDomAttribute(anyString())).thenReturn("false");
        when(localElement.getAttribute(anyString())).thenReturn("false");
        when(localDriver.getPageSource()).thenReturn(UNCHANGED_SOURCE);
        doAnswer(invocation -> null).when(localDriver).executeScript(anyString(), any());
        doThrow(new RuntimeException("gesture rejected")).when(localDriver).perform(any());

        Throwable failure = failureFrom(() -> IosNonHittableTap.afterReportedClick(localDriver, localElement));
        assertNoEffect(failure);
        verify(localDriver, times(1)).perform(anyCollection());
    }

    @Test
    public void w3cElementCenterTapRejectsNonAppiumDriver() {
        WebDriver plain = mock(WebDriver.class);
        WebElement element = mock(WebElement.class);
        when(element.getRect()).thenReturn(FIXTURE_RECT);

        assertFalse(IosNonHittableTap.w3cElementCenterTap(plain, element));
    }

    private static void assertW3cCentreCoords(Collection<Sequence> sequences, int expectedX, int expectedY) {
        assertFalse(sequences == null || sequences.isEmpty(), "expected a W3C Sequence");
        Sequence tap = sequences.iterator().next();
        @SuppressWarnings("unchecked")
        Map<String, Object> encoded = tap.encode();
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> actions = (List<Map<String, Object>>) encoded.get("actions");
        Map<String, Object> move = actions.stream()
                .filter(action -> "pointerMove".equals(action.get("type")))
                .findFirst()
                .orElseThrow(() -> new AssertionError("no pointerMove in " + actions));
        assertEquals(((Number) move.get("x")).intValue(), expectedX, "W3C centre x: " + move);
        assertEquals(((Number) move.get("y")).intValue(), expectedY, "W3C centre y: " + move);
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

    private void assertAnchoredTapAtElementCentre() {
        @SuppressWarnings("unchecked")
        ArgumentCaptor<Map<String, Object>> params = ArgumentCaptor.forClass(Map.class);
        verify(driver, org.mockito.Mockito.atLeast(2)).executeScript(eq("mobile: tap"), params.capture());
        assertFalse(params.getAllValues().isEmpty());
        for (Map<String, Object> tap : params.getAllValues()) {
            assertEquals(tap.get("elementId"), "login_iam_loginButton");
            assertEquals(((Number) tap.get("x")).doubleValue(), 60.0, "element-relative centre x: " + tap);
            assertEquals(((Number) tap.get("y")).doubleValue(), 22.0, "element-relative centre y: " + tap);
        }
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
        assertTrue(hasType(thrown, IosTapHadNoEffectException.class), text.toString());
    }

    private static boolean hasType(Throwable thrown, Class<? extends Throwable> type) {
        Throwable current = thrown;
        int depth = 0;
        while (current != null && depth < 8) {
            if (type.isInstance(current)) {
                return true;
            }
            current = current.getCause();
            depth++;
        }
        return false;
    }
}
