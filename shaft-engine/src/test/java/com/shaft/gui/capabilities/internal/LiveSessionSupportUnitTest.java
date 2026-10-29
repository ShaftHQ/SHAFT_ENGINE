package com.shaft.gui.capabilities.internal;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.remote.SessionId;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.HasCapabilities;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.testng.Assert;
import org.testng.annotations.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

public class LiveSessionSupportUnitTest {

    @Test
    public void capabilitiesShouldReturnNullForPlainWebDriver() {
        WebDriver driver = mock(WebDriver.class);

        Assert.assertNull(LiveSessionSupport.capabilities(driver));
        Assert.assertFalse(LiveSessionSupport.isRemoteOrAppiumSession(driver));
        Assert.assertFalse(LiveSessionSupport.isClosedRemoteSession(driver));
        Assert.assertFalse(LiveSessionSupport.hasLiveRemoteSession(driver));
    }

    @Test
    public void capabilitiesShouldReturnNegotiatedCapabilitiesWhenExposed() {
        MutableCapabilities expected = new MutableCapabilities();
        expected.setCapability("browserName", "chrome");
        WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(HasCapabilities.class));
        when(((HasCapabilities) driver).getCapabilities()).thenReturn(expected);

        Capabilities actual = LiveSessionSupport.capabilities(driver);

        Assert.assertSame(actual, expected);
        Assert.assertNotNull(actual);
        Assert.assertEquals(actual.getBrowserName(), "chrome");
    }

    @Test
    public void capabilitiesShouldReturnNullWhenReadingCapabilitiesFails() {
        WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(HasCapabilities.class));
        when(((HasCapabilities) driver).getCapabilities()).thenThrow(new RuntimeException("session gone"));

        Assert.assertNull(LiveSessionSupport.capabilities(driver));
    }

    @Test
    public void remoteWebDriverClosedAndLiveSessionDetection() {
        RemoteWebDriver closed = mock(RemoteWebDriver.class);
        when(closed.getSessionId()).thenReturn(null);
        RemoteWebDriver live = mock(RemoteWebDriver.class);
        when(live.getSessionId()).thenReturn(new org.openqa.selenium.remote.SessionId("selenium-live"));

        Assert.assertTrue(LiveSessionSupport.isRemoteOrAppiumSession(closed));
        Assert.assertTrue(LiveSessionSupport.isClosedRemoteSession(closed));
        Assert.assertFalse(LiveSessionSupport.hasLiveRemoteSession(closed));

        Assert.assertTrue(LiveSessionSupport.isRemoteOrAppiumSession(live));
        Assert.assertFalse(LiveSessionSupport.isClosedRemoteSession(live));
        Assert.assertTrue(LiveSessionSupport.hasLiveRemoteSession(live));
    }

    @Test
    public void appiumDriverClosedAndLiveSessionDetection() {
        AppiumDriver closed = mock(AppiumDriver.class);
        when(closed.getSessionId()).thenReturn(null);
        AppiumDriver live = mock(AppiumDriver.class);
        when(live.getSessionId()).thenReturn(new SessionId("appium-live"));

        Assert.assertTrue(LiveSessionSupport.isRemoteOrAppiumSession(closed));
        Assert.assertTrue(LiveSessionSupport.isClosedRemoteSession(closed));
        Assert.assertFalse(LiveSessionSupport.hasLiveRemoteSession(closed));

        Assert.assertTrue(LiveSessionSupport.isRemoteOrAppiumSession(live));
        Assert.assertFalse(LiveSessionSupport.isClosedRemoteSession(live));
        Assert.assertTrue(LiveSessionSupport.hasLiveRemoteSession(live));
    }

    @Test
    public void nullDriverIsNotTreatedAsRemoteOrClosed() {
        Assert.assertFalse(LiveSessionSupport.isRemoteOrAppiumSession(null));
        Assert.assertFalse(LiveSessionSupport.isClosedRemoteSession(null));
        Assert.assertFalse(LiveSessionSupport.hasLiveRemoteSession(null));
        Assert.assertNull(LiveSessionSupport.capabilities(null));
    }

    @Test
    public void utilityClassConstructorIsPrivate() throws Exception {
        var ctor = LiveSessionSupport.class.getDeclaredConstructor();
        Assert.assertTrue(java.lang.reflect.Modifier.isPrivate(ctor.getModifiers()));
        ctor.setAccessible(true);
        try {
            ctor.newInstance();
            Assert.fail("expected IllegalStateException");
        } catch (java.lang.reflect.InvocationTargetException ex) {
            Assert.assertTrue(ex.getCause() instanceof IllegalStateException);
            Assert.assertEquals(ex.getCause().getMessage(), "Utility class");
        }
    }
}
