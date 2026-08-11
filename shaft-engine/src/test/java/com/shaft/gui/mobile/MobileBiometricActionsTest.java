package com.shaft.gui.mobile;

import com.shaft.driver.SHAFT;
import com.shaft.gui.driver.MobileBiometricActionsContract;
import com.shaft.gui.driver.MobileFingerprintActionsContract;
import com.shaft.gui.driver.MobileTouchIdActionsContract;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.mockito.Mockito;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.Test;

public class MobileBiometricActionsTest {
    @Test
    public void androidFingerprintShouldValidateDelegateAndRetainFluentOwnership() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("android-biometric"));
        var mobile = new SHAFT.GUI.WebDriver(driver).mobile();
        MobileBiometricActionsContract biometrics = mobile.biometrics();
        MobileFingerprintActionsContract fingerprint = biometrics.fingerprint();

        Assert.assertSame(fingerprint.authenticate(1), fingerprint);
        Mockito.verify(driver).fingerPrint(1);
        Assert.assertSame(fingerprint.and(), biometrics);
        Assert.assertSame(biometrics.and(), mobile);
        Assert.expectThrows(IllegalArgumentException.class, () -> fingerprint.authenticate(0));
        Assert.expectThrows(IllegalArgumentException.class, () -> fingerprint.authenticate(11));
        Mockito.verify(driver, Mockito.times(1)).fingerPrint(Mockito.anyInt());
        Assert.expectThrows(UnsupportedOperationException.class, biometrics::touchId);
    }

    @Test
    public void iosTouchIdShouldExposeIntentMethodsAndRejectAndroidFingerprint() {
        IOSDriver driver = Mockito.mock(IOSDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("ios-biometric"));
        var mobile = new SHAFT.GUI.WebDriver(driver).mobile();
        MobileBiometricActionsContract biometrics = mobile.biometrics();
        MobileTouchIdActionsContract touchId = biometrics.touchId();

        Assert.assertSame(touchId.match(), touchId);
        Assert.assertSame(touchId.reject(), touchId);
        Assert.assertSame(touchId.enroll(), touchId);
        Assert.assertSame(touchId.unenroll(), touchId);
        Mockito.verify(driver).performTouchID(true);
        Mockito.verify(driver).performTouchID(false);
        Mockito.verify(driver).toggleTouchIDEnrollment(true);
        Mockito.verify(driver).toggleTouchIDEnrollment(false);
        Assert.assertSame(touchId.and(), biometrics);
        Assert.expectThrows(UnsupportedOperationException.class, biometrics::fingerprint);
    }

    @Test
    public void unsupportedStaleAndProviderFailuresShouldFailClosedWithoutChangingIdentity() {
        AppiumDriver unsupported = Mockito.mock(AppiumDriver.class);
        Mockito.when(unsupported.getSessionId()).thenReturn(new SessionId("unsupported-biometric"));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new SHAFT.GUI.WebDriver(unsupported).mobile().biometrics());

        AndroidDriver stale = Mockito.mock(AndroidDriver.class);
        Mockito.when(stale.getSessionId()).thenReturn(new SessionId("stale-biometric"));
        MobileFingerprintActionsContract cached =
                new SHAFT.GUI.WebDriver(stale).mobile().biometrics().fingerprint();
        Mockito.when(stale.getSessionId()).thenReturn(null);
        Assert.expectThrows(UnsupportedOperationException.class, () -> cached.authenticate(1));
        Assert.expectThrows(IllegalArgumentException.class, () -> cached.authenticate(0));
        Assert.expectThrows(IllegalArgumentException.class, () -> cached.authenticate(11));
        Mockito.verify(stale, Mockito.never()).fingerPrint(Mockito.anyInt());

        AndroidDriver failed = Mockito.mock(AndroidDriver.class);
        Mockito.when(failed.getSessionId()).thenReturn(new SessionId("failed-biometric"));
        IllegalStateException providerFailure = new IllegalStateException("fingerprint provider failed");
        Mockito.doThrow(providerFailure).when(failed).fingerPrint(2);
        MobileFingerprintActionsContract fingerprint =
                new SHAFT.GUI.WebDriver(failed).mobile().biometrics().fingerprint();

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, () -> fingerprint.authenticate(2));

        Assert.assertSame(thrown, providerFailure);
    }
}
