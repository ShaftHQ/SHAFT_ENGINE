package com.shaft.listeners.internal;

import org.openqa.selenium.SessionNotCreatedException;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.net.UnknownHostException;

public class InfraFailureGuardUnitTest {
    @AfterMethod(alwaysRun = true)
    public void reset() {
        InfraFailureGuard.reset();
        System.clearProperty(InfraFailureGuard.LIMIT_PROPERTY);
    }

    @Test
    public void classifiesInfrastructureCausesAndNotProductFailures() {
        Assert.assertTrue(InfraFailureGuard.isInfra(new SessionNotCreatedException("no session")));
        Assert.assertTrue(InfraFailureGuard.isInfra(new RuntimeException("wrapped", new UnknownHostException("api.example.gov"))));
        Assert.assertTrue(InfraFailureGuard.isInfra(new IllegalStateException("Preflight: device emulator-5554 not booted")));
        Assert.assertTrue(InfraFailureGuard.isInfra(new RuntimeException("adb: device offline")));
        Assert.assertFalse(InfraFailureGuard.isInfra(new AssertionError("expected [Welcome] but found [Login]")));
        Assert.assertFalse(InfraFailureGuard.isInfra(null));
    }

    @Test
    public void stopsAfterTwoInfraSetupFailuresInARowAndSuccessResets() {
        Assert.assertNull(InfraFailureGuard.stopReason());
        InfraFailureGuard.onConfigurationFailure(new SessionNotCreatedException("down"));
        Assert.assertNull(InfraFailureGuard.stopReason());
        InfraFailureGuard.onConfigurationFailure(new AssertionError("product bug"));
        Assert.assertNull(InfraFailureGuard.stopReason(), "a product failure does not count");
        InfraFailureGuard.onConfigurationFailure(new SessionNotCreatedException("down"));
        Assert.assertTrue(InfraFailureGuard.stopReason().startsWith("Stopped: 2 infrastructure setup failures in a row"));
        InfraFailureGuard.onConfigurationSuccess();
        Assert.assertNull(InfraFailureGuard.stopReason());
    }

    @Test
    public void limitZeroDisablesTheStop() {
        System.setProperty(InfraFailureGuard.LIMIT_PROPERTY, "0");
        InfraFailureGuard.onConfigurationFailure(new SessionNotCreatedException("down"));
        InfraFailureGuard.onConfigurationFailure(new SessionNotCreatedException("down"));
        Assert.assertNull(InfraFailureGuard.stopReason());
    }
}
