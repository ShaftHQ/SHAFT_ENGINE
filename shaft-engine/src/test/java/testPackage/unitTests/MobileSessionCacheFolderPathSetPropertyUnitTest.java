package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Paths;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

/**
 * Coverage: {@code SHAFT.Properties.paths.mobileSessionCache()} default value and fluent override.
 */
@Test(singleThreaded = true)
public class MobileSessionCacheFolderPathSetPropertyUnitTest {
    private final String originalMobileSessionCache = SHAFT.Properties.paths.mobileSessionCache();

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        SHAFT.Properties.paths.set().mobileSessionCache(originalMobileSessionCache);
    }

    @Test(description = "Coverage: mobileSessionCache defaults to target/mobile-session-cache/ unless explicitly configured")
    public void mobileSessionCacheShouldDefaultToTargetFolder() {
        SHAFT.Properties.paths.set().mobileSessionCache("target/mobile-session-cache/");
        Assert.assertEquals(SHAFT.Properties.paths.mobileSessionCache(), "target/mobile-session-cache/");
    }

    @Test(description = "Coverage: mobileSessionCache fluent setter updates the effective value and returns the same builder")
    public void mobileSessionCacheFluentSetterShouldUpdateEffectiveValue() {
        Paths.SetProperty setProperty = SHAFT.Properties.paths.set();
        Assert.assertSame(setProperty.mobileSessionCache("target/custom-mobile-cache/"), setProperty);
        Assert.assertEquals(SHAFT.Properties.paths.mobileSessionCache(), "target/custom-mobile-cache/");
    }
}
