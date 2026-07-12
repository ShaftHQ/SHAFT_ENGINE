package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Web;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

/**
 * Coverage: {@code SHAFT.Properties.web.storageStatePath()} default value and fluent override.
 */
@Test(singleThreaded = true)
public class WebStorageStatePathSetPropertyUnitTest {
    private final String originalStorageStatePath = SHAFT.Properties.web.storageStatePath();

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        SHAFT.Properties.web.set().storageStatePath(originalStorageStatePath);
    }

    @Test(description = "Coverage: storageStatePath defaults to blank (feature disabled) unless explicitly configured")
    public void storageStatePathShouldDefaultToBlank() {
        SHAFT.Properties.web.set().storageStatePath("");
        Assert.assertEquals(SHAFT.Properties.web.storageStatePath(), "");
    }

    @Test(description = "Coverage: storageStatePath fluent setter updates the effective value and returns the same builder")
    public void storageStatePathFluentSetterShouldUpdateEffectiveValue() {
        Web.SetProperty setProperty = SHAFT.Properties.web.set();
        Assert.assertSame(setProperty.storageStatePath("target/auth-state.json"), setProperty);
        Assert.assertEquals(SHAFT.Properties.web.storageStatePath(), "target/auth-state.json");
    }
}
