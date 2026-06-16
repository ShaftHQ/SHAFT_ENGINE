package testPackage.properties;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

public class NaturalActionsTests {
    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void defaultsShouldKeepNaturalActionsDeterministicAndDisabled() {
        Assert.assertFalse(SHAFT.Properties.naturalActions.enabled());
        Assert.assertEquals(SHAFT.Properties.naturalActions.minimumTrustPercentage(), 85);
        Assert.assertEquals(SHAFT.Properties.naturalActions.planner(), "deterministic");
        Assert.assertFalse(SHAFT.Properties.naturalActions.aiFallbackEnabled());
        Assert.assertEquals(SHAFT.Properties.naturalActions.allowedActions(), "browser,element,touch");
    }

    @Test
    public void settersShouldApplyToCurrentThread() {
        SHAFT.Properties.naturalActions.set()
                .enabled(true)
                .minimumTrustPercentage(90)
                .planner("auto")
                .aiFallbackEnabled(true)
                .allowedActions("element");

        Assert.assertTrue(SHAFT.Properties.naturalActions.enabled());
        Assert.assertEquals(SHAFT.Properties.naturalActions.minimumTrustPercentage(), 90);
        Assert.assertEquals(SHAFT.Properties.naturalActions.planner(), "auto");
        Assert.assertTrue(SHAFT.Properties.naturalActions.aiFallbackEnabled());
        Assert.assertEquals(SHAFT.Properties.naturalActions.allowedActions(), "element");
    }
}
