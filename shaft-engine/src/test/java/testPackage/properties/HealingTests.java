package testPackage.properties;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

public class HealingTests {
    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void defaultsShouldDisableOptionalRecoveryAndExternalEvidence() {
        Assert.assertEquals(SHAFT.Properties.healing.strategy(), "disabled");
        Assert.assertFalse(SHAFT.Properties.healing.aiEnabled());
        Assert.assertFalse(SHAFT.Properties.healing.visualEnabled());
        Assert.assertFalse(SHAFT.Properties.healing.sourcePatchEnabled());
    }

    @Test
    public void settersShouldApplyToCurrentThread() {
        SHAFT.Properties.healing.set()
                .strategy("shaft-heal")
                .minimumConfidence(0.8)
                .ambiguityMargin(0.2)
                .historyMaxEntries(25);

        Assert.assertEquals(SHAFT.Properties.healing.strategy(), "shaft-heal");
        Assert.assertEquals(SHAFT.Properties.healing.minimumConfidence(), 0.8);
        Assert.assertEquals(SHAFT.Properties.healing.ambiguityMargin(), 0.2);
        Assert.assertEquals(SHAFT.Properties.healing.historyMaxEntries(), 25);
    }
}
