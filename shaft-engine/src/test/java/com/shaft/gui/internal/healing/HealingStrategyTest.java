package com.shaft.gui.internal.healing;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class HealingStrategyTest {
    @BeforeMethod
    public void resetConfiguration() {
        Properties.clearForCurrentThread();
        SHAFT.Properties.healenium.set().healEnabled(false);
        SHAFT.Properties.healing.set().strategy("disabled");
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void legacyHealeniumFlagShouldRemainCompatible() {
        SHAFT.Properties.healenium.set().healEnabled(true);

        Assert.assertEquals(HealingStrategy.current(), HealingStrategy.HEALENIUM);
    }

    @Test
    public void explicitShaftHealShouldOverrideLegacyHealeniumFlag() {
        SHAFT.Properties.healenium.set().healEnabled(true);
        SHAFT.Properties.healing.set().strategy("shaft-heal");

        Assert.assertEquals(HealingStrategy.current(), HealingStrategy.SHAFT_HEAL);
        Assert.assertFalse(HealingStrategy.current().usesHealenium());
        Assert.assertTrue(HealingStrategy.current().usesShaftHeal());
    }

    @Test
    public void compositeShouldEnableBothRecoverySystems() {
        SHAFT.Properties.healing.set().strategy("composite");

        Assert.assertTrue(HealingStrategy.current().usesHealenium());
        Assert.assertTrue(HealingStrategy.current().usesShaftHeal());
    }

    @Test
    public void unknownStrategyShouldFailClosed() {
        Assert.assertEquals(HealingStrategy.parse("unknown"), HealingStrategy.DISABLED);
    }
}
