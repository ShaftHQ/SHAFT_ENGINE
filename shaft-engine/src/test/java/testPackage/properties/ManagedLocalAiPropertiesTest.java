package testPackage.properties;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

public class ManagedLocalAiPropertiesTest {
    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void defaultsKeepManagedLocalAiDisabledAndBatteriesIncluded() {
        Assert.assertFalse(SHAFT.Properties.managedLocalAi.enabled());
        Assert.assertTrue(SHAFT.Properties.managedLocalAi.transparentProvisioning());
        Assert.assertEquals(SHAFT.Properties.managedLocalAi.model(), "auto");
        Assert.assertEquals(SHAFT.Properties.managedLocalAi.cacheDirectory(), "");
        Assert.assertEquals(SHAFT.Properties.managedLocalAi.downloadTimeoutSeconds(), 900);
        Assert.assertEquals(SHAFT.Properties.managedLocalAi.lockTimeoutSeconds(), 30);
        Assert.assertEquals(SHAFT.Properties.managedLocalAi.launchTimeoutSeconds(), 120);
    }

    @Test
    public void settersAreCurrentThreadOnlyAndClearRestoresDefaults() throws Exception {
        SHAFT.Properties.managedLocalAi.set()
                .enabled(true)
                .transparentProvisioning(false)
                .model("qwen3-1.7b-q8_0")
                .cacheDirectory("target/custom-managed-ai")
                .downloadTimeoutSeconds(60)
                .lockTimeoutSeconds(5)
                .launchTimeoutSeconds(20);

        Assert.assertTrue(SHAFT.Properties.managedLocalAi.enabled());
        Assert.assertFalse(SHAFT.Properties.managedLocalAi.transparentProvisioning());
        Assert.assertEquals(SHAFT.Properties.managedLocalAi.model(), "qwen3-1.7b-q8_0");
        Assert.assertEquals(SHAFT.Properties.managedLocalAi.cacheDirectory(), "target/custom-managed-ai");

        CountDownLatch read = new CountDownLatch(1);
        AtomicReference<Boolean> siblingEnabled = new AtomicReference<>();
        Thread sibling = Thread.ofPlatform().start(() -> {
            siblingEnabled.set(SHAFT.Properties.managedLocalAi.enabled());
            read.countDown();
            Properties.clearForCurrentThread();
        });
        Assert.assertTrue(read.await(5, TimeUnit.SECONDS));
        sibling.join(5000);
        Assert.assertFalse(sibling.isAlive());
        Assert.assertFalse(siblingEnabled.get());

        Properties.clearForCurrentThread();
        Assert.assertFalse(SHAFT.Properties.managedLocalAi.enabled());
        Assert.assertEquals(SHAFT.Properties.managedLocalAi.model(), "auto");
    }
}
