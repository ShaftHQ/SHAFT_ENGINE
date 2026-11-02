package com.shaft.gui.driver;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.time.Duration;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

public class MobileRecordingOptionsTest {
    private static final long MAX_BYTES = 256L * 1024 * 1024;

    @Test
    public void shouldAcceptInclusiveProviderAndResultBounds() {
        Assert.assertEquals(new MobileRecordingOptions(Duration.ofSeconds(1), 1).timeLimit(),
                Duration.ofSeconds(1));
        Assert.assertEquals(new MobileRecordingOptions(Duration.ofMinutes(30), MAX_BYTES).maxBytes(), MAX_BYTES);
    }

    @Test
    public void shouldRejectInvalidProviderAndResultBounds() {
        Assert.expectThrows(NullPointerException.class, () -> new MobileRecordingOptions(null, 1));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new MobileRecordingOptions(Duration.ZERO, 1));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new MobileRecordingOptions(Duration.ofMillis(999), 1));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new MobileRecordingOptions(Duration.ofSeconds(-1), 1));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new MobileRecordingOptions(Duration.ofMinutes(30).plusNanos(1), 1));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new MobileRecordingOptions(Duration.ofMinutes(1), 0));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new MobileRecordingOptions(Duration.ofMinutes(1), -1));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new MobileRecordingOptions(Duration.ofMinutes(1), MAX_BYTES + 1));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new MobileRecordingOptions(Duration.ofMinutes(1), Long.MAX_VALUE));
    }

    @Test
    public void shouldExposeBoundedDefaultsWithoutDuplicatingPolicyAtCallSites() throws Exception {
        Method defaults = MobileRecordingOptions.class.getDeclaredMethod("defaults");

        Assert.assertTrue(Modifier.isPublic(defaults.getModifiers()));
        Assert.assertTrue(Modifier.isStatic(defaults.getModifiers()));
        Assert.assertEquals(defaults.getReturnType(), MobileRecordingOptions.class);
        Assert.assertEquals(MobileRecordingOptions.MIN_TIME_LIMIT, Duration.ofSeconds(1));
        Assert.assertEquals(MobileRecordingOptions.MAX_TIME_LIMIT, Duration.ofMinutes(30));
        Assert.assertEquals(MobileRecordingOptions.DEFAULT_TIME_LIMIT, Duration.ofMinutes(3));
        Assert.assertEquals(MobileRecordingOptions.MAX_RESULT_BYTES, 256L * 1024 * 1024);
        Assert.assertEquals(MobileRecordingOptions.DEFAULT_MAX_BYTES, 64L * 1024 * 1024);
        Assert.assertEquals(defaults.invoke(null), new MobileRecordingOptions(
                MobileRecordingOptions.DEFAULT_TIME_LIMIT, MobileRecordingOptions.DEFAULT_MAX_BYTES));
    }
}
