package com.shaft.cli;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.time.Duration;
import java.util.Map;

public class SshShellOptionsUnitTest {
    @Test(description = "Default shell options should be usable without a PTY")
    public void defaultsShouldUseNonPtyShellWithTimeout() {
        SshShellOptions options = SshShellOptions.defaults();

        Assert.assertFalse(options.isPty());
        Assert.assertEquals(options.getPtyType(), "vt100");
        Assert.assertEquals(options.getColumns(), 80);
        Assert.assertEquals(options.getRows(), 24);
        Assert.assertEquals(options.getDefaultTimeout(), Duration.ofSeconds(30));
        Assert.assertTrue(options.getEnvironmentVariables().isEmpty());
    }

    @Test(description = "Builder should preserve PTY and environment options")
    public void builderShouldPreservePtyAndEnvironmentOptions() {
        SshShellOptions options = SshShellOptions.builder()
                .pty(true)
                .ptyType("xterm")
                .columns(120)
                .rows(40)
                .defaultTimeout(Duration.ofSeconds(5))
                .environmentVariables(Map.of("LC_ALL", "C"))
                .build();

        Assert.assertTrue(options.isPty());
        Assert.assertEquals(options.getPtyType(), "xterm");
        Assert.assertEquals(options.getColumns(), 120);
        Assert.assertEquals(options.getRows(), 40);
        Assert.assertEquals(options.getDefaultTimeout(), Duration.ofSeconds(5));
        Assert.assertEquals(options.getEnvironmentVariables().get("LC_ALL"), "C");
    }

    @Test(description = "Builder should reject invalid terminal dimensions")
    public void builderShouldRejectInvalidDimensions() {
        IllegalArgumentException columnsFailure = Assert.expectThrows(IllegalArgumentException.class,
                () -> SshShellOptions.builder().columns(0).build());
        Assert.assertTrue(columnsFailure.getMessage().contains("columns"));

        IllegalArgumentException rowsFailure = Assert.expectThrows(IllegalArgumentException.class,
                () -> SshShellOptions.builder().rows(0).build());
        Assert.assertTrue(rowsFailure.getMessage().contains("rows"));
    }

    @Test(description = "Builder should reject invalid default timeouts")
    public void builderShouldRejectInvalidDefaultTimeouts() {
        IllegalArgumentException failure = Assert.expectThrows(IllegalArgumentException.class,
                () -> SshShellOptions.builder().defaultTimeout(Duration.ZERO).build());
        Assert.assertTrue(failure.getMessage().contains("timeout"));
    }
}
