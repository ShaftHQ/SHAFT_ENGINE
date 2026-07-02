package testPackage.unitTests;

import com.shaft.cli.SshShellOptions;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.time.Duration;
import java.util.Map;

public class SshShellOptionsUnitTest {
    @Test(description = "Builder should apply documented defaults")
    public void builderShouldApplyDefaults() {
        SshShellOptions options = SshShellOptions.builder().build();

        Assert.assertFalse(options.isPty());
        Assert.assertEquals(options.getPtyType(), "vt100");
        Assert.assertEquals(options.getColumns(), 80);
        Assert.assertEquals(options.getRows(), 24);
        Assert.assertEquals(options.getDefaultTimeout(), Duration.ofSeconds(30));
        Assert.assertTrue(options.getEnvironment().isEmpty());
    }

    @Test(description = "Builder should copy environment variables")
    public void builderShouldCopyEnvironmentVariables() {
        SshShellOptions options = SshShellOptions.builder()
                .environment(Map.of("LC_ALL", "C"))
                .build();

        Assert.assertEquals(options.getEnvironment().get("LC_ALL"), "C");
    }

    @Test(description = "Builder should reject non-positive PTY dimensions")
    public void builderShouldRejectNonPositivePtyDimensions() {
        IllegalArgumentException failure = Assert.expectThrows(IllegalArgumentException.class,
                () -> SshShellOptions.builder().columns(0).build());
        Assert.assertTrue(failure.getMessage().contains("columns"));
    }

    @Test(description = "Builder should reject non-positive default timeout")
    public void builderShouldRejectNonPositiveDefaultTimeout() {
        IllegalArgumentException failure = Assert.expectThrows(IllegalArgumentException.class,
                () -> SshShellOptions.builder().defaultTimeout(Duration.ZERO).build());
        Assert.assertTrue(failure.getMessage().contains("timeout"));
    }
}
