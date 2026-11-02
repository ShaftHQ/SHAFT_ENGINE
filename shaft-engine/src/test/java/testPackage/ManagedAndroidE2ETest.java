package testPackage;

import com.shaft.driver.SHAFT;
import com.shaft.infrastructure.AndroidSetupPlanner;
import com.shaft.infrastructure.AndroidSetupRequest;
import com.shaft.infrastructure.SetupApproval;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.android.options.UiAutomator2Options;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.Test;

import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.time.Instant;
import java.util.Set;

/** Opt-in real acceptance for the release-pinned owned emulator, Appium, UiAutomator2, and aapt2. */
public class ManagedAndroidE2ETest {
    @Test
    public void startsReceiptBoundAndroidSessionAndProbesAapt2() throws Exception {
        if (!Boolean.getBoolean("runManagedAndroidE2E")) {
            throw new SkipException("Set -DrunManagedAndroidE2E=true to run the real managed Android acceptance.");
        }
        var options = SHAFT.Infrastructure.options();
        var request = AndroidSetupRequest.defaults();
        var plan = SHAFT.Infrastructure.plan(options, request);
        var approval = new SetupApproval(plan.digest(), Instant.now(),
                Set.of(AndroidSetupPlanner.ANDROID_SDK_LICENSE));

        try (var environment = SHAFT.Infrastructure.start(plan, approval, options, request)) {
            probeAapt2(environment.connectionProperties());
            String serial = environment.connectionProperties().get("ANDROID_SERIAL");
            UiAutomator2Options capabilities = new UiAutomator2Options()
                    .setUdid(serial)
                    .setDeviceName(request.avdName())
                    .setAppPackage("com.android.settings")
                    .setAppActivity(".Settings");
            AndroidDriver driver = new AndroidDriver(environment.endpoint().orElseThrow().toURL(), capabilities);
            try {
                Assert.assertNotNull(driver.getSessionId());
                Assert.assertFalse(driver.getPageSource().isBlank());
            } finally {
                driver.quit();
            }
        }
    }

    private static void probeAapt2(java.util.Map<String, String> connectionProperties) throws Exception {
        Path sdk = Path.of(connectionProperties.get("ANDROID_SDK_ROOT"));
        String executable = System.getProperty("os.name", "").toLowerCase().contains("win")
                ? "aapt2.exe" : "aapt2";
        ProcessBuilder builder = new ProcessBuilder(sdk.resolve("build-tools")
                .resolve(AndroidSetupPlanner.BUILD_TOOLS_VERSION).resolve(executable).toString(), "version");
        builder.environment().putAll(connectionProperties);
        Process process = builder.redirectErrorStream(true).start();
        String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        Assert.assertEquals(process.waitFor(), 0, output);
        Assert.assertTrue(output.trim().startsWith("Android Asset Packaging Tool (aapt) 2."), output);
    }
}
