package testPackage.properties;

import com.shaft.driver.SHAFT;
import com.shaft.infrastructure.SetupApproval;
import com.shaft.infrastructure.SetupMode;
import com.shaft.infrastructure.SetupPlan;
import com.shaft.infrastructure.SetupProfile;
import com.shaft.properties.internal.Properties;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.CountDownLatch;

public class InfrastructurePropertiesTests {
    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void defaultsRemainExternalAndNonMutating() throws Exception {
        Path root = Files.createTempDirectory("shaft-infrastructure-default-").toAbsolutePath();
        Files.delete(root);
        SHAFT.Properties.infrastructure.set().cacheDirectory(root.toString());

        Assert.assertEquals(SHAFT.Properties.infrastructure.mode(), SetupMode.EXTERNAL);
        Assert.assertEquals(SHAFT.Properties.infrastructure.profile(), SetupProfile.REPORTING);
        Assert.assertFalse(SHAFT.Properties.infrastructure.offline());
        Assert.assertFalse(SHAFT.Properties.infrastructure.autoStart());
        SetupPlan plan = SHAFT.Infrastructure.plan();
        Assert.assertEquals(plan.mode(), SetupMode.EXTERNAL);
        Assert.assertTrue(plan.actions().stream().allMatch(action -> action.kind().name().equals("DIAGNOSE")));
        SHAFT.Infrastructure.status();
        Assert.assertFalse(Files.exists(root));
    }

    @Test
    public void settersBuildTypedManagedOptionsForCurrentThread() throws Exception {
        Path root = Files.createTempDirectory("shaft-infrastructure-options-").toAbsolutePath();
        SHAFT.Properties.infrastructure.set().mode(SetupMode.MANAGED).profile(SetupProfile.REPORTING)
                .cacheDirectory(root.toString()).offline(true).autoStart(true)
                .preferSystemTools(false).reuseOwnedProcesses(false)
                .startupTimeout("PT45S").shutdownTimeout("PT10S");

        var options = SHAFT.Infrastructure.options();
        Assert.assertEquals(options.mode(), SetupMode.MANAGED);
        Assert.assertTrue(options.offline());
        Assert.assertTrue(options.autoStart());
        Assert.assertFalse(options.preferSystemTools());
        Assert.assertFalse(options.reuseOwnedProcesses());
        Assert.assertEquals(options.startupTimeout().toSeconds(), 45);
        Assert.assertEquals(options.shutdownTimeout().toSeconds(), 10);
        Assert.assertEquals(options.paths().cacheRoot(), root);
    }

    @Test
    public void staleApprovalAndRelativeCacheFailBeforeMutation() throws Exception {
        Path root = Files.createTempDirectory("shaft-infrastructure-approval-").toAbsolutePath();
        Files.delete(root);
        SHAFT.Properties.infrastructure.set().mode(SetupMode.MANAGED).cacheDirectory(root.toString());
        SetupPlan plan = SHAFT.Infrastructure.plan();

        Assert.expectThrows(com.shaft.infrastructure.StaleSetupApprovalException.class,
                () -> SHAFT.Infrastructure.install(plan, new SetupApproval(
                        "sha256:" + "0".repeat(64), Instant.EPOCH, Set.of())));
        Assert.assertFalse(Files.exists(root));

        SHAFT.Properties.infrastructure.set().cacheDirectory("relative-cache");
        Assert.expectThrows(IllegalArgumentException.class, SHAFT.Infrastructure::options);

        SHAFT.Properties.infrastructure.set().cacheDirectory(root.toString()).startupTimeout("not-a-duration");
        IllegalArgumentException malformed = Assert.expectThrows(IllegalArgumentException.class,
                SHAFT.Infrastructure::options);
        Assert.assertTrue(malformed.getMessage().contains("infrastructure.startupTimeout"));
        SHAFT.Properties.infrastructure.set().startupTimeout("PT0S");
        Assert.assertTrue(Assert.expectThrows(IllegalArgumentException.class, SHAFT.Infrastructure::options)
                .getMessage().contains("infrastructure.startupTimeout"));
    }

    @Test
    public void overridesAreThreadLocalAndClearRestoresDefaults() throws Exception {
        AtomicReference<SetupMode> writerBeforeClear = new AtomicReference<>();
        AtomicReference<SetupMode> writerAfterClear = new AtomicReference<>();
        CountDownLatch overrideActive = new CountDownLatch(1);
        CountDownLatch observerRead = new CountDownLatch(1);
        Thread writer = new Thread(() -> {
            SHAFT.Properties.infrastructure.set().mode(SetupMode.MANAGED);
            writerBeforeClear.set(SHAFT.Properties.infrastructure.mode());
            overrideActive.countDown();
            try {
                observerRead.await();
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                throw new IllegalStateException(interrupted);
            }
            Properties.clearForCurrentThread();
            writerAfterClear.set(SHAFT.Properties.infrastructure.mode());
        });
        writer.start();
        overrideActive.await();
        Assert.assertEquals(SHAFT.Properties.infrastructure.mode(), SetupMode.EXTERNAL);
        observerRead.countDown();
        writer.join();

        Assert.assertEquals(writerBeforeClear.get(), SetupMode.MANAGED);
        Assert.assertEquals(writerAfterClear.get(), SetupMode.EXTERNAL);
        Assert.assertEquals(SHAFT.Properties.infrastructure.mode(), SetupMode.EXTERNAL);
    }

    @Test
    public void explicitRemoteExecutionWinsForEndpointProfiles() throws Exception {
        Path root = Files.createTempDirectory("shaft-infrastructure-remote-").toAbsolutePath();
        SHAFT.Properties.infrastructure.set().mode(SetupMode.MANAGED).profile(SetupProfile.SELENIUM_GRID)
                .cacheDirectory(root.toString());
        SHAFT.Properties.platform.set().executionAddress("grid.example:4444");

        var options = SHAFT.Infrastructure.options();
        Assert.assertEquals(options.mode(), SetupMode.MANAGED);
        Assert.assertEquals(options.effectiveMode(), SetupMode.EXTERNAL);
        Assert.assertEquals(options.remoteEndpoint().orElseThrow().toString(), "http://grid.example:4444");

        SHAFT.Properties.infrastructure.set().profile(SetupProfile.REPORTING);
        var reporting = SHAFT.Infrastructure.options();
        Assert.assertEquals(reporting.effectiveMode(), SetupMode.MANAGED);
        Assert.assertTrue(reporting.remoteEndpoint().isEmpty());
    }
}
