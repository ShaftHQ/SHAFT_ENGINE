package com.shaft.driver.internal.DriverFactory;

import com.shaft.infrastructure.AndroidSetupRequest;
import com.shaft.infrastructure.ManagedEnvironment;
import com.shaft.infrastructure.SetupApproval;
import com.shaft.infrastructure.SetupArchitecture;
import com.shaft.infrastructure.SetupMode;
import com.shaft.infrastructure.SetupOptions;
import com.shaft.infrastructure.SetupPlan;
import com.shaft.infrastructure.SetupPlatform;
import com.shaft.infrastructure.SetupProfile;
import com.shaft.infrastructure.SetupReceipt;
import com.shaft.infrastructure.ShaftCachePaths;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicBoolean;

public class ManagedAndroidBootstrapTest {
    @Test
    public void explicitRemoteWinsWithoutConsultingManagedInfrastructure() throws Exception {
        var calls = new AtomicInteger();
        var gateway = new StubGateway(calls, null, null);
        var options = managedOptions().withRemoteEndpoint(URI.create("https://grid.example/appium"));

        Optional<ManagedAndroidBootstrap.Session> result = ManagedAndroidBootstrap.startIfConfigured(
                "https://grid.example/appium", "Android", options, request(), gateway);

        Assert.assertTrue(result.isEmpty());
        Assert.assertEquals(calls.get(), 0);
    }

    @Test
    public void localAutoStartUsesReceiptBoundStartAndKeepsConnectionMetadataScoped() throws Exception {
        var calls = new AtomicInteger();
        SetupPlan plan = com.shaft.infrastructure.AndroidSetupPlanner.plan(SetupPlatform.WINDOWS,
                SetupArchitecture.X64, SetupMode.MANAGED, request());
        ManagedEnvironment environment = new ManagedEnvironment(SetupProfile.MOBILE_ANDROID,
                new SetupReceipt(plan.digest(), Instant.EPOCH, plan.actions()),
                Optional.of(URI.create("http://127.0.0.1:4723/")),
                Map.of("ANDROID_SERIAL", "emulator-5554"), () -> { });
        var gateway = new StubGateway(calls, plan, environment);
        String originalSerial = System.getProperty("ANDROID_SERIAL");

        ManagedAndroidBootstrap.Session session = ManagedAndroidBootstrap.startIfConfigured(
                "local", "Android", managedOptions(), request(), gateway).orElseThrow();

        Assert.assertEquals(calls.get(), 2);
        Assert.assertEquals(session.endpoint(), URI.create("http://127.0.0.1:4723/"));
        Assert.assertEquals(session.connectionProperties().get("ANDROID_SERIAL"), "emulator-5554");
        Assert.assertEquals(System.getProperty("ANDROID_SERIAL"), originalSerial);
        session.close();
    }

    @Test(expectedExceptions = IOException.class,
            expectedExceptionsMessageRegExp = ".*compatible install receipt.*")
    public void missingReceiptFailureIsNotConvertedIntoHiddenInstallation() throws Exception {
        SetupPlan plan = com.shaft.infrastructure.AndroidSetupPlanner.plan(SetupPlatform.WINDOWS,
                SetupArchitecture.X64, SetupMode.MANAGED, request());
        ManagedAndroidBootstrap.startIfConfigured("local", "Android", managedOptions(), request(),
                new ManagedAndroidBootstrap.Gateway() {
                    @Override public SetupPlan plan(SetupOptions options, AndroidSetupRequest request) { return plan; }
                    @Override public ManagedEnvironment start(SetupPlan ignored, SetupApproval approval,
                            SetupOptions options, AndroidSetupRequest request) throws IOException {
                        throw new IOException("A compatible install receipt is required.");
                    }
                });
    }

    @Test
    public void closingHelperWithoutALiveDriverStillReleasesManagedRuntimeLease() throws Exception {
        AtomicBoolean released = new AtomicBoolean();
        SetupPlan plan = com.shaft.infrastructure.AndroidSetupPlanner.plan(SetupPlatform.WINDOWS,
                SetupArchitecture.X64, SetupMode.MANAGED, request());
        ManagedEnvironment environment = new ManagedEnvironment(SetupProfile.MOBILE_ANDROID,
                new SetupReceipt(plan.digest(), Instant.EPOCH, plan.actions()),
                Optional.of(URI.create("http://127.0.0.1:4723/")), Map.of(), () -> released.set(true));
        ManagedAndroidBootstrap.Session session = new ManagedAndroidBootstrap.Session(
                URI.create("http://127.0.0.1:4723/"), Map.of(), environment);
        DriverFactoryHelper helper = new DriverFactoryHelper();
        var field = DriverFactoryHelper.class.getDeclaredField("managedAndroidSession");
        field.setAccessible(true);
        field.set(helper, session);

        helper.closeDriver();

        Assert.assertTrue(released.get());
    }

    private SetupOptions managedOptions() throws IOException {
        var root = Files.createTempDirectory("managed-android-bootstrap-").toAbsolutePath();
        var paths = new ShaftCachePaths(root.resolve("cache"), root.resolve("data"),
                root.resolve("cache/downloads"), root.resolve("data/tools"),
                root.resolve("data/state"), root.resolve("data/receipts"));
        return SetupOptions.defaults(SetupProfile.MOBILE_ANDROID, paths)
                .withMode(SetupMode.MANAGED).withAutoStart(true);
    }

    private static AndroidSetupRequest request() {
        return new AndroidSetupRequest(36, "pixel_8", "google_apis", "x86_64",
                "shaft_pixel_8_api_36_x86_64", 4096, 2, 4723);
    }

    private record StubGateway(AtomicInteger calls, SetupPlan plan, ManagedEnvironment environment)
            implements ManagedAndroidBootstrap.Gateway {
        @Override public SetupPlan plan(SetupOptions options, AndroidSetupRequest request) {
            calls.incrementAndGet();
            return plan;
        }

        @Override public ManagedEnvironment start(SetupPlan plan, SetupApproval approval,
                SetupOptions options, AndroidSetupRequest request) {
            calls.incrementAndGet();
            return environment;
        }
    }
}
