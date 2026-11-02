package com.shaft.api;

import com.shaft.api.internal.OpenApiCoverageReporter;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.sun.net.httpserver.HttpServer;
import org.testng.Assert;
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Behavior contract for the SHAFT API response-status flag family (#6326):
 * {@code automaticallyAssertResponseStatusCode}, {@code setTargetStatusCode},
 * the three API timeouts, {@code swagger.validation.*} and {@code openapi.coverage.*}.
 *
 * <p>Every request goes to a local {@link HttpServer}, so the class needs no network.</p>
 */
@Test(singleThreaded = true)
public class ApiStatusCodeFlagFamilyUnitTest {
    private HttpServer server;
    private String baseUrl;

    @BeforeClass
    public void startServer() throws IOException {
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/status/", exchange -> {
            String path = exchange.getRequestURI().getPath();
            int code;
            try {
                code = Integer.parseInt(path.substring(path.lastIndexOf('/') + 1));
            } catch (NumberFormatException notAStatus) {
                code = 400;
            }
            byte[] body = "{}".getBytes();
            exchange.getResponseHeaders().add("Content-Type", "application/json");
            exchange.sendResponseHeaders(code, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        server.createContext("/slow", exchange -> {
            try {
                Thread.sleep(3_000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
            byte[] body = "{}".getBytes();
            exchange.sendResponseHeaders(200, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        server.setExecutor(java.util.concurrent.Executors.newCachedThreadPool());
        server.start();
        baseUrl = "http://127.0.0.1:" + server.getAddress().getPort() + "/";
    }

    @AfterClass(alwaysRun = true)
    public void stopServer() {
        if (server != null) {
            server.stop(0);
        }
    }

    @AfterMethod(alwaysRun = true)
    public void restoreDefaults() {
        SHAFT.Properties.flags.set().automaticallyAssertResponseStatusCode(true);
        Properties.clearForCurrentThread();
    }

    private SHAFT.API api() {
        return new SHAFT.API(baseUrl);
    }

    private static void autoAssert(boolean value) {
        SHAFT.Properties.flags.set().automaticallyAssertResponseStatusCode(value);
    }

    // ---------------------------------------------------------------- automaticallyAssertResponseStatusCode=true (default)

    @Test(description = "default flag: a 2xx response without a target passes")
    public void defaultFlagNoTargetPassesOn2xx() {
        api().get("status/204").perform();
    }

    @Test(description = "default flag: a 404 without a target fails and names the implicit 2xx expectation")
    public void defaultFlagNoTargetFailsOnNon2xxWithReadableMessage() {
        AssertionError error = Assert.expectThrows(AssertionError.class, () -> api().get("status/404").perform());
        Assert.assertTrue(error.getMessage().contains("Expected a 2xx status but found 404"), error.getMessage());
        Assert.assertFalse(error.getMessage().contains("Expected 0"), error.getMessage());
    }

    @Test(description = "default flag: an explicit target that matches passes, also for 4xx")
    public void defaultFlagExplicitTargetMatchPasses() {
        api().get("status/422").setTargetStatusCode(422).perform();
    }

    @Test(description = "default flag: an explicit target that does not match fails")
    public void defaultFlagExplicitTargetMismatchFails() {
        AssertionError error = Assert.expectThrows(AssertionError.class,
                () -> api().get("status/404").setTargetStatusCode(200).perform());
        Assert.assertTrue(error.getMessage().contains("Expected 200 but found 404"), error.getMessage());
    }

    // ---------------------------------------------------------------- automaticallyAssertResponseStatusCode=false (#6326)

    @Test(description = "flag off: a 404 without a target passes (implicit 2xx check disabled, as documented)")
    public void flagOffNoTargetPassesOnNon2xx() {
        autoAssert(false);
        api().get("status/404").perform();
    }

    @Test(description = "#6326 SC-001: flag off still asserts an explicit target (200 expected, 404 returned)")
    public void flagOffExplicitTargetMismatchStillFails() {
        autoAssert(false);
        AssertionError error = Assert.expectThrows(AssertionError.class,
                () -> api().get("status/404").setTargetStatusCode(200).perform());
        Assert.assertTrue(error.getMessage().contains("Expected 200 but found 404"), error.getMessage());
    }

    @Test(description = "#6326: flag off still asserts an explicit 4xx target (422 expected, 200 returned)")
    public void flagOffExplicitNegativeTargetMismatchStillFails() {
        autoAssert(false);
        AssertionError error = Assert.expectThrows(AssertionError.class,
                () -> api().get("status/200").setTargetStatusCode(422).perform());
        Assert.assertTrue(error.getMessage().contains("Expected 422 but found 200"), error.getMessage());
    }

    @Test(description = "flag off: an explicit target that matches passes")
    public void flagOffExplicitTargetMatchPasses() {
        autoAssert(false);
        api().get("status/404").setTargetStatusCode(404).perform();
    }

    @Test(description = "the flag is read when the request is performed, not when SHAFT.API is created (off after creation)")
    public void flagTurnedOffAfterSessionCreationIsHonored() {
        SHAFT.API session = api();
        autoAssert(false);
        session.get("status/404").perform();
    }

    @Test(description = "the flag is read when the request is performed, not when SHAFT.API is created (on after creation)")
    public void flagTurnedOnAfterSessionCreationIsHonored() {
        autoAssert(false);
        SHAFT.API session = api();
        autoAssert(true);
        Assert.expectThrows(AssertionError.class, () -> session.get("status/404").perform());
    }

    @Test(description = "repeated set() calls: the last value wins")
    public void repeatedFlagSetLastValueWins() {
        autoAssert(false);
        autoAssert(true);
        autoAssert(false);
        api().get("status/500").perform();
    }

    // ---------------------------------------------------------------- setTargetStatusCode values

    @DataProvider
    public Object[][] validTargets() {
        return new Object[][]{{100}, {200}, {422}, {599}, {999}};
    }

    @Test(dataProvider = "validTargets", description = "three-digit target status codes are accepted")
    public void validTargetStatusCodesAreAccepted(int code) {
        api().get("status/200").setTargetStatusCode(code);
    }

    @Test(description = "target 0 means 'no explicit target': the implicit 2xx check applies")
    public void zeroTargetMeansImplicitCheck() {
        api().get("status/201").setTargetStatusCode(0).perform();
        Assert.expectThrows(AssertionError.class, () -> api().get("status/404").setTargetStatusCode(0).perform());
    }

    @DataProvider
    public Object[][] invalidTargets() {
        return new Object[][]{{-1}, {1}, {99}, {1000}, {Integer.MAX_VALUE}, {Integer.MIN_VALUE}};
    }

    @Test(dataProvider = "invalidTargets", description = "invalid target status codes fail fast with a fix-next line")
    public void invalidTargetStatusCodesFailFast(int code) {
        IllegalArgumentException error = Assert.expectThrows(IllegalArgumentException.class,
                () -> api().get("status/200").setTargetStatusCode(code));
        Assert.assertTrue(error.getMessage().contains(String.valueOf(code)), error.getMessage());
        Assert.assertTrue(error.getMessage().contains("Fix:"), error.getMessage());
    }

    @Test(description = "repeated setTargetStatusCode calls: the last value wins")
    public void repeatedTargetLastValueWins() {
        api().get("status/404").setTargetStatusCode(200).setTargetStatusCode(404).perform();
    }

    // ---------------------------------------------------------------- API timeouts

    @Test(description = "default socket timeout (30s) lets a 3s response through")
    public void defaultSocketTimeoutAllowsSlowResponse() {
        api().get("slow").perform();
    }

    @Test(description = "a per-thread socket timeout set after SHAFT.API creation is honored")
    public void socketTimeoutSetAfterSessionCreationIsHonored() {
        SHAFT.API session = api();
        SHAFT.Properties.timeouts.set().apiSocketTimeout(1);
        Assert.expectThrows(Throwable.class, () -> session.get("slow").perform());
    }

    @Test(description = "another thread creating SHAFT.API does not overwrite this thread's socket timeout")
    public void socketTimeoutIsIsolatedPerThread() throws Exception {
        SHAFT.Properties.timeouts.set().apiSocketTimeout(1);
        SHAFT.API session = api();
        AtomicReference<Throwable> other = new AtomicReference<>();
        Thread thread = new Thread(() -> {
            try {
                new SHAFT.API(baseUrl); // default 30s on this thread
            } catch (Throwable t) {
                other.set(t);
            }
        });
        thread.start();
        thread.join();
        Assert.assertNull(other.get());
        Assert.expectThrows(Throwable.class, () -> session.get("slow").perform());
    }

    @Test(description = "timeout 0 means 'no timeout' and is accepted")
    public void zeroTimeoutsAreAccepted() {
        SHAFT.Properties.timeouts.set().apiSocketTimeout(0).apiConnectionTimeout(0).apiConnectionManagerTimeout(0);
        api().get("status/200").perform();
    }

    @DataProvider
    public Object[][] invalidTimeouts() {
        return new Object[][]{
                {"apiSocketTimeout", -1}, {"apiConnectionTimeout", -5}, {"apiConnectionManagerTimeout", -30},
                {"apiSocketTimeout", 2_147_484}, {"apiConnectionTimeout", Integer.MAX_VALUE},
                {"apiConnectionManagerTimeout", 3_000_000}};
    }

    @Test(dataProvider = "invalidTimeouts", description = "negative or overflowing API timeouts fail with a fix-next line")
    public void invalidTimeoutsFailFast(String key, int value) {
        var setter = SHAFT.Properties.timeouts.set();
        switch (key) {
            case "apiSocketTimeout" -> setter.apiSocketTimeout(value);
            case "apiConnectionTimeout" -> setter.apiConnectionTimeout(value);
            default -> setter.apiConnectionManagerTimeout(value);
        }
        IllegalArgumentException error = Assert.expectThrows(IllegalArgumentException.class,
                () -> api().get("status/200").perform());
        Assert.assertTrue(error.getMessage().contains(key), error.getMessage());
        Assert.assertTrue(error.getMessage().contains(String.valueOf(value)), error.getMessage());
        Assert.assertTrue(error.getMessage().contains("Fix:"), error.getMessage());
    }

    // ---------------------------------------------------------------- swagger.validation.*

    @Test(description = "swagger validation is off by default: no spec is needed")
    public void swaggerValidationDisabledByDefault() {
        Assert.assertFalse(SHAFT.Properties.api.swaggerValidationEnabled());
        api().get("status/200").perform();
    }

    @DataProvider
    public Object[][] blankUrls() {
        return new Object[][]{{""}, {"   "}, {"\t"}};
    }

    @Test(dataProvider = "blankUrls", description = "swagger validation enabled with a missing or blank URL fails with a fix-next line")
    public void swaggerValidationWithBlankUrlFailsWithFix(String url) {
        SHAFT.Properties.api.set().swaggerValidationEnabled(true).swaggerValidationUrl(url);
        Throwable error = Assert.expectThrows(Throwable.class, () -> api().get("status/200").perform());
        String message = String.valueOf(error.getMessage());
        Assert.assertTrue(message.contains("swagger.validation.url"), message);
        Assert.assertTrue(message.contains("Fix:"), message);
    }

    // ---------------------------------------------------------------- openapi.coverage.*

    @DataProvider
    public Object[][] invalidThresholds() {
        return new Object[][]{{-1}, {101}, {Integer.MIN_VALUE}, {Integer.MAX_VALUE}};
    }

    @Test(dataProvider = "invalidThresholds", description = "an out-of-range coverage threshold names the value and the fix")
    public void invalidCoverageThresholdFailsWithFix(int threshold) {
        IllegalArgumentException error = Assert.expectThrows(IllegalArgumentException.class,
                () -> OpenApiCoverageReporter.start("https://example.invalid/openapi.json", threshold));
        Assert.assertTrue(error.getMessage().contains(String.valueOf(threshold)), error.getMessage());
        Assert.assertTrue(error.getMessage().contains("openapi.coverage.threshold"), error.getMessage());
        Assert.assertTrue(error.getMessage().contains("Fix:"), error.getMessage());
    }

    @Test(dataProvider = "blankUrls", description = "coverage reporting without a spec URL names the property and the fix")
    public void coverageWithoutSpecUrlFailsWithFix(String url) {
        IllegalArgumentException error = Assert.expectThrows(IllegalArgumentException.class,
                () -> OpenApiCoverageReporter.start(url, 0));
        Assert.assertTrue(error.getMessage().contains("swagger.validation.url"), error.getMessage());
        Assert.assertTrue(error.getMessage().contains("Fix:"), error.getMessage());
    }
}
