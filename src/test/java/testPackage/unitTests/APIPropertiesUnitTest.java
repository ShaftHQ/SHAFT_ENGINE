package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Unit tests for API properties defaults, fluent overrides, and thread-local isolation.
 */
public class APIPropertiesUnitTest {
    private static final int THREAD_JOIN_TIMEOUT_MS = 5000;

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        Properties.clearForCurrentThread();
    }

    @Test(description = "Validate API property defaults are accessible")
    public void testApiPropertiesDefaults() {
        assertFalse(SHAFT.Properties.api.swaggerValidationEnabled());
        assertEquals(SHAFT.Properties.api.swaggerValidationUrl(), "");
    }

    @Test(description = "Validate API fluent setters update values and support chaining")
    public void testApiPropertySetterChainingAndReads() {
        SHAFT.Properties.api.set()
                .swaggerValidationEnabled(true)
                .swaggerValidationUrl("https://example.org/swagger.json");

        assertTrue(SHAFT.Properties.api.swaggerValidationEnabled());
        assertEquals(SHAFT.Properties.api.swaggerValidationUrl(), "https://example.org/swagger.json");
    }

    @Test(description = "Validate API properties are isolated per thread")
    public void testApiPropertiesThreadIsolation() throws InterruptedException {
        String defaultSwaggerUrl = SHAFT.Properties.api.swaggerValidationUrl();
        CountDownLatch threadASet = new CountDownLatch(1);
        CountDownLatch threadBRead = new CountDownLatch(1);
        AtomicReference<String> threadBObserved = new AtomicReference<>();

        Thread threadA = Thread.ofPlatform().unstarted(() -> {
            try {
                SHAFT.Properties.api.set().swaggerValidationUrl("https://thread-a/swagger.json");
                threadASet.countDown();
                threadBRead.await();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            } finally {
                Properties.clearForCurrentThread();
            }
        });

        Thread threadB = Thread.ofPlatform().unstarted(() -> {
            try {
                threadASet.await();
                threadBObserved.set(SHAFT.Properties.api.swaggerValidationUrl());
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            } finally {
                threadBRead.countDown();
                Properties.clearForCurrentThread();
            }
        });

        threadA.start();
        threadB.start();
        threadA.join(THREAD_JOIN_TIMEOUT_MS);
        threadB.join(THREAD_JOIN_TIMEOUT_MS);
        if (threadA.isAlive() || threadB.isAlive()) {
            threadA.interrupt();
            threadB.interrupt();
            throw new IllegalStateException("Timed out waiting for API properties thread isolation test to complete.");
        }

        assertEquals(threadBObserved.get(), defaultSwaggerUrl);
    }

    @Test(description = "Validate clearForCurrentThread restores API defaults")
    public void testClearForCurrentThreadRestoresApiDefaults() {
        boolean defaultEnabled = SHAFT.Properties.api.swaggerValidationEnabled();
        String defaultSwaggerUrl = SHAFT.Properties.api.swaggerValidationUrl();

        SHAFT.Properties.api.set()
                .swaggerValidationEnabled(!defaultEnabled)
                .swaggerValidationUrl("https://temp/swagger.json");
        assertEquals(SHAFT.Properties.api.swaggerValidationEnabled(), !defaultEnabled);
        assertEquals(SHAFT.Properties.api.swaggerValidationUrl(), "https://temp/swagger.json");

        Properties.clearForCurrentThread();

        assertEquals(SHAFT.Properties.api.swaggerValidationEnabled(), defaultEnabled);
        assertEquals(SHAFT.Properties.api.swaggerValidationUrl(), defaultSwaggerUrl);
    }

    private void assertEquals(Object actual, Object expected) {
        SHAFT.Validations.assertThat().object(actual).isEqualTo(expected).perform();
    }

    private void assertTrue(boolean actual) {
        SHAFT.Validations.assertThat().object(actual).isTrue().perform();
    }

    private void assertFalse(boolean actual) {
        SHAFT.Validations.assertThat().object(actual).isFalse().perform();
    }
}
