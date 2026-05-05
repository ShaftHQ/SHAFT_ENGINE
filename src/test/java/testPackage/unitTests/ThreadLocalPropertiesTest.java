package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.RetryAnalyzer;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.PropertyFileManager;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.core.LoggerContext;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Unit tests to verify that SHAFT configuration properties are correctly
 * resolved through {@link ThreadLocalPropertiesManager} rather than
 * relying solely on {@code System.getProperties()}.
 */
public class ThreadLocalPropertiesTest {

    private static final long THREAD_JOIN_TIMEOUT_MS = 5000;

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        // Clear thread-local overrides after each test to avoid cross-test contamination
        Properties.clearForCurrentThread();
    }

    @Test(description = "getEffectiveProperties merges system and thread-local properties with thread-local winning")
    public void testGetEffectivePropertiesMergesCorrectly() {
        String key = "shaft.test.effective.merge";
        String threadLocalValue = "threadLocalWins";

        // Set via System property
        System.setProperty(key, "systemValue");
        try {
            // Set via thread-local (should override)
            ThreadLocalPropertiesManager.setProperty(key, threadLocalValue);

            java.util.Properties effective = ThreadLocalPropertiesManager.getEffectiveProperties();
            Assert.assertEquals(effective.getProperty(key), threadLocalValue,
                    "Thread-local override should take precedence over system property");
        } finally {
            System.clearProperty(key);
        }
    }

    @Test(description = "getEffectiveProperties returns system properties when no thread-local override exists")
    public void testGetEffectivePropertiesFallsBackToSystem() {
        String key = "shaft.test.effective.fallback";
        String systemValue = "fromSystem";

        System.setProperty(key, systemValue);
        try {
            java.util.Properties effective = ThreadLocalPropertiesManager.getEffectiveProperties();
            Assert.assertEquals(effective.getProperty(key), systemValue,
                    "Should fall back to system property when no thread-local override exists");
        } finally {
            System.clearProperty(key);
        }
    }

    @Test(description = "getAppiumDesiredCapabilities picks up thread-local mobile_ properties")
    public void testGetAppiumDesiredCapabilitiesReadsThreadLocal() {
        String testAppUrl = "https://example.com/test-thread-local.apk";

        // Set a mobile_ property via thread-local only (not in System properties)
        ThreadLocalPropertiesManager.setProperty("mobile_app", testAppUrl);

        Map<String, String> caps = PropertyFileManager.getAppiumDesiredCapabilities();
        Assert.assertEquals(caps.get("mobile_app"), testAppUrl,
                "getAppiumDesiredCapabilities should read mobile_ properties from thread-local overrides");
    }

    @Test(description = "getCustomWebDriverDesiredCapabilities picks up thread-local capabilities.* properties")
    public void testGetCustomWebDriverDesiredCapabilitiesReadsThreadLocal() {
        String capKey = "capabilities.goog:loggingPrefs";
        String capValue = "{\"browser\":\"ALL\"}";

        ThreadLocalPropertiesManager.setProperty(capKey, capValue);

        var caps = PropertyFileManager.getCustomWebDriverDesiredCapabilities();
        Assert.assertEquals(caps.getCapability("goog:loggingPrefs"), capValue,
                "getCustomWebDriverDesiredCapabilities should read capabilities.* from thread-local overrides");
    }

    @Test(description = "getCustomBrowserstackCapabilities picks up thread-local browserStack.* properties")
    public void testGetCustomBrowserstackCapabilitiesReadsThreadLocal() {
        String capKey = "browserStack.sessionName";
        String capValue = "ThreadLocalTest";

        ThreadLocalPropertiesManager.setProperty(capKey, capValue);

        var caps = PropertyFileManager.getCustomBrowserstackCapabilities();
        Assert.assertEquals(caps.get("sessionName"), capValue,
                "getCustomBrowserstackCapabilities should read browserStack.* from thread-local overrides");
    }

    @Test(description = "API swagger validation enabled property is accessible through SHAFT.Properties.api")
    public void testApiSwaggerValidationPropertyAccessible() {
        // Verify the default value is false
        boolean enabled = SHAFT.Properties.api.swaggerValidationEnabled();
        Assert.assertFalse(enabled, "swagger.validation.enabled should default to false");

        // Set via the SHAFT property API (thread-local)
        SHAFT.Properties.api.set().swaggerValidationEnabled(true);
        Assert.assertTrue(SHAFT.Properties.api.swaggerValidationEnabled(),
                "swagger.validation.enabled should be true after thread-local set");
    }

    @Test(description = "getProperty returns thread-local override when set")
    public void testGetPropertyReturnsThreadLocalOverride() {
        String key = "shaft.test.getProperty.override";
        String systemValue = "systemValue";
        String threadLocalValue = "threadLocalValue";

        System.setProperty(key, systemValue);
        try {
            ThreadLocalPropertiesManager.setProperty(key, threadLocalValue);
            Assert.assertEquals(ThreadLocalPropertiesManager.getProperty(key), threadLocalValue,
                    "getProperty should return thread-local override when set");
        } finally {
            System.clearProperty(key);
        }
    }

    @Test(description = "getProperty falls back to system property when no thread-local override exists")
    public void testGetPropertyFallsBackToSystem() {
        String key = "shaft.test.getProperty.fallback";
        String systemValue = "fromSystem";

        System.setProperty(key, systemValue);
        try {
            Assert.assertEquals(ThreadLocalPropertiesManager.getProperty(key), systemValue,
                    "getProperty should fall back to system property when no thread-local override exists");
        } finally {
            System.clearProperty(key);
        }
    }

    @Test(description = "getProperty returns null when neither thread-local nor system property is set")
    public void testGetPropertyReturnsNullWhenUnset() {
        String key = "shaft.test.getProperty.unset." + System.nanoTime();
        Assert.assertNull(ThreadLocalPropertiesManager.getProperty(key),
                "getProperty should return null when neither thread-local nor system property is set");
    }

    @Test(description = "getProperty thread-local override does not leak to other threads")
    public void testGetPropertyThreadIsolation() throws InterruptedException {
        String key = "shaft.test.getProperty.isolation";
        String threadLocalValue = "onlyForThisThread";

        ThreadLocalPropertiesManager.setProperty(key, threadLocalValue);

        // Verify current thread sees the override
        Assert.assertEquals(ThreadLocalPropertiesManager.getProperty(key), threadLocalValue,
                "Current thread should see its own thread-local override");

        // Verify another thread does NOT see the override
        final String[] otherThreadValue = {null};
        Thread otherThread = new Thread(() -> {
            otherThreadValue[0] = ThreadLocalPropertiesManager.getProperty(key);
        });
        otherThread.start();
        otherThread.join(THREAD_JOIN_TIMEOUT_MS);

        Assert.assertNull(otherThreadValue[0],
                "Other thread should NOT see the current thread's thread-local override");
    }

    @Test(description = "Flags configuration is engine-global even when set from another thread")
    public void testFlagsConfigurationIsSharedAcrossThreads() throws InterruptedException {
        int originalRetryCount = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        try {
            Thread otherThread = new Thread(() -> SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(4));
            otherThread.start();
            otherThread.join(THREAD_JOIN_TIMEOUT_MS);
            Assert.assertFalse(otherThread.isAlive(), "Flags configuration thread should finish within timeout");

            Assert.assertEquals(SHAFT.Properties.flags.retryMaximumNumberOfAttempts(), 4,
                    "retryMaximumNumberOfAttempts should be shared across threads");
        } finally {
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(originalRetryCount);
        }
    }

    @Test(description = "Global overrides should take precedence over system properties")
    public void testGlobalOverridesTakePrecedenceOverSystemProperties() {
        String key = "shaft.test.global.override";
        String systemValue = "fromSystem";
        String globalValue = "fromGlobal";

        System.setProperty(key, systemValue);
        try {
            ThreadLocalPropertiesManager.setGlobalProperty(key, globalValue);

            Assert.assertEquals(ThreadLocalPropertiesManager.getProperty(key), globalValue,
                    "Global override should take precedence over system property");
            Assert.assertEquals(ThreadLocalPropertiesManager.getEffectiveProperties().getProperty(key), globalValue,
                    "Effective properties should expose the global override value");
        } finally {
            System.clearProperty(key);
        }
    }

    @Test(description = "Flags setters should update SHAFT global properties without mutating JVM system properties")
    public void testFlagsSetterDoesNotMutateJvmSystemProperties() {
        int originalRetryCount = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        String originalSystemRetryValue = System.getProperty("retryMaximumNumberOfAttempts");
        int updatedRetryCount = originalRetryCount == 7 ? 6 : 7;

        try {
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(updatedRetryCount);
            Assert.assertEquals(SHAFT.Properties.flags.retryMaximumNumberOfAttempts(), updatedRetryCount,
                    "Flag setter should update the effective SHAFT flag value");
            Assert.assertEquals(System.getProperty("retryMaximumNumberOfAttempts"), originalSystemRetryValue,
                    "Flag setter should not mutate JVM system properties");
        } finally {
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(originalRetryCount);
        }
    }

    @Test(description = "Reporting logging remains enabled by default on fresh worker threads")
    public void testReportingDisableLoggingDefaultsToFalseOnFreshThread() throws InterruptedException {
        final boolean[] disableLoggingInWorkerThread = {true};

        Thread workerThread = new Thread(() -> {
            try {
                Properties.clearForCurrentThread();
                disableLoggingInWorkerThread[0] = SHAFT.Properties.reporting.disableLogging();
            } finally {
                Properties.clearForCurrentThread();
            }
        });
        workerThread.start();
        workerThread.join(THREAD_JOIN_TIMEOUT_MS);

        Assert.assertFalse(workerThread.isAlive(), "Worker thread should finish within timeout");
        Assert.assertFalse(disableLoggingInWorkerThread[0],
                "Fresh worker threads should inherit logging enabled by default");
    }

    @Test(description = "Failed-test retry enables debug file logging diagnostics")
    public void testRetryEnablesDebugFileLogging() throws java.io.IOException {
        int originalRetryCount = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        File logFile = new File(SHAFT.Properties.log4j.appenderFile_FileName());
        Level originalRootLogLevel = getRootLogLevel();
        if (logFile.exists()) {
            logFile.delete();
        }

        ITestNGMethod testMethod = mock(ITestNGMethod.class);
        when(testMethod.getMethodName()).thenReturn("retryLoggingTest");
        ITestResult testResult = mock(ITestResult.class);
        when(testResult.getMethod()).thenReturn(testMethod);

        try {
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
            Assert.assertTrue(new RetryAnalyzer().retry(testResult), "Retry should be scheduled for the first failure");
            Assert.assertTrue(logFile.isFile(), "Retry diagnostics should ensure the log file exists");
            Assert.assertTrue(logFile.length() > 0, "Retry diagnostics should write to the log file");
            Assert.assertEquals(getRootLogLevel(), Level.DEBUG,
                    "Retry diagnostics should temporarily push the root log level to debug");
            String retryLog = Files.readString(logFile.toPath(), StandardCharsets.UTF_8);
            Assert.assertTrue(retryLog.contains("[DEBUG"),
                    "Retry diagnostics should include at least one debug-level entry");
            ReportManagerHelper.attachEngineLog("retry-diagnostics-test");
            Assert.assertFalse(logFile.exists(), "Generated retry diagnostics log should be attached and removed");
            Assert.assertEquals(getRootLogLevel(), originalRootLogLevel,
                    "Retry diagnostics should restore the root log level after attaching logs");
        } finally {
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(originalRetryCount);
            if (logFile.exists()) {
                logFile.delete();
            }
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Retry diagnostics should reject a directory path as the debug log target")
    public void retryDiagnosticsShouldRejectDirectoryLogPath() throws Exception {
        String originalLogFilePath = SHAFT.Properties.log4j.appenderFile_FileName();
        Path nonFileLogPath = Files.createTempDirectory("shaft-debug-log-directory");

        Method ensureLogFileExists = ReportManagerHelper.class.getDeclaredMethod("ensureLogFileExists");
        ensureLogFileExists.setAccessible(true);

        try {
            ThreadLocalPropertiesManager.setProperty("appender.file.fileName", nonFileLogPath.toString());
            boolean isLogFileReady = (boolean) ensureLogFileExists.invoke(null);
            Assert.assertFalse(isLogFileReady,
                    "Retry diagnostics should reject non-regular log file paths");
        } finally {
            ThreadLocalPropertiesManager.setProperty("appender.file.fileName", originalLogFilePath);
            Files.deleteIfExists(nonFileLogPath);
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "Retry diagnostics debug log writer should preserve unicode content")
    public void retryDiagnosticsDebugLogWriterShouldPreserveUnicodeCharacters() throws Exception {
        String originalLogFilePath = SHAFT.Properties.log4j.appenderFile_FileName();
        Path tempDirectory = Files.createTempDirectory("shaft-debug-log-utf8");
        Path logFilePath = tempDirectory.resolve("retry-diagnostics.log");
        String unicodeLogEntry = "Retry diagnostics UTF-8 check - مرحبا 🌍";

        Method enableDebugFileLogging = ReportManagerHelper.class.getDeclaredMethod("enableDebugFileLogging");
        enableDebugFileLogging.setAccessible(true);
        Method writeToDebugLogFile = ReportManagerHelper.class
                .getDeclaredMethod("writeToDebugLogFile", String.class, Level.class);
        writeToDebugLogFile.setAccessible(true);
        Method resetRetryDiagnosticLogging = ReportManagerHelper.class
                .getDeclaredMethod("resetRetryDiagnosticLogging");
        resetRetryDiagnosticLogging.setAccessible(true);

        try {
            ThreadLocalPropertiesManager.setProperty("appender.file.fileName", logFilePath.toString());
            ReportManager.logDiscrete("Initialize logger for UTF-8 retry diagnostics test");
            enableDebugFileLogging.invoke(null);
            writeToDebugLogFile.invoke(null, unicodeLogEntry, Level.DEBUG);

            String writtenLog = Files.readString(logFilePath, StandardCharsets.UTF_8);
            Assert.assertTrue(writtenLog.contains(unicodeLogEntry),
                    "Retry debug log writer should preserve Unicode characters when written/read as UTF-8");
        } finally {
            resetRetryDiagnosticLogging.invoke(null);
            ThreadLocalPropertiesManager.setProperty("appender.file.fileName", originalLogFilePath);
            Files.deleteIfExists(logFilePath);
            Files.deleteIfExists(tempDirectory);
            Properties.clearForCurrentThread();
        }
    }

    private Level getRootLogLevel() {
        return ((LoggerContext) LogManager.getContext(false)).getConfiguration().getRootLogger().getLevel();
    }

    @Test(description = "Engine log attachment should collapse consecutive duplicate lines")
    public void testConsecutiveDuplicateLinesAreCollapsed() throws Exception {
        String duplicatedLog = String.join(System.lineSeparator(),
                "[INFO] action-one",
                "[INFO] action-one",
                "[DEBUG] action-one details",
                "[DEBUG] action-one details",
                "[INFO] action-two");

        Method deduplicateMethod = ReportManagerHelper.class
                .getDeclaredMethod("deduplicateConsecutiveLogLines", byte[].class);
        deduplicateMethod.setAccessible(true);

        byte[] processed = (byte[]) deduplicateMethod.invoke(null, duplicatedLog.getBytes(StandardCharsets.UTF_8));
        String processedLog = new String(processed, StandardCharsets.UTF_8);

        long actionOneInfoCount = processedLog.lines().filter(line -> line.equals("[INFO] action-one")).count();
        long actionOneDebugCount = processedLog.lines().filter(line -> line.equals("[DEBUG] action-one details")).count();
        Assert.assertEquals(actionOneInfoCount, 1,
                "Consecutive duplicate INFO lines should be collapsed before attachment");
        Assert.assertEquals(actionOneDebugCount, 1,
                "Consecutive duplicate DEBUG lines should be collapsed before attachment");
    }

    @Test(description = "Engine log attachment should preserve non-consecutive repeated lines")
    public void testNonConsecutiveRepeatedLinesArePreserved() throws Exception {
        String duplicatedLog = String.join(System.lineSeparator(),
                "[INFO] action-one",
                "[DEBUG] context",
                "[INFO] action-one");

        Method deduplicateMethod = ReportManagerHelper.class
                .getDeclaredMethod("deduplicateConsecutiveLogLines", byte[].class);
        deduplicateMethod.setAccessible(true);

        byte[] processed = (byte[]) deduplicateMethod.invoke(null, duplicatedLog.getBytes(StandardCharsets.UTF_8));
        String processedLog = new String(processed, StandardCharsets.UTF_8);

        long actionOneInfoCount = processedLog.lines().filter(line -> line.equals("[INFO] action-one")).count();
        Assert.assertEquals(actionOneInfoCount, 2,
                "Non-consecutive repeated INFO lines represent different events and should remain");
    }

    @Test(description = "Thread-local mobile_ properties do not leak to other threads")
    public void testMobilePropertiesThreadIsolation() throws InterruptedException {
        String testAppUrl = "https://example.com/thread-isolation-test.apk";

        // Set mobile_app in current thread's thread-local
        ThreadLocalPropertiesManager.setProperty("mobile_app", testAppUrl);

        // Verify current thread sees it
        Map<String, String> currentThreadCaps = PropertyFileManager.getAppiumDesiredCapabilities();
        Assert.assertEquals(currentThreadCaps.get("mobile_app"), testAppUrl,
                "Current thread should see its own thread-local mobile_app");

        // Verify another thread does NOT see it
        final Map<String, String>[] otherThreadCaps = new Map[]{null};
        Thread otherThread = new Thread(() -> {
            otherThreadCaps[0] = PropertyFileManager.getAppiumDesiredCapabilities();
        });
        otherThread.start();
        otherThread.join(THREAD_JOIN_TIMEOUT_MS);

        String otherThreadApp = otherThreadCaps[0] != null ? otherThreadCaps[0].get("mobile_app") : null;
        Assert.assertNotEquals(otherThreadApp, testAppUrl,
                "Other thread should NOT see the current thread's thread-local mobile_app override");
    }
}
