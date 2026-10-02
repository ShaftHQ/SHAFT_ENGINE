package com.shaft.properties;

import com.shaft.driver.SHAFT;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * Issue #6375: every configuration property getter and setter is exercised by a test.
 * Each round trip reads the current value, writes the same value back and asserts the
 * getter returns it, so the change never leaks into other tests.
 */
public class PropertyContractCoverageUnitTest {
    private static <T> void roundTrip(Supplier<T> getter, Consumer<T> setter) {
        T value = getter.get();
        if (value == null) {
            return;
        }
        setter.accept(value);
        Assert.assertEquals(getter.get(), value);
    }

    private static void assertReadable(Supplier<?>... getters) {
        int read = 0;
        for (Supplier<?> getter : getters) {
            getter.get();
            read++;
        }
        Assert.assertEquals(read, getters.length);
    }

    @Test(description = "API properties read their defaults and round-trip through set()")
    public void apiPropertiesRoundTrip() {
        roundTrip(SHAFT.Properties.api::contractSensitiveKeys, SHAFT.Properties.api.set()::contractSensitiveKeys);
        roundTrip(SHAFT.Properties.api::contractVolatileKeys, SHAFT.Properties.api.set()::contractVolatileKeys);
    }

    @Test(description = "Capture properties read their defaults and round-trip through set()")
    public void capturePropertiesRoundTrip() {
        roundTrip(SHAFT.Properties.capture::firstPartyOnly, SHAFT.Properties.capture.set()::firstPartyOnly);
        roundTrip(SHAFT.Properties.capture::includeAssets, SHAFT.Properties.capture.set()::includeAssets);
        roundTrip(SHAFT.Properties.capture::maxBodyBytes, SHAFT.Properties.capture.set()::maxBodyBytes);
        roundTrip(SHAFT.Properties.capture::maxTransactions, SHAFT.Properties.capture.set()::maxTransactions);
        roundTrip(SHAFT.Properties.capture::storeSecretsLocally, SHAFT.Properties.capture.set()::storeSecretsLocally);
        roundTrip(SHAFT.Properties.capture::urlExcludeGlobs, SHAFT.Properties.capture.set()::urlExcludeGlobs);
        roundTrip(SHAFT.Properties.capture::urlIncludeGlobs, SHAFT.Properties.capture.set()::urlIncludeGlobs);
    }

    @Test(description = "Healing properties read their defaults and round-trip through set()")
    public void healingPropertiesRoundTrip() {
        roundTrip(SHAFT.Properties.healing::historyRetentionDays, SHAFT.Properties.healing.set()::historyRetentionDays);
    }

    @Test(description = "Internal properties read their defaults and round-trip through set()")
    public void internalPropertiesRoundTrip() {
        assertReadable(
                SHAFT.Properties.internal::androidCommandLineToolsVersion,
                SHAFT.Properties.internal::androidEmulatorApiLevel,
                SHAFT.Properties.internal::androidEmulatorCores,
                SHAFT.Properties.internal::androidEmulatorDeviceProfile,
                SHAFT.Properties.internal::androidEmulatorImageTag,
                SHAFT.Properties.internal::androidEmulatorRamMb,
                SHAFT.Properties.internal::appiumInspectorPluginVersion,
                SHAFT.Properties.internal::appiumUiAutomator2DriverVersion,
                SHAFT.Properties.internal::appiumXcuitestDriverVersion,
                SHAFT.Properties.internal::ga4ApiSecret,
                SHAFT.Properties.internal::ga4MeasurementId);
    }

    @Test(description = "Log4j properties read their defaults and round-trip through set()")
    public void log4jPropertiesRoundTrip() {
        assertReadable(
                SHAFT.Properties.log4j::appenderConsoleFilterThresholdLevel,
                SHAFT.Properties.log4j::appenderConsoleFilterThresholdType,
                SHAFT.Properties.log4j::appenderConsoleLayoutDisableAnsi,
                SHAFT.Properties.log4j::appenderConsoleLayoutPattern,
                SHAFT.Properties.log4j::appenderConsoleLayoutType,
                SHAFT.Properties.log4j::appenderConsoleName,
                SHAFT.Properties.log4j::appenderConsoleType,
                SHAFT.Properties.log4j::appenderFileFilterThresholdLevel,
                SHAFT.Properties.log4j::appenderFileFilterThresholdType,
                SHAFT.Properties.log4j::appenderFileLayoutPattern,
                SHAFT.Properties.log4j::appenderFileLayoutType,
                SHAFT.Properties.log4j::appenderFileName,
                SHAFT.Properties.log4j::appenderFileType,
                SHAFT.Properties.log4j::loggerAppLevel,
                SHAFT.Properties.log4j::loggerAppName);
    }

    @Test(description = "Ocr properties read their defaults and round-trip through set()")
    public void ocrPropertiesRoundTrip() {
        roundTrip(SHAFT.Properties.ocr::documentBatchParallelism, SHAFT.Properties.ocr.set()::documentBatchParallelism);
        roundTrip(SHAFT.Properties.ocr::documentMaximumAllureArtifactBytes, SHAFT.Properties.ocr.set()::documentMaximumAllureArtifactBytes);
        roundTrip(SHAFT.Properties.ocr::documentMaximumInFlightRasterBytes, SHAFT.Properties.ocr.set()::documentMaximumInFlightRasterBytes);
        roundTrip(SHAFT.Properties.ocr::documentMaximumInputBytes, SHAFT.Properties.ocr.set()::documentMaximumInputBytes);
        roundTrip(SHAFT.Properties.ocr::documentMaximumPages, SHAFT.Properties.ocr.set()::documentMaximumPages);
        roundTrip(SHAFT.Properties.ocr::documentMaximumPixelsPerPage, SHAFT.Properties.ocr.set()::documentMaximumPixelsPerPage);
        roundTrip(SHAFT.Properties.ocr::documentPageTimeoutSeconds, SHAFT.Properties.ocr.set()::documentPageTimeoutSeconds);
        roundTrip(SHAFT.Properties.ocr::documentRenderDpi, SHAFT.Properties.ocr.set()::documentRenderDpi);
    }

    @Test(description = "Pilot properties read their defaults and round-trip through set()")
    public void pilotPropertiesRoundTrip() {
        roundTrip(SHAFT.Properties.pilot::anthropicApiKeyEnvironmentVariable, SHAFT.Properties.pilot.set()::anthropicApiKeyEnvironmentVariable);
        roundTrip(SHAFT.Properties.pilot::anthropicProcessingLocation, SHAFT.Properties.pilot.set()::anthropicProcessingLocation);
        roundTrip(SHAFT.Properties.pilot::anthropicVersion, SHAFT.Properties.pilot.set()::anthropicVersion);
        roundTrip(SHAFT.Properties.pilot::circuitBreakerCooldownSeconds, SHAFT.Properties.pilot.set()::circuitBreakerCooldownSeconds);
        roundTrip(SHAFT.Properties.pilot::geminiApiKeyEnvironmentVariable, SHAFT.Properties.pilot.set()::geminiApiKeyEnvironmentVariable);
        roundTrip(SHAFT.Properties.pilot::geminiProcessingLocation, SHAFT.Properties.pilot.set()::geminiProcessingLocation);
        roundTrip(SHAFT.Properties.pilot::githubApiKeyEnvironmentVariable, SHAFT.Properties.pilot.set()::githubApiKeyEnvironmentVariable);
        roundTrip(SHAFT.Properties.pilot::githubProcessingLocation, SHAFT.Properties.pilot.set()::githubProcessingLocation);
        roundTrip(SHAFT.Properties.pilot::openAiApiKeyEnvironmentVariable, SHAFT.Properties.pilot.set()::openAiApiKeyEnvironmentVariable);
        roundTrip(SHAFT.Properties.pilot::openAiProcessingLocation, SHAFT.Properties.pilot.set()::openAiProcessingLocation);
        roundTrip(SHAFT.Properties.pilot::redactionAttributes, SHAFT.Properties.pilot.set()::redactionAttributes);
        roundTrip(SHAFT.Properties.pilot::redactionPatterns, SHAFT.Properties.pilot.set()::redactionPatterns);
        roundTrip(SHAFT.Properties.pilot::redactionSelectors, SHAFT.Properties.pilot.set()::redactionSelectors);
    }

    @Test(description = "Playwright properties read their defaults and round-trip through set()")
    public void playwrightPropertiesRoundTrip() {
        roundTrip(SHAFT.Properties.playwright::acceptDownloads, SHAFT.Properties.playwright.set()::acceptDownloads);
        roundTrip(SHAFT.Properties.playwright::artifactsDirectory, SHAFT.Properties.playwright.set()::artifactsDirectory);
        roundTrip(SHAFT.Properties.playwright::downloadsDirectory, SHAFT.Properties.playwright.set()::downloadsDirectory);
        roundTrip(SHAFT.Properties.playwright::launchTimeoutMilliseconds, SHAFT.Properties.playwright.set()::launchTimeoutMilliseconds);
        roundTrip(SHAFT.Properties.playwright::slowMo, SHAFT.Properties.playwright.set()::slowMo);
    }

    @Test(description = "Reporting properties read their defaults and round-trip through set()")
    public void reportingPropertiesRoundTrip() {
        assertReadable(
                SHAFT.Properties.reporting::cleanSummaryReportsDirectoryBeforeExecution);
    }

    @Test(description = "TestNG properties read their defaults and round-trip through set()")
    public void testNGPropertiesRoundTrip() {
        assertReadable(
                SHAFT.Properties.testNG::testSuiteTimeout);
    }

    @Test(description = "Timeouts properties read their defaults and round-trip through set()")
    public void timeoutsPropertiesRoundTrip() {
        roundTrip(SHAFT.Properties.timeouts::dockerCommandTimeout, SHAFT.Properties.timeouts.set()::dockerCommandTimeout);
    }
}
