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
        roundTrip(SHAFT.Properties.capture::enabled, SHAFT.Properties.capture.set()::enabled);
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
        roundTrip(SHAFT.Properties.healing::ambiguityMargin, SHAFT.Properties.healing.set()::ambiguityMargin);
        roundTrip(SHAFT.Properties.healing::aiEnabled, SHAFT.Properties.healing.set()::aiEnabled);
        roundTrip(SHAFT.Properties.healing::aiTrigger, SHAFT.Properties.healing.set()::aiTrigger);
        roundTrip(SHAFT.Properties.healing::evidenceCategories, SHAFT.Properties.healing.set()::evidenceCategories);
        roundTrip(SHAFT.Properties.healing::historyEnabled, SHAFT.Properties.healing.set()::historyEnabled);
        roundTrip(SHAFT.Properties.healing::historyMaxEntries, SHAFT.Properties.healing.set()::historyMaxEntries);
        roundTrip(SHAFT.Properties.healing::historyPath, SHAFT.Properties.healing.set()::historyPath);
        roundTrip(SHAFT.Properties.healing::historyRetentionDays, SHAFT.Properties.healing.set()::historyRetentionDays);
        roundTrip(SHAFT.Properties.healing::ladderBudgetSeconds, SHAFT.Properties.healing.set()::ladderBudgetSeconds);
        roundTrip(SHAFT.Properties.healing::minimumConfidence, SHAFT.Properties.healing.set()::minimumConfidence);
        roundTrip(SHAFT.Properties.healing::minimumTrustPercentage, SHAFT.Properties.healing.set()::minimumTrustPercentage);
        roundTrip(SHAFT.Properties.healing::sourcePatchEnabled, SHAFT.Properties.healing.set()::sourcePatchEnabled);
        roundTrip(SHAFT.Properties.healing::strategy, SHAFT.Properties.healing.set()::strategy);
        roundTrip(SHAFT.Properties.healing::testIdAttributes, SHAFT.Properties.healing.set()::testIdAttributes);
        roundTrip(SHAFT.Properties.healing::visualEnabled, SHAFT.Properties.healing.set()::visualEnabled);
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
        roundTrip(SHAFT.Properties.pilot::allowedEvidenceCategories, SHAFT.Properties.pilot.set()::allowedEvidenceCategories);
        roundTrip(SHAFT.Properties.pilot::anthropicApiKeyEnvironmentVariable, SHAFT.Properties.pilot.set()::anthropicApiKeyEnvironmentVariable);
        roundTrip(SHAFT.Properties.pilot::anthropicEndpoint, SHAFT.Properties.pilot.set()::anthropicEndpoint);
        roundTrip(SHAFT.Properties.pilot::anthropicModel, SHAFT.Properties.pilot.set()::anthropicModel);
        roundTrip(SHAFT.Properties.pilot::anthropicProcessingLocation, SHAFT.Properties.pilot.set()::anthropicProcessingLocation);
        roundTrip(SHAFT.Properties.pilot::anthropicVersion, SHAFT.Properties.pilot.set()::anthropicVersion);
        roundTrip(SHAFT.Properties.pilot::circuitBreakerCooldownSeconds, SHAFT.Properties.pilot.set()::circuitBreakerCooldownSeconds);
        roundTrip(SHAFT.Properties.pilot::circuitBreakerFailureThreshold, SHAFT.Properties.pilot.set()::circuitBreakerFailureThreshold);
        roundTrip(SHAFT.Properties.pilot::enabled, SHAFT.Properties.pilot.set()::enabled);
        roundTrip(SHAFT.Properties.pilot::geminiApiKeyEnvironmentVariable, SHAFT.Properties.pilot.set()::geminiApiKeyEnvironmentVariable);
        roundTrip(SHAFT.Properties.pilot::geminiEndpoint, SHAFT.Properties.pilot.set()::geminiEndpoint);
        roundTrip(SHAFT.Properties.pilot::geminiModel, SHAFT.Properties.pilot.set()::geminiModel);
        roundTrip(SHAFT.Properties.pilot::geminiProcessingLocation, SHAFT.Properties.pilot.set()::geminiProcessingLocation);
        roundTrip(SHAFT.Properties.pilot::githubApiKeyEnvironmentVariable, SHAFT.Properties.pilot.set()::githubApiKeyEnvironmentVariable);
        roundTrip(SHAFT.Properties.pilot::githubEndpoint, SHAFT.Properties.pilot.set()::githubEndpoint);
        roundTrip(SHAFT.Properties.pilot::githubModel, SHAFT.Properties.pilot.set()::githubModel);
        roundTrip(SHAFT.Properties.pilot::githubProcessingLocation, SHAFT.Properties.pilot.set()::githubProcessingLocation);
        roundTrip(SHAFT.Properties.pilot::lmStudioApiKeyEnvironmentVariable, SHAFT.Properties.pilot.set()::lmStudioApiKeyEnvironmentVariable);
        roundTrip(SHAFT.Properties.pilot::lmStudioEndpoint, SHAFT.Properties.pilot.set()::lmStudioEndpoint);
        roundTrip(SHAFT.Properties.pilot::lmStudioModel, SHAFT.Properties.pilot.set()::lmStudioModel);
        roundTrip(SHAFT.Properties.pilot::localConsent, SHAFT.Properties.pilot.set()::localConsent);
        roundTrip(SHAFT.Properties.pilot::maxConcurrency, SHAFT.Properties.pilot.set()::maxConcurrency);
        roundTrip(SHAFT.Properties.pilot::maxCostUsd, SHAFT.Properties.pilot.set()::maxCostUsd);
        roundTrip(SHAFT.Properties.pilot::maxInputTokens, SHAFT.Properties.pilot.set()::maxInputTokens);
        roundTrip(SHAFT.Properties.pilot::maxOutputTokens, SHAFT.Properties.pilot.set()::maxOutputTokens);
        roundTrip(SHAFT.Properties.pilot::maxRequestBytes, SHAFT.Properties.pilot.set()::maxRequestBytes);
        roundTrip(SHAFT.Properties.pilot::ollamaApiKeyEnvironmentVariable, SHAFT.Properties.pilot.set()::ollamaApiKeyEnvironmentVariable);
        roundTrip(SHAFT.Properties.pilot::ollamaApiKeyHeader, SHAFT.Properties.pilot.set()::ollamaApiKeyHeader);
        roundTrip(SHAFT.Properties.pilot::ollamaApiKeyPrefix, SHAFT.Properties.pilot.set()::ollamaApiKeyPrefix);
        roundTrip(SHAFT.Properties.pilot::ollamaEndpoint, SHAFT.Properties.pilot.set()::ollamaEndpoint);
        roundTrip(SHAFT.Properties.pilot::ollamaModel, SHAFT.Properties.pilot.set()::ollamaModel);
        roundTrip(SHAFT.Properties.pilot::ollamaProcessingLocation, SHAFT.Properties.pilot.set()::ollamaProcessingLocation);
        roundTrip(SHAFT.Properties.pilot::onPremConsent, SHAFT.Properties.pilot.set()::onPremConsent);
        roundTrip(SHAFT.Properties.pilot::openAiApiKeyEnvironmentVariable, SHAFT.Properties.pilot.set()::openAiApiKeyEnvironmentVariable);
        roundTrip(SHAFT.Properties.pilot::openAiEndpoint, SHAFT.Properties.pilot.set()::openAiEndpoint);
        roundTrip(SHAFT.Properties.pilot::openAiModel, SHAFT.Properties.pilot.set()::openAiModel);
        roundTrip(SHAFT.Properties.pilot::openAiProcessingLocation, SHAFT.Properties.pilot.set()::openAiProcessingLocation);
        roundTrip(SHAFT.Properties.pilot::provider, SHAFT.Properties.pilot.set()::provider);
        roundTrip(SHAFT.Properties.pilot::redactionAttributes, SHAFT.Properties.pilot.set()::redactionAttributes);
        roundTrip(SHAFT.Properties.pilot::redactionPatterns, SHAFT.Properties.pilot.set()::redactionPatterns);
        roundTrip(SHAFT.Properties.pilot::redactionSelectors, SHAFT.Properties.pilot.set()::redactionSelectors);
        roundTrip(SHAFT.Properties.pilot::remoteConsent, SHAFT.Properties.pilot.set()::remoteConsent);
        roundTrip(SHAFT.Properties.pilot::retryMaxAttempts, SHAFT.Properties.pilot.set()::retryMaxAttempts);
        roundTrip(SHAFT.Properties.pilot::telemetryEnabled, SHAFT.Properties.pilot.set()::telemetryEnabled);
        roundTrip(SHAFT.Properties.pilot::timeoutSeconds, SHAFT.Properties.pilot.set()::timeoutSeconds);
    }

    @Test(description = "Playwright properties read their defaults and round-trip through set()")
    public void playwrightPropertiesRoundTrip() {
        roundTrip(SHAFT.Properties.playwright::acceptDownloads, SHAFT.Properties.playwright.set()::acceptDownloads);
        roundTrip(SHAFT.Properties.playwright::browserName, SHAFT.Properties.playwright.set()::browserName);
        roundTrip(SHAFT.Properties.playwright::connectionMode, SHAFT.Properties.playwright.set()::connectionMode);
        roundTrip(SHAFT.Properties.playwright::artifactsDirectory, SHAFT.Properties.playwright.set()::artifactsDirectory);
        roundTrip(SHAFT.Properties.playwright::channel, SHAFT.Properties.playwright.set()::channel);
        roundTrip(SHAFT.Properties.playwright::defaultTimeoutMilliseconds, SHAFT.Properties.playwright.set()::defaultTimeoutMilliseconds);
        roundTrip(SHAFT.Properties.playwright::deviceName, SHAFT.Properties.playwright.set()::deviceName);
        roundTrip(SHAFT.Properties.playwright::endpoint, SHAFT.Properties.playwright.set()::endpoint);
        roundTrip(SHAFT.Properties.playwright::downloadsDirectory, SHAFT.Properties.playwright.set()::downloadsDirectory);
        roundTrip(SHAFT.Properties.playwright::javaScriptEnabled, SHAFT.Properties.playwright.set()::javaScriptEnabled);
        roundTrip(SHAFT.Properties.playwright::launchTimeoutMilliseconds, SHAFT.Properties.playwright.set()::launchTimeoutMilliseconds);
        roundTrip(SHAFT.Properties.playwright::locale, SHAFT.Properties.playwright.set()::locale);
        roundTrip(SHAFT.Properties.playwright::navigationTimeoutMilliseconds, SHAFT.Properties.playwright.set()::navigationTimeoutMilliseconds);
        roundTrip(SHAFT.Properties.playwright::slowMo, SHAFT.Properties.playwright.set()::slowMo);
        roundTrip(SHAFT.Properties.playwright::timezoneId, SHAFT.Properties.playwright.set()::timezoneId);
        roundTrip(SHAFT.Properties.playwright::tracingEnabled, SHAFT.Properties.playwright.set()::tracingEnabled);
        roundTrip(SHAFT.Properties.playwright::tracingOnRetryOnly, SHAFT.Properties.playwright.set()::tracingOnRetryOnly);
        roundTrip(SHAFT.Properties.playwright::tracingScreenshots, SHAFT.Properties.playwright.set()::tracingScreenshots);
        roundTrip(SHAFT.Properties.playwright::tracingSnapshots, SHAFT.Properties.playwright.set()::tracingSnapshots);
        roundTrip(SHAFT.Properties.playwright::tracingSources, SHAFT.Properties.playwright.set()::tracingSources);
        roundTrip(SHAFT.Properties.playwright::userAgent, SHAFT.Properties.playwright.set()::userAgent);
    }

    @Test(description = "Path properties read their defaults and round-trip through set()")
    public void pathPropertiesRoundTrip() {
        roundTrip(SHAFT.Properties.paths::aiAgentWorkspaceRoot, SHAFT.Properties.paths.set()::aiAgentWorkspaceRoot);
        roundTrip(SHAFT.Properties.paths::allureResults, SHAFT.Properties.paths.set()::allureResults);
        roundTrip(SHAFT.Properties.paths::applitoolsApiKey, SHAFT.Properties.paths.set()::applitoolsApiKey);
        roundTrip(SHAFT.Properties.paths::ariaSnapshot, SHAFT.Properties.paths.set()::ariaSnapshot);
        roundTrip(SHAFT.Properties.paths::authCache, SHAFT.Properties.paths.set()::authCache);
        roundTrip(SHAFT.Properties.paths::downloads, SHAFT.Properties.paths.set()::downloads);
        roundTrip(SHAFT.Properties.paths::dynamicObjectRepository, SHAFT.Properties.paths.set()::dynamicObjectRepository);
        roundTrip(SHAFT.Properties.paths::executionSummaryReport, SHAFT.Properties.paths.set()::executionSummaryReport);
        roundTrip(SHAFT.Properties.paths::extentReports, SHAFT.Properties.paths.set()::extentReports);
        roundTrip(SHAFT.Properties.paths::mobileSessionCache, SHAFT.Properties.paths.set()::mobileSessionCache);
        roundTrip(SHAFT.Properties.paths::services, SHAFT.Properties.paths.set()::services);
        roundTrip(SHAFT.Properties.paths::testData, SHAFT.Properties.paths.set()::testData);
        roundTrip(SHAFT.Properties.paths::video, SHAFT.Properties.paths.set()::video);
    }

    @Test(description = "Web properties read their defaults and round-trip through set()")
    public void webPropertiesRoundTrip() {
        roundTrip(SHAFT.Properties.web::baseURL, SHAFT.Properties.web.set()::baseURL);
        roundTrip(SHAFT.Properties.web::browserConsoleErrorAllowlist, SHAFT.Properties.web.set()::browserConsoleErrorAllowlist);
        roundTrip(SHAFT.Properties.web::browserWindowHeight, SHAFT.Properties.web.set()::browserWindowHeight);
        roundTrip(SHAFT.Properties.web::browserWindowWidth, SHAFT.Properties.web.set()::browserWindowWidth);
        roundTrip(SHAFT.Properties.web::failOnBrowserConsoleErrors, SHAFT.Properties.web.set()::failOnBrowserConsoleErrors);
        roundTrip(SHAFT.Properties.web::forceBrowserDownload, SHAFT.Properties.web.set()::forceBrowserDownload);
        roundTrip(SHAFT.Properties.web::headlessExecution, SHAFT.Properties.web.set()::headlessExecution);
        roundTrip(SHAFT.Properties.web::incognitoMode, SHAFT.Properties.web.set()::incognitoMode);
        roundTrip(SHAFT.Properties.web::isMobileEmulation, SHAFT.Properties.web.set()::isMobileEmulation);
        roundTrip(SHAFT.Properties.web::mobileEmulationDeviceName, SHAFT.Properties.web.set()::mobileEmulationDeviceName);
        roundTrip(SHAFT.Properties.web::mobileEmulationIsCustomDevice, SHAFT.Properties.web.set()::mobileEmulationIsCustomDevice);
        roundTrip(SHAFT.Properties.web::mobileEmulationPixelRatio, SHAFT.Properties.web.set()::mobileEmulationPixelRatio);
        roundTrip(SHAFT.Properties.web::mobileEmulationUserAgent, SHAFT.Properties.web.set()::mobileEmulationUserAgent);
        roundTrip(SHAFT.Properties.web::pageLoadStrategy, SHAFT.Properties.web.set()::pageLoadStrategy);
        roundTrip(SHAFT.Properties.web::readinessState, SHAFT.Properties.web.set()::readinessState);
        roundTrip(SHAFT.Properties.web::storageStatePath, SHAFT.Properties.web.set()::storageStatePath);
        roundTrip(SHAFT.Properties.web::targetBrowserName, SHAFT.Properties.web.set()::targetBrowserName);
    }

    @Test(description = "Reporting properties read their defaults and round-trip through set()")
    public void reportingPropertiesRoundTrip() {
        roundTrip(SHAFT.Properties.reporting::whenToAttachSourceCode, SHAFT.Properties.reporting.set()::whenToAttachSourceCode);
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
