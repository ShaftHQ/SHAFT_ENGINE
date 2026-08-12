package com.shaft.gui.mobile;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.FailureTraceReporter;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.InteractsWithApps;
import io.appium.java_client.remote.SupportsContextSwitching;
import io.appium.java_client.remote.SupportsRotation;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.ScreenOrientation;
import org.openqa.selenium.remote.SessionId;

import java.io.ByteArrayOutputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

/** Collects bounded, current-context mobile evidence components without changing session state. */
final class MobileEvidenceCollector {
    private static final String NATIVE_APP = "NATIVE_APP";
    private static final byte[] PNG_SIGNATURE = new byte[]{
            (byte) 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a};

    private MobileEvidenceCollector() {
        throw new IllegalStateException("Utility class");
    }

    static Capture collect(AppiumDriver driver, long maxBytes) {
        Objects.requireNonNull(driver, "driver");
        if (maxBytes < 1) {
            throw new IllegalArgumentException("maxBytes must be positive");
        }
        SessionId initialSessionId = driver.getSessionId();
        if (initialSessionId == null) {
            throw new UnsupportedOperationException("Mobile evidence capture requires a live Appium session.");
        }

        String contextBefore = currentContext(driver);
        Map<String, String> applicationMetadata = applicationMetadata(driver);
        Map<String, String> deviceMetadata = deviceMetadata(driver);
        Map<String, String> omissions = new LinkedHashMap<>();
        captureApplicationState(driver, applicationMetadata, omissions);

        byte[] screenshot = captureScreenshot(driver, maxBytes, omissions);
        String sourceKind = NATIVE_APP.equalsIgnoreCase(contextBefore)
                ? "native-accessibility-source"
                : knownContext(contextBefore) ? "page-source" : "current-context-source";
        long remainingBytes = maxBytes - (screenshot == null ? 0 : screenshot.length);
        byte[] source = captureSource(driver, contextBefore, remainingBytes, omissions);

        String contextAfter = currentContext(driver);
        if (!contextBefore.equals(contextAfter)) {
            if (screenshot != null) {
                screenshot = null;
                omissions.put("screenshot", "changed-during-capture");
            }
            if (source != null) {
                source = null;
                omissions.put("source", "changed-during-capture");
            }
        }
        SessionId finalSessionId = driver.getSessionId();
        if (!initialSessionId.equals(finalSessionId)) {
            throw new UnsupportedOperationException("Mobile evidence capture requires one live Appium session.");
        }
        return new Capture(contextBefore, applicationMetadata, deviceMetadata, screenshot, source,
                sourceKind, omissions);
    }

    private static byte[] captureScreenshot(AppiumDriver driver, long maxBytes,
                                            Map<String, String> omissions) {
        if (!SHAFT.Properties.reporting.traceIncludeScreenshots()
                || FailureTraceReporter.shouldOmitSensitiveBrowserEvidence()) {
            omissions.put("screenshot", "sensitive");
            return null;
        }
        try {
            byte[] screenshot = driver.getScreenshotAs(OutputType.BYTES);
            if (screenshot == null || screenshot.length < PNG_SIGNATURE.length
                    || !Arrays.equals(PNG_SIGNATURE, Arrays.copyOf(screenshot, PNG_SIGNATURE.length))) {
                omissions.put("screenshot", screenshot == null || screenshot.length == 0
                        ? "empty"
                        : "provider-failed");
                return null;
            }
            if (screenshot.length > maxBytes) {
                omissions.put("screenshot", "oversized");
                return null;
            }
            return Arrays.copyOf(screenshot, screenshot.length);
        } catch (RuntimeException providerFailure) {
            FailureTraceReporter.registerSensitiveThrowable(providerFailure);
            omissions.put("screenshot", "provider-failed");
            return null;
        }
    }

    private static byte[] captureSource(AppiumDriver driver, String context, long maxBytes,
                                        Map<String, String> omissions) {
        if (!knownContext(context)) {
            omissions.put("source", "unsupported");
            return null;
        }
        boolean nativeContext = NATIVE_APP.equalsIgnoreCase(context);
        boolean included = nativeContext
                ? SHAFT.Properties.reporting.traceIncludeNativePageSource()
                : SHAFT.Properties.reporting.traceIncludeDomSnapshots();
        if (!included || FailureTraceReporter.shouldOmitSensitiveBrowserEvidence()) {
            omissions.put("source", "sensitive");
            return null;
        }
        if (maxBytes < 1) {
            omissions.put("source", "oversized");
            return null;
        }
        try {
            String source = driver.getPageSource();
            if (source == null || source.isBlank()) {
                omissions.put("source", "empty");
                return null;
            }
            if (source.length() > maxBytes) {
                omissions.put("source", "oversized");
                return null;
            }
            byte[] redacted = boundedUtf8(FailureTraceReporter.redactInvocationText(source), maxBytes);
            if (redacted == null) {
                omissions.put("source", "oversized");
                return null;
            }
            return redacted;
        } catch (RuntimeException providerFailure) {
            FailureTraceReporter.registerSensitiveThrowable(providerFailure);
            omissions.put("source", "provider-failed");
            return null;
        }
    }

    private static Map<String, String> applicationMetadata(AppiumDriver driver) {
        Map<String, String> metadata = new LinkedHashMap<>();
        Capabilities capabilities = capabilities(driver);
        putCapability(metadata, "appPackage", capabilities, "appium:appPackage", "appPackage");
        putCapability(metadata, "appActivity", capabilities, "appium:appActivity", "appActivity");
        putCapability(metadata, "bundleId", capabilities, "appium:bundleId", "bundleId");
        metadata.values().forEach(MobileEvidenceCollector::registerSensitive);
        return metadata;
    }

    private static Map<String, String> deviceMetadata(AppiumDriver driver) {
        Map<String, String> metadata = new LinkedHashMap<>();
        Capabilities capabilities = capabilities(driver);
        putCapability(metadata, "platformName", capabilities, "platformName");
        if (!metadata.containsKey("platformName") && capabilities != null) {
            try {
                if (capabilities.getPlatformName() != null) {
                    metadata.put("platformName", capabilities.getPlatformName().name());
                }
            } catch (RuntimeException providerFailure) {
                FailureTraceReporter.registerSensitiveThrowable(providerFailure);
            }
        }
        if (metadata.containsKey("platformName")) {
            metadata.put("platformName", normalizePlatform(metadata.get("platformName")));
        }
        putCapability(metadata, "platformVersion", capabilities,
                "appium:platformVersion", "platformVersion");
        putCapability(metadata, "automationName", capabilities,
                "appium:automationName", "automationName");
        try {
            ScreenOrientation orientation = driver instanceof SupportsRotation provider
                    ? provider.getOrientation()
                    : null;
            if (orientation != null) {
                metadata.put("orientation", orientation.name());
            }
        } catch (RuntimeException providerFailure) {
            FailureTraceReporter.registerSensitiveThrowable(providerFailure);
        }
        try {
            Dimension size = driver.manage().window().getSize();
            if (size != null && size.getWidth() > 0 && size.getHeight() > 0) {
                metadata.put("windowSize", size.getWidth() + "x" + size.getHeight());
            }
        } catch (RuntimeException providerFailure) {
            FailureTraceReporter.registerSensitiveThrowable(providerFailure);
        }
        return metadata;
    }

    private static void captureApplicationState(AppiumDriver driver, Map<String, String> applicationMetadata,
                                                Map<String, String> omissions) {
        String applicationId = applicationMetadata.getOrDefault("appPackage",
                applicationMetadata.get("bundleId"));
        if (!(driver instanceof InteractsWithApps provider) || applicationId == null) {
            omissions.put("applicationState", "unsupported");
            return;
        }
        try {
            metadataValue(provider.queryAppState(applicationId)).ifPresentOrElse(
                    state -> applicationMetadata.put("applicationState", state),
                    () -> omissions.put("applicationState", "unsupported"));
        } catch (RuntimeException providerFailure) {
            FailureTraceReporter.registerSensitiveThrowable(providerFailure);
            omissions.put("applicationState", "provider-failed");
        }
    }

    private static Capabilities capabilities(AppiumDriver driver) {
        try {
            return driver.getCapabilities();
        } catch (RuntimeException providerFailure) {
            FailureTraceReporter.registerSensitiveThrowable(providerFailure);
            return null;
        }
    }

    private static void putCapability(Map<String, String> target, String targetKey,
                                      Capabilities capabilities, String... sourceKeys) {
        if (capabilities == null) {
            return;
        }
        for (String sourceKey : sourceKeys) {
            try {
                var value = metadataValue(capabilities.getCapability(sourceKey));
                if (value.isPresent() && !value.get().isBlank()) {
                    target.put(targetKey, value.get());
                    return;
                }
            } catch (RuntimeException providerFailure) {
                FailureTraceReporter.registerSensitiveThrowable(providerFailure);
                return;
            }
        }
    }

    private static java.util.Optional<String> metadataValue(Object value) {
        if (value instanceof String string) {
            return java.util.Optional.of(string.trim());
        }
        if (value instanceof Boolean) {
            return java.util.Optional.of(String.valueOf(value));
        }
        if (value instanceof Enum<?> enumeration) {
            return java.util.Optional.of(enumeration.name());
        }
        Class<?> type = value == null ? null : value.getClass();
        if (type == Byte.class || type == Short.class || type == Integer.class
                || type == Long.class || type == Float.class || type == Double.class
                || type == BigInteger.class || type == BigDecimal.class) {
            return java.util.Optional.of(value.toString());
        }
        return java.util.Optional.empty();
    }

    private static String currentContext(AppiumDriver driver) {
        if (!(driver instanceof SupportsContextSwitching provider)) {
            return "unavailable";
        }
        try {
            String context = provider.getContext();
            return context == null || context.isBlank() ? "unavailable" : context;
        } catch (RuntimeException providerFailure) {
            FailureTraceReporter.registerSensitiveThrowable(providerFailure);
            return "unavailable";
        }
    }

    private static boolean knownContext(String context) {
        return context != null && !context.equals("unavailable");
    }

    private static byte[] boundedUtf8(String value, long maxBytes) {
        if (value.length() > maxBytes) {
            return null;
        }
        var encoder = StandardCharsets.UTF_8.newEncoder()
                .onMalformedInput(CodingErrorAction.REPLACE)
                .onUnmappableCharacter(CodingErrorAction.REPLACE);
        CharBuffer input = CharBuffer.wrap(value);
        ByteBuffer buffer = ByteBuffer.allocate((int) Math.min(8192L, Math.max(1L, maxBytes)));
        ByteArrayOutputStream output = new ByteArrayOutputStream(Math.min(value.length(), 8192));
        while (true) {
            var result = encoder.encode(input, buffer, true);
            if (!drain(buffer, output, maxBytes)) {
                return null;
            }
            if (!result.isOverflow()) {
                break;
            }
        }
        while (true) {
            var result = encoder.flush(buffer);
            if (!drain(buffer, output, maxBytes)) {
                return null;
            }
            if (!result.isOverflow()) {
                break;
            }
        }
        return output.toByteArray();
    }

    private static boolean drain(ByteBuffer buffer, ByteArrayOutputStream output, long maxBytes) {
        buffer.flip();
        if ((long) output.size() + buffer.remaining() > maxBytes) {
            return false;
        }
        output.write(buffer.array(), buffer.position(), buffer.remaining());
        buffer.clear();
        return true;
    }

    private static String normalizePlatform(String platform) {
        if ("ANDROID".equalsIgnoreCase(platform)) {
            return "Android";
        }
        if ("IOS".equalsIgnoreCase(platform)) {
            return "iOS";
        }
        return platform;
    }

    private static void registerSensitive(String value) {
        FailureTraceReporter.registerSensitiveSourceValue(value);
        FailureTraceReporter.registerSensitiveValue(value);
    }

    record Capture(String context, Map<String, String> applicationMetadata,
                   Map<String, String> deviceMetadata, byte[] screenshot, byte[] source,
                   String sourceKind, Map<String, String> omissions) {
        Capture {
            context = Objects.requireNonNull(context, "context");
            applicationMetadata = immutableMap(applicationMetadata);
            deviceMetadata = immutableMap(deviceMetadata);
            screenshot = screenshot == null ? null : Arrays.copyOf(screenshot, screenshot.length);
            source = source == null ? null : Arrays.copyOf(source, source.length);
            sourceKind = Objects.requireNonNull(sourceKind, "sourceKind");
            omissions = immutableMap(omissions);
        }

        @Override
        public byte[] screenshot() {
            return screenshot == null ? null : Arrays.copyOf(screenshot, screenshot.length);
        }

        @Override
        public byte[] source() {
            return source == null ? null : Arrays.copyOf(source, source.length);
        }

        @Override
        public String toString() {
            return "Capture[applicationMetadata=" + applicationMetadata.size()
                    + ", deviceMetadata=" + deviceMetadata.size()
                    + ", screenshotBytes=" + (screenshot == null ? 0 : screenshot.length)
                    + ", sourceBytes=" + (source == null ? 0 : source.length)
                    + ", omissions=" + omissions.size() + "]";
        }

        private static Map<String, String> immutableMap(Map<String, String> values) {
            return values == null || values.isEmpty()
                    ? Map.of()
                    : Collections.unmodifiableMap(new LinkedHashMap<>(Map.copyOf(values)));
        }
    }
}
