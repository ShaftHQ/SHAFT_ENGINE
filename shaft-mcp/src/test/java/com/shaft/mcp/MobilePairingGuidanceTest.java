package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;

class MobilePairingGuidanceTest {

    @Test
    void androidGuidanceCoversAdbReverseAndTheApi24CaRestriction() {
        List<String> guidance = MobilePairingGuidance.forPlatform("Android", 8080);

        assertTrue(guidance.stream().anyMatch(line -> line.contains("adb reverse tcp:8080 tcp:8080")));
        assertTrue(guidance.stream().anyMatch(line -> line.contains("Android 7+")));
    }

    @Test
    void iosGuidanceCoversTheSimulatorProxySetting() {
        List<String> guidance = MobilePairingGuidance.forPlatform("iOS", 8080);

        assertTrue(guidance.stream().anyMatch(line -> line.contains("127.0.0.1:8080")));
        assertTrue(guidance.stream().anyMatch(line -> line.contains("Certificate Trust Settings")));
    }

    @Test
    void everyPlatformGetsThePinningAndLoopbackAndWebviewLimitationWarnings() {
        for (String platform : List.of("Android", "iOS", "", "SomeOtherPlatform")) {
            List<String> guidance = MobilePairingGuidance.forPlatform(platform, 8080);

            assertTrue(guidance.stream().anyMatch(line -> line.contains("certificate pinning")),
                    "missing pinning warning for platform=" + platform);
            assertTrue(guidance.stream().anyMatch(line -> line.contains("127.0.0.1 only")),
                    "missing loopback-only warning for platform=" + platform);
            assertTrue(guidance.stream().anyMatch(line -> line.contains("mobile_get_contexts")),
                    "missing Tier-2 webview guidance for platform=" + platform);
        }
    }

    @Test
    void anUnrecognizedPlatformStillGetsAHelpfulHint() {
        List<String> guidance = MobilePairingGuidance.forPlatform("BlackBerry", 8080);

        assertTrue(guidance.stream().anyMatch(line -> line.contains("Unrecognized platform \"BlackBerry\"")));
    }
}
