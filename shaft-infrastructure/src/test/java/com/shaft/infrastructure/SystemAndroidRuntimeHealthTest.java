package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.net.http.HttpClient;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SystemAndroidRuntimeHealthTest {
    @Test
    void emulatorConsoleAvdNameProvesExactOwnedIdentity(@TempDir Path temp) {
        RecordingRunner runner = new RecordingRunner("shaft_android\nOK\n");
        SystemAndroidRuntimeHealth health = new SystemAndroidRuntimeHealth(runner, HttpClient.newHttpClient());

        assertDoesNotThrow(() -> health.awaitEmulator("emulator-5554", layout(temp), Map.of(),
                Duration.ofMillis(100)));

        assertTrue(runner.commands.stream().anyMatch(command -> command.containsAll(
                List.of("-s", "emulator-5554", "emu", "avd", "name"))));
    }

    @Test
    void differentEmulatorConsoleAvdNameIsNeverAccepted(@TempDir Path temp) {
        SystemAndroidRuntimeHealth health = new SystemAndroidRuntimeHealth(
                new RecordingRunner("some_other_avd\nOK\n"), HttpClient.newHttpClient());

        assertThrows(java.io.IOException.class, () -> health.awaitEmulator("emulator-5554", layout(temp),
                Map.of(), Duration.ofMillis(10)));
    }

    private static AndroidRuntimeLayout layout(Path root) {
        return new AndroidRuntimeLayout(root.resolve("node"), root.resolve("appium/index.js"),
                root.resolve("appium"), root.resolve("sdk"), root.resolve("sdk/adb"),
                root.resolve("sdk/emulator"), root.resolve("avd"), root.resolve("avd/shaft_android.avd"),
                root.resolve("logs/emulator.log"), root.resolve("logs/appium.log"),
                "shaft_android", "emulator-5554");
    }

    private static final class RecordingRunner implements AndroidCommandRunner {
        private final String avdNameOutput;
        private final java.util.ArrayList<List<String>> commands = new java.util.ArrayList<>();

        private RecordingRunner(String avdNameOutput) {
            this.avdNameOutput = avdNameOutput;
        }

        @Override
        public ReportingSetupService.ProcessResult run(List<String> command, Path workingDirectory,
                                                       Map<String, String> environment, java.util.Set<String> removed,
                                                       String input, Path log, Duration timeout) {
            commands.add(List.copyOf(command));
            String output = command.contains("get-state") ? "device"
                    : command.contains("sys.boot_completed") ? "1"
                    : command.contains("pm") ? "package:/system/framework/framework-res.apk"
                    : command.contains("emu") ? avdNameOutput : "";
            return new ReportingSetupService.ProcessResult(0, output);
        }
    }
}
