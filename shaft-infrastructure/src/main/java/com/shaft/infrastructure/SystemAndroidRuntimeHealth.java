package com.shaft.infrastructure;

import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Set;

final class SystemAndroidRuntimeHealth implements AndroidRuntimeHealth {
    private static final JsonMapper JSON = JsonMapper.builder().build();
    private final AndroidCommandRunner runner;
    private final HttpClient http;

    SystemAndroidRuntimeHealth(ShaftCachePaths paths, SetupPlatform platform, SetupArchitecture architecture) {
        this(AndroidCommandRunner.system(paths, platform, architecture),
                HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(2))
                        .followRedirects(HttpClient.Redirect.NEVER).build());
    }

    SystemAndroidRuntimeHealth(AndroidCommandRunner runner, HttpClient http) {
        this.runner = runner;
        this.http = http;
    }

    @Override
    public void awaitEmulator(String serial, AndroidRuntimeLayout layout, Map<String, String> environment,
                              Duration timeout) throws IOException {
        Instant deadline = Instant.now().plus(timeout);
        IOException last = new IOException("Android emulator did not become ready.");
        while (Instant.now().isBefore(deadline)) {
            try {
                requireOutput(layout, environment, serial, List.of("get-state"), "device");
                requireOutput(layout, environment, serial, List.of("shell", "getprop", "sys.boot_completed"), "1");
                requireContains(layout, environment, serial, List.of("shell", "pm", "path", "android"),
                        "package:");
                requireAvdIdentity(layout, environment, serial);
                return;
            } catch (IOException notReady) {
                last = notReady;
                pause(deadline);
            }
        }
        throw new IOException("Android emulator readiness timed out for " + serial
                + "; device, boot completion, package manager, and AVD identity are required.", last);
    }

    @Override
    public void awaitAppium(URI endpoint, Duration timeout) throws IOException {
        Instant deadline = Instant.now().plus(timeout);
        IOException last = new IOException("Appium did not become ready.");
        URI status = endpoint.resolve("status");
        while (Instant.now().isBefore(deadline)) {
            try {
                HttpRequest request = HttpRequest.newBuilder(status).timeout(Duration.ofSeconds(2)).GET().build();
                HttpResponse<String> response = http.send(request, HttpResponse.BodyHandlers.ofString());
                String version = JSON.readTree(response.body()).path("value").path("build").path("version").asText();
                if (response.statusCode() == 200 && AndroidSetupPlanner.APPIUM_VERSION.equals(version)) return;
                last = new IOException("Appium /status returned HTTP " + response.statusCode()
                        + " and version " + version + '.');
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                throw new IOException("Interrupted while waiting for Appium readiness.", interrupted);
            } catch (Exception notReady) {
                last = notReady instanceof IOException io ? io : new IOException(notReady);
            }
            pause(deadline);
        }
        throw new IOException("Appium readiness timed out at " + status + '.', last);
    }

    private void requireOutput(AndroidRuntimeLayout layout, Map<String, String> environment, String serial,
                               List<String> arguments, String expected) throws IOException {
        ReportingSetupService.ProcessResult result = adb(layout, environment, serial, arguments);
        if (result.exitCode() != 0 || !result.output().trim().equals(expected)) {
            throw new IOException("adb " + String.join(" ", arguments) + " is not ready.");
        }
    }

    private void requireContains(AndroidRuntimeLayout layout, Map<String, String> environment, String serial,
                                 List<String> arguments, String expected) throws IOException {
        ReportingSetupService.ProcessResult result = adb(layout, environment, serial, arguments);
        if (result.exitCode() != 0 || !result.output().contains(expected)) {
            throw new IOException("adb " + String.join(" ", arguments) + " is not ready.");
        }
    }

    private void requireAvdIdentity(AndroidRuntimeLayout layout, Map<String, String> environment,
                                    String serial) throws IOException {
        List<String> arguments = List.of("emu", "avd", "name");
        ReportingSetupService.ProcessResult result = adb(layout, environment, serial, arguments);
        List<String> response = result.output().lines().map(String::trim).filter(line -> !line.isEmpty()).toList();
        if (result.exitCode() != 0 || !response.equals(List.of(layout.avdName(), "OK"))) {
            throw new IOException("adb emu avd name did not confirm the reviewed AVD identity.");
        }
    }

    private ReportingSetupService.ProcessResult adb(AndroidRuntimeLayout layout, Map<String, String> environment,
                                                     String serial, List<String> arguments) throws IOException {
        java.util.ArrayList<String> command = new java.util.ArrayList<>(List.of(layout.adb().toString(),
                "-s", serial));
        command.addAll(arguments);
        return runner.run(command, layout.sdkRoot(), environment, Set.of("ANDROID_SERIAL"), null, null,
                Duration.ofSeconds(5));
    }

    private static void pause(Instant deadline) throws IOException {
        long millis = Math.min(500, Math.max(1, Duration.between(Instant.now(), deadline).toMillis()));
        try {
            Thread.sleep(millis);
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while waiting for Android runtime readiness.", interrupted);
        }
    }
}
