package com.shaft.driver.internal.DriverFactory;

import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.SessionNotCreatedException;

import java.io.IOException;
import java.net.ConnectException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

/**
 * Cheap checks before the first native mobile driver (#7291, #7284): Appium {@code /status} is ready,
 * the local device finished booting, and the device resolves each host in
 * {@code mobile_preflightResolveHosts}. A failure throws one clear {@code Preflight:} message, which
 * {@link com.shaft.listeners.internal.InfraFailureGuard} classifies as infrastructure (no retries).
 */
final class MobilePreflight {
    private static final Pattern READY_TRUE = Pattern.compile("\"ready\"\\s*:\\s*true");
    private static final Pattern READY_FALSE = Pattern.compile("\"ready\"\\s*:\\s*false");
    private static final Pattern SAFE_HOST = Pattern.compile("[A-Za-z0-9.-]{1,253}");

    /** Runs a command and returns its combined output, or empty when it cannot run. */
    interface CommandRunner {
        Optional<String> run(String... command);
    }

    /** Fetches a URL and returns the status code and body. */
    interface StatusFetcher {
        Response fetch(URI uri) throws IOException;
    }

    record Response(int statusCode, String body) {
    }

    private MobilePreflight() {
        throw new IllegalStateException("Utility class");
    }

    private static final java.util.Set<String> PASSED = java.util.concurrent.ConcurrentHashMap.newKeySet();

    static void beforeSession(String appiumUrl, String udid, String resolveHosts) {
        String key = appiumUrl + "|" + udid + "|" + resolveHosts;
        if (PASSED.contains(key)) {
            return;
        }
        check(appiumUrl, udid, resolveHosts, MobilePreflight::fetchStatus, MobilePreflight::runCommand);
        PASSED.add(key);
    }

    static void check(String appiumUrl, String udid, String resolveHosts, StatusFetcher fetcher, CommandRunner runner) {
        String endpoint = appiumUrl == null ? "" : appiumUrl.trim();
        if (endpoint.isEmpty()) {
            return;
        }
        URI statusUri = URI.create(endpoint.replaceAll("/+$", "") + "/status");
        checkAppiumStatus(statusUri, DriverFactoryHelper.redactUriCredentials(endpoint), fetcher);
        if (isLocal(statusUri)) {
            checkLocalDevice(udid, resolveHosts, runner);
        }
    }

    private static void checkAppiumStatus(URI statusUri, String redacted, StatusFetcher fetcher) {
        try {
            Response response = fetcher.fetch(statusUri);
            if (reportsNotReady(response)) {
                throw fail("Appium is not ready at " + redacted + " (/status reports ready=false).");
            }
        } catch (ConnectException e) {
            throw fail("Appium is not reachable at " + redacted + ": start Appium or fix executionAddress.");
        } catch (IOException | IllegalArgumentException e) {
            ReportManagerHelper.logDiscrete("Mobile preflight could not read Appium /status: " + e.getMessage(), Level.DEBUG);
        }
    }

    private static boolean reportsNotReady(Response response) {
        return response.statusCode() == 200 && READY_FALSE.matcher(response.body()).find()
                && !READY_TRUE.matcher(response.body()).find();
    }

    private static void checkLocalDevice(String udid, String resolveHosts, CommandRunner runner) {
        boolean hasUdid = udid != null && !udid.isBlank();
        List<String> adb = new ArrayList<>(List.of("adb"));
        if (hasUdid) {
            adb.addAll(List.of("-s", udid.trim()));
        }
        Optional<String> bootOutput = runner.run(with(adb, "shell", "getprop", "sys.boot_completed"));
        if (bootOutput.isEmpty()) {
            return; // no adb (iOS or a remote device): nothing more to check locally
        }
        String boot = bootOutput.get();
        if (!"1".equals(boot.trim())) {
            throw fail("device " + (hasUdid ? udid.trim() : "(default)")
                    + " has not finished booting (sys.boot_completed=" + boot.trim() + ").");
        }
        for (String host : hosts(resolveHosts)) {
            checkResolves(adb, host, runner);
        }
    }

    private static void checkResolves(List<String> adb, String host, CommandRunner runner) {
        Optional<String> ping = runner.run(with(adb, "shell", "ping -c1 -W1 " + host));
        // #7284: the PING header proves DNS resolution; the exit code does not (ICMP replies can drop).
        if (ping.isEmpty() || !resolved(ping.get(), host)) {
            throw fail("the device cannot resolve " + host + ". On an emulator, restart it with "
                    + "-dns-server <gateway> (see broken DNSSEC on validating resolvers).");
        }
    }

    static boolean resolved(String pingOutput, String host) {
        return pingOutput.lines().anyMatch(line -> line.startsWith("PING " + host + " ("));
    }

    static List<String> hosts(String resolveHosts) {
        List<String> hosts = new ArrayList<>();
        if (resolveHosts == null) {
            return hosts;
        }
        for (String host : resolveHosts.split(",", -1)) {
            String trimmed = host.trim();
            if (trimmed.isEmpty()) {
                continue;
            }
            if (!SAFE_HOST.matcher(trimmed).matches()) {
                throw fail("mobile_preflightResolveHosts holds an invalid host name: " + trimmed);
            }
            hosts.add(trimmed);
        }
        return hosts;
    }

    private static boolean isLocal(URI uri) {
        String host = uri.getHost() == null ? "" : uri.getHost().toLowerCase(Locale.ROOT);
        return host.equals("localhost") || host.equals("127.0.0.1") || host.equals("[::1]") || host.equals("::1");
    }

    private static String[] with(List<String> prefix, String... rest) {
        List<String> all = new ArrayList<>(prefix);
        all.addAll(List.of(rest));
        return all.toArray(String[]::new);
    }

    private static SessionNotCreatedException fail(String reason) {
        String message = "Preflight: " + reason;
        ReportManagerHelper.logDiscrete(message, Level.ERROR);
        return new SessionNotCreatedException(message);
    }

    private static Response fetchStatus(URI uri) throws IOException {
        HttpClient client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(5)).build();
        try {
            HttpResponse<String> response = client.send(HttpRequest.newBuilder(uri).timeout(Duration.ofSeconds(10)).GET().build(),
                    HttpResponse.BodyHandlers.ofString());
            return new Response(response.statusCode(), response.body());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while reading " + uri, e);
        }
    }

    private static Optional<String> runCommand(String... command) {
        try {
            Process process = new ProcessBuilder(command).redirectErrorStream(true).start();
            if (!process.waitFor(15, TimeUnit.SECONDS)) {
                process.destroyForcibly();
                return Optional.of("");
            }
            return Optional.of(new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8));
        } catch (IOException e) {
            return Optional.empty();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return Optional.empty();
        }
    }
}
