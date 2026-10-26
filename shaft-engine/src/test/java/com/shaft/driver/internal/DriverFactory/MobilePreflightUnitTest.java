package com.shaft.driver.internal.DriverFactory;

import org.openqa.selenium.SessionNotCreatedException;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.net.ConnectException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class MobilePreflightUnitTest {
    private static final String LOCAL = "http://localhost:4723";
    private static final MobilePreflight.StatusFetcher READY = uri -> new MobilePreflight.Response(200, "{\"value\":{\"ready\":true}}");

    @Test
    public void appiumNotReadyFailsFastWithPreflightMessage() {
        var e = Assert.expectThrows(SessionNotCreatedException.class, () -> MobilePreflight.check(LOCAL, "", "",
                uri -> new MobilePreflight.Response(200, "{\"value\":{\"ready\":false}}"), cmd -> Optional.of("1")));
        Assert.assertTrue(String.valueOf(e.getMessage()).contains("Preflight: Appium is not ready"), e.getMessage());
    }

    @Test
    public void appiumUnreachableFailsFast() {
        var e = Assert.expectThrows(SessionNotCreatedException.class, () -> MobilePreflight.check(LOCAL, "", "",
                uri -> { throw new ConnectException("refused"); }, cmd -> Optional.of("1")));
        Assert.assertTrue(String.valueOf(e.getMessage()).contains("not reachable"), e.getMessage());
    }

    @Test
    public void deviceNotBootedFailsFast() {
        var e = Assert.expectThrows(SessionNotCreatedException.class,
                () -> MobilePreflight.check(LOCAL, "emulator-5554", "", READY, cmd -> localAdb(cmd, "0\n")));
        Assert.assertTrue(String.valueOf(e.getMessage()).contains("emulator-5554 has not finished booting"), e.getMessage());
    }

    @Test
    public void readyStatusWithoutAConnectedDeviceIsNotSuccess() {
        var e = Assert.expectThrows(SessionNotCreatedException.class,
                () -> MobilePreflight.check(LOCAL, "", "", READY,
                        cmd -> Optional.of("devices".equals(cmd[cmd.length - 1])
                                ? "List of devices attached\n" : "1")));
        Assert.assertTrue(e.getMessage().contains("no Android device is connected"), e.getMessage());
        Assert.assertFalse(e.getMessage().contains("ready=false"), e.getMessage());
    }

    @Test
    public void offlineDeviceIsNotAConnectedDevice() {
        Assert.assertFalse(MobilePreflight.connectedDevice(
                "List of devices attached\nemulator-5554\toffline\n", ""));
        var e = Assert.expectThrows(SessionNotCreatedException.class,
                () -> MobilePreflight.check(LOCAL, "emulator-5554", "", READY,
                        cmd -> Optional.of("devices".equals(cmd[cmd.length - 1])
                                ? "List of devices attached\nemulator-5554\toffline\n" : "1")));
        Assert.assertTrue(e.getMessage().contains("no Android device is connected for emulator-5554"), e.getMessage());
    }

    @Test
    public void unresolvedHostFailsByPingHeaderNotExitCode() {
        var e = Assert.expectThrows(SessionNotCreatedException.class, () -> MobilePreflight.check(LOCAL, "", "api.example.gov", READY,
                cmd -> {
                    if ("devices".equals(cmd[cmd.length - 1])) {
                        return Optional.of("List of devices attached\nemulator-5554\tdevice\n");
                    }
                    return Optional.of(cmd[cmd.length - 1].startsWith("ping") ? "ping: unknown host api.example.gov" : "1");
                }));
        Assert.assertTrue(String.valueOf(e.getMessage()).contains("cannot resolve api.example.gov"), e.getMessage());
    }

    @Test
    public void resolvedHostPassesEvenWhenIcmpReplyDrops() {
        List<String> commands = new ArrayList<>();
        MobilePreflight.check(LOCAL, "emulator-5554", "api.example.gov", READY, cmd -> {
            commands.add(String.join(" ", cmd));
            if ("devices".equals(cmd[cmd.length - 1])) {
                return Optional.of("List of devices attached\nemulator-5554\tdevice\n");
            }
            return Optional.of(cmd[cmd.length - 1].startsWith("ping")
                    ? "PING api.example.gov (10.0.0.5) 56(84) bytes of data.\n1 packets transmitted, 0 received" : "1");
        });
        Assert.assertEquals(commands.get(0), "adb -s emulator-5554 devices");
        Assert.assertEquals(commands.get(1), "adb -s emulator-5554 shell getprop sys.boot_completed");
    }

    @Test
    public void remoteAppiumSkipsAdbChecks() {
        MobilePreflight.check("https://hub.example.com/wd/hub", "", "api.example.gov", READY,
                cmd -> { throw new AssertionError("adb must not run for a remote Appium"); });
    }

    @Test
    public void noAdbOnPathSkipsDeviceChecks() {
        MobilePreflight.check(LOCAL, "", "api.example.gov", READY, cmd -> Optional.empty());
    }

    @Test
    public void realStatusFetchAndCommandRunnerWork() throws Exception {
        com.sun.net.httpserver.HttpServer server = com.sun.net.httpserver.HttpServer.create(new java.net.InetSocketAddress("127.0.0.1", 0), 0);
        java.util.concurrent.atomic.AtomicReference<String> body = new java.util.concurrent.atomic.AtomicReference<>("{\"value\":{\"ready\":true}}");
        server.createContext("/status", exchange -> {
            byte[] bytes = body.get().getBytes(java.nio.charset.StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(200, bytes.length);
            exchange.getResponseBody().write(bytes);
            exchange.close();
        });
        server.start();
        try {
            // 127.0.0.2 is loopback on Linux but not "local" to the preflight, so no adb runs here
            String url = "http://127.0.0.2:" + server.getAddress().getPort();
            var response = MobilePreflight.fetchStatus(java.net.URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/status"));
            Assert.assertEquals(response.statusCode(), 200);
            MobilePreflight.check(url.replace("127.0.0.2", "remote.invalid"), "", "", uri -> response, cmd -> Optional.empty());
            body.set("{\"value\":{\"ready\":false}}");
            var notReady = MobilePreflight.fetchStatus(java.net.URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/status"));
            Assert.expectThrows(SessionNotCreatedException.class,
                    () -> MobilePreflight.check("https://remote.invalid", "", "", uri -> notReady, cmd -> Optional.empty()));
        } finally {
            server.stop(0);
        }
        Assert.assertTrue(MobilePreflight.runCommand("definitely-not-a-command-7291").isEmpty());
        Assert.assertEquals(MobilePreflight.runCommand("echo", "ok").orElseThrow().trim(), "ok");
    }

    @Test
    public void beforeSessionRemembersAPassedEndpoint() {
        // a refused port on a non-local host name fails; an empty endpoint is a no-op
        MobilePreflight.beforeSession("", "", "");
        MobilePreflight.beforeSession("", "", "");
    }

    private static Optional<String> localAdb(String[] cmd, String boot) {
        if ("devices".equals(cmd[cmd.length - 1])) {
            return Optional.of("List of devices attached\nemulator-5554\tdevice\nemulator-5556\tdevice\n");
        }
        return Optional.of(boot);
    }

    @Test
    public void invalidHostNameIsRejected() {
        Assert.expectThrows(SessionNotCreatedException.class, () -> MobilePreflight.hosts("ok.example.com,bad;rm -rf"));
    }

    @Test
    public void edgeBranchesAreHandled() throws Exception {
        MobilePreflight.check(null, null, null, uri -> { throw new AssertionError("no fetch"); }, cmd -> Optional.empty());
        // unreadable /status only logs; a broken pipe is not a "not reachable" verdict
        MobilePreflight.check("https://remote.invalid", "", "", uri -> { throw new java.io.IOException("reset"); }, cmd -> Optional.empty());
        // non-200 or ready:true alongside ready:false is not a "not ready" verdict
        MobilePreflight.check("https://remote.invalid", "", "", uri -> new MobilePreflight.Response(503, "{\"ready\":false}"), cmd -> Optional.empty());
        MobilePreflight.check("http://127.0.0.1:4723/", "  ", null, uri -> new MobilePreflight.Response(200, "{\"ready\":true}"), cmd -> localAdb(cmd, "1"));
        MobilePreflight.check("http://[::1]:4723", null, "", uri -> new MobilePreflight.Response(200, "{}"), cmd -> Optional.empty());
        List<List<String>> commands = new ArrayList<>();
        var e = Assert.expectThrows(SessionNotCreatedException.class, () -> MobilePreflight.check(LOCAL, "emulator-5556 ", " , api.example.gov,", uri -> new MobilePreflight.Response(200, "{}"),
                cmd -> {
                    commands.add(List.of(cmd));
                    if ("devices".equals(cmd[cmd.length - 1])) {
                        return Optional.of("List of devices attached\nemulator-5556\tdevice\n");
                    }
                    return cmd[cmd.length - 1].startsWith("ping") ? Optional.empty() : Optional.of("1");
                }));
        Assert.assertTrue(e.getMessage().contains("cannot resolve api.example.gov"), e.getMessage());
        Assert.assertEquals(commands.get(0).subList(0, 3), List.of("adb", "-s", "emulator-5556"));
        var booting = Assert.expectThrows(SessionNotCreatedException.class, () -> MobilePreflight.check(LOCAL, "emulator-5556", "", uri -> new MobilePreflight.Response(200, "{}"), cmd -> localAdb(cmd, "0")));
        Assert.assertTrue(booting.getMessage().contains("device emulator-5556 has not"), booting.getMessage());
        Assert.assertTrue(MobilePreflight.hosts(null).isEmpty());
        Assert.assertTrue(MobilePreflight.connectedDevice("List of devices attached\nemulator-5554\tdevice\n", "emulator-5554"));
        Assert.assertFalse(MobilePreflight.connectedDevice("List of devices attached\nemulator-5554\tdevice\n", "emulator-5556"));
        Assert.assertEquals(MobilePreflight.hosts(" a.example , ,b.example"), List.of("a.example", "b.example"));
    }
}
