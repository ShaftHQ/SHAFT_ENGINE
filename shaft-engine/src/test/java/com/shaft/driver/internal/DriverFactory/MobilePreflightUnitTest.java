package com.shaft.driver.internal.DriverFactory;

import org.openqa.selenium.SessionNotCreatedException;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.net.ConnectException;
import java.util.ArrayList;
import java.util.List;

public class MobilePreflightUnitTest {
    private static final String LOCAL = "http://localhost:4723";
    private static final MobilePreflight.StatusFetcher READY = uri -> new MobilePreflight.Response(200, "{\"value\":{\"ready\":true}}");

    @Test
    public void appiumNotReadyFailsFastWithPreflightMessage() {
        var e = Assert.expectThrows(SessionNotCreatedException.class, () -> MobilePreflight.check(LOCAL, "", "",
                uri -> new MobilePreflight.Response(200, "{\"value\":{\"ready\":false}}"), cmd -> "1"));
        Assert.assertTrue(e.getMessage().startsWith("Preflight: Appium is not ready"), e.getMessage());
    }

    @Test
    public void appiumUnreachableFailsFast() {
        var e = Assert.expectThrows(SessionNotCreatedException.class, () -> MobilePreflight.check(LOCAL, "", "",
                uri -> { throw new ConnectException("refused"); }, cmd -> "1"));
        Assert.assertTrue(e.getMessage().contains("not reachable"), e.getMessage());
    }

    @Test
    public void deviceNotBootedFailsFast() {
        var e = Assert.expectThrows(SessionNotCreatedException.class,
                () -> MobilePreflight.check(LOCAL, "emulator-5554", "", READY, cmd -> "0\n"));
        Assert.assertTrue(e.getMessage().contains("emulator-5554 has not finished booting"), e.getMessage());
    }

    @Test
    public void unresolvedHostFailsByPingHeaderNotExitCode() {
        var e = Assert.expectThrows(SessionNotCreatedException.class, () -> MobilePreflight.check(LOCAL, "", "api.example.gov", READY,
                cmd -> cmd[cmd.length - 1].startsWith("ping") ? "ping: unknown host api.example.gov" : "1"));
        Assert.assertTrue(e.getMessage().contains("cannot resolve api.example.gov"), e.getMessage());
    }

    @Test
    public void resolvedHostPassesEvenWhenIcmpReplyDrops() {
        List<String> commands = new ArrayList<>();
        MobilePreflight.check(LOCAL, "emulator-5554", "api.example.gov", READY, cmd -> {
            commands.add(String.join(" ", cmd));
            return cmd[cmd.length - 1].startsWith("ping")
                    ? "PING api.example.gov (10.0.0.5) 56(84) bytes of data.\n1 packets transmitted, 0 received" : "1";
        });
        Assert.assertEquals(commands.getFirst(), "adb -s emulator-5554 shell getprop sys.boot_completed");
    }

    @Test
    public void remoteAppiumSkipsAdbChecks() {
        MobilePreflight.check("https://hub.example.com/wd/hub", "", "api.example.gov", READY,
                cmd -> { throw new AssertionError("adb must not run for a remote Appium"); });
    }

    @Test
    public void noAdbOnPathSkipsDeviceChecks() {
        MobilePreflight.check(LOCAL, "", "api.example.gov", READY, cmd -> null);
    }

    @Test
    public void invalidHostNameIsRejected() {
        Assert.expectThrows(SessionNotCreatedException.class, () -> MobilePreflight.hosts("ok.example.com,bad;rm -rf"));
    }
}
