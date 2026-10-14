package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.List;

public class ReportPublishGuardTest {
    @Test
    public void decodedBase64SecretBlocksPublishWithoutEchoingTheSecret() throws Exception {
        String secret = "sk-abcdefghijklmnopqrst";
        String encoded = Base64.getEncoder().encodeToString(secret.getBytes(StandardCharsets.UTF_8));
        Path report = Files.createTempFile("shaft-report", ".html");
        report.toFile().deleteOnExit();
        Files.writeString(report, "<html><script>widget=" + encoded + "</script></html>");
        Assert.assertFalse(Files.readString(report).contains(secret));

        List<String> classes = ReportPublishGuard.prepare(report);

        String published = Files.readString(report);
        String blocked = Files.readString(report.resolveSibling("PUBLISH_BLOCKED.txt"));
        Assert.assertTrue(classes.contains("api-token"), classes.toString());
        Assert.assertFalse(published.contains(secret), published);
        Assert.assertFalse(blocked.contains(secret), blocked);
        Assert.assertTrue(blocked.contains("publish blocked"), blocked);
        Assert.assertTrue(blocked.contains("api-token"), blocked);
    }
}
