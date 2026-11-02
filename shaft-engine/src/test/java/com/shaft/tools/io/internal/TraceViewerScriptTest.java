package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;

/** Runs the trace viewer's JavaScript unit tests ({@code src/test/js}) with node's built-in test runner. */
public class TraceViewerScriptTest {
    private static final Path SCRIPT_TEST = Path.of("src/test/js/trace-viewer-core.test.js");

    @Test
    public void viewerCoreScriptUnitTestsShouldPass() throws Exception {
        Assert.assertTrue(Files.isRegularFile(SCRIPT_TEST), "Missing " + SCRIPT_TEST.toAbsolutePath());
        Process process = new ProcessBuilder("node", "--test", SCRIPT_TEST.toString())
                .redirectErrorStream(true)
                .start();
        String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        Assert.assertTrue(process.waitFor(60, TimeUnit.SECONDS), "node --test timed out: " + output);
        Assert.assertEquals(process.exitValue(), 0, "Trace viewer script tests failed:\n" + output);
    }
}
