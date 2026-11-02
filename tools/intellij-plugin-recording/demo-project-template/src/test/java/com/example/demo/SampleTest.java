package com.example.demo;

import org.testng.annotations.Test;

import static org.testng.Assert.assertTrue;

/**
 * Trivial passing test that exists only as scaffolding for the SHAFT
 * IntelliJ plugin capture demo (see
 * tools/intellij-plugin-recording/video-capture-demo.md). It proves the
 * project compiles and runs under TestNG with selenium-java on the
 * classpath, ready for the plugin's codegen to add real Selenium code
 * during the recorded capture.
 */
public class SampleTest {

    @Test
    public void trivialAssertionPasses() {
        assertTrue(true);
    }
}
