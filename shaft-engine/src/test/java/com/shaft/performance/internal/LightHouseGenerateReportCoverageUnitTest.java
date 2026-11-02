package com.shaft.performance.internal;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.net.MalformedURLException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class LightHouseGenerateReportCoverageUnitTest {

    @Test
    public void managedCommandKeepsTheUrlAsOneArgumentAndUsesNoGeneratedScript() {
        var runtime = new com.shaft.infrastructure.LighthouseRuntime(
                List.of("C:/shaft/node.exe", "C:/shaft/lighthouse/index.js"), Path.of("C:/shaft/cache"));
        Path output = Path.of("C:/work/lighthouse reports/report.html");

        List<String> command = LightHouseGenerateReport.command(runtime,
                "https://example.com/search?q=shaft&lang=en", 9999, output);

        Assert.assertEquals(command, List.of("C:/shaft/node.exe", "C:/shaft/lighthouse/index.js",
                "https://example.com/search?q=shaft&lang=en", "--port=9999", "--preset=desktop",
                "--output=html", "--output-path=" + output.toAbsolutePath().normalize(),
                "--only-categories=performance"));
        Assert.assertFalse(command.stream().anyMatch(value -> value.endsWith("GenerateLHScript.js")));
    }

    @Test
    public void debuggerAddressCapabilityWinsOverTheConfiguredFallbackPort() {
        RemoteWebDriver driver = mock(RemoteWebDriver.class);
        Capabilities capabilities = mock(Capabilities.class);
        when(driver.getCapabilities()).thenReturn(capabilities);
        when(capabilities.getCapability("goog:chromeOptions"))
                .thenReturn(Map.of("debuggerAddress", "127.0.0.1:9222"));

        Assert.assertEquals(LightHouseGenerateReport.debuggerPort(driver, 8888), 9222);
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        SHAFT.Properties.performance.set().isEnabled(false);
        SHAFT.Properties.reporting.set().openLighthouseReportWhileExecution(false);
        Properties.clearForCurrentThread();
    }

    @Test
    public void generateLightHouseReportShouldSkipExecutionWhenFeatureIsDisabled() {
        WebDriver driver = mock(WebDriver.class);
        when(driver.getCurrentUrl()).thenReturn("https://example.com/home");
        LightHouseGenerateReport reportGenerator = new LightHouseGenerateReport(driver,
                options -> { throw new AssertionError("Disabled Lighthouse must not resolve a runtime."); },
                (command, directory, timeout) -> { throw new AssertionError("Disabled Lighthouse must not run."); },
                Path.of("lighthouse-reports"));

        SHAFT.Properties.performance.set().isEnabled(false);
        reportGenerator.generateLightHouseReport();
    }

    @Test
    public void generateLightHouseReportUsesManagedRuntimeAndProducesTheReviewedReport() throws Exception {
        WebDriver driver = mock(WebDriver.class);
        when(driver.getCurrentUrl()).thenReturn("https://example.com/search?q=shaft&lang=en");
        Path reportDirectory = Files.createTempDirectory("shaft-lighthouse-test-");
        var runtime = new com.shaft.infrastructure.LighthouseRuntime(
                List.of("C:/shaft/node.exe", "C:/shaft/lighthouse/index.js"), reportDirectory);
        AtomicReference<List<String>> invoked = new AtomicReference<>();
        LightHouseGenerateReport reportGenerator = new LightHouseGenerateReport(driver, options -> runtime,
                (command, workingDirectory, timeout) -> {
                    invoked.set(command);
                    Path output = Path.of(command.stream().filter(value -> value.startsWith("--output-path="))
                            .findFirst().orElseThrow().substring("--output-path=".length()));
                    Files.writeString(output, "<html>verified report</html>");
                    return new LightHouseGenerateReport.CommandResult(0, "ok");
                }, reportDirectory);
        SHAFT.Properties.performance.set().isEnabled(true);
        SHAFT.Properties.performance.set().port(9999);
        SHAFT.Properties.reporting.set().openLighthouseReportWhileExecution(false);

        reportGenerator.generateLightHouseReport();

        Assert.assertNotNull(invoked.get());
        Assert.assertEquals(invoked.get().get(2), "https://example.com/search?q=shaft&lang=en");
        Assert.assertTrue(invoked.get().contains("--port=9999"));
        Assert.assertFalse(Files.exists(reportDirectory.resolve("GenerateLHScript.js")));
        Assert.assertFalse(Files.exists(reportDirectory.resolve("OpenLHReport.js")));
        Assert.assertEquals(Files.list(reportDirectory).filter(path -> path.toString().endsWith(".html")).count(), 1L);
    }

    @Test
    public void nonzeroExitAndMissingOutputFailInsteadOfReportingSuccess() throws Exception {
        WebDriver driver = mock(WebDriver.class);
        when(driver.getCurrentUrl()).thenReturn("https://example.com");
        Path reportDirectory = Files.createTempDirectory("shaft-lighthouse-failure-");
        var runtime = new com.shaft.infrastructure.LighthouseRuntime(List.of("node", "lighthouse"), reportDirectory);
        SHAFT.Properties.performance.set().isEnabled(true);

        var nonzero = new LightHouseGenerateReport(driver, options -> runtime,
                (command, directory, timeout) -> new LightHouseGenerateReport.CommandResult(7, "boom"),
                reportDirectory);
        IllegalStateException exitFailure = Assert.expectThrows(IllegalStateException.class,
                nonzero::generateLightHouseReport);
        Assert.assertTrue(exitFailure.getMessage().contains("code 7"));

        var missing = new LightHouseGenerateReport(driver, options -> runtime,
                (command, directory, timeout) -> new LightHouseGenerateReport.CommandResult(0, "ok"),
                reportDirectory);
        IllegalStateException missingFailure = Assert.expectThrows(IllegalStateException.class,
                missing::generateLightHouseReport);
        Assert.assertTrue(missingFailure.getMessage().contains("without creating"));
    }

    @Test
    public void preexistingReportIsNeverAcceptedAsFreshSuccess() throws Exception {
        WebDriver driver = mock(WebDriver.class);
        when(driver.getCurrentUrl()).thenReturn("https://example.com");
        Path reportDirectory = Files.createTempDirectory("shaft-lighthouse-stale-");
        Files.writeString(reportDirectory.resolve("fixed.html"), "<html>old</html>");
        var runtime = new com.shaft.infrastructure.LighthouseRuntime(List.of("node", "lighthouse"), reportDirectory);
        SHAFT.Properties.performance.set().isEnabled(true);
        LightHouseGenerateReport generator = new LightHouseGenerateReport(driver, options -> runtime,
                (command, directory, timeout) -> new LightHouseGenerateReport.CommandResult(0, "ok"),
                reportDirectory) {
            @Override public String getPageName() { return "fixed"; }
        };

        IllegalStateException failure = Assert.expectThrows(IllegalStateException.class,
                generator::generateLightHouseReport);

        Assert.assertTrue(failure.getMessage().contains("Refusing to overwrite"));
        Assert.assertEquals(Files.readString(reportDirectory.resolve("fixed.html")), "<html>old</html>");
    }

    @Test
    public void deprecatedScriptHelpersFailClosedAndPageNameRemainsReadable() throws Exception {
        WebDriver driver = mock(WebDriver.class);
        when(driver.getCurrentUrl()).thenReturn("https://example.com/path/sub-path");
        Path reportDirectory = Files.createTempDirectory("shaft-lighthouse-helper-");
        LightHouseGenerateReport reportGenerator = new LightHouseGenerateReport(driver,
                options -> { throw new AssertionError(); }, (command, directory, timeout) -> { throw new AssertionError(); },
                reportDirectory);

        reportGenerator.createLighthouseReportFolderInProjectDirectory();
        Assert.assertTrue(Files.isDirectory(reportDirectory));
        String pageName = reportGenerator.getPageName();
        Assert.assertTrue(pageName.contains("--path-sub-path"));
        Assert.assertTrue(pageName.startsWith(LocalDate.now().format(java.time.format.DateTimeFormatter.ofPattern("dd-MM-yyyy"))));
    }

    @Test
    public void getPageNameShouldReturnFallbackMessageWhenUrlConversionFails() throws MalformedURLException {
        WebDriver driver = mock(WebDriver.class);
        when(driver.getCurrentUrl()).thenReturn("https://example.com/invalid");
        LightHouseGenerateReport reportGenerator = new LightHouseGenerateReport(driver);

        URI mockedUri = mock(URI.class);
        when(mockedUri.toURL()).thenThrow(new MalformedURLException("forced"));

        try (MockedStatic<URI> mockedUriStatic = Mockito.mockStatic(URI.class)) {
            mockedUriStatic.when(() -> URI.create("https://example.com/invalid")).thenReturn(mockedUri);

            Assert.assertEquals(reportGenerator.getPageName(), "Error Occurred while creating the requested page name");
        }
    }
}
