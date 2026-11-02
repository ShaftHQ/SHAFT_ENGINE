package com.shaft.performance.internal;

import com.shaft.driver.SHAFT;
import com.shaft.infrastructure.LighthouseRuntime;
import com.shaft.infrastructure.SetupOptions;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.RemoteWebDriver;

import java.awt.Desktop;
import java.awt.GraphicsEnvironment;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Generates Google Lighthouse performance reports for web pages by
 * invoking the Lighthouse CLI tool via Node.js.
 *
 * <p>Reports are saved to the project's performance report directory and
 * can be attached to Allure reports for CI/CD integration.
 */
public class LightHouseGenerateReport {
    private static final DateTimeFormatter FILENAME_FORMATTER = DateTimeFormatter.ofPattern("dd-MM-yyyy_HH-mm-ss-SSS");
    final WebDriver driver;
    int PortNum;
    String PageName;
    private final RuntimeResolver runtimeResolver;
    private final CommandRunner commandRunner;
    private final Path reportDirectory;

    public LightHouseGenerateReport(WebDriver driver) {
        this(driver, LighthouseRuntime::requireReady, LightHouseGenerateReport::runCommand,
                Path.of("lighthouse-reports"));
    }

    LightHouseGenerateReport(WebDriver driver, RuntimeResolver runtimeResolver, CommandRunner commandRunner,
                             Path reportDirectory) {
        this.driver = driver;
        this.runtimeResolver = runtimeResolver;
        this.commandRunner = commandRunner;
        this.reportDirectory = reportDirectory.toAbsolutePath().normalize();
    }

    static List<String> command(com.shaft.infrastructure.LighthouseRuntime runtime, String url, int port,
                                java.nio.file.Path output) {
        var command = new ArrayList<>(runtime.commandPrefix());
        command.add(url);
        command.add("--port=" + port);
        command.add("--preset=desktop");
        command.add("--output=html");
        command.add("--output-path=" + output.toAbsolutePath().normalize());
        command.add("--only-categories=performance");
        return List.copyOf(command);
    }

    public void generateLightHouseReport() {
        if (!SHAFT.Properties.performance.isEnabled()) {
            return;
        }
        PortNum = SHAFT.Properties.performance.port();
        PageName = getPageName();
        try {
            SetupOptions options = SHAFT.Infrastructure.options().withProfile(com.shaft.infrastructure.SetupProfile.LIGHTHOUSE);
            LighthouseRuntime runtime = runtimeResolver.resolve(options);
            Files.createDirectories(reportDirectory);
            Path report = reportDirectory.resolve(PageName + ".html");
            if (Files.exists(report)) {
                throw new IOException("Refusing to overwrite an existing Lighthouse report: " + report);
            }
            int port = debuggerPort(driver, PortNum);
            boolean completed = false;
            try {
                CommandResult result = commandRunner.run(command(runtime, driver.getCurrentUrl(), port, report),
                        runtime.workingDirectory(), options.startupTimeout());
                if (result.exitCode() != 0) {
                    throw new IOException("Lighthouse exited with code " + result.exitCode() + ": " + result.output());
                }
                if (!Files.isRegularFile(report) || Files.size(report) == 0) {
                    throw new IOException("Lighthouse completed without creating a non-empty report at " + report);
                }
                String html = Files.readString(report, StandardCharsets.UTF_8);
                if (!html.toLowerCase(java.util.Locale.ROOT).contains("<html")) {
                    throw new IOException("Lighthouse output is not an HTML document: " + report);
                }
                completed = true;
                openReportWhenRequested(report);
                SHAFT.Report.report("Lighthouse Report Generated successfully");
                SHAFT.Report.attach("LightHouse HTML", "Report", html);
            } finally {
                if (!completed) Files.deleteIfExists(report);
            }
        } catch (IOException e) {
            throw new IllegalStateException("Failed to generate the Lighthouse report: " + e.getMessage(), e);
        }
    }

    static int debuggerPort(WebDriver driver, int fallback) {
        if (driver instanceof RemoteWebDriver remote) {
            Object chromeOptions = remote.getCapabilities().getCapability("goog:chromeOptions");
            if (chromeOptions instanceof Map<?, ?> options) {
                Object address = options.get("debuggerAddress");
                if (address instanceof String value) {
                    int separator = value.lastIndexOf(':');
                    if (separator >= 0 && separator + 1 < value.length()) {
                        try {
                            return Integer.parseInt(value.substring(separator + 1));
                        } catch (NumberFormatException ignored) {
                            // Fall back to the explicitly configured Lighthouse port.
                        }
                    }
                }
            }
        }
        return fallback;
    }

    private static void openReportWhenRequested(Path report) throws IOException {
        if (!SHAFT.Properties.reporting.openLighthouseReportWhileExecution()) {
            return;
        }
        if (GraphicsEnvironment.isHeadless() || !Desktop.isDesktopSupported()) {
            throw new IOException("Opening the Lighthouse report was requested, but this environment has no desktop handler.");
        }
        Desktop.getDesktop().browse(report.toUri());
        SHAFT.Report.report("Lighthouse Report Opened in the default browser successfully");
    }

    private static CommandResult runCommand(List<String> command, Path workingDirectory, Duration timeout)
            throws IOException {
        com.shaft.infrastructure.ManagedProcessRunner.Result result =
                com.shaft.infrastructure.ManagedProcessRunner.run(command, workingDirectory, timeout);
        return new CommandResult(result.exitCode(), result.output());
    }

    @FunctionalInterface
    interface RuntimeResolver {
        LighthouseRuntime resolve(SetupOptions options) throws IOException;
    }

    @FunctionalInterface
    interface CommandRunner {
        CommandResult run(List<String> command, Path workingDirectory, Duration timeout) throws IOException;
    }

    record CommandResult(int exitCode, String output) {
    }

    public void createLighthouseReportFolderInProjectDirectory() {
        try {
            Files.createDirectories(reportDirectory);
        } catch (IOException failure) {
            throw new IllegalStateException("Unable to create the Lighthouse report directory.", failure);
        }
    }

    /** @deprecated Reports are opened directly from the verified output path. */
    @Deprecated(forRemoval = true)
    public void openLighthouseReportWhileExecution() {
        if (!SHAFT.Properties.reporting.openLighthouseReportWhileExecution()) return;
        String name = PageName == null ? getPageName() : PageName;
        try {
            openReportWhenRequested(reportDirectory.resolve(name + ".html"));
        } catch (IOException failure) {
            throw new IllegalStateException("Unable to open the Lighthouse report.", failure);
        }
    }

    /** @deprecated Generated report-opener scripts were removed; use the report output path. */
    @Deprecated(forRemoval = true)
    public void writeReportPathToFilesInProjectDirectory(String pageName) {
        throw new UnsupportedOperationException("Generated Lighthouse opener scripts are no longer supported.");
    }

    /** @deprecated Generated executable scripts were removed; use managed Lighthouse setup. */
    @Deprecated(forRemoval = true)
    public void writeNodeScriptFileInProjectDirectory() {
        throw new UnsupportedOperationException("Generated Lighthouse scripts are no longer supported. "
                + "Install the managed LIGHTHOUSE profile and call generateLightHouseReport().");
    }

    public String getPageName() {
        String Pagename;
        String CurrentUrl;
        CurrentUrl = driver.getCurrentUrl();
        try {
            URL url = URI.create(CurrentUrl).toURL();
            Pagename = url.getPath();
            Pagename = Pagename.replace("/", "-");
            return FILENAME_FORMATTER.format(ZonedDateTime.now()) + "-" + Pagename;
        } catch (MalformedURLException e) {
            ReportManagerHelper.log(e);
//            return  (new SimpleDateFormat("dd-MM-yyyy_HH-mm-ss-SSSS-aaa")).format(System.currentTimeMillis())+ "-" + Pagename;
            return "Error Occurred while creating the requested page name";
        }
    }

}
