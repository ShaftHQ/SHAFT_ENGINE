package com.shaft.tools.io.internal;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Provides report-level helper operations for attaching execution artifacts.
 */
public class ReportHelper {
    private static final DateTimeFormatter TIMESTAMP_FORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss");

    /**
     * Attaches the engine log file to the report.
     */
    public static void attachEngineLog() {
        String executionEndTimestamp = TIMESTAMP_FORMATTER.format(ZonedDateTime.now());
        ReportManagerHelper.attachEngineLog(executionEndTimestamp);
    }

    /**
     * Attaches the issues log file to the report.
     */
    public static void attachIssuesLog() {
        String executionEndTimestamp = TIMESTAMP_FORMATTER.format(ZonedDateTime.now());
        ReportManagerHelper.attachIssuesLog(executionEndTimestamp);
    }

    /**
     * Enables logging output temporarily.
     */
    public static void enableLogging() {
        SHAFT.Properties.reporting.set().disableLogging(false);
    }

    /**
     * Disables logging output temporarily.
     */
    public static void disableLogging() {
        SHAFT.Properties.reporting.set().disableLogging(true);
    }

    /**
     * Attaches loaded property files as nested report steps.
     */
    public static void attachPropertyFiles() {
        ReportManager.logDiscrete("Attaching SHAFT property files.");
        disableLogging();
        try {
            String propertiesPath = SHAFT.Properties.paths.properties();
            if (FileActions.getInstance(true).doesFileExist(propertiesPath)) {
                List<List<Object>> attachments = new ArrayList<>();
                Arrays.stream(FileActions.getInstance(true).listFilesInDirectory(propertiesPath, null).split(System.lineSeparator()))
                        .map(String::trim)
                        .filter(file -> !file.isBlank())
                        .filter(file -> !"default".equals(file))
                        .filter(file -> file.endsWith(".properties"))
                        .filter(file -> Files.isRegularFile(Path.of(propertiesPath, file)))
                        .forEach(file -> attachments.add(Arrays.asList("Properties", file.replace(".properties", ""),
                                FileActions.getInstance(true).readFile(propertiesPath + File.separator + file))));
                if (!attachments.isEmpty()) {
                    ReportManagerHelper.logNestedSteps("Property Files", attachments);
                }
            }
        } finally {
            enableLogging();
        }
    }

    /**
     * Attaches the generated Cucumber HTML report when available.
     */
    public static void attachCucumberReport() {
        if (FileActions.getInstance(true).doesFileExist("allure-results/cucumberReport.html")) {
            ReportManagerHelper.attach("HTML", "Cucumber Execution Report", FileActions.getInstance(true).readFile("allure-results/cucumberReport.html"));
        }
    }
}
