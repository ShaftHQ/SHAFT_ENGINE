package com.shaft.tools.io.internal;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;

import java.io.File;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ReportHelper {
    private static final DateTimeFormatter TIMESTAMP_FORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss");

    public static void attachEngineLog() {
        String executionEndTimestamp = TIMESTAMP_FORMATTER.format(ZonedDateTime.now());
        ReportManagerHelper.attachEngineLog(executionEndTimestamp);
    }

    public static void attachIssuesLog() {
        String executionEndTimestamp = TIMESTAMP_FORMATTER.format(ZonedDateTime.now());
        ReportManagerHelper.attachIssuesLog(executionEndTimestamp);
    }

    public static void enableLogging() {
        SHAFT.Properties.reporting.set().disableLogging(false);
    }

    public static void disableLogging() {
        SHAFT.Properties.reporting.set().disableLogging(true);
    }

    public static void attachPropertyFiles() {
        ReportManager.logDiscrete("Initializing Properties...");
        disableLogging();
        if (FileActions.getInstance(true).doesFileExist(SHAFT.Properties.paths.properties())) {
            List<List<Object>> attachments = new ArrayList<>();
            var propertyFiles = Arrays.asList(FileActions.getInstance(true).listFilesInDirectory(SHAFT.Properties.paths.properties(), null).replaceAll("default" + System.lineSeparator(), "").trim().split(System.lineSeparator()));
            propertyFiles.forEach(file -> attachments.add(Arrays.asList("Properties", file.replace(".properties", ""), FileActions.getInstance(true).readFile(SHAFT.Properties.paths.properties() + File.separator + file))));
            ReportManagerHelper.logNestedSteps("Property Files", attachments);
        }
        enableLogging();
    }

    public static void attachCucumberReport() {
        if (FileActions.getInstance(true).doesFileExist("allure-results/cucumberReport.html")) {
            ReportManagerHelper.attach("HTML", "Cucumber Execution Report", FileActions.getInstance(true).readFile("allure-results/cucumberReport.html"));
        }
    }
}