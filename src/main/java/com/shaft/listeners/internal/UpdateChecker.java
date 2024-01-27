package com.shaft.listeners.internal;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.restassured.RestAssured;
import lombok.SneakyThrows;
import org.apache.logging.log4j.Level;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

public class UpdateChecker {
    @SneakyThrows
    public static void check() {
        ReportManager.logDiscrete("Checking for engine updates...");
        // Go fetch the latest engine version (will only wait for this thread if needed to save time)
        AtomicReference<String> latestVersion = new AtomicReference<>();
        Thread thread1 = Thread.ofVirtual().start(() -> latestVersion.set(RestAssured.given().baseUri("https://api.github.com/").and().basePath("repos/ShaftHQ/SHAFT_ENGINE/releases/")
                .when().get("latest")
                .thenReturn().body().jsonPath().getString("name")));
        // In parallel learn today's date and read the last checked date from the file then synchronize
        String todayDate = new SimpleDateFormat("dd-MM-yyyy").format(new Date());
        String lastCheckedFile = "engine_check";
        AtomicBoolean doesEngineCheckFileExists = new AtomicBoolean(false);
        AtomicBoolean doesFileContainTodayDate = new AtomicBoolean(false);
        Thread thread2 = Thread.ofVirtual().start(() -> {
            doesEngineCheckFileExists.set(FileActions.getInstance(true).doesFileExist("target/", lastCheckedFile, 1));
            if (doesEngineCheckFileExists.get())
                doesFileContainTodayDate.set(FileActions.getInstance(true).readFile("target/", lastCheckedFile).equals(todayDate));
        });
        thread1.join();
        thread2.join();
        if (!doesEngineCheckFileExists.get() || !doesFileContainTodayDate.get()) {
            String logMessage;
            try {
                String currentVersion = SHAFT.Properties.internal.shaftEngineVersion();
                thread1.join(); //only wait for the API call to join the main thread if needed
                if (!currentVersion.equalsIgnoreCase(latestVersion.get())) {
                    logMessage = "⚠\uFE0F You're using an outdated engine version \"" + currentVersion + "\" ⚠\uFE0F\nKindly upgrade to the latest one \"" + latestVersion + "\" to ensure the best experience.\nFor more information click here: https://github.com/ShaftHQ/SHAFT_ENGINE/releases/latest .";
                } else {
                    logMessage = "You're using the latest engine version \"" + latestVersion + "\". \uD83D\uDC4D";
                }
            } catch (Throwable throwable) {
                logMessage = "Failed to check for updates... proceeding with engine setup...";
            }
            // Write the last update date to a file in a separate asynchronous virtual thread
            Thread.ofVirtual().start(() -> FileActions.getInstance(true).writeToFile("target/", lastCheckedFile, todayDate));
            if (logMessage.contains("⚠\uFE0F")) {
                ReportManagerHelper.logDiscrete(logMessage, Level.WARN);
            } else {
                ReportManagerHelper.logDiscrete(logMessage, Level.INFO);
            }
        } else {
            ReportManagerHelper.logDiscrete("Engine Update check done for the day. \uD83D\uDC4D", Level.INFO);
        }
    }
}