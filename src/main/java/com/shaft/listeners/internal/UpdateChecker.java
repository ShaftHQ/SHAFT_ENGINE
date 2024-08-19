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
import java.util.concurrent.atomic.AtomicReference;

public class UpdateChecker {
    public static AtomicReference<String> latestVersion = new AtomicReference<>();

    @SneakyThrows
    public static void check() {
        ReportManager.logDiscrete("Checking for engine updates...");

        String todayDate = new SimpleDateFormat("dd-MM-yyyy").format(new Date());
        String currentVersion = SHAFT.Properties.internal.shaftEngineVersion();
        String latestVersion = getLatestVersionFromGitHub();

        Thread.ofVirtual().start(() -> FileActions.getInstance(true).writeToFile("target/", "engine_check", todayDate+"_"+latestVersion));

        if (currentVersion.equalsIgnoreCase(latestVersion)){
            var logMessage = "You're using the latest engine version \"" + latestVersion + "\". \uD83D\uDC4D";
            ReportManagerHelper.logDiscrete(logMessage, Level.INFO);
        } else {
            var logMessage = "⚠\uFE0F You're using an outdated engine version \"" + currentVersion + "\" ⚠\uFE0F\nKindly upgrade to the latest one \"" + latestVersion + "\" to ensure the best experience.\nFor more information click here: https://github.com/ShaftHQ/SHAFT_ENGINE/releases/latest .";
            ReportManagerHelper.logImportantEntry(logMessage, Level.WARN);
        }
    }

    private static String getLatestVersionFromGitHub() {
        return RestAssured.given().baseUri("https://api.github.com/").and().basePath("repos/ShaftHQ/SHAFT_ENGINE/releases/")
                .when().get("latest")
                .thenReturn().body().jsonPath().getString("name");
    }
}