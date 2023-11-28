package com.shaft.listeners.internal;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.restassured.RestAssured;
import org.apache.logging.log4j.Level;

public class UpdateChecker {

    public static void check() {
        boolean isLoggingDisabled = SHAFT.Properties.reporting.disableLogging();
        String logMessage = "";
        try {
            ReportManager.logDiscrete("Checking for engine updates...");
            SHAFT.Properties.reporting.set().disableLogging(true);
            String latestVersion = RestAssured.given().baseUri("https://api.github.com/").and().basePath("repos/ShaftHQ/SHAFT_ENGINE/releases/")
                    .when().get("latest")
                    .thenReturn().body().jsonPath().getString("name");
            String currentVersion = SHAFT.Properties.internal.shaftEngineVersion();
            if (!currentVersion.equalsIgnoreCase(latestVersion)) {
                logMessage = "⚠\uFE0F You're using an outdated engine version \"" + currentVersion + "\" ⚠\uFE0F\nKindly upgrade to the latest one \"" + latestVersion + "\" to ensure the best experience.\nFor more information click here: https://github.com/ShaftHQ/SHAFT_ENGINE/releases/latest .";
            } else {
                logMessage = "You're using the latest engine version \"" + latestVersion + "\". \uD83D\uDC4D";
            }
        } catch (Throwable throwable) {
            logMessage = "Failed to check for updates... proceeding with engine setup...";
        }
        SHAFT.Properties.reporting.set().disableLogging(isLoggingDisabled);
        if (logMessage.contains("⚠\uFE0F")) {
            ReportManagerHelper.logDiscrete(logMessage, Level.WARN);
        } else {
            ReportManager.logDiscrete(logMessage);
        }
    }
}
