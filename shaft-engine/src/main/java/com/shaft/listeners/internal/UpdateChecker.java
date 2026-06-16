package com.shaft.listeners.internal;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.restassured.RestAssured;
import org.apache.logging.log4j.Level;

import java.math.BigInteger;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;

/**
 * Checks GitHub for the latest published SHAFT Engine version.
 */
public class UpdateChecker {
    public static AtomicReference<String> latestVersion = new AtomicReference<>();
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("dd-MM-yyyy");

    /**
     * Performs a best-effort update check without interrupting test execution when GitHub is unavailable.
     */
    public static void check() {
        check(UpdateChecker::getLatestVersionFromGitHub);
    }

    static void check(Supplier<String> latestVersionSupplier) {
        ReportManager.logDiscrete("Checking for SHAFT engine updates.");

        try {
            String todayDate = DATE_FORMATTER.format(LocalDate.now());
            String currentVersion = SHAFT.Properties.internal.shaftEngineVersion();
            String latestVersion = latestVersionSupplier.get();

            Thread.ofVirtual().start(() -> FileActions.getInstance(true).writeToFile("target/", "engine_check", todayDate + "_" + latestVersion));

            if (currentVersion.equalsIgnoreCase(latestVersion)) {
                var logMessage = "You're using the latest engine version \"" + latestVersion + "\". \uD83D\uDC4D";
                ReportManagerHelper.logDiscrete(logMessage, Level.INFO);
            } else if (isCurrentVersionOutdated(currentVersion, latestVersion)) {
                var logMessage = "⚠\uFE0F You're using an outdated engine version \"" + currentVersion + "\" ⚠\uFE0F\nKindly upgrade to the latest one \"" + latestVersion + "\" to ensure the best experience.\nFor more information click here: https://github.com/ShaftHQ/SHAFT_ENGINE/releases/latest .";
                ReportManagerHelper.logImportantEntry(logMessage, Level.WARN);
            } else {
                var logMessage = "You're using engine version \"" + currentVersion
                        + "\", which is newer than the latest published release \"" + latestVersion + "\".";
                ReportManagerHelper.logDiscrete(logMessage, Level.INFO);
            }
        } catch (Exception exception) {
            ReportManagerHelper.logDiscrete("Engine update check was not completed because GitHub is unavailable.", Level.DEBUG);
            ReportManagerHelper.logDiscrete(exception, Level.DEBUG);
        }
    }

    static boolean isCurrentVersionOutdated(String currentVersion, String latestVersion) {
        Optional<BigInteger[]> currentParts = parseNumericVersion(currentVersion);
        Optional<BigInteger[]> latestParts = parseNumericVersion(latestVersion);
        if (currentParts.isEmpty() || latestParts.isEmpty()) {
            return false;
        }

        BigInteger[] current = currentParts.orElseThrow();
        BigInteger[] latest = latestParts.orElseThrow();
        int length = Math.max(current.length, latest.length);
        for (int index = 0; index < length; index++) {
            BigInteger currentPart = index < current.length ? current[index] : BigInteger.ZERO;
            BigInteger latestPart = index < latest.length ? latest[index] : BigInteger.ZERO;
            int comparison = currentPart.compareTo(latestPart);
            if (comparison != 0) {
                return comparison < 0;
            }
        }
        return false;
    }

    private static Optional<BigInteger[]> parseNumericVersion(String version) {
        if (version == null || version.isBlank()) {
            return Optional.empty();
        }
        String normalizedVersion = version.trim().replaceFirst("^[vV]", "");
        String[] tokens = normalizedVersion.split("\\.");
        BigInteger[] parts = new BigInteger[tokens.length];
        for (int index = 0; index < tokens.length; index++) {
            if (!tokens[index].matches("\\d+")) {
                return Optional.empty();
            }
            parts[index] = new BigInteger(tokens[index]);
        }
        return Optional.of(parts);
    }

    private static String getLatestVersionFromGitHub() {
        return RestAssured.given().baseUri("https://api.github.com/").and().basePath("repos/ShaftHQ/SHAFT_ENGINE/releases/")
                .when().get("latest")
                .thenReturn().body().jsonPath().getString("name");
    }
}
