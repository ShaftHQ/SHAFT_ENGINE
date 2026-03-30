package com.shaft.tools.internal.support;

import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Best-effort parsing of {@code aapt}/{@code aapt2 dump badging} output to fill missing
 * {@code appPackage}/{@code appActivity} when only {@code app} (APK path) is configured.
 */
public final class AndroidApkBadgingReader {

    private static final Pattern PACKAGE_LINE = Pattern.compile("^package:\\s*name='([^']+)'");
    private static final Pattern LAUNCHABLE_LINE = Pattern.compile("^launchable-activity:\\s*name='([^']+)'");

    public record PackageActivity(String packageName, String launchableActivity) {
    }

    private AndroidApkBadgingReader() {
    }

    /**
     * @param apk existing .apk file on disk
     * @return package and first launchable activity when {@code dump badging} succeeds
     */
    public static Optional<PackageActivity> readPackageAndLaunchableActivity(File apk) {
        if (apk == null || !apk.isFile() || !apk.getName().toLowerCase().endsWith(".apk")) {
            return Optional.empty();
        }
        String androidHome = firstNonBlank(System.getenv("ANDROID_HOME"), System.getenv("ANDROID_SDK_ROOT"));
        if (androidHome == null || androidHome.isBlank()) {
            ReportManager.logDiscrete(
                    "ANDROID_HOME / ANDROID_SDK_ROOT is not set; cannot infer app package/activity from APK via aapt.");
            return Optional.empty();
        }
        File buildTools = new File(androidHome, "build-tools");
        if (!buildTools.isDirectory()) {
            return Optional.empty();
        }
        File[] versions = buildTools.listFiles(File::isDirectory);
        if (versions == null || versions.length == 0) {
            return Optional.empty();
        }
        Arrays.sort(versions, Comparator.comparingLong((File f) -> f.lastModified()).reversed());
        for (File dir : versions) {
            String aaptExecutable = resolveAaptExecutable(dir);
            if (aaptExecutable == null) {
                continue;
            }
            try {
                ProcessBuilder pb = new ProcessBuilder(aaptExecutable, "dump", "badging", apk.getAbsolutePath());
                pb.redirectErrorStream(true);
                Process p = pb.start();
                boolean finished = p.waitFor(45, TimeUnit.SECONDS);
                if (!finished) {
                    p.destroyForcibly();
                    continue;
                }
                if (p.exitValue() != 0) {
                    continue;
                }
                try (var reader = new BufferedReader(new InputStreamReader(p.getInputStream(), StandardCharsets.UTF_8))) {
                    Optional<PackageActivity> parsed = parseBadgingLines(reader);
                    if (parsed.isPresent()) {
                        return parsed;
                    }
                }
            } catch (Exception e) {
                ReportManagerHelper.logDiscrete(e);
            }
        }
        return Optional.empty();
    }

    /**
     * Parses {@code aapt dump badging} / {@code aapt2 dump badging} stdout (for unit tests and tooling).
     */
    public static Optional<PackageActivity> parseAaptDumpBadgingOutput(String output) throws IOException {
        if (output == null || output.isBlank()) {
            return Optional.empty();
        }
        try (var reader = new BufferedReader(new StringReader(output))) {
            return parseBadgingLines(reader);
        }
    }

    private static Optional<PackageActivity> parseBadgingLines(BufferedReader reader) throws IOException {
        String pkg = null;
        String act = null;
        String line;
        while ((line = reader.readLine()) != null) {
            Matcher m = PACKAGE_LINE.matcher(line);
            if (m.find()) {
                pkg = m.group(1);
            }
            m = LAUNCHABLE_LINE.matcher(line);
            if (m.find() && act == null) {
                act = m.group(1);
            }
        }
        if (pkg != null) {
            return Optional.of(new PackageActivity(pkg, act));
        }
        return Optional.empty();
    }

    private static String resolveAaptExecutable(File buildToolsDir) {
        boolean win = System.getProperty("os.name", "").toLowerCase().contains("win");
        File aapt2 = new File(buildToolsDir, win ? "aapt2.exe" : "aapt2");
        if (aapt2.isFile()) {
            return aapt2.getAbsolutePath();
        }
        File aapt = new File(buildToolsDir, win ? "aapt.exe" : "aapt");
        if (aapt.isFile()) {
            return aapt.getAbsolutePath();
        }
        return null;
    }

    private static String firstNonBlank(String a, String b) {
        if (a != null && !a.isBlank()) {
            return a;
        }
        if (b != null && !b.isBlank()) {
            return b;
        }
        return null;
    }
}
