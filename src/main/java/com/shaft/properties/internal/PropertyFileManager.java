package com.shaft.properties.internal;

import com.shaft.cli.FileActions;
import com.shaft.tools.internal.support.AndroidApkBadgingReader;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import lombok.Getter;
import org.apache.commons.io.FileUtils;
import org.openqa.selenium.MutableCapabilities;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.util.*;

public final class PropertyFileManager {

    @Getter
    private static final String CUSTOM_PROPERTIES_FOLDER_PATH = "src/main/resources/properties";

    private PropertyFileManager() {
        throw new IllegalStateException("Utility class");
    }

    public static Map<String, String> getAppiumDesiredCapabilities() {
        Map<String, String> appiumDesiredCapabilities = new HashMap<>();

        // Collect mobile_ properties from the effective (system + thread-local) view.
        // Thread-local overrides take precedence over system properties.
        collectMobileProperties(ThreadLocalPropertiesManager.getEffectiveProperties(), appiumDesiredCapabilities);

        var app = appiumDesiredCapabilities.get("mobile_app");
        if (app != null && !app.isEmpty() && !isRemoteAppUrl(app)) {
            if (app.startsWith("src\\") || app.startsWith("src/")) {
                appiumDesiredCapabilities.put("mobile_app", FileActions.getInstance(true).getAbsolutePath(app));
            } else if (!new File(app).isAbsolute()) {
                appiumDesiredCapabilities.put("mobile_app",
                        new File(System.getProperty("user.dir"), app).getAbsolutePath());
            }
        }
        maybeInferMissingAndroidAppIdentifiers(appiumDesiredCapabilities);
        return appiumDesiredCapabilities;
    }

    /**
     * When {@code mobile_app} points to an APK but package/activity are omitted, UiAutomator2 may fail to
     * resolve the main activity. If {@code ANDROID_HOME} / {@code ANDROID_SDK_ROOT} is set, run
     * {@code aapt dump badging} and fill missing {@code mobile_appPackage} / {@code mobile_appActivity}.
     * <p>
     * Opt out: {@code -Dshaft.skipApkPackageActivityInference=true}
     */
    private static void maybeInferMissingAndroidAppIdentifiers(Map<String, String> caps) {
        if (Boolean.parseBoolean(System.getProperty("shaft.skipApkPackageActivityInference", "false"))) {
            return;
        }
        String appPath = caps.get("mobile_app");
        if (appPath == null || appPath.isBlank() || !appPath.toLowerCase().endsWith(".apk")) {
            return;
        }
        String pkg = Optional.ofNullable(caps.get("mobile_appPackage")).orElse("").trim();
        String act = Optional.ofNullable(caps.get("mobile_appActivity")).orElse("").trim();
        if (!pkg.isEmpty() && !act.isEmpty()) {
            return;
        }
        File apk = new File(appPath);
        if (!apk.isFile()) {
            return;
        }
        var inferred = AndroidApkBadgingReader.readPackageAndLaunchableActivity(apk);
        if (inferred.isEmpty()) {
            ReportManager.logDiscrete(
                    "Could not infer mobile_appPackage / mobile_appActivity from APK (install Android build-tools and set ANDROID_HOME, or set mobile_appPackage and mobile_appActivity in properties).");
            return;
        }
        var pa = inferred.get();
        if (pkg.isEmpty() && pa.packageName() != null && !pa.packageName().isBlank()) {
            caps.put("mobile_appPackage", pa.packageName());
            ReportManager.logDiscrete("Inferred mobile_appPackage from APK badging: " + pa.packageName());
        }
        if (act.isEmpty() && pa.launchableActivity() != null && !pa.launchableActivity().isBlank()) {
            caps.put("mobile_appActivity", pa.launchableActivity());
            ReportManager.logDiscrete("Inferred mobile_appActivity from APK badging: " + pa.launchableActivity());
        }
    }

    /**
     * Returns {@code true} when the app path looks like a remote/cloud app URL that must not
     * be resolved as a local filesystem path. Recognized schemes include {@code bs://} (BrowserStack),
     * {@code lt://} (LambdaTest), {@code http://}, and {@code https://}.
     */
    private static boolean isRemoteAppUrl(String app) {
        return app.startsWith("bs://") || app.startsWith("lt://")
                || app.startsWith("http://") || app.startsWith("https://");
    }

    private static void collectMobileProperties(java.util.Properties source, Map<String, String> target) {
        source.forEach((key, value) -> {
            if (String.valueOf(key).contains("mobile_")) {
                target.put(String.valueOf(key), String.valueOf(value));
            }
        });
    }

    public static MutableCapabilities getCustomWebDriverDesiredCapabilities() {
        MutableCapabilities customDriverOptions = new MutableCapabilities();
        java.util.Properties props = ThreadLocalPropertiesManager.getEffectiveProperties();
        props.forEach((key, value) -> {
            if (String.valueOf(key).startsWith("capabilities.") && !String.valueOf(value).isBlank()) {
                customDriverOptions.setCapability(String.valueOf(key).split("capabilities.")[1], String.valueOf(value));
            }
        });
        return customDriverOptions;
    }

    public static HashMap<String, Object> getCustomBrowserstackCapabilities() {
        HashMap<String, Object> browserstackOptions = new HashMap<>();
        java.util.Properties props = ThreadLocalPropertiesManager.getEffectiveProperties();
        props.forEach((key, value) -> {
            if (String.valueOf(key).startsWith("browserStack.") && !String.valueOf(value).isBlank()) {
                Set<String> excludedKeys = Set.of(
                        "appName",
                        "appUrl",
                        "appRelativeFilePath"
                );
                var parsedKey = String.valueOf(key).split("browserStack.")[1];
                if (!excludedKeys.contains(parsedKey))
                    browserstackOptions.put(parsedKey, String.valueOf(value));
            }
        });
        return browserstackOptions;
    }

    private static void readPropertyFiles(String propertiesFolderPath) {
        if (propertiesFolderPath != null) {
            ReportManager.logDiscrete("Reading properties directory: " + propertiesFolderPath);
            try {
                java.util.Properties properties = new java.util.Properties();
                if (propertiesFolderPath.contains(".jar")) {
                    // unpacks default properties to target folder
                    URL url = URI.create(propertiesFolderPath.substring(0, propertiesFolderPath.indexOf("!"))).toURL();
                    FileActions.getInstance(true).unpackArchive(url, "target/");
                    propertiesFolderPath = "target/resources/properties/default/";
                }
                // reading regular files
                Collection<File> propertiesFilesList;
                if (FileActions.getInstance(true).doesFileExist(propertiesFolderPath)) {
                    propertiesFilesList = FileUtils.listFiles(new File(propertiesFolderPath), new String[]{"properties"},
                            false);
                    File propertyFile;
                    for (int i = 0; i < propertiesFilesList.size(); i++) {
                        propertyFile = (File) (propertiesFilesList.toArray())[i];
                        ReportManager.logDiscrete("Loading properties file: " + propertyFile);
                        loadPropertiesFileIntoSystemProperties(properties, propertyFile);
                    }
                } else {
                    ReportManager.logDiscrete(
                            "The desired propertiesFolderPath directory doesn't exist. ["
                                    + propertiesFolderPath + "]");
                }
            } catch (Exception e) {
                ReportManagerHelper.logDiscrete(e);
            }
        }
    }

    private static void loadPropertiesFileIntoSystemProperties(java.util.Properties properties, File propertyFile) {
        try {
            properties.load(new FileInputStream(propertyFile));
            // merge: effective properties (system + thread-local) override file-based properties
            properties.putAll(ThreadLocalPropertiesManager.getEffectiveProperties());
            // write merged result back to system properties so that ConfigFactory picks them up
            System.getProperties().putAll(properties);
        } catch (IOException e) {
            ReportManagerHelper.logDiscrete(e);
        }
    }

    public static void readCustomPropertyFiles() {
        readPropertyFiles(Objects.requireNonNullElse(Properties.paths.properties(), CUSTOM_PROPERTIES_FOLDER_PATH));
    }
}