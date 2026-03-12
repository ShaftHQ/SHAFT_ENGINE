package com.shaft.driver.internal.DriverFactory;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.regex.Pattern;

/**
 * Utility class for generating a {@code browserstack.yml} configuration file from SHAFT's
 * BrowserStack property system. This enables integration with the BrowserStack SDK, which
 * reads the YAML file at runtime to configure test execution on BrowserStack's cloud platform.
 *
 * <p>The generated file maps SHAFT's {@code browserStack.*} properties to the SDK's expected
 * YAML format, supporting desktop web, mobile web, and native mobile app scenarios.
 *
 * <p>Usage: Call {@link #generateBrowserStackYml()} before driver creation to produce a
 * {@code browserstack.yml} file in the project root directory. When the BrowserStack SDK
 * Java agent is active, it reads this file and intercepts WebDriver creation accordingly.
 *
 * @see <a href="https://www.browserstack.com/docs/automate/selenium/how-sdk-works">How BrowserStack SDK Works</a>
 * @see <a href="https://www.browserstack.com/docs/automate/selenium/sdk-params">SDK Configuration Parameters</a>
 */
public class BrowserStackSdkHelper {

    private BrowserStackSdkHelper() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Generates a {@code browserstack.yml} configuration file in the project root directory
     * by mapping SHAFT's BrowserStack properties to the SDK's expected YAML format.
     *
     * <p>If the {@code browserStack.customBrowserStackYmlPath} property is set to a non-empty
     * value, the specified file is copied to the project root as {@code browserstack.yml} and
     * SHAFT's property-to-YAML mapping is skipped entirely. This allows users to provide their
     * own SDK configuration file and override SHAFT's properties.
     *
     * <p>The generated file includes authentication credentials, platform configuration,
     * build/project naming, and optional settings like local testing, debugging, and
     * geo-location.
     *
     * @return the absolute path to the {@code browserstack.yml} file (generated or copied)
     * @throws IllegalStateException if the file cannot be written or the custom file is not found
     */
    public static String generateBrowserStackYml() {
        var customYmlPath = SHAFT.Properties.browserStack.customBrowserStackYmlPath();
        if (customYmlPath != null && !customYmlPath.isEmpty()) {
            return useCustomBrowserStackYml(customYmlPath);
        }
        ReportManager.logDiscrete("Generating browserstack.yml from SHAFT properties...");
        var config = buildConfiguration();
        var yamlPath = writeYamlFile(config);
        ReportManager.logDiscrete("Generated browserstack.yml at: " + yamlPath);
        return yamlPath;
    }

    /**
     * Copies a user-provided {@code browserstack.yml} file to the project root directory,
     * overriding any SHAFT-generated configuration. The user's file is used as-is by the
     * BrowserStack SDK.
     *
     * @param customYmlPath the path to the custom YAML file (absolute or relative to project root)
     * @return the absolute path to the copied {@code browserstack.yml} file in the project root
     * @throws IllegalStateException if the custom file does not exist or cannot be copied
     */
    private static String useCustomBrowserStackYml(String customYmlPath) {
        var sourceFile = new File(customYmlPath);
        if (!sourceFile.isAbsolute()) {
            sourceFile = new File(System.getProperty("user.dir"), customYmlPath);
        }
        if (!sourceFile.exists()) {
            throw new IllegalStateException("Custom browserstack.yml not found at: " + sourceFile.getAbsolutePath());
        }

        var targetPath = System.getProperty("user.dir") + File.separator + "browserstack.yml";
        var targetFile = new File(targetPath);

        // If source is already at the target location, no copy needed
        if (sourceFile.getAbsolutePath().equals(targetFile.getAbsolutePath())) {
            ReportManager.logDiscrete("Using custom browserstack.yml at project root: " + targetPath);
            return targetPath;
        }

        try {
            java.nio.file.Files.copy(sourceFile.toPath(), targetFile.toPath(),
                    java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException e) {
            throw new IllegalStateException("Failed to copy custom browserstack.yml from: "
                    + sourceFile.getAbsolutePath() + " to: " + targetPath, e);
        }

        ReportManager.logDiscrete("Using custom browserstack.yml from: " + sourceFile.getAbsolutePath());
        return targetPath;
    }

    /**
     * Builds the complete SDK configuration map from SHAFT's BrowserStack properties.
     * The map structure follows the BrowserStack SDK YAML format with root-level
     * capabilities and platform-specific settings.
     *
     * @return an ordered map representing the {@code browserstack.yml} structure
     */
    static Map<String, Object> buildConfiguration() {
        var config = new LinkedHashMap<String, Object>();

        // Authentication
        config.put("userName", SHAFT.Properties.browserStack.userName());
        config.put("accessKey", SHAFT.Properties.browserStack.accessKey());

        // Platform configuration
        var platform = buildPlatformEntry();
        if (!platform.isEmpty()) {
            config.put("platforms", Collections.singletonList(platform));
        }

        // Build and project naming
        config.put("buildName", resolveBuildName());
        config.put("projectName", resolveProjectName());
        config.put("buildIdentifier", "${BUILD_NUMBER}");

        // BrowserStack Automation flag
        config.put("browserstackAutomation", SHAFT.Properties.browserStack.browserstackAutomation());

        // Parallelization
        var parallels = SHAFT.Properties.browserStack.parallelsPerPlatform();
        if (parallels > 0) {
            config.put("parallelsPerPlatform", parallels);
        }

        // Local testing
        config.put("browserstackLocal", SHAFT.Properties.browserStack.local());

        // Debugging and logging
        config.put("debug", SHAFT.Properties.browserStack.debug());
        config.put("networkLogs", SHAFT.Properties.browserStack.networkLogs());

        // Session capabilities
        config.put("acceptInsecureCerts", SHAFT.Properties.browserStack.acceptInsecureCerts());

        // Selenium/Appium versions
        addIfNotEmpty(config, "seleniumVersion", SHAFT.Properties.browserStack.seleniumVersion());
        addIfNotEmpty(config, "appiumVersion", SHAFT.Properties.browserStack.appiumVersion());

        // Geo-location
        addIfNotEmpty(config, "geoLocation", SHAFT.Properties.browserStack.geoLocation());

        // App configuration for native mobile testing
        addAppConfiguration(config);

        return config;
    }

    /**
     * Builds a platform entry map based on the current execution scenario
     * (desktop web, mobile web, or native mobile app).
     *
     * @return a map representing a single platform entry for the SDK configuration
     */
    private static Map<String, Object> buildPlatformEntry() {
        var platform = new LinkedHashMap<String, Object>();

        if (isNativeAppExecution()) {
            // Native mobile app execution
            addIfNotEmpty(platform, "deviceName", SHAFT.Properties.browserStack.deviceName());
            addIfNotEmpty(platform, "osVersion", SHAFT.Properties.browserStack.platformVersion());
        } else if (DriverFactoryHelper.isMobileWebExecution()) {
            // Mobile web execution
            addIfNotEmpty(platform, "deviceName", SHAFT.Properties.browserStack.deviceName());
            addIfNotEmpty(platform, "osVersion", SHAFT.Properties.browserStack.osVersion());
            addIfNotEmpty(platform, "browserName", SHAFT.Properties.web.targetBrowserName());
        } else {
            // Desktop web execution
            var os = SHAFT.Properties.platform.targetPlatform();
            if (os != null && !os.isEmpty()) {
                if (os.toLowerCase().contains("mac")) {
                    platform.put("os", "OS X");
                } else if (os.toLowerCase().contains("windows")) {
                    platform.put("os", "Windows");
                } else {
                    platform.put("os", os);
                }
            }
            addIfNotEmpty(platform, "osVersion", SHAFT.Properties.browserStack.osVersion());
            addIfNotEmpty(platform, "browserName", SHAFT.Properties.web.targetBrowserName());
            addIfNotEmpty(platform, "browserVersion", SHAFT.Properties.browserStack.browserVersion());
        }

        return platform;
    }

    /**
     * Adds app-related configuration to the SDK config for native mobile app testing.
     * Supports pre-uploaded apps via URL and new app uploads via relative file path.
     *
     * @param config the configuration map to add app settings to
     */
    private static void addAppConfiguration(Map<String, Object> config) {
        var appUrl = SHAFT.Properties.browserStack.appUrl();
        var appRelativePath = SHAFT.Properties.browserStack.appRelativeFilePath();
        var customId = SHAFT.Properties.browserStack.customID();

        if (!appUrl.isEmpty()) {
            config.put("app", appUrl);
        } else if (!appRelativePath.isEmpty()) {
            config.put("app", appRelativePath);
            if (!customId.isEmpty()) {
                config.put("appStoreConfiguration", Map.of("customId", customId));
            }
        }
    }

    /**
     * Resolves the build name from SHAFT properties. If no custom build name is configured,
     * generates one from the project directory name and current date.
     *
     * @return the resolved build name
     */
    private static String resolveBuildName() {
        var customBuildName = SHAFT.Properties.browserStack.buildName();
        if (!customBuildName.isEmpty()) {
            return customBuildName;
        }
        var pathItems = System.getProperty("user.dir").split(Pattern.quote(File.separator));
        return pathItems[pathItems.length - 1] + "_" + LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMdd"));
    }

    /**
     * Resolves the project name from SHAFT properties. If no custom project name is configured,
     * uses the current test class name.
     *
     * @return the resolved project name
     */
    private static String resolveProjectName() {
        var customProjectName = SHAFT.Properties.browserStack.projectName();
        if (!customProjectName.isEmpty()) {
            return customProjectName;
        }
        return ReportManagerHelper.getTestClassName();
    }

    /**
     * Determines whether the current execution scenario is a native mobile app test.
     *
     * @return {@code true} if an app URL or app file path is configured
     */
    private static boolean isNativeAppExecution() {
        return !SHAFT.Properties.browserStack.appUrl().isEmpty()
                || !SHAFT.Properties.browserStack.appRelativeFilePath().isEmpty();
    }

    /**
     * Adds a key-value pair to the map only if the value is non-null and non-empty.
     *
     * @param map   the map to add the entry to
     * @param key   the key
     * @param value the value to add if non-empty
     */
    private static void addIfNotEmpty(Map<String, Object> map, String key, String value) {
        if (value != null && !value.isEmpty()) {
            map.put(key, value);
        }
    }

    /**
     * Writes the configuration map to a {@code browserstack.yml} file in the project root
     * directory using SnakeYAML with block-style formatting.
     *
     * @param config the configuration map to write
     * @return the absolute path to the written file
     * @throws IllegalStateException if the file cannot be written
     */
    private static String writeYamlFile(Map<String, Object> config) {
        var dumperOptions = new DumperOptions();
        dumperOptions.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
        dumperOptions.setPrettyFlow(true);
        dumperOptions.setIndent(2);
        dumperOptions.setIndicatorIndent(0);

        var yaml = new Yaml(dumperOptions);
        var filePath = System.getProperty("user.dir") + File.separator + "browserstack.yml";

        try (var writer = new FileWriter(filePath)) {
            yaml.dump(config, writer);
        } catch (IOException e) {
            throw new IllegalStateException("Failed to write browserstack.yml at: " + filePath, e);
        }

        return filePath;
    }
}
