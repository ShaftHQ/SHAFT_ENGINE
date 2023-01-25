package io.github.shafthq.shaft.properties;

import com.shaft.cli.FileActions;
import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import io.github.shafthq.shaft.enums.Browsers;
import io.github.shafthq.shaft.tools.io.helpers.ReportManagerHelper;
import lombok.Getter;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;
import org.openqa.selenium.MutableCapabilities;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class PropertyFileManager {
    private static final String OS_WINDOWS = "Windows-64";
    private static final String OS_LINUX = "Linux-64";
    private static final String OS_MAC = "Mac-64";
    private static final String DEFAULT_PROPERTIES_FOLDER_PATH = "src/main/resources/properties/default";
    @Getter
    private static final String CUSTOM_PROPERTIES_FOLDER_PATH = "src/main/resources/properties";
    private static final String CUSTOM_PROPERTIES_FOLDER_PROPERTY_NAME = "propertiesFolderPath";
    private static Boolean readPropertyFiles = true;

    private PropertyFileManager() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Reads properties from all system variables that contain the word
     * propertiesFolderPath, enables reading properties from multiple folders
     * following this naming convention
     * <p>
     * Priorities follow this order: MVN system properties + pom.xml THEN Explicit
     * properties files THEN Base properties files (the lowest priority)
     */
    public static void readPropertyFiles() {
        if (Boolean.TRUE.equals(readPropertyFiles)) {
            var isDiscrete = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);

            // read base system properties
            java.util.Properties props = System.getProperties();

            // read properties from any explicit properties files
            for (int i = 0; i < props.size(); i++) {
                String propertyKey = ((String) (props.keySet().toArray())[i]).trim();
                if (propertyKey.contains(CUSTOM_PROPERTIES_FOLDER_PROPERTY_NAME)
                        && !propertyKey.equals(CUSTOM_PROPERTIES_FOLDER_PROPERTY_NAME)
                        && !props.getProperty(propertyKey).trim().equals("")) {
                    readPropertyFiles(props.getProperty(propertyKey));
                }
            }

            // read properties form the base properties file
            String basePropertiesPath = Properties.paths.properties();
            readPropertyFiles(Objects.requireNonNullElse(basePropertiesPath, CUSTOM_PROPERTIES_FOLDER_PATH));

            // This section set the default properties values for Execution/path/pattern
            readPropertyFiles(getDefaultPropertiesFolderPath());

            overrideTargetOperatingSystemForLocalExecution();
            manageMaximumPerformanceMode();

            manageSafariBrowser();

            readPropertyFiles = false;
            ReportManagerHelper.setDiscreteLogging(isDiscrete);
        }
    }

    private static void manageSafariBrowser() {
        if (SHAFT.Properties.web.targetBrowserName().equals(Browsers.SAFARI)) {
            System.setProperty("screenshotParams_screenshotType", "element");
        }
    }

    public static Map<String, String> getAppiumDesiredCapabilities() {
        Map<String, String> appiumDesiredCapabilities = new HashMap<>();

        java.util.Properties props = System.getProperties();
        props.forEach((key, value) -> {
            if (String.valueOf(key).toLowerCase().contains("mobile_")) {
                appiumDesiredCapabilities.put(String.valueOf(key), String.valueOf(value));
            }
        });
        var app = appiumDesiredCapabilities.get("mobile_app");
        if (app != null && !app.isEmpty() &&
                (app.startsWith("src\\") || app.startsWith("src/"))) {
            appiumDesiredCapabilities.put("mobile_app", FileActions.getInstance().getAbsolutePath(app));
        }
        return appiumDesiredCapabilities;
    }

    public static MutableCapabilities getCustomWebdriverDesiredCapabilities() {
        MutableCapabilities customDriverOptions = new MutableCapabilities();
        java.util.Properties props = System.getProperties();
        props.forEach((key, value) -> {
            if (String.valueOf(key).toLowerCase().startsWith("capabilities.") && !String.valueOf(value).isBlank()) {
                customDriverOptions.setCapability(String.valueOf(key).split("capabilities.")[1], String.valueOf(value));
            }
        });
        return customDriverOptions;
    }

    private static void readPropertyFiles(String propertiesFolderPath) {
        if (propertiesFolderPath != null) {
            ReportManager.logDiscrete("Reading properties directory: " + propertiesFolderPath);
            try {
                java.util.Properties properties = new java.util.Properties();
                if (propertiesFolderPath.contains(".jar")) {
                    // unpacks default properties to target folder
                    URL url = new URL(propertiesFolderPath.substring(0, propertiesFolderPath.indexOf("!")));
                    FileActions.getInstance().unpackArchive(url, "target/");
                    propertiesFolderPath = "target/resources/properties/default/";
                }
                // reading regular files
                Collection<File> propertiesFilesList;
                if (FileActions.getInstance().doesFileExist(propertiesFolderPath)) {
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

    public static String getDefaultPropertiesFolderPath() {
        URL propertiesFolder = PropertyFileManager.class.getResource("/resources/properties/default/");

        if (propertiesFolder != null) {
            return propertiesFolder.getFile();
        } else {
            return DEFAULT_PROPERTIES_FOLDER_PATH;
        }
    }

    /**
     * When Maximum Performance Mode is enabled the following properties will be
     * overridden:
     * <p>
     * <ul>
     * <li>aiPoweredSelfHealingElementIdentification=>false;
     * <li>headlessExecution=>true;
     * <li>autoMaximizeBrowserWindow=>false;
     * <li>forceCheckForElementVisibility=>false;
     * <li>forceCheckElementLocatorIsUnique=>false;
     * <li>screenshotParams_whenToTakeAScreenshot=>FailuresOnly;
     * <li>screenshotParams_highlightElements=>false;
     * <li>screenshotParams_screenshotType=>Regular;
     * <li>screenshotParams_watermark=>true;
     * <li>createAnimatedGif=>false;
     * <li>recordVideo=>false;
     * <li>debugMode"=>"false;
     * </ul>
     */
    private static void manageMaximumPerformanceMode() {
        String maximumPerformanceMode = System.getProperty("maximumPerformanceMode");
        switch (maximumPerformanceMode) {
            case "true", "1", "2" -> {
                System.setProperty("aiPoweredSelfHealingElementIdentification", String.valueOf(false));
                System.setProperty("autoMaximizeBrowserWindow", String.valueOf(true));
                System.setProperty("screenshotParams_whenToTakeAScreenshot", "ValidationPointsOnly");
                System.setProperty("screenshotParams_highlightElements", String.valueOf(true));
                System.setProperty("screenshotParams_highlightMethod", "AI");
                System.setProperty("screenshotParams_screenshotType", "Regular");
                System.setProperty("screenshotParams_watermark", String.valueOf(true));
                System.setProperty("createAnimatedGif", String.valueOf(false));
                System.setProperty("videoParams_recordVideo", String.valueOf(false));
                System.setProperty("debugMode", String.valueOf(false));
                System.setProperty("captureClickedElementText", String.valueOf(false));
                System.setProperty("headlessExecution", String.valueOf(false));
                if (maximumPerformanceMode.equals("2") && !DriverFactory.DriverType.DESKTOP_SAFARI.getValue().equals(System.getProperty("targetBrowserName"))) System.setProperty("headlessExecution", String.valueOf(true));
            }
            case "false", "0" -> {
            }
        }
    }

    private static void loadPropertiesFileIntoSystemProperties(java.util.Properties properties, File propertyFile) {
        try {
            properties.load(new FileInputStream(propertyFile));
            // load properties from the properties file
            properties.putAll(System.getProperties());
            // override properties file with system properties
            System.getProperties().putAll(properties);
            // reset system properties
        } catch (IOException e) {
            ReportManagerHelper.logDiscrete(e);
        }
    }

    private static void overrideTargetOperatingSystemForLocalExecution() {
        String targetOperatingSystemPropertyName = "targetOperatingSystem";
        if (System.getProperty("executionAddress").trim().equals("local")) {
            if (SystemUtils.IS_OS_WINDOWS) {
                System.setProperty(targetOperatingSystemPropertyName, OS_WINDOWS);
            } else if (SystemUtils.IS_OS_LINUX) {
                System.setProperty(targetOperatingSystemPropertyName, OS_LINUX);
            } else if (SystemUtils.IS_OS_MAC) {
                System.setProperty(targetOperatingSystemPropertyName, OS_MAC);
            }
        }
    }
}