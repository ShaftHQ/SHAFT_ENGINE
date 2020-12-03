package com.shaft.tools.io;

import com.shaft.cli.FileActions;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.*;

public class PropertyFileManager {
    private static final String OS_WINDOWS = "Windows-64";
    private static final String OS_LINUX = "Linux-64";
    private static final String OS_MAC = "Mac-64";
    private static final String DEFAULT_PROPERTIES_FOLDER_PATH = "src/main/resources/defaultProperties";
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
     * properties files THEN Base properties files (lowest priority)
     */
    public static synchronized void readPropertyFiles() {
        if (Boolean.TRUE.equals(readPropertyFiles)) {
            // read base system properties
            Properties props = System.getProperties();

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
            String basePropertiesPath = System.getProperty(CUSTOM_PROPERTIES_FOLDER_PROPERTY_NAME);
            readPropertyFiles(Objects.requireNonNullElse(basePropertiesPath, "src/test/resources/Properties"));

            // This section set the default properties values for Execution/path/pattern
            readPropertyFiles(getDefaultPropertiesFolderPath());

            overrideTargetOperatingSystemForLocalExecution();

            manageMaximumPerformanceMode();
            readPropertyFiles = false;
        }
    }

    public static synchronized Map<String, String> getAppiumDesiredCapabilities() {
        Map<String, String> appiumDesiredCapabilities = new HashMap<>();

        Properties props = System.getProperties();
        props.forEach((key, value) -> {
            if (String.valueOf(key).toLowerCase().contains("mobile_")) {
                appiumDesiredCapabilities.put(String.valueOf(key), String.valueOf(value));
            }
        });
        return appiumDesiredCapabilities;
    }

    public static synchronized void readPropertyFiles(String propertiesFolderPath) {
        if (propertiesFolderPath != null) {
            //propertiesFolderPath = propertiesFolderPath.replace("/", File.separator).replace("\\", File.separator);
            ReportManager.logDiscrete("Reading properties directory: " + propertiesFolderPath);
            try {
                Properties properties = new Properties();
                if (propertiesFolderPath.contains(".jar")) {
                    // unpacks default properties to target folder
                    URL url = new URL(propertiesFolderPath.substring(0, propertiesFolderPath.indexOf("!")));
                    FileActions.unpackArchive(url, "target/");
                    propertiesFolderPath = "target/resources/defaultProperties/";
                }
                // reading regular files
                Collection<File> propertiesFilesList;
                if (FileActions.doesFileExist(propertiesFolderPath)) {
                    propertiesFilesList = FileUtils.listFiles(new File(propertiesFolderPath), new String[]{"properties"},
                            true);
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
                ReportManager.log(e);
            }
        }
    }

    // TODO: create directory under src/test/resources and write the default
    public static String getDefaultPropertiesFolderPath() {
        URL propertiesFolder = PropertyFileManager.class.getResource("/resources/defaultProperties/");

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
     * <li>aiPoweredElementIdentification=>false;
     * <li>headlessExecution=>true;
     * <li>autoMaximizeBrowserWindow=>false;
     * <li>forceCheckForElementVisibility=>false;
     * <li>forceCheckElementLocatorIsUnique=>false;
     * <li>screenshotParams_whenToTakeAScreenshot=>FailuresOnly;
     * <li>screenshotParams_highlightElements=>false;
     * <li>screenshotParams_screenshotType=>Regular;
     * <li>screenshotParams_watermark=>false;
     * <li>createAnimatedGif=>false;
     * <li>recordVideo=>false;
     * <li>debugMode"=>"false;
     * </ul>
     */
    private static void manageMaximumPerformanceMode() {
        if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("maximumPerformanceMode")))) {
            // Beast Mode On
            System.setProperty("aiPoweredSelfHealingElementIdentification", String.valueOf(false));
            System.setProperty("headlessExecution", String.valueOf(true));
            System.setProperty("autoMaximizeBrowserWindow", String.valueOf(true));
            System.setProperty("forceCheckForElementVisibility", String.valueOf(false));
            System.setProperty("forceCheckElementLocatorIsUnique", String.valueOf(false));
            System.setProperty("screenshotParams_whenToTakeAScreenshot", "ValidationPointsOnly");
            System.setProperty("screenshotParams_highlightElements", String.valueOf(true));
            System.setProperty("screenshotParams_highlightMethod", "AI");
            System.setProperty("screenshotParams_screenshotType", "Regular");
            System.setProperty("screenshotParams_watermark", String.valueOf(true));
            System.setProperty("createAnimatedGif", String.valueOf(false));
            System.setProperty("recordVideo", String.valueOf(false));
            System.setProperty("debugMode", String.valueOf(false));
        }

    }

    private static void loadPropertiesFileIntoSystemProperties(Properties properties, File propertyFile) {
        try {
            properties.load(new FileInputStream(propertyFile));
            // load properties from the properties file
            properties.putAll(System.getProperties());
            // override properties file with system properties
            System.getProperties().putAll(properties);
            // reset system properties
        } catch (IOException e) {
            ReportManager.log(e);
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