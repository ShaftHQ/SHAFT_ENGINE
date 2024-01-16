package com.shaft.properties.internal;

import com.shaft.cli.FileActions;
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
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public final class PropertyFileManager {

    @Getter
    private static final String CUSTOM_PROPERTIES_FOLDER_PATH = "src/main/resources/properties";

    private PropertyFileManager() {
        throw new IllegalStateException("Utility class");
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
            appiumDesiredCapabilities.put("mobile_app", FileActions.getInstance(true).getAbsolutePath(app));
        }
        return appiumDesiredCapabilities;
    }

    public static MutableCapabilities getCustomWebDriverDesiredCapabilities() {
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
            // load properties from the properties file
            properties.putAll(System.getProperties());
            // override properties file with system properties
            System.getProperties().putAll(properties);
            // reset system properties
        } catch (IOException e) {
            ReportManagerHelper.logDiscrete(e);
        }
    }

    public static void readCustomPropertyFiles() {
        readPropertyFiles(Objects.requireNonNullElse(Properties.paths.properties(), CUSTOM_PROPERTIES_FOLDER_PATH));
    }
}