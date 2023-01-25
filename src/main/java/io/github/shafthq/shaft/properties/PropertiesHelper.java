package io.github.shafthq.shaft.properties;

import com.shaft.cli.FileActions;
import com.shaft.tools.io.ReportManager;
import io.github.shafthq.shaft.enums.OperatingSystems;
import org.aeonbits.owner.ConfigFactory;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;

import java.io.File;
import java.net.URL;
import java.util.Arrays;

public class PropertiesHelper {
    private static final String DEFAULT_PROPERTIES_FOLDER_PATH = "src/main/resources/properties/default";
    private static final String TARGET_PROPERTIES_FOLDER_PATH = DEFAULT_PROPERTIES_FOLDER_PATH.replace("/default", "");

    public static void initialize() {
        //initialize default properties
        initializeDefaultProperties();
        //attach property files
        attachPropertyFiles();

        loadProperties();

        //TODO: replace and remove legacy properties loader
        PropertyFileManager.readPropertyFiles();
    }

    public static void loadProperties() {
        //load property objects
        //TODO: implement missing property interfaces
        Properties.paths = ConfigFactory.create(Paths.class);
        Properties.platform = ConfigFactory.create(Platform.class);
        Properties.web = ConfigFactory.create(Web.class);
        Properties.mobile = ConfigFactory.create(Mobile.class);
        Properties.browserStack = ConfigFactory.create(BrowserStack.class);

        //TODO: post-processing based on loaded properties
        postProcessing();
    }

    private static void postProcessing() {
        overrideTargetOperatingSystemForLocalExecution();
        setMobilePlatform();
    }

    private static void overrideTargetOperatingSystemForLocalExecution() {
        if (Properties.platform.executionAddress().equals("local")) {
            if (SystemUtils.IS_OS_WINDOWS) {
                Properties.platform.set().targetOperatingSystem(OperatingSystems.WINDOWS);
            } else if (SystemUtils.IS_OS_LINUX) {
                Properties.platform.set().targetOperatingSystem(OperatingSystems.LINUX);
            } else if (SystemUtils.IS_OS_MAC) {
                Properties.platform.set().targetOperatingSystem(OperatingSystems.MACOS);
            }
        }
    }

    public static void setMobilePlatform() {
        String targetOperatingSystem = Properties.platform.targetOperatingSystem();
        if (Arrays.asList("Android", "iOS").contains(targetOperatingSystem)) {
            Properties.mobile.set().platformName(Properties.platform.targetOperatingSystem().toLowerCase());
        }
    }

    private static void initializeDefaultProperties() {
        //  https://www.selenium.dev/blog/2022/using-java11-httpclient/
        System.setProperty("webdriver.http.factory", "jdk-http-client");

        URL propertiesFolder = PropertyFileManager.class.getResource(DEFAULT_PROPERTIES_FOLDER_PATH.replace("src/main", "") + "/");
        var propertiesFolderPath = "";
        if (propertiesFolder != null) {
            propertiesFolderPath = propertiesFolder.getFile();
        } else {
            propertiesFolderPath = DEFAULT_PROPERTIES_FOLDER_PATH;
        }

        // always override default properties
        if (propertiesFolderPath.contains("file:")) {
            FileActions.getInstance().copyFolderFromJar(propertiesFolderPath, DEFAULT_PROPERTIES_FOLDER_PATH);
        } else {
            FileActions.getInstance().copyFolder(propertiesFolderPath, DEFAULT_PROPERTIES_FOLDER_PATH);
        }

        // override target properties only if they do not exist
        if (!FileActions.getInstance().doesFileExist(TARGET_PROPERTIES_FOLDER_PATH + "/path.properties")) {
            if (propertiesFolderPath.contains("file:")) {
                FileActions.getInstance().copyFolderFromJar(propertiesFolderPath, TARGET_PROPERTIES_FOLDER_PATH);
            } else {
                FileActions.getInstance().copyFolder(propertiesFolderPath, TARGET_PROPERTIES_FOLDER_PATH);
            }
        }
    }

    private static void attachPropertyFiles() {
        ReportManager.logDiscrete("Reading properties directory: " + TARGET_PROPERTIES_FOLDER_PATH);
        FileUtils.listFiles(new File(TARGET_PROPERTIES_FOLDER_PATH), new String[]{"properties"},
                false).forEach(propertyFile -> ReportManager.logDiscrete("Loading properties file: " + propertyFile));
    }
}
