package com.shaft.tools.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collection;
import java.util.Properties;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;

public class PropertiesFileManager {
    // supported operating systems
    private static final String OS_WINDOWS = "Windows-64";
    private static final String OS_LINUX = "Linux-64";
    private static final String OS_MAC = "Mac-64";

    private PropertiesFileManager() {
	throw new IllegalStateException("Utility class");
    }

    /**
     * Reads properties from all system variables that contain the word
     * propertiesFolderPath, enables reading properties from multiple folders
     * following this naming convention
     * 
     * Priorities follow this order: MVN system properties + pom.xml THEN Explicit
     * properties files THEN Base properties files (lowest priority)
     * 
     */
    public static void readPropertyFiles() {
	// read base system properties
	Properties props = System.getProperties();

	// read properties from any explicit properties files
	for (int i = 0; i < props.size(); i++) {
	    String propertyKey = ((String) (props.keySet().toArray())[i]).trim();
	    if (propertyKey.contains("propertiesFolderPath") && !propertyKey.equals("propertiesFolderPath")
		    && !props.getProperty(propertyKey).trim().equals("")) {
		readPropertyFiles(props.getProperty(propertyKey));
	    }
	}

	// read properties form the base properties file
	readPropertyFiles(System.getProperty("propertiesFolderPath"));

	// This section set the default properties values for Execution/path/pattern
	setDefaultProperties();

	overrideTargetOperatingSystemForLocalExecution();
    }

    private static void readPropertyFiles(String propertiesFolderPath) {
	try {
	    Properties properties = new Properties();
	    Collection<File> propertiesFilesList;
	    propertiesFilesList = FileUtils.listFiles(new File(propertiesFolderPath), new String[] { "properties" },
		    true);

	    File propertyFile;
	    for (int i = 0; i < propertiesFilesList.size(); i++) {
		propertyFile = (File) (propertiesFilesList.toArray())[i];
		loadPropertiesFileIntoSystemProperties(properties, propertyFile);
	    }
	} catch (Exception e) {
	    ReportManager.log(e);
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
	if (System.getProperty("executionAddress").trim().equals("local")) {
	    if (SystemUtils.IS_OS_WINDOWS) {
		System.setProperty("targetOperatingSystem", OS_WINDOWS);
	    } else if (SystemUtils.IS_OS_LINUX) {
		System.setProperty("targetOperatingSystem", OS_LINUX);
	    } else if (SystemUtils.IS_OS_MAC) {
		System.setProperty("targetOperatingSystem", OS_MAC);
	    }
	}
    }

    // TODO: add list of supported properties, and add link to hosted docs in the
    // provided sample properties file

    private static void setDefaultProperties() {
	Properties properties = new Properties();
	// read default properties
	properties.putAll(setDefaultExecutionProperties());
	properties.putAll(setPathProperties());
	properties.putAll(setPatternProperties());

	// override default properties with current system properties in the properties
	// object
	properties.putAll(System.getProperties());

	// set new prioritized properties
	System.getProperties().putAll(properties);
    }

    /**
     * This method set Engine execution default properties
     */
    private static Properties setDefaultExecutionProperties() {
	Properties properties = new Properties();
	properties.put("executionAddress", "local");
	// Platform
	// local | seleniumGridHubIP:port
	properties.put("targetOperatingSystem", "Windows-64");
	// Windows-64 | Linux-64 | Mac-64
	// Note: Will be ignored in case of local execution and SHAFT will identify the
	// correct OS version automatically
	properties.put("targetBrowserName", "GoogleChrome");
	// MozillaFirefox | MicrosoftInternetExplorer | GoogleChrome | MicrosoftEdge |
	// Safari
	properties.put("headlessExecution", "false");
	// true | false, This only works for chrome/Firefox
	properties.put("browserObjectSingleton", "true");
	// true | false, This makes sure that every time you attempt to open a browser,
	// all other instances will be closed
	//
	////////// Platform Flags and Timeouts
	properties.put("browserNavigationTimeout", "30");
	// Timeout in seconds to be used if navigating to a new URL (1 minute = 60
	// seconds)
	properties.put("defaultElementIdentificationTimeout", "5");
	// Accepts integer values that represent the default timeout for finding a
	// webElement
	properties.put("attemptsBeforeThrowingElementNotFoundException", "5");
	// Accepts integer values that represent the number of attempts before failing
	// to find a webElement
	properties.put("shellSessionTimeout", "30");
	// Timeout in seconds to be used if creating any kind of shell session (1 minute
	// = 60 seconds), should be greater than or equal to the docker timeout in case
	// of dockerized execution
	properties.put("dockerCommandTimeout", "30");
	// Timeout in seconds to be used if executing a command inside a docker (1
	// minute = 60 seconds)
	properties.put("databaseLoginTimeout", "30");
	// Timeout in seconds to be used when attempting to login to a database (1
	// minute = 60 seconds)
	properties.put("databaseNetworkTimeout", "60");
	// Timeout in seconds to be used when attempting to connect to a database (1
	// minute = 60 seconds)
	properties.put("databaseQueryTimeout", "60");
	// Timeout in seconds to be used when attempting to execute a query on a
	// database (1 minute = 60 seconds)
	properties.put("autoMaximizeBrowserWindow", "true");
	// true | false
	properties.put("forceCheckForElementVisibility", "true");
	// true | false
	properties.put("waitImplicitly", "false");
	// true | false
	// Note: Implicit waiting may increase execution time by 20% but it also
	// increases test stability in flaky environments
	//
	////////// Screen-shot/AnimatedGif/Video Parameters
	properties.put("screenshotParams_whenToTakeAScreenshot", "ValidationPointsOnly");
	// Always | Never | ValidationPointsOnly | FailuresOnly
	properties.put("screenshotParams_highlightElements", "true");
	// true | false
	properties.put("screenshotParams_screenshotType", "Regular");
	// Regular | FullPage | Element
	properties.put("screenshotParams_skippedElementsFromScreenshot", "");
	// The above element will be skipped in case of persistent elements/menus that
	// show up multiple times in the full page screenshots
	properties.put("screenshotParams_watermark", "true");
	// true | false
	properties.put("screenshotParams_watermarkOpacity", "0.2");
	// a number between 0 and 1.0 where 0 means invisible and 1.0 means 100% visible
	properties.put("createAnimatedGif", "true");
	// true | false
	properties.put("animatedGif_frameDelay", "500");
	// Time in milliseconds to delay the frames of the animated GIF, default is 500
	// millisecond
	properties.put("recordVideo", "false");
	// This only works for local execution
	properties.put("aiSupportedElementIdentification", "false");
	// true | false
	// Note: this is an experimental feature
	//
	////////// Logging/Reporting Parameters
	properties.put("alwaysLogDiscreetly", "false");
	// true | false
	properties.put("debugMode", "false");
	// true | false
	properties.put("automaticallyCleanAllureResultsDirectoryBeforeExecution", "false");
	// true | false
	properties.put("automaticallyGenerateAllureReport", "false");
	// true | false

	return properties;
    }

    /**
     * Set Engine Path Properties
     */
    private static Properties setPathProperties() {
	Properties properties = new Properties();
	properties.put("testDataFolderPath", "src/test/resources/TestDataFiles/");
	properties.put("testSuiteFolderPath", "src/test/resources/TestSuites/");
	properties.put("jsonFolderPath", "src/test/resources/TestJsonFiles/");
	properties.put("watermarkImagePath", "/images/shaft.png");
	properties.put("downloadsFolderPath", "target/downloadedFiles/");
	properties.put("allureResultsFolderPath", "allure-results/");
	return properties;
    }

    /**
     * Set Engine Pattern Properties
     */
    private static Properties setPatternProperties() {
	Properties properties = new Properties();
	properties.put("testDataColumnNamePrefix", "Data");
	properties.put("allure.link.issue.pattern", "");
	properties.put("allure.link.tms.pattern", "");
	return properties;
    }

}