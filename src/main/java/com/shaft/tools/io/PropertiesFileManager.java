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
	// read properties from any explicit properties files
	Properties props = System.getProperties();
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
	setDefaultExecutionProperties();
	setPathProperties();
	setPatternProperties();

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
    /**
     * This method set Engine execution default properties
     */
    private static void setDefaultExecutionProperties() {

	System.setProperty("executionAddress", "local");
	// Platform
	// local | seleniumGridHubIP:port
	System.setProperty("targetOperatingSystem", "Windows-64");
	// Windows-64 | Linux-64 | Mac-64
	// Note: Will be ignored in case of local execution and SHAFT will identify the
	// correct OS version automatically
	System.setProperty("targetBrowserName", "GoogleChrome");
	// MozillaFirefox | MicrosoftInternetExplorer | GoogleChrome | MicrosoftEdge |
	// Safari
	System.setProperty("headlessExecution", "false");
	// true | false, This only works for chrome/Firefox
	System.setProperty("browserObjectSingleton", "true");
	// true | false, This makes sure that every time you attempt to open a browser,
	// all other instances will be closed
	//
	////////// Platform Flags and Timeouts
	System.setProperty("browserNavigationTimeout", "30");
	// Timeout in seconds to be used if navigating to a new URL (1 minute = 60
	// seconds)
	System.setProperty("defaultElementIdentificationTimeout", "5");
	// Accepts integer values that represent the default timeout for finding a
	// webElement
	System.setProperty("attemptsBeforeThrowingElementNotFoundException", "5");
	// Accepts integer values that represent the number of attempts before failing
	// to find a webElement
	System.setProperty("shellSessionTimeout", "30");
	// Timeout in seconds to be used if creating any kind of shell session (1 minute
	// = 60 seconds), should be greater than or equal to the docker timeout in case
	// of dockerized execution
	System.setProperty("dockerCommandTimeout", "30");
	// Timeout in seconds to be used if executing a command inside a docker (1
	// minute = 60 seconds)
	System.setProperty("databaseLoginTimeout", "30");
	// Timeout in seconds to be used when attempting to login to a database (1
	// minute = 60 seconds)
	System.setProperty("databaseNetworkTimeout", "60");
	// Timeout in seconds to be used when attempting to connect to a database (1
	// minute = 60 seconds)
	System.setProperty("databaseQueryTimeout", "60");
	// Timeout in seconds to be used when attempting to execute a query on a
	// database (1 minute = 60 seconds)
	System.setProperty("autoMaximizeBrowserWindow", "true");
	// true | false
	System.setProperty("forceCheckForElementVisibility", "true");
	// true | false
	System.setProperty("waitImplicitly", "false");
	// true | false
	// Note: Implicit waiting may increase execution time by 20% but it also
	// increases test stability in flaky environments
	//
	////////// Screen-shot/AnimatedGif/Video Parameters
	System.setProperty("screenshotParams_whenToTakeAScreenshot", "ValidationPointsOnly");
	// Always | Never | ValidationPointsOnly | FailuresOnly
	System.setProperty("screenshotParams_highlightElements", "true");
	// true | false
	System.setProperty("screenshotParams_screenshotType", "Regular");
	// Regular | FullPage | Element
	System.setProperty("screenshotParams_skippedElementsFromScreenshot", "");
	// The above element will be skipped in case of persistent elements/menus that
	// show up multiple times in the full page screenshots
	System.setProperty("screenshotParams_watermark", "true");
	// true | false
	System.setProperty("screenshotParams_watermarkOpacity", "0.2");
	// a number between 0 and 1.0 where 0 means invisible and 1.0 means 100% visible
	System.setProperty("createAnimatedGif", "true");
	// true | false
	System.setProperty("animatedGif_frameDelay", "500");
	// Time in milliseconds to delay the frames of the animated GIF, default is 500
	// millisecond
	System.setProperty("recordVideo", "false");
	// This only works for local execution
	System.setProperty("aiSupportedElementIdentification", "false");
	// true | false
	// Note: this is an experimental feature
	//
	////////// Logging/Reporting Parameters
	System.setProperty("alwaysLogDiscreetly", "false");
	// true | false
	System.setProperty("debugMode", "false");
	// true | false
	System.setProperty("automaticallyGenerateAllureReport", "false");
	// true | false

    }

    /**
     * Set Engine Path Properties
     */
    private static void setPathProperties() {

	System.setProperty("testDataFolderPath", "src/test/resources/TestDataFiles/");
	System.setProperty("testSuiteFolderPath", "src/test/resources/TestSuites/");
	System.setProperty("jsonFolderPath", "src/test/resources/TestJsonFiles/");
	System.setProperty("watermarkImagePath", "src/main/resources/images/shaft.png");
	System.setProperty("downloadsFolderPath", "target/downloadedFiles/");
	System.setProperty("allureResultsFolderPath", "allure-results/");

    }

    /**
     * Set Engine Pattern Properties
     */
    private static void setPatternProperties() {

	System.setProperty("testDataColumnNamePrefix", "Data");
	System.setProperty("allure.link.issue.pattern", "");
	System.setProperty("allure.link.tms.pattern", "");

    }

}