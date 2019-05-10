package com.shaft.io;

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
	    if (propertyKey.contains("propertiesFolderPath") && !propertyKey.equals("propertiesFolderPath")) {
		readPropertyFiles(props.getProperty(propertyKey));
	    }
	}

	// read properties form the base properties file
	readPropertyFiles(System.getProperty("propertiesFolderPath"));
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
	    overrideTargetOperatingSystemForLocalExecution();
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
}