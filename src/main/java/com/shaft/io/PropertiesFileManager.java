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
     * Priorities follow this order: Explicit Properties File 2 > Explicit
     * Properties File 1 > Main Properties File > pom.xml
     * 
     */
    public static void readPropertyFiles() {
	readPropertyFiles(System.getProperty("propertiesFolderPath"));
	Properties props = System.getProperties();

	for (int i = 0; i < props.size(); i++) {
	    String propertyKey = ((String) (props.keySet().toArray())[i]).trim();
	    if (propertyKey.contains("propertiesFolderPath") && !propertyKey.equals("propertiesFolderPath")) {
		readPropertyFiles(props.getProperty(propertyKey));
	    }
	}

//	// replace it with anything else other than foreach
//	props.forEach((propertyKey, propertyValue) -> {
//	    if (String.valueOf(propertyKey).trim().contains("propertiesFolderPath")
//		    && !String.valueOf(propertyKey).trim().equals("propertiesFolderPath")) {
//		readPropertyFiles(String.valueOf(propertyValue).trim());
//	    }
//	});

//	ReportManager.attachSystemProperties();
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
		try {
		    properties.putAll(System.getProperties()); // set system properties from the main properties file
		    properties.load(new FileInputStream(propertyFile));
		    // override the current system properties with the alternate properties files
		    System.getProperties().putAll(properties);
		} catch (IOException e) {
		    ReportManager.log(e);
		}
	    }

//	    propertiesFilesList.forEach(propertyFile -> {
//		try {
//		    properties.load(new FileInputStream(propertyFile));
//		    properties.putAll(System.getProperties());
//		    System.getProperties().putAll(properties);
//		} catch (IOException e) {
//		    // do nothing
//		}
//	    });
	    overrideTargetOperatingSystemForLocalExecution();
	} catch (Exception e) {
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