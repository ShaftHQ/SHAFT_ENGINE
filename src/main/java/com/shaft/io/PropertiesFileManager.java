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

    public static void readPropertyFiles() {
	readPropertyFiles(System.getProperty("propertiesFolderPath"));
    }

    public static void readPropertyFiles(String customFolderPath) {
	try {
	    Properties properties = new Properties();
	    Collection<File> propertiesFilesList;
	    propertiesFilesList = FileUtils.listFiles(new File(customFolderPath), new String[] { "properties" }, true);
	    propertiesFilesList.forEach(propertyFile -> {
		try {
		    properties.load(new FileInputStream(propertyFile));
		    properties.putAll(System.getProperties());
		    System.getProperties().putAll(properties);
		} catch (IOException e) {
		    // ReportManager.log(e);
		}
	    });
	    overrideTargetOperatingSystemForLocalExecution();
	} catch (Exception e) {
	    // ReportManager.log(e);
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