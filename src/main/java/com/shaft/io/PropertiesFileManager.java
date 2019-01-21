package com.shaft.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collection;
import java.util.Properties;

import org.apache.commons.io.FileUtils;

public class PropertiesFileManager {
    private PropertiesFileManager() {
	throw new IllegalStateException("Utility class");
    }

    public static void readPropertyFiles() {
	try {
	    Properties properties = new Properties();
	    Collection<File> propertiesFilesList;
	    propertiesFilesList = FileUtils.listFiles(new File(System.getProperty("propertiesFolderPath")),
		    new String[] { "properties" }, true);
	    propertiesFilesList.forEach(propertyFile -> {
		try {
		    properties.load(new FileInputStream(propertyFile));
		    properties.putAll(System.getProperties());
		    System.getProperties().putAll(properties);
		} catch (IOException e) {
		    ReportManager.log(e);
		}
	    });
	} catch (Exception e) {
	    ReportManager.log(e);
	}
    }
}