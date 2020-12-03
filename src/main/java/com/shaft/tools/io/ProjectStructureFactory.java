package com.shaft.tools.io;

import com.shaft.cli.FileActions;

import java.nio.file.Paths;

public class ProjectStructureFactory {
    public static void initialize() {
        if (System.getProperty("executionAddress").trim().equals("local")
                && !FileActions.doesFileExist(System.getProperty("propertiesFolderPath") + "ExecutionPlatform.properties")
                && !Paths.get(System.getProperty("user.dir")).getFileName().toString().equals("SHAFT_Engine")) {
            FileActions.createFolder(System.getProperty("propertiesFolderPath"));
            FileActions.createFolder(System.getProperty("testDataFolderPath"));
            FileActions.createFolder(System.getProperty("testSuiteFolderPath"));
            FileActions.createFolder(System.getProperty("jsonFolderPath"));
            String propertiesFolderPath = PropertyFileManager.getDefaultPropertiesFolderPath();
            if (propertiesFolderPath.contains("file:")) {
                FileActions.copyFolderFromJar(propertiesFolderPath, System.getProperty("propertiesFolderPath"));
            } else {
                FileActions.copyFolder(propertiesFolderPath, System.getProperty("propertiesFolderPath"));
            }
        }
    }
}
