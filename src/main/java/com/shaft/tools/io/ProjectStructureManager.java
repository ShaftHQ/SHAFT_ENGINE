package com.shaft.tools.io;

import com.shaft.cli.FileActions;

import java.nio.file.Paths;
import java.util.Arrays;

public class ProjectStructureManager {
    public static void initialize() {
        ReportManager.logDiscrete("Initializing Project Structure...");
        System.setProperty("disableLogging", "true");
        if (System.getProperty("executionAddress").trim().equals("local")
                && !FileActions.doesFileExist(System.getProperty("propertiesFolderPath") + "ExecutionPlatform.properties")
                && !Paths.get(System.getProperty("user.dir")).getFileName().toString().equals("SHAFT_Engine")) {
            FileActions.createFolder(System.getProperty("propertiesFolderPath"));
            FileActions.createFolder(System.getProperty("dynamicObjectRepositoryPath"));
            FileActions.createFolder(System.getProperty("testDataFolderPath"));
            String propertiesFolderPath = PropertyFileManager.getDefaultPropertiesFolderPath();
            if (propertiesFolderPath.contains("file:")) {
                FileActions.copyFolderFromJar(propertiesFolderPath, System.getProperty("propertiesFolderPath"));
            } else {
                FileActions.copyFolder(propertiesFolderPath, System.getProperty("propertiesFolderPath"));
            }
        }
        System.setProperty("disableLogging", "false");
    }

    public static void migrateToNewStructure(){
        var isDiscrete = ReportManagerHelper.getDiscreteLogging();
        ReportManagerHelper.setDiscreteLogging(true);
        if (FileActions.doesFileExist("src/test/resources/Properties")) {
            var directoriesToBeMoved = Arrays.asList("src/test/resources/Properties", "src/test/resources/DynamicObjectRepository");
            directoriesToBeMoved.forEach(directoryPath -> {
                if (FileActions.doesFileExist(directoryPath)) {
                    FileActions.copyFolder(directoryPath, directoryPath.replace("test", "main").replace('P', 'p').replace('D', 'd'));
                    FileActions.deleteFile(FileActions.getAbsolutePath(directoryPath));
                }
            });
            ReportManager.logDiscrete("*** Kindly note that both your properties and dynamicObjectRepository folders were moved from under src/test/resources to be under src/main/resources ***");
        }
        ReportManagerHelper.setDiscreteLogging(isDiscrete);
    }
}
