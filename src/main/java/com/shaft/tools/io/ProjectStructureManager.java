package com.shaft.tools.io;

import com.shaft.cli.FileActions;

import java.nio.file.Paths;
import java.util.Arrays;

public class ProjectStructureManager {
    public static void initialize() {
        ReportManager.logDiscrete("Initializing Project Structure...");
        System.setProperty("disableLogging", "true");
        if (System.getProperty("executionAddress").trim().equals("local")
                && !FileActions.getInstance().doesFileExist(System.getProperty("propertiesFolderPath") + "ExecutionPlatform.properties")
                && !Paths.get(System.getProperty("user.dir")).getFileName().toString().equals("SHAFT_Engine")) {
            FileActions.getInstance().createFolder(System.getProperty("propertiesFolderPath"));
            FileActions.getInstance().createFolder(System.getProperty("dynamicObjectRepositoryPath"));
            FileActions.getInstance().createFolder(System.getProperty("testDataFolderPath"));
            String propertiesFolderPath = PropertyFileManager.getDefaultPropertiesFolderPath();
            if (propertiesFolderPath.contains("file:")) {
                FileActions.getInstance().copyFolderFromJar(propertiesFolderPath, System.getProperty("propertiesFolderPath"));
            } else {
                FileActions.getInstance().copyFolder(propertiesFolderPath, System.getProperty("propertiesFolderPath"));
            }
        }
        System.setProperty("disableLogging", "false");
    }

    public static void migrateToNewStructure() {
        var isDiscrete = ReportManagerHelper.getDiscreteLogging();
        ReportManagerHelper.setDiscreteLogging(true);
        if (FileActions.getInstance().doesFileExist("src/test/resources/Properties")) {
            var directoriesToBeMoved = Arrays.asList("src/test/resources/Properties", "src/test/resources/DynamicObjectRepository");
            directoriesToBeMoved.forEach(directoryPath -> {
                if (FileActions.getInstance().doesFileExist(directoryPath)) {
                    FileActions.getInstance().copyFolder(directoryPath, directoryPath.replace("test", "main").replace('P', 'p').replace('D', 'd'));
                    FileActions.getInstance().deleteFile(FileActions.getInstance().getAbsolutePath(directoryPath));
                }
            });
            ReportManager.logDiscrete("*** Kindly note that both your properties and dynamicObjectRepository folders were moved from under src/test/resources to be under src/main/resources ***");
        }
        ReportManagerHelper.setDiscreteLogging(isDiscrete);
    }
}
