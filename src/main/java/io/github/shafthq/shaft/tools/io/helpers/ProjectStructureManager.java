package io.github.shafthq.shaft.tools.io.helpers;

import com.shaft.cli.FileActions;
import com.shaft.tools.io.ReportManager;

import java.nio.file.Paths;

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
        }
        FileActions.getInstance().deleteFile(System.getProperty("appender.file.fileName"));
        System.setProperty("disableLogging", "false");
    }
}
