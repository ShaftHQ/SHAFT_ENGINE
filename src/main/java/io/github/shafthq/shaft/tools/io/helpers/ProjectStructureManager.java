package io.github.shafthq.shaft.tools.io.helpers;

import com.shaft.cli.FileActions;
import com.shaft.tools.io.ReportManager;
import io.github.shafthq.shaft.properties.Properties;

import java.nio.file.Paths;

public class ProjectStructureManager {
    public static void initialize() {
        ReportManager.logDiscrete("Initializing Project Structure...");
        System.setProperty("disableLogging", "true");
        if (Properties.platform.executionAddress().trim().equals("local")
                && !FileActions.getInstance().doesFileExist(Properties.paths.properties() + "ExecutionPlatform.properties")
                && !Paths.get(System.getProperty("user.dir")).getFileName().toString().equals("SHAFT_Engine")) {
            FileActions.getInstance().createFolder(Properties.paths.properties());
            FileActions.getInstance().createFolder(Properties.paths.dynamicObjectRepository());
            FileActions.getInstance().createFolder(Properties.paths.testData());
        }
        // delete previous run execution log
        FileActions.getInstance().deleteFile(System.getProperty("appender.file.fileName"));
        System.setProperty("disableLogging", "false");
    }
}
