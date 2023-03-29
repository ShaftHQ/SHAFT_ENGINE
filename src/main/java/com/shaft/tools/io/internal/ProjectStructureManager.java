package com.shaft.tools.io.internal;

import com.shaft.cli.FileActions;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.ReportManager;

import java.nio.file.Paths;

public class ProjectStructureManager {
    public static void initialize() {
        ReportManager.logDiscrete("Initializing Project Structure...");
        System.setProperty("disableLogging", "true");
        if (Properties.platform.executionAddress().equals("local")
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
