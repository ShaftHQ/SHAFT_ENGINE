package com.shaft.tools.io.internal;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.ReportManager;

import java.nio.file.Paths;

public class ProjectStructureManager {
    public static void initialize(RunType runType) {
        ReportManager.logDiscrete("Initializing Project Structure...");
        SHAFT.Properties.reporting.set().disableLogging(true);
        if (Properties.platform.executionAddress().equals("local")
                && !Paths.get(System.getProperty("user.dir")).getFileName().toString().equals("SHAFT_Engine")) {
            FileActions.getInstance().createFolder(Properties.paths.properties());
            FileActions.getInstance().createFolder(Properties.paths.dynamicObjectRepository());
            FileActions.getInstance().createFolder(Properties.paths.testData());
        }

        // manually override listeners configuration
        if (Properties.platform.executionAddress().equals("local")) {
            FileActions.getInstance().deleteFolder(Properties.paths.services());
            switch (runType) {
                case JUNIT -> {
                    FileActions.getInstance().createFolder(Properties.paths.services());
                    FileActions.getInstance().writeToFile(Properties.paths.services(), "org.junit.platform.launcher.LauncherSessionListener", "com.shaft.listeners.JunitListener");
                }
                case TESTNG -> {
                    FileActions.getInstance().createFolder(Properties.paths.services());
                    FileActions.getInstance().writeToFile(Properties.paths.services(), "org.testng.ITestNGListener", "com.shaft.listeners.TestNGListener");
                }
                case CUCUMBER -> {
                    FileActions.getInstance().createFolder(Properties.paths.services());
                    FileActions.getInstance().writeToFile(Properties.paths.services(), "io.cucumber.plugin.ConcurrentEventListener", "com.shaft.listeners.CucumberFeatureListener");
                }
            }
        }
        // delete previous run execution log
        FileActions.getInstance().deleteFile(System.getProperty("appender.file.fileName"));
        SHAFT.Properties.reporting.set().disableLogging(false);
    }

    public enum RunType {TESTNG, JUNIT, CUCUMBER}
}
