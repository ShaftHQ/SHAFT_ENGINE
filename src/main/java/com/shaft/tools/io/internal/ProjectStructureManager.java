package com.shaft.tools.io.internal;

import com.shaft.cli.FileActions;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.ReportManager;

import java.nio.file.Paths;
import java.util.Arrays;

public class ProjectStructureManager {
    public static void initialize(RunType runType) {
        ReportManager.logDiscrete("Initializing Project Structure...");
        if (Properties.platform.executionAddress().equals("local")
                && !Paths.get(System.getProperty("user.dir")).getFileName().toString().equals("SHAFT_Engine")) {
            FileActions.getInstance(true).createFolder(Properties.paths.properties());
            FileActions.getInstance(true).createFolder(Properties.paths.dynamicObjectRepository());
            FileActions.getInstance(true).createFolder(Properties.paths.testData());
        }
        // manually override listeners configuration
        if (Properties.platform.executionAddress().equals("local")) {
            FileActions.getInstance(true).deleteFolder(Properties.paths.services());
            switch (runType) {
                case JUNIT -> {
                    FileActions.getInstance(true).createFolder(Properties.paths.services());
                    FileActions.getInstance(true).writeToFile(Properties.paths.services(), "org.junit.platform.launcher.LauncherSessionListener", "com.shaft.listeners.JunitListener");
                }
                case TESTNG -> {
                    FileActions.getInstance(true).createFolder(Properties.paths.services());
                    FileActions.getInstance(true).writeToFile(Properties.paths.services(), "org.testng.ITestNGListener", "com.shaft.listeners.TestNGListener");
                }
                case CUCUMBER -> {
                    FileActions.getInstance(true).createFolder(Properties.paths.services());
                    FileActions.getInstance(true).writeToFile(Properties.paths.services(), "io.cucumber.plugin.ConcurrentEventListener", "com.shaft.listeners.CucumberFeatureListener");
                }
            }
            createAllureListenersMetaFiles();
        }
    }

    private static void createAllureListenersMetaFiles() {
        FileActions.getInstance(true).createFolder(com.shaft.properties.internal.Properties.paths.services());
        Arrays.asList("io.qameta.allure.listener.ContainerLifecycleListener", "io.qameta.allure.listener.FixtureLifecycleListener",
                "io.qameta.allure.listener.StepLifecycleListener", "io.qameta.allure.listener.TestLifecycleListener").forEach(fileName -> FileActions.getInstance(true).writeToFile(Properties.paths.services(), fileName, "com.shaft.listeners.AllureListener"));
    }

    public enum RunType {TESTNG, JUNIT, CUCUMBER}
}
