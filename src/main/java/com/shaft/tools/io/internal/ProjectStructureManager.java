package com.shaft.tools.io.internal;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.ReportManager;

import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class ProjectStructureManager {
    public static void initialize() {
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
            FileActions.getInstance().createFolder(Properties.paths.services());
            var junitServices = List.of("org.junit.platform.launcher.LauncherSessionListener");
            junitServices.forEach(service -> FileActions.getInstance().writeToFile(Properties.paths.services(), service, "com.shaft.listeners.JunitListener"));
            var testNgServices = Arrays.asList("org.testng.IAlterSuiteListener", "org.testng.IAnnotationTransformer", "org.testng.IExecutionListener"
                    , "org.testng.IInvokedMethodListener", "org.testng.ISuiteListener", "org.testng.ITestListener");
            testNgServices.forEach(service -> FileActions.getInstance().writeToFile(Properties.paths.services(), service, "com.shaft.listeners.TestNGListener"));

        }
        // delete previous run execution log
        FileActions.getInstance().deleteFile(System.getProperty("appender.file.fileName"));
        SHAFT.Properties.reporting.set().disableLogging(false);
    }
}
