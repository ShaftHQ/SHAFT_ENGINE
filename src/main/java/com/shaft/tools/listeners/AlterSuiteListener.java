package com.shaft.tools.listeners;

import com.shaft.tools.io.LogsReporter;
import com.shaft.tools.io.PropertiesFileManager;
import com.shaft.tools.io.ReportManager;
import org.apache.log4j.PropertyConfigurator;
import org.testng.IAlterSuiteListener;
import org.testng.xml.XmlClass;
import org.testng.xml.XmlSuite;
import org.testng.xml.XmlSuite.ParallelMode;

import java.util.List;
import java.util.Properties;

public class AlterSuiteListener implements IAlterSuiteListener {

    @Override
    public void alter(List<XmlSuite> suites) {
        addListeners(suites);
        //log4j configuration needed to fix this warning
        //log4j:WARN No appenders could be found for logger (com.automation.remarks.video.recorder.monte.MonteRecorder).
        //log4j:WARN Please initialize the log4j system properly.
        //log4j:WARN See http://logging.apache.org/log4j/1.2/faq.html#noconfig for more info.
        Properties log4jProperties = new Properties();
        log4jProperties.setProperty("log4j.rootLogger", "INFO, stdout");
        log4jProperties.setProperty("log4j.appender.stdout", "org.apache.log4j.ConsoleAppender");
        log4jProperties.setProperty("log4j.appender.stdout.Target", "System.out");
        log4jProperties.setProperty("log4j.appender.stdout.layout", "org.apache.log4j.PatternLayout");
        log4jProperties.setProperty("log4j.appender.stdout.layout.ConversionPattern", "[main] %p %c - %m @%d{dd-MM-yyyy HH:mm:ss.SSSS aaa}%n");
        PropertyConfigurator.configure(log4jProperties);
        //TODO: unify log patterns for log4j and slf4j
        PropertiesFileManager.readPropertyFiles();
        setExecutionProperties(suites);
        renameDefaultSuiteAndTest(suites);
        addLogsReporterToFirstTest(suites);
    }

    private void setExecutionProperties(List<XmlSuite> suites) {
        suites.forEach(suite -> {
            suite.setPreserveOrder(Boolean.valueOf(System.getProperty("setPreserveOrder")));
            suite.setGroupByInstances(Boolean.parseBoolean(System.getProperty("setGroupByInstances")));
            suite.setVerbose(Integer.parseInt(System.getProperty("setVerbose")));
            suite.setParallel(ParallelMode.valueOf(System.getProperty("setParallel")));
            suite.setThreadCount(Integer.parseInt(System.getProperty("setThreadCount")));
            suite.setDataProviderThreadCount(Integer.parseInt(System.getProperty("setDataProviderThreadCount")));

            if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("debugMode")))) {
                ReportManager.log("getPreserveOrder: " + suite.getPreserveOrder());
                ReportManager.log("getDataProviderThreadCount: " + suite.getDataProviderThreadCount());
                ReportManager.log("getThreadCount: " + suite.getThreadCount());
                ReportManager.log("getVerbose: " + suite.getVerbose());
                ReportManager.log("getGroupByInstances: " + suite.getGroupByInstances());
                ReportManager.log("getParallel: " + suite.getParallel());
            }
        });
    }

    private void renameDefaultSuiteAndTest(List<XmlSuite> suites) {
        String prefix = "SHAFT_Engine: ";
        // rename default suite and test
        suites.forEach(suite -> {
            if (suite.getName().toLowerCase().trim().equals("default suite")
                    || suite.getName().toLowerCase().trim().equals("surefire suite")) {
                suite.setName(prefix + "Custom Suite");
            } else {
                suite.setName(prefix + suite.getName());
            }
            suite.getTests().forEach(test -> {
                if (test.getName().toLowerCase().trim().equals("default test")
                        || test.getName().toLowerCase().trim().equals("surefire test")) {
                    test.setName(prefix + "Custom Test");
                } else {
                    test.setName(prefix + test.getName());
                }
            });
        });
    }

    private void addLogsReporterToFirstTest(List<XmlSuite> suites) {
        // alter first test and add the afterSuiteMethod
        XmlClass logsReporter = new XmlClass(LogsReporter.class.getName());
        suites.get(0).getTests().get(0).getClasses().add(logsReporter);
    }

    private void addListeners(List<XmlSuite> suites) {
        suites.forEach(suite -> {
            suite.addListener("com.shaft.tools.listeners.InvokedMethodListener");
            suite.addListener("com.shaft.tools.listeners.SuiteListener");
        });

    }
}