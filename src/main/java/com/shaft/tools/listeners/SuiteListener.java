package com.shaft.tools.listeners;

import com.shaft.cli.FileActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.tools.io.ProjectStructureFactory;
import com.shaft.tools.io.PropertyFileManager;
import com.shaft.tools.io.ReportManager;
import org.testng.ISuite;
import org.testng.ISuiteListener;

public class SuiteListener implements ISuiteListener {

    @Override
    public void onStart(ISuite suite) {
        // read existing elements that were identified by AI to keep the reference table
        if (Boolean.TRUE.equals(ScreenshotManager.getAiSupportedElementIdentification())
                && FileActions.doesFileExist(ScreenshotManager.getAiAidedElementIdentificationFolderpath(),
                ElementActions.getAiReferenceFileName(), 1)) {
            PropertyFileManager.readPropertyFiles(ScreenshotManager.getAiAidedElementIdentificationFolderpath());
        }
        ProjectStructureFactory.initialize();
        ReportManager.prepareAllureReportingEnvironment();
        ReportManager.logEngineVersion();
        if (!(suite.getAllMethods().size() == 1 && suite.getAllMethods().get(0).getMethodName().equals("runScenario"))) {
            // not cucumber test runner
            ReportManager.setTotalNumberOfTests(suite.getAllMethods().size());
        }
        ReportManager.setDiscreteLogging(Boolean.parseBoolean(System.getProperty("alwaysLogDiscreetly")));
        ReportManager.setDebugMode(Boolean.valueOf(System.getProperty("debugMode")));
    }

    @Override
    public void onFinish(ISuite suite) {
        ReportManager.setDiscreteLogging(true);
        ReportManager.generateAllureReportArchive();
        ReportManager.openAllureReportAfterExecution();
    }
}