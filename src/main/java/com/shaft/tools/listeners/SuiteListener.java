package com.shaft.tools.listeners;

import com.shaft.cli.FileActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.tools.io.ProjectStructureFactory;
import com.shaft.tools.io.PropertyFileManager;
import com.shaft.tools.io.ReportManagerHelper;
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
        ReportManagerHelper.initializeExtentReports();
        ReportManagerHelper.prepareAllureReportingEnvironment();
        ReportManagerHelper.logEngineVersion();
        if (!(suite.getAllMethods().size() == 1 && suite.getAllMethods().get(0).getMethodName().equals("runScenario"))) {
            // not cucumber test runner
            ReportManagerHelper.setTotalNumberOfTests(suite.getAllMethods().size());
        }
        ReportManagerHelper.setDiscreteLogging(Boolean.parseBoolean(System.getProperty("alwaysLogDiscreetly")));
        ReportManagerHelper.setDebugMode(Boolean.valueOf(System.getProperty("debugMode")));
    }
}