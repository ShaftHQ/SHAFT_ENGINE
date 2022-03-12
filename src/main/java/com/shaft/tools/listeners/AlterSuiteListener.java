package com.shaft.tools.listeners;

import com.shaft.gui.image.ImageProcessingActions;
import com.shaft.tools.io.LogsHelper;
import com.shaft.tools.io.PropertyFileManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import com.shaft.tools.tms.XrayIntegrationHelper;
import io.qameta.allure.*;
import org.testng.*;
import org.testng.annotations.ITestAnnotation;
import org.testng.xml.XmlClass;
import org.testng.xml.XmlSuite;
import org.testng.xml.XmlSuite.ParallelMode;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.List;

public class AlterSuiteListener implements IAlterSuiteListener, IRetryAnalyzer, IAnnotationTransformer,
        IInvokedMethodListener, IExecutionListener {

    private int retryCount = 0;
    private static int retryMaximumNumberOfAttempts = 0;
    private boolean testSuccess = true;

    @Override
    public void alter(List<XmlSuite> suites) {
        addListeners(suites);
        //TODO: manage slf4j log patterns
        System.setProperty("disableLogging","true");
        PropertyFileManager.readPropertyFiles();
        ImageProcessingActions.loadOpenCV();
        System.setProperty("disableLogging","false");
        ReportManagerHelper.logEngineVersion();

        retryMaximumNumberOfAttempts = Integer.parseInt(System.getProperty("retryMaximumNumberOfAttempts"));
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
//        var prefix = "SHAFT: ";
        var prefix = "";
        // rename default suite and test
        suites.forEach(suite -> {
            if (suite.getName().trim().equalsIgnoreCase("default suite")
                    || suite.getName().trim().equalsIgnoreCase("surefire suite")) {
                suite.setName(prefix + "Custom Suite");
            } else {
                suite.setName(prefix + suite.getName());
            }
            suite.getTests().forEach(test -> {
                if (test.getName().trim().equalsIgnoreCase("default test")
                        || test.getName().trim().equalsIgnoreCase("surefire test") || test.getName().trim().equalsIgnoreCase("SHAFT_ENGINE")) {
                    test.setName(prefix + "Custom Test");
                } else {
                    test.setName(prefix + test.getName());
                }
            });
        });
    }

    private void addLogsReporterToFirstTest(List<XmlSuite> suites) {
        // alter first test and add the afterSuiteMethod
    	var logsReporter = new XmlClass(LogsHelper.class.getName());
        suites.get(0).getTests().get(0).getClasses().add(logsReporter);
    }

    private void addListeners(List<XmlSuite> suites) {
        suites.forEach(suite -> {
            suite.addListener("com.shaft.tools.listeners.InvokedMethodListener");
//            suite.addListener("com.shaft.tools.listeners.CucumberFeatureListener");
        });

    }

    @Override
    public boolean retry(ITestResult iTestResult) {
        if (!iTestResult.isSuccess()) {                      //Check if test not succeed
            if (retryCount < retryMaximumNumberOfAttempts) {                            //Check if maxtry count is reached
                retryCount++;                                     //Increase the maxTry count by 1
                iTestResult.setStatus(ITestResult.FAILURE);  //Mark test as failed
                return true;                                 //Tells TestNG to re-run the test
            } else {
                iTestResult.setStatus(ITestResult.FAILURE);  //If maxCount reached,test marked as failed
            }
        } else {
            iTestResult.setStatus(ITestResult.SUCCESS);      //If test passes, TestNG marks it as passed
        }
        return false;
    }

    @Override
    public void transform(ITestAnnotation annotation, Class testClass, Constructor testConstructor, Method testMethod) {
        annotation.setRetryAnalyzer(AlterSuiteListener.class);
    }

    /**
     * The method is to update testng-results.xml with the required data to integrate with xray plugin
     * The Method uses Epic, Feature, Story and Test allure annotations' values.
     *
     * @see Allure
     */

    public void beforeInvocation(IInvokedMethod method, ITestResult testResult) {
        if(method.isTestMethod() ) {
            if (annotationPresent(method, Feature.class))
                testResult.setAttribute("requirement", method.getTestMethod().getConstructorOrMethod().getMethod().getAnnotation(Feature.class).value());
            if (annotationPresent(method, Epic.class))
                testResult.setAttribute("requirement", method.getTestMethod().getConstructorOrMethod().getMethod().getAnnotation(Epic.class).value());
            if (annotationPresent(method, Story.class))
                testResult.setAttribute("requirement", method.getTestMethod().getConstructorOrMethod().getMethod().getAnnotation(Story.class).value());
            if (annotationPresent(method, TmsLink.class))
                testResult.setAttribute("test", method.getTestMethod().getConstructorOrMethod().getMethod().getAnnotation(TmsLink.class).value());
            //TO-DO: testResult.setAttribute("labels", method.getTestMethod().getConstructorOrMethod().getMethod().getAnnotation().labels());
        }
    }


    private boolean annotationPresent(IInvokedMethod method, Class clazz) {
        return method.getTestMethod().getConstructorOrMethod().getMethod().isAnnotationPresent(clazz);
    }

    public void afterInvocation(IInvokedMethod method, ITestResult testResult) {
        if(method.isTestMethod()&& !testSuccess ) {
            testResult.setStatus(ITestResult.FAILURE);
        }
    }

    @Override
    public void onExecutionStart() {
//        ReportManager.logDiscrete("TestNG is going to start");
    }

    @Override
    public void onExecutionFinish() {
//        ReportManager.logDiscrete("TestNG is finished");
        reportExecutionStatusToJira();
    }

    public static void reportExecutionStatusToJira(){
        if(System.getProperty("jiraInteraction").equalsIgnoreCase("true"))
        {
            try {
                if(System.getProperty("reportPath").contains("testng-results.xml"))
                {
                    XrayIntegrationHelper.importTestNGResults(System.getProperty("reportPath"));
                }
                else if (System.getProperty("reportPath").contains("cucumber.json")) {
                    XrayIntegrationHelper.importCucumberResults(System.getProperty("reportPath"));
                }

                XrayIntegrationHelper.renameTestExecutionSuit(System.getProperty("ExecutionName"),
                        System.getProperty("ExecutionDescription") );

            } catch (Exception e) {
                ReportManagerHelper.log(e);
            }
        }
    }
}