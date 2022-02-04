
package com.shaft.tools.listeners;

import com.shaft.tms.XRayIntegration;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import io.qameta.allure.*;
import org.testng.*;

/**
 * The listener interface for receiving Xray events.
 * The Listener will be automatically invoked when TestNG tests are run.
 * The Listener uses Epic, Feature, Story and Test allure annotations' values.
 *
 * @see Allure
 */


public class XRayListener implements IInvokedMethodListener, ITestListener, IExecutionListener  {
    
    boolean testSuccess = true;

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
        boolean retVal = method.getTestMethod().getConstructorOrMethod().getMethod().isAnnotationPresent(clazz) ? true : false;
        return retVal;
    }


    public void afterInvocation(IInvokedMethod method, ITestResult testResult) {
        if(method.isTestMethod()) {
            if( !testSuccess ) {
                testResult.setStatus(ITestResult.FAILURE);
            }
        }
    }

    public void onTestStart(ITestResult result) {
        // TODO Auto-generated method stub
        
    }

    public void onTestSuccess(ITestResult result) {
        // TODO Auto-generated method stub
        
    }

    public void onTestFailure(ITestResult result) {
        // TODO Auto-generated method stub
        
    }

    public void onTestSkipped(ITestResult result) {
        // TODO Auto-generated method stub
        
    }

    public void onTestFailedButWithinSuccessPercentage(ITestResult result) {
        // TODO Auto-generated method stub
        
    }

    public void onStart(ITestContext context) {
        
    }

    public void onFinish(ITestContext context) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void onExecutionStart() {
        ReportManager.logDiscrete("TestNG is going to start");

    }

    @Override
    public void onExecutionFinish() {
        ReportManager.logDiscrete("TestNG is finished");
        if(System.getProperty("jiraInteraction").equalsIgnoreCase("true"))
        {
            try {
                    if(System.getProperty("reportPath").contains("testng-results.xml"))
                    {
                        XRayIntegration.importTestNGResults(System.getProperty("reportPath"));
                    }
                    else if (System.getProperty("reportPath").contains("cucumber.json")) {
                        XRayIntegration.importCucumberResults(System.getProperty("reportPath"));
                    }

                XRayIntegration.renameTestExecutionSuit(System.getProperty("ExecutionName"),
                        System.getProperty("ExecutionDescription") );

            } catch (Exception e) {
                ReportManagerHelper.log(e);
            }
        }
    }

}
