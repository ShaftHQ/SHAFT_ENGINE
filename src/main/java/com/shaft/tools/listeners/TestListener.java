package com.shaft.tools.listeners;

import com.shaft.tools.io.ReportManager;
import org.testng.ITestListener;
import org.testng.ITestResult;

public class TestListener implements ITestListener {
    @Override
    public void onTestStart(ITestResult result) {
        ReportManager.extentReportsCreateTest(ReportManager.getTestMethodName());
    }

    @Override
    public void onTestSuccess(ITestResult result) {
        ReportManager.extentReportsPass(ReportManager.getTestMethodName() + " is Passed");
    }

    @Override
    public void onTestFailure(ITestResult result) {
        if (result.getThrowable() != null) {
            ReportManager.extentReportsFail(result.getThrowable());
        } else {
            ReportManager.extentReportsFail(ReportManager.getTestMethodName() + " is Failed");
        }
    }

    @Override
    public void onTestSkipped(ITestResult result) {
        if (result.getThrowable() != null) {
            ReportManager.extentReportsSkip(result.getThrowable());
        } else {
            ReportManager.extentReportsSkip(ReportManager.getTestMethodName() + " is Skipped");
        }
    }
}
