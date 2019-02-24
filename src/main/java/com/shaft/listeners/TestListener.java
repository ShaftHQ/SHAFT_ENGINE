package com.shaft.listeners;

import org.testng.ITestContext;
import org.testng.ITestListener;
import org.testng.ITestResult;

import com.shaft.browser.BrowserFactory;
import com.shaft.io.ReportManager;

public class TestListener implements ITestListener {

    @Override
    public void onStart(ITestContext context) {
	// This is to confirm that no browser sessions were leaked from the previous
	// test
	Boolean discreetLoggingState = ReportManager.isDiscreteLogging();
	ReportManager.setDiscreteLogging(true);
	BrowserFactory.closeAllDrivers();
	ReportManager.setDiscreteLogging(discreetLoggingState);
    }

    @Override
    public void onTestStart(ITestResult result) {
	// Auto-generated method stub

    }

    @Override
    public void onTestSuccess(ITestResult result) {
	// Auto-generated method stub

    }

    @Override
    public void onTestFailure(ITestResult result) {
	// Auto-generated method stub

    }

    @Override
    public void onTestSkipped(ITestResult result) {
	// Auto-generated method stub

    }

    @Override
    public void onTestFailedButWithinSuccessPercentage(ITestResult result) {
	// Auto-generated method stub

    }

    @Override
    public void onFinish(ITestContext context) {
	// Auto-generated method stub

    }

}
