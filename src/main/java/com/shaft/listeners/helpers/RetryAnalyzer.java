package com.shaft.listeners.helpers;

import com.shaft.tools.io.ReportManager;
import org.testng.IRetryAnalyzer;
import org.testng.ITestResult;

public class RetryAnalyzer implements IRetryAnalyzer {
    private int counter = 0;
    private final int maxRetryCount = Integer.parseInt(System.getProperty("retryMaximumNumberOfAttempts"));

    @Override
    public boolean retry(ITestResult iTestResult) {
        if (counter < maxRetryCount) {
            counter++;
            ReportManager.logDiscrete("Retry #" + counter + " for test: " + iTestResult.getMethod().getMethodName() + ", on thread: " + Thread.currentThread().getName());
            return true;
        }
        return false;
    }
}