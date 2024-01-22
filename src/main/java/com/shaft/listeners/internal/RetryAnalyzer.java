package com.shaft.listeners.internal;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import org.testng.IRetryAnalyzer;
import org.testng.ITestResult;

public class RetryAnalyzer implements IRetryAnalyzer {
    private final int maxRetryCount = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
    private int counter = 0;

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