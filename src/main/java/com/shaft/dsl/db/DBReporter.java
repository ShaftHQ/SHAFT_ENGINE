package com.shaft.dsl.db;

import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import org.testng.Assert;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class DBReporter {
    final static int reportingThreadIndex = 3;
    static void passAction(String actionName, String testData, String queryResult) {
        reportActionResult(actionName, testData, queryResult, true);
    }

    static void passAction(String testData, String queryResult) {

        String actionName = Thread.currentThread().getStackTrace()[reportingThreadIndex].getMethodName();
        passAction(actionName, testData, queryResult);
    }

    static void passAction(String testData) {
        String actionName = Thread.currentThread().getStackTrace()[reportingThreadIndex].getMethodName();
        passAction(actionName, testData, null);
    }

    static void passAction() {
        String actionName = Thread.currentThread().getStackTrace()[reportingThreadIndex].getMethodName();
        passAction(actionName, null, null);
    }

    static void failAction(String actionName, String testData, Exception... rootCauseException) {
        String message = reportActionResult(actionName, testData, null, false);
        if (rootCauseException != null && rootCauseException.length >= 1) {
            Assert.fail(message, rootCauseException[0]);
        } else {
            Assert.fail(message);
        }
    }

    static void failAction(String testData, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[reportingThreadIndex].getMethodName();
        failAction(actionName, testData, rootCauseException);
    }

    static void failAction(Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[reportingThreadIndex].getMethodName();
        failAction(actionName, null, rootCauseException);
    }
    static String reportActionResult(String actionName, String testData, String queryResult,
                                             Boolean passFailStatus) {
        actionName = actionName.substring(0, 1).toUpperCase() + actionName.substring(1);
        String message;
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = "Database Action [" + actionName + "] successfully performed.";
        } else {
            message = "Database Action [" + actionName + "] failed.";
        }

        List<List<Object>> attachments = new ArrayList<>();
        if (testData != null && testData.length() >= 500) {
            List<Object> actualValueAttachment = Arrays.asList("Database Action Test Data - " + actionName,
                    "Actual Value", testData);
            attachments.add(actualValueAttachment);
        } else if (testData != null && !testData.isEmpty()) {
            message = message + " With the following test data [" + testData + "].";
        }

        if (queryResult != null && !queryResult.trim().equals("")) {
            attachments.add(Arrays.asList("Database Action Actual Result", "Query Result", queryResult));
        }

        if (!attachments.equals(new ArrayList<>())) {
            ReportManagerHelper.log(message, attachments);
        } else {
            ReportManager.log(message);
        }
        return message;
    }
}
