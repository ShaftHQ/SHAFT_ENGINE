package com.shaft.db;

import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import org.testng.Assert;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class DBReporter {
    final static int reportingThreadIndex = 3;
    static void passAction(String actionName, String testData, String queryResult) {
        String message="Database Action [" + actionName + "] successfully performed.";
        reportActionResult(actionName, testData, queryResult, message);
    }

    static void passAction(String testData, String queryResult) {
        String actionName = Thread.currentThread().getStackTrace()[reportingThreadIndex].getMethodName();
        passAction(actionName, testData, queryResult);
    }

    static void passAction(String testData) {
        String actionName = Thread.currentThread().getStackTrace()[reportingThreadIndex].getMethodName();
        passAction(actionName, testData);
    }

    static void passAction() {
        String actionName = Thread.currentThread().getStackTrace()[reportingThreadIndex].getMethodName();
        passAction(actionName);
    }

    static void failAction(String actionName, String testData, Exception... rootCauseException) {
        String message = "Database Action [" + actionName + "] failed.";
        String result = reportActionResult(actionName, testData, null, message);
        if (rootCauseException != null && rootCauseException.length >= 1) {
            Assert.fail(result, rootCauseException[0]);
        } else {
            Assert.fail(result);
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
                                             String executionMsg) {
        actionName = actionName.substring(0, 1).toUpperCase() + actionName.substring(1);
        String message=executionMsg;
        List<List<Object>> attachments = new ArrayList<>();
        if (testData != null && !testData.isEmpty()){
            if (testData.length() >= 500) {
            List<Object> actualValueAttachment = Arrays.asList("Database Action Test Data - " + actionName,
                    "Actual Value", testData);
            attachments.add(actualValueAttachment);
        } else { message = message + " With the following test data [" + testData + "].";}
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
