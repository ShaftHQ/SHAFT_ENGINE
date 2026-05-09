package com.shaft.tools.internal.support;

import org.testng.Assert;
import org.testng.annotations.Test;

class JavaHelperTest {

    @Test
    void unixStyleAbsolutePathShouldNotHaveTestDataPrepended() {
        // "//tmp/file.json" normalises to "/tmp/file.json" (absolute on Unix) → no testData prefix.
        // Single-slash paths like "/file.json" are treated as relative with an accidental leading
        // slash and DO get the testData folder prepended (see appendTestDataToRelativePath contract).
        String result = JavaHelper.appendTestDataToRelativePath("//tmp/file.json");
        Assert.assertFalse(result.contains("testDataFiles"),
            "Unix-absolute path must not have testData folder prepended, got: " + result);
    }

    @Test
    void relativePathShouldHaveTestDataFolderPrepended() {
        String result = JavaHelper.appendTestDataToRelativePath("reports/file.json");
        Assert.assertTrue(result.endsWith("reports/file.json"),
            "Result must end with the original filename, got: " + result);
        Assert.assertNotEquals(result, "reports/file.json",
            "Relative path must have a folder prefix prepended");
        Assert.assertFalse(result.isBlank(), "Result must not be blank");
    }

    @Test
    void pathAlreadyContainingTestDataShouldNotBeDoubled() {
        String result1 = JavaHelper.appendTestDataToRelativePath("testDataFiles/file.json");
        String result2 = JavaHelper.appendTestDataToRelativePath("testDataFiles/file.json");
        Assert.assertEquals(result1, result2, "Re-running on the same path must be idempotent");
        Assert.assertFalse(result1.contains("testDataFilestestDataFiles"),
            "testData folder must not be doubled, got: " + result1);
    }

    @Test
    void isClassAvailableShouldReturnTrueForKnownClass() {
        Assert.assertTrue(JavaHelper.isClassAvailable("java.util.ArrayList"));
    }

    @Test
    void isClassAvailableShouldReturnFalseForUnknownClass() {
        Assert.assertFalse(JavaHelper.isClassAvailable("com.shaft.nonexistent.DoesNotExist"));
    }
}
