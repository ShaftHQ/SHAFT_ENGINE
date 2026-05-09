package com.shaft.tools.internal.support;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class JavaHelperTest {

    @Test
    void unixStyleAbsolutePathShouldNotHaveTestDataPrepended() {
        String result = JavaHelper.appendTestDataToRelativePath("/tmp/file.json");
        assertFalse(result.contains("testDataFiles"),
            "Unix-absolute path must not have testData folder prepended, got: " + result);
    }

    @Test
    void relativePathShouldHaveTestDataFolderPrepended() {
        String result = JavaHelper.appendTestDataToRelativePath("reports/file.json");
        assertTrue(result.endsWith("reports/file.json"),
            "Result must end with the original filename, got: " + result);
        assertNotEquals("reports/file.json", result,
            "Relative path must have a folder prefix prepended");
        assertFalse(result.isBlank(), "Result must not be blank");
    }

    @Test
    void pathAlreadyContainingTestDataShouldNotBeDoubled() {
        String result1 = JavaHelper.appendTestDataToRelativePath("testDataFiles/file.json");
        String result2 = JavaHelper.appendTestDataToRelativePath("testDataFiles/file.json");
        assertEquals(result1, result2, "Re-running on the same path must be idempotent");
        assertFalse(result1.contains("testDataFilestestDataFiles"),
            "testData folder must not be doubled, got: " + result1);
    }

    @Test
    void isClassAvailableShouldReturnTrueForKnownClass() {
        assertTrue(JavaHelper.isClassAvailable("java.util.ArrayList"));
    }

    @Test
    void isClassAvailableShouldReturnFalseForUnknownClass() {
        assertFalse(JavaHelper.isClassAvailable("com.shaft.nonexistent.DoesNotExist"));
    }
}
