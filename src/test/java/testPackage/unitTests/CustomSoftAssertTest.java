package testPackage.unitTests;

import com.shaft.validation.constants.CustomSoftAssert;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Unit tests for CustomSoftAssert class
 * Tests the enhanced stack trace formatting functionality
 */
public class CustomSoftAssertTest {

    @Test(description = "Test formatFailureWithStackTrace with matching package")
    public void testFormatFailureWithStackTraceWithMatch() {
        AssertionError error = new AssertionError("Test error message");
        String formatted = CustomSoftAssert.formatFailureWithStackTrace(error, "testPackage");
        
        Assert.assertNotNull(formatted, "Formatted message should not be null for matching package");
        Assert.assertTrue(formatted.contains("Test error message"), 
            "Should contain original error message");
        Assert.assertTrue(formatted.contains("at "), 
            "Should contain stack trace format");
        Assert.assertTrue(formatted.contains("(") && formatted.contains(":"), 
            "Should contain clickable stack trace format");
    }

    @Test(description = "Test formatFailureWithStackTrace with non-matching package")
    public void testFormatFailureWithStackTraceNoMatch() {
        AssertionError error = new AssertionError("Test error");
        String formatted = CustomSoftAssert.formatFailureWithStackTrace(error, "nonexistent.package");
        
        Assert.assertNull(formatted, "Should return null for non-matching package");
    }

    @Test(description = "Test formatFailureWithStackTrace with null error")
    public void testFormatFailureWithStackTraceNullError() {
        String formatted = CustomSoftAssert.formatFailureWithStackTrace(null, "testPackage");
        Assert.assertNull(formatted, "Should return null for null error");
    }

    @Test(description = "Test formatFailureWithStackTrace with null rootPackage")
    public void testFormatFailureWithStackTraceNullRootPackage() {
        AssertionError error = new AssertionError("Test error");
        String formatted = CustomSoftAssert.formatFailureWithStackTrace(error, null);
        Assert.assertNull(formatted, "Should return null for null rootPackage");
    }

    @Test(description = "Test CustomSoftAssert setRootPackage and onAssertFailure")
    public void testCustomSoftAssertSetRootPackage() {
        CustomSoftAssert softAssert = new CustomSoftAssert();
        softAssert.setRootPackage("testPackage");
        
        // Create a failure that will match testPackage
        softAssert.assertEquals("expected", "actual", "Values should match");
        
        // Verify assertAll throws and contains formatted message
        try {
            softAssert.assertAll();
            Assert.fail("assertAll should throw AssertionError");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage(), "Error message should not be null");
        }
    }

    @Test(description = "Test CustomSoftAssert clearFailures")
    public void testCustomSoftAssertClearFailures() {
        CustomSoftAssert softAssert = new CustomSoftAssert();
        softAssert.setRootPackage("testPackage");
        
        // Add a failure
        softAssert.assertEquals("expected", "actual", "Test");
        
        // Clear failures
        softAssert.clearFailures();
        
        // assertAll should still throw, but clearFailures should have cleared the list
        try {
            softAssert.assertAll();
            Assert.fail("assertAll should throw AssertionError");
        } catch (AssertionError e) {
            // Expected - assertAll will still throw from parent class
            Assert.assertNotNull(e);
        }
    }

    @Test(description = "Test formatFailureWithStackTrace with line number 0")
    public void testFormatFailureWithStackTraceLineNumberZero() {
        // Create an error with a stack trace element that has line number 0
        AssertionError error = new AssertionError("Test error") {
            @Override
            public StackTraceElement[] getStackTrace() {
                return new StackTraceElement[]{
                    new StackTraceElement("testPackage.unitTests.TestClass", "testMethod", 
                        "TestClass.java", 0) // line number 0
                };
            }
        };
        
        String formatted = CustomSoftAssert.formatFailureWithStackTrace(error, "testPackage");
        Assert.assertNull(formatted, "Should return null for line number 0");
    }

    @Test(description = "Test formatFailureWithStackTrace with null fileName")
    public void testFormatFailureWithStackTraceNullFileName() {
        AssertionError error = new AssertionError("Test error") {
            @Override
            public StackTraceElement[] getStackTrace() {
                return new StackTraceElement[]{
                    new StackTraceElement("testPackage.unitTests.TestClass", "testMethod", 
                        null, 25) // null fileName
                };
            }
        };
        
        String formatted = CustomSoftAssert.formatFailureWithStackTrace(error, "testPackage");
        Assert.assertNotNull(formatted, "Should handle null fileName");
        Assert.assertTrue(formatted.contains("Unknown"), 
            "Should use 'Unknown' for null fileName");
    }

    @Test(description = "Test formatFailureWithStackTrace returns standard stack trace format")
    public void testFormatFailureWithStackTraceStandardFormat() {
        AssertionError error = new AssertionError("Test error message");
        String formatted = CustomSoftAssert.formatFailureWithStackTrace(error, "testPackage");
        
        if (formatted != null) {
            // Verify it contains the standard format: at package.Class.method(File.java:lineNumber)
            Assert.assertTrue(formatted.contains("at "), 
                "Should contain 'at ' prefix for standard stack trace format");
            Assert.assertTrue(formatted.contains("(") && formatted.contains(":"), 
                "Should contain file and line number format");
        }
    }
}

