package testPackage;

import constants.CustomSoftAssert;
import org.testng.annotations.Test;

public class CustomSoftAssertTest {

    @Test
    public void testCustomSoftAssertWithFailures() {
        CustomSoftAssert softAssert = new CustomSoftAssert();
        
        // Test data
        String expectedValue = "Hello World";
        String actualValue = "Hello Universe";
        int expectedNumber = 42;
        int actualNumber = 24;
        boolean expectedBoolean = true;
        boolean actualBoolean = false;
        
        System.out.println("=== Testing CustomSoftAssert with Multiple Failures ===");
        
        // These assertions will fail and be captured by CustomSoftAssert
        softAssert.assertEquals(actualValue, expectedValue, "String values should match");
        softAssert.assertEquals(actualNumber, expectedNumber, "Numbers should be equal");
        softAssert.assertEquals(actualBoolean, expectedBoolean, "Boolean values should match");
        softAssert.assertTrue(actualBoolean, "Boolean should be true");
        softAssert.assertFalse(expectedBoolean, "Boolean should be false");
        
        // This assertion will pass
        softAssert.assertEquals("Success", "Success", "This should pass");
        
        // Call assertAll() to trigger the custom failure summary
        softAssert.assertAll();
    }
    
    @Test
    public void testCustomSoftAssertWithSuccess() {
        CustomSoftAssert softAssert = new CustomSoftAssert();
        
        System.out.println("=== Testing CustomSoftAssert with All Passes ===");
        
        // All these assertions will pass
        softAssert.assertEquals("Hello", "Hello", "Strings should match");
        softAssert.assertEquals(100, 100, "Numbers should be equal");
        softAssert.assertTrue(true, "Boolean should be true");
        softAssert.assertFalse(false, "Boolean should be false");
        softAssert.assertNotNull("Not null", "Value should not be null");
        
        // Call assertAll() - should show no failures
        softAssert.assertAll();
    }
    
    @Test
    public void testCustomSoftAssertClearFailures() {
        CustomSoftAssert softAssert = new CustomSoftAssert();
        
        System.out.println("=== Testing CustomSoftAssert Clear Failures Feature ===");
        
        // Add some failures
        softAssert.assertEquals("Wrong", "Right", "First failure");
        softAssert.assertEquals(1, 2, "Second failure");
        
        System.out.println("Failures before clear:");
        softAssert.assertAll();
        
        // Clear failures
        softAssert.clearFailures();
        
        System.out.println("Failures after clear:");
        softAssert.assertAll();
        
        // Add new failures
        softAssert.assertEquals("New Wrong", "New Right", "New failure after clear");
        
        System.out.println("New failures after clear:");
        softAssert.assertAll();
    }
    
    @Test
    public void testCustomSoftAssertMixedResults() {
        CustomSoftAssert softAssert = new CustomSoftAssert();
        
        System.out.println("=== Testing CustomSoftAssert with Mixed Results ===");
        
        // Mix of passing and failing assertions
        softAssert.assertEquals("Pass", "Pass", "This will pass");
        softAssert.assertEquals("Fail", "Pass", "This will fail");
        softAssert.assertTrue(true, "This will pass");
        softAssert.assertTrue(false, "This will fail");
        softAssert.assertNotNull("Not null", "This will pass");
        softAssert.assertNull("Not null", "This will fail");
        
        // Call assertAll() to see the enhanced failure summary
        softAssert.assertAll();
    }
}
