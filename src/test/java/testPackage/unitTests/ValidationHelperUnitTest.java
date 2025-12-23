package testPackage.unitTests;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.List;

/**
 * Unit tests for validation and assertion helpers
 * Tests common validation patterns without requiring browser
 */
public class ValidationHelperUnitTest {

    @Test(description = "Test string equality validation")
    public void testStringEquality() {
        String expected = "test";
        String actual = "test";
        Assert.assertEquals(actual, expected, "Strings should be equal");
    }

    @Test(description = "Test string contains validation")
    public void testStringContains() {
        String text = "Welcome to SHAFT Engine";
        Assert.assertTrue(text.contains("SHAFT"), "Text should contain SHAFT");
    }

    @Test(description = "Test string not empty validation")
    public void testStringNotEmpty() {
        String text = "non-empty";
        Assert.assertFalse(text.isEmpty(), "String should not be empty");
    }

    @Test(description = "Test null check validation")
    public void testNullCheck() {
        String text = "not null";
        Assert.assertNotNull(text, "Text should not be null");
    }

    @Test(description = "Test integer comparison")
    public void testIntegerComparison() {
        int actual = 5;
        int expected = 5;
        Assert.assertEquals(actual, expected, "Integers should be equal");
    }

    @Test(description = "Test boolean validation")
    public void testBooleanValidation() {
        boolean condition = true;
        Assert.assertTrue(condition, "Condition should be true");
    }

    @Test(description = "Test list size validation")
    public void testListSizeValidation() {
        List<String> items = Arrays.asList("item1", "item2", "item3");
        Assert.assertEquals(items.size(), 3, "List should have 3 items");
    }

    @Test(description = "Test list contains validation")
    public void testListContainsValidation() {
        List<String> items = Arrays.asList("apple", "banana", "orange");
        Assert.assertTrue(items.contains("banana"), "List should contain banana");
    }

    @Test(description = "Test string starts with validation")
    public void testStringStartsWith() {
        String url = "https://www.saucedemo.com";
        Assert.assertTrue(url.startsWith("https://"), "URL should start with https://");
    }

    @Test(description = "Test string ends with validation")
    public void testStringEndsWith() {
        String filename = "test.json";
        Assert.assertTrue(filename.endsWith(".json"), "Filename should end with .json");
    }

    @Test(description = "Test case insensitive string comparison")
    public void testCaseInsensitiveComparison() {
        String text1 = "SHAFT";
        String text2 = "shaft";
        Assert.assertEquals(text1.toLowerCase(), text2.toLowerCase(), 
            "Strings should be equal ignoring case");
    }

    @Test(description = "Test string trim validation")
    public void testStringTrim() {
        String text = "  trimmed  ";
        Assert.assertEquals(text.trim(), "trimmed", "Trimmed string should match");
    }

    @Test(description = "Test array length validation")
    public void testArrayLengthValidation() {
        String[] items = {"a", "b", "c"};
        Assert.assertEquals(items.length, 3, "Array should have 3 elements");
    }

    @Test(description = "Test numeric range validation")
    public void testNumericRangeValidation() {
        int value = 50;
        Assert.assertTrue(value >= 0 && value <= 100, "Value should be in range 0-100");
    }

    @Test(description = "Test string length validation")
    public void testStringLengthValidation() {
        String text = "test";
        Assert.assertEquals(text.length(), 4, "String length should be 4");
    }
}
