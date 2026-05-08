package testPackage.unitTests;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.UUID;

/**
 * Unit tests for string manipulation and helper methods
 * Tests common string operations used in test automation
 */
public class StringHelperUnitTest {

    @Test(description = "Test string concatenation")
    public void testStringConcatenation() {
        String first = "Hello";
        String second = "World";
        String result = first + " " + second;
        Assert.assertEquals(result, "Hello World", "Concatenated string should match");
    }

    @Test(description = "Test string replacement")
    public void testStringReplacement() {
        String original = "Hello World";
        String replaced = original.replace("World", "SHAFT");
        Assert.assertEquals(replaced, "Hello SHAFT", "Replaced string should match");
    }

    @Test(description = "Test string splitting")
    public void testStringSplitting() {
        String text = "apple,banana,orange";
        String[] parts = text.split(",");
        Assert.assertEquals(parts.length, 3, "Should have 3 parts");
        Assert.assertEquals(parts[0], "apple", "First part should be apple");
    }

    @Test(description = "Test string to uppercase")
    public void testStringToUppercase() {
        String text = "shaft";
        String uppercase = text.toUpperCase();
        Assert.assertEquals(uppercase, "SHAFT", "Uppercase should match");
    }

    @Test(description = "Test string to lowercase")
    public void testStringToLowercase() {
        String text = "SHAFT";
        String lowercase = text.toLowerCase();
        Assert.assertEquals(lowercase, "shaft", "Lowercase should match");
    }

    @Test(description = "Test string substring")
    public void testStringSubstring() {
        String text = "Hello World";
        String sub = text.substring(0, 5);
        Assert.assertEquals(sub, "Hello", "Substring should match");
    }

    @Test(description = "Test string indexOf")
    public void testStringIndexOf() {
        String text = "Hello World";
        int index = text.indexOf("World");
        Assert.assertEquals(index, 6, "Index should be 6");
    }

    @Test(description = "Test string format")
    public void testStringFormat() {
        String formatted = String.format("User: %s, Age: %d", "John", 30);
        Assert.assertEquals(formatted, "User: John, Age: 30", "Formatted string should match");
    }

    @Test(description = "Test string builder")
    public void testStringBuilder() {
        StringBuilder sb = new StringBuilder();
        sb.append("Hello");
        sb.append(" ");
        sb.append("World");
        Assert.assertEquals(sb.toString(), "Hello World", "StringBuilder result should match");
    }

    @Test(description = "Test UUID generation")
    public void testUUIDGeneration() {
        String uuid = UUID.randomUUID().toString();
        Assert.assertNotNull(uuid, "UUID should not be null");
        Assert.assertTrue(uuid.length() > 0, "UUID should not be empty");
        Assert.assertTrue(uuid.contains("-"), "UUID should contain hyphens");
    }

    @Test(description = "Test string matches regex")
    public void testStringMatchesRegex() {
        String email = "test@example.com";
        boolean matches = email.matches(".*@.*\\..*");
        Assert.assertTrue(matches, "Email should match regex pattern");
    }

    @Test(description = "Test string isEmpty")
    public void testStringIsEmpty() {
        String empty = "";
        String notEmpty = "text";
        Assert.assertTrue(empty.isEmpty(), "Empty string should be empty");
        Assert.assertFalse(notEmpty.isEmpty(), "Non-empty string should not be empty");
    }

    @Test(description = "Test string isBlank")
    public void testStringIsBlank() {
        String blank = "   ";
        String notBlank = "text";
        Assert.assertTrue(blank.isBlank(), "Blank string should be blank");
        Assert.assertFalse(notBlank.isBlank(), "Non-blank string should not be blank");
    }

    @Test(description = "Test string strip")
    public void testStringStrip() {
        String text = "  Hello World  ";
        String stripped = text.strip();
        Assert.assertEquals(stripped, "Hello World", "Stripped string should match");
    }

    @Test(description = "Test string repeat")
    public void testStringRepeat() {
        String text = "AB";
        String repeated = text.repeat(3);
        Assert.assertEquals(repeated, "ABABAB", "Repeated string should match");
    }
}
