package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import org.testng.Assert;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

/**
 * Unit tests for SHAFT's TestData functionality
 * Tests JSON test data loading and retrieval
 */
public class TestDataUnitTest {

    private SHAFT.TestData.JSON testData;
    private String testJsonFilePath;

    @BeforeClass
    public void setUp() throws IOException {
        // Create a temporary JSON test data file
        testJsonFilePath = "src/test/resources/testDataFiles/unitTestData.json";
        File testFile = new File(testJsonFilePath);
        testFile.getParentFile().mkdirs();
        
        try (FileWriter writer = new FileWriter(testFile)) {
            writer.write("{\n");
            writer.write("  \"username\": \"testUser\",\n");
            writer.write("  \"password\": \"testPass123\",\n");
            writer.write("  \"email\": \"test@example.com\",\n");
            writer.write("  \"age\": \"25\",\n");
            writer.write("  \"isActive\": \"true\",\n");
            writer.write("  \"emptyValue\": \"\",\n");
            writer.write("  \"specialChars\": \"!@#$%^&*()\",\n");
            writer.write("  \"url\": \"https://www.saucedemo.com\",\n");
            writer.write("  \"searchQuery\": \"selenium\",\n");
            writer.write("  \"expectedTitle\": \"Swag Labs\"\n");
            writer.write("}\n");
        }
        
        testData = new SHAFT.TestData.JSON("unitTestData.json");
    }

    @Test(description = "Test retrieving string value from JSON")
    public void testGetStringValue() {
        String username = testData.getTestData("username");
        Assert.assertEquals(username, "testUser", "Username should match");
    }

    @Test(description = "Test retrieving password from JSON")
    public void testGetPasswordValue() {
        String password = testData.getTestData("password");
        Assert.assertEquals(password, "testPass123", "Password should match");
    }

    @Test(description = "Test retrieving email from JSON")
    public void testGetEmailValue() {
        String email = testData.getTestData("email");
        Assert.assertEquals(email, "test@example.com", "Email should match");
    }

    @Test(description = "Test retrieving numeric string from JSON")
    public void testGetNumericStringValue() {
        String age = testData.getTestData("age");
        Assert.assertEquals(age, "25", "Age should match");
    }

    @Test(description = "Test retrieving boolean string from JSON")
    public void testGetBooleanStringValue() {
        String isActive = testData.getTestData("isActive");
        Assert.assertEquals(isActive, "true", "IsActive should match");
    }

    @Test(description = "Test retrieving empty value from JSON")
    public void testGetEmptyValue() {
        String emptyValue = testData.getTestData("emptyValue");
        Assert.assertNotNull(emptyValue, "Empty value should not be null");
        Assert.assertEquals(emptyValue, "", "Empty value should be empty string");
    }

    @Test(description = "Test retrieving value with special characters")
    public void testGetSpecialCharactersValue() {
        String specialChars = testData.getTestData("specialChars");
        Assert.assertEquals(specialChars, "!@#$%^&*()", "Special chars should match");
    }

    @Test(description = "Test retrieving URL from JSON")
    public void testGetUrlValue() {
        String url = testData.getTestData("url");
        Assert.assertEquals(url, "https://www.saucedemo.com", "URL should match");
        Assert.assertTrue(url.startsWith("https://"), "URL should start with https");
    }

    @Test(description = "Test retrieving search query from JSON")
    public void testGetSearchQueryValue() {
        String searchQuery = testData.getTestData("searchQuery");
        Assert.assertEquals(searchQuery, "selenium", "Search query should match");
    }

    @Test(description = "Test retrieving expected title from JSON")
    public void testGetExpectedTitleValue() {
        String expectedTitle = testData.getTestData("expectedTitle");
        Assert.assertEquals(expectedTitle, "Swag Labs", "Expected title should match");
    }

    @Test(description = "Test that test data object is not null")
    public void testTestDataObjectNotNull() {
        Assert.assertNotNull(testData, "TestData object should not be null");
    }

    @Test(description = "Test retrieving multiple values in sequence")
    public void testGetMultipleValuesInSequence() {
        String username = testData.getTestData("username");
        String password = testData.getTestData("password");
        String email = testData.getTestData("email");
        
        Assert.assertEquals(username, "testUser", "Username should match");
        Assert.assertEquals(password, "testPass123", "Password should match");
        Assert.assertEquals(email, "test@example.com", "Email should match");
    }

    @Test(description = "Test case sensitivity of keys")
    public void testKeyCaseSensitivity() {
        String username = testData.getTestData("username");
        Assert.assertEquals(username, "testUser", "Case sensitive key should work");
    }
}
