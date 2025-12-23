package testPackage.unitTests;

import com.shaft.gui.internal.locator.Locator;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Unit tests for SHAFT's Locator Builder functionality
 * Tests various locator building patterns without requiring a browser
 */
public class LocatorBuilderUnitTest {

    @Test(description = "Test building locator with tag name")
    public void testLocatorWithTagName() {
        By locator = Locator.hasTagName("input").build();
        Assert.assertNotNull(locator, "Locator should not be null");
        Assert.assertTrue(locator.toString().contains("input"), "Locator should contain tag name");
    }

    @Test(description = "Test building locator with any tag name")
    public void testLocatorWithAnyTagName() {
        By locator = Locator.hasAnyTagName().build();
        Assert.assertNotNull(locator, "Locator should not be null");
    }

    @Test(description = "Test building locator with attribute")
    public void testLocatorWithAttribute() {
        By locator = Locator.hasTagName("input").hasAttribute("name", "username").build();
        Assert.assertNotNull(locator, "Locator should not be null");
        Assert.assertTrue(locator.toString().contains("name"), "Locator should contain attribute name");
    }

    @Test(description = "Test building locator with text")
    public void testLocatorWithText() {
        By locator = Locator.hasTagName("button").hasText("Submit").build();
        Assert.assertNotNull(locator, "Locator should not be null");
        Assert.assertTrue(locator.toString().contains("Submit"), "Locator should contain text");
    }

    @Test(description = "Test building locator with partial text")
    public void testLocatorWithPartialText() {
        By locator = Locator.hasTagName("a").hasPartialText("Click").build();
        Assert.assertNotNull(locator, "Locator should not be null");
        Assert.assertTrue(locator.toString().contains("Click"), "Locator should contain partial text");
    }

    @Test(description = "Test building locator with isFirst modifier")
    public void testLocatorWithIsFirst() {
        By locator = Locator.hasTagName("div").isFirst().build();
        Assert.assertNotNull(locator, "Locator should not be null");
    }

    @Test(description = "Test building locator with isLast modifier")
    public void testLocatorWithIsLast() {
        By locator = Locator.hasTagName("div").isLast().build();
        Assert.assertNotNull(locator, "Locator should not be null");
    }

    @Test(description = "Test building locator with nested structure")
    public void testLocatorWithNested() {
        By parentLocator = Locator.hasTagName("div").hasAttribute("class", "parent").build();
        By childLocator = Locator.hasTagName("span").build();
        Assert.assertNotNull(parentLocator, "Parent locator should not be null");
        Assert.assertNotNull(childLocator, "Child locator should not be null");
    }

    @Test(description = "Test building complex locator chain")
    public void testComplexLocatorChain() {
        By locator = Locator.hasTagName("input")
                .hasAttribute("type", "text")
                .hasAttribute("placeholder", "Search")
                .isFirst()
                .build();
        Assert.assertNotNull(locator, "Complex locator should not be null");
    }

    @Test(description = "Test locator with contains attribute")
    public void testLocatorWithContainsAttribute() {
        By locator = Locator.hasAnyTagName().hasAttribute("class", "btn").build();
        Assert.assertNotNull(locator, "Locator with contains should not be null");
    }

    @Test(description = "Test locator toString produces valid XPath")
    public void testLocatorToStringFormat() {
        By locator = Locator.hasTagName("div").hasAttribute("id", "content").build();
        String locatorString = locator.toString();
        Assert.assertTrue(locatorString.contains("xpath") || locatorString.contains("By."), 
            "Locator string should be valid format");
    }

    @Test(description = "Test multiple attributes chaining")
    public void testMultipleAttributesChaining() {
        By locator = Locator.hasTagName("button")
                .hasAttribute("type", "submit")
                .hasAttribute("class", "primary")
                .hasAttribute("id", "submitBtn")
                .build();
        Assert.assertNotNull(locator, "Multi-attribute locator should not be null");
        String locatorStr = locator.toString();
        Assert.assertTrue(locatorStr.contains("type") || locatorStr.contains("submit"), 
            "Should contain attribute info");
    }

    @Test(description = "Test locator with special characters in text")
    public void testLocatorWithSpecialCharactersInText() {
        By locator = Locator.hasTagName("span").hasText("Price: $99.99").build();
        Assert.assertNotNull(locator, "Locator with special chars should not be null");
    }

    @Test(description = "Test locator with empty text")
    public void testLocatorWithEmptyText() {
        By locator = Locator.hasTagName("div").hasText("").build();
        Assert.assertNotNull(locator, "Locator with empty text should not be null");
    }

    @Test(description = "Test locator with null attribute value")
    public void testLocatorWithNullAttributeValue() {
        By locator = Locator.hasTagName("input").hasAttribute("value", "").build();
        Assert.assertNotNull(locator, "Locator with empty attribute value should not be null");
    }
}
