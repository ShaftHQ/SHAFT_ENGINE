package com.shaft.gui.element.internal;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

public class FailureEvidenceTest {
    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Test(description = "Element-not-found evidence lists URL, title, per-frame matches and nearest candidates, redacted")
    public void notFoundEvidenceDescribesPageFramesAndCandidates() throws Exception {
        WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class)
                .defaultAnswer(RETURNS_DEEP_STUBS));
        By locator = By.id("submit-order");
        when(driver.getCurrentUrl()).thenReturn("https://shop.test/checkout?token=raw-secret-token");
        when(driver.getTitle()).thenReturn("Checkout");
        when(driver.findElements(By.tagName("iframe"))).thenReturn(List.of(mock(WebElement.class)));
        when(driver.findElements(locator)).thenReturn(List.of(mock(WebElement.class)));
        when(((JavascriptExecutor) driver).executeScript(anyString(), any())).thenReturn(List.of(
                Map.of("tagName", "button", "id", "submitOrder", "text", "Submit order", "score", 2)));

        String json = FailureEvidence.render(locator, driver, List.of(), new NoSuchElementException("missing"), Map.of(), true);
        JsonNode node = MAPPER.readTree(json);

        Assert.assertEquals(node.path("locator").asText(), "By.id: submit-order");
        Assert.assertEquals(node.path("title").asText(), "Checkout");
        Assert.assertEquals(node.path("matchCount").asInt(), 0);
        Assert.assertEquals(node.path("frameMatchCounts").get(0).path("matchCount").asInt(), 1,
                "a match inside iframe 0 must be reported");
        Assert.assertEquals(node.path("nearestCandidates").get(0).path("id").asText(), "submitOrder");
        Assert.assertTrue(node.path("screenshotAttached").asBoolean());
        Assert.assertFalse(json.contains("raw-secret-token"), "URL secrets must be redacted: " + json);
        verify(driver.switchTo(), times(1)).parentFrame();
    }

    @Test(description = "Evidence never exceeds 256 KB even with huge page text")
    public void evidenceIsCappedAt256Kb() {
        WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class)
                .defaultAnswer(RETURNS_DEEP_STUBS));
        String huge = "x".repeat(400 * 1024);
        when(driver.getTitle()).thenReturn(huge);
        when(((JavascriptExecutor) driver).executeScript(anyString(), any())).thenReturn(List.of(Map.of("text", huge)));

        String json = FailureEvidence.render(By.id("a"), driver, List.of(), new RuntimeException(huge), Map.of(), false);

        Assert.assertTrue(json.getBytes(StandardCharsets.UTF_8).length <= FailureEvidence.MAX_BYTES,
                "evidence must stay within the 256 KB cap");
    }

    @Test(description = "A unique match skips the frame scan and candidate probe")
    public void uniqueMatchSkipsFrameScan() throws Exception {
        WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class)
                .defaultAnswer(RETURNS_DEEP_STUBS));
        JsonNode node = MAPPER.readTree(FailureEvidence.render(By.id("a"), driver, List.of(mock(WebElement.class)),
                new RuntimeException("intercepted"), Map.of("matchCount", 1, "locator", "By.id: a"), false));

        Assert.assertTrue(node.path("frameMatchCounts").isMissingNode());
        Assert.assertTrue(node.path("nearestCandidates").isMissingNode());
        verify(driver.switchTo(), times(0)).frame(anyInt());
    }
}
