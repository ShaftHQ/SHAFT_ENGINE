package com.shaft.gui.element.internal;

import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

public class ElementActionabilityDiagnosticsTest {

    @Test(description = "Diagnostics should describe visible disabled elements and center-point blockers")
    public void collectShouldDescribeVisibleDisabledObscuredElement() {
        WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
        WebElement element = mock(WebElement.class);
        when(driver.getCurrentUrl()).thenReturn("https://shop.test/checkout?token=raw-token");
        when(element.isDisplayed()).thenReturn(true);
        when(element.isEnabled()).thenReturn(false);
        when(element.isSelected()).thenReturn(false);
        when(element.getTagName()).thenReturn("button");
        when(element.getAccessibleName()).thenReturn("Pay now");
        when(element.getText()).thenReturn("Pay now");
        when(element.getRect()).thenReturn(new Rectangle(10, 20, 100, 40));
        when(element.getCssValue("display")).thenReturn("block");
        when(element.getCssValue("visibility")).thenReturn("visible");
        when(element.getCssValue("opacity")).thenReturn("1");
        when(element.getCssValue("pointer-events")).thenReturn("auto");
        when(element.getDomAttribute("aria-label")).thenReturn("Pay now");
        when(element.getDomAttribute("title")).thenReturn("Checkout");
        when(((JavascriptExecutor) driver).executeScript(anyString(), same(element))).thenReturn(Map.of(
                "viewport", Map.of("width", 1280, "height", 720),
                "centerPoint", Map.of("x", 60, "y", 40),
                "obscuringElement", Map.of("selector", ".modal-backdrop", "tagName", "div", "textPreview", "Close modal"),
                "centerTargetMatchesElement", false));

        Map<String, Object> diagnostics = ElementActionabilityDiagnostics.collect(
                By.id("pay"), driver, List.of(element), new RuntimeException("click intercepted"));

        Assert.assertEquals(diagnostics.get("locator"), "By.id: pay");
        Assert.assertEquals(diagnostics.get("matchCount"), 1);
        Assert.assertEquals(diagnostics.get("displayed"), true);
        Assert.assertEquals(diagnostics.get("enabled"), false);
        Assert.assertEquals(diagnostics.get("tagName"), "button");
        Assert.assertEquals(diagnostics.get("accessibleName"), "Pay now");
        Assert.assertTrue(String.valueOf(diagnostics.get("recommendation")).contains("blocking element"));
        Assert.assertEquals(((Map<?, ?>) diagnostics.get("css")).get("pointerEvents"), "auto");
        Assert.assertEquals(((Map<?, ?>) diagnostics.get("obscuringElement")).get("selector"), ".modal-backdrop");
    }

    @Test(description = "Diagnostics should explain missing and multi-match locator failures")
    public void collectShouldDescribeMissingAndMultiMatchFallbacks() {
        WebDriver driver = mock(WebDriver.class);
        WebElement first = mock(WebElement.class);
        WebElement second = mock(WebElement.class);

        Map<String, Object> missing = ElementActionabilityDiagnostics.collect(
                By.cssSelector(".missing"), driver, List.of(), new RuntimeException("missing"));
        Map<String, Object> multi = ElementActionabilityDiagnostics.collect(
                By.cssSelector(".pay"), driver, List.of(first, second), new RuntimeException("multiple"));

        Assert.assertEquals(missing.get("matchCount"), 0);
        Assert.assertTrue(String.valueOf(missing.get("recommendation")).contains("locator"));
        Assert.assertEquals(multi.get("matchCount"), 2);
        Assert.assertTrue(String.valueOf(multi.get("recommendation")).contains("unique"));
    }

    @Test(description = "Diagnostics should preserve stale-element and probe failures as warnings")
    public void collectShouldKeepWarningsForStaleAndProbeFailures() {
        WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
        WebElement element = mock(WebElement.class);
        doThrow(new StaleElementReferenceException("gone")).when(element).isDisplayed();
        when(((JavascriptExecutor) driver).executeScript(anyString(), same(element)))
                .thenThrow(new JavascriptException("probe broke"));

        Map<String, Object> diagnostics = ElementActionabilityDiagnostics.collect(
                By.id("pay"), driver, List.of(element), new RuntimeException("failed"));

        Assert.assertEquals(diagnostics.get("stale"), true);
        Assert.assertTrue(String.valueOf(diagnostics.get("warnings")).contains("stale"));
        Assert.assertTrue(String.valueOf(diagnostics.get("warnings")).contains("probe"));
    }
}
