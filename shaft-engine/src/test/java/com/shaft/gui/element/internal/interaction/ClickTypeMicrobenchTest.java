package com.shaft.gui.element.internal.interaction;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.ReportManager;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.TestPageServer;

import java.lang.reflect.Method;

/**
 * Wave F (#5732): headless microbench for default happy-path text type + button click,
 * including an overlay-intercepted click recovered by the Wave B click ladder (JS fallback).
 *
 * <p>Baseline vs Wave B/C tradeoff (documented, not a historical checkout):
 * pre-strategy Selenium TYPE/CLICK for text-like inputs and buttons already used
 * {@code sendKeys}/{@code WebElement.click()}. Wave B/C add a cheap
 * {@link ElementClassifier#classify} attribute read before routing; TEXT_LIKE and BUTTON
 * still execute the same native primitives. Measured classifier overhead is asserted below;
 * end-to-end SHAFT timings include existing sync/screenshot budget and are logged for CI.
 */
@Test(singleThreaded = true)
public class ClickTypeMicrobenchTest {
    private static final By NAME = By.id("name");
    private static final By GO = By.id("go");
    private static final By RESULT = By.id("result");
    private static final String SAMPLE = "wave-f-proof";
    private static final int CLASSIFIER_ITERATIONS = 50_000;
    /** Pure classify(ElementSignals) budget — excludes Mockito/WebElement I/O. */
    private static final long CLASSIFIER_BUDGET_NANOS = 100_000_000L; // 100ms for 100k classifications
    private static final long ACTION_BUDGET_MILLIS = 15_000L;

    private final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @BeforeMethod(alwaysRun = true)
    public void init(Method method) {
        if ("classifierOverheadStaysNegligibleVersusPreStrategy".equals(method.getName())) {
            return;
        }
        if (!"local".equalsIgnoreCase(SHAFT.Properties.platform.executionAddress())
                || !"chrome".equalsIgnoreCase(SHAFT.Properties.web.targetBrowserName())) {
            throw new SkipException("Wave F microbench requires local Chrome.");
        }
        SHAFT.Properties.web.set().headlessExecution(true).targetBrowserName("chrome");
        SHAFT.Properties.flags.set().clickUsingJavascriptWhenWebDriverClickFails(true);
        SHAFT.Properties.flags.set().forceCheckElementLocatorIsUnique(false);
        SHAFT.Properties.flags.set().clearBeforeTypingMode("off");
        SHAFT.Properties.flags.set().forceCheckTextWasTypedCorrectly(false);
        SHAFT.Properties.flags.set().attemptToClickBeforeTyping(false);
        SHAFT.Properties.visuals.set().createAnimatedGif(false);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Never");
        SHAFT.Properties.visuals.set().screenshotParamsWatermark(false);
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void tear() {
        try {
            if (driver.get() != null) {
                driver.get().quit();
            }
        } finally {
            driver.remove();
            Properties.clearForCurrentThread();
        }
    }

    @Test
    public void classifierOverheadStaysNegligibleVersusPreStrategy() {
        // Measure pure routing cost (ElementSignals), not WebElement attribute I/O / Mockito.
        ElementSignals text = new ElementSignals(
                "input", "text", null, null, null, null, null, null, null, null, null, null, null);
        ElementSignals button = new ElementSignals(
                "button", "button", null, null, null, null, null, null, null, null, null, null, null);

        ElementClassifier.classify(text);
        ElementClassifier.classify(button);

        long start = System.nanoTime();
        for (int i = 0; i < CLASSIFIER_ITERATIONS; i++) {
            if (ElementClassifier.classify(text) != ElementKind.TEXT_LIKE
                    || ElementClassifier.classify(button) != ElementKind.BUTTON) {
                Assert.fail("Unexpected classify result during microbench loop");
            }
        }
        long elapsedNanos = System.nanoTime() - start;
        double perCallNanos = elapsedNanos / (double) (CLASSIFIER_ITERATIONS * 2L);

        ReportManager.log("Wave F classifier overhead: " + elapsedNanos + " ns for "
                + (CLASSIFIER_ITERATIONS * 2) + " classify(ElementSignals) calls ("
                + String.format("%.2f", perCallNanos) + " ns/call)");
        Assert.assertTrue(elapsedNanos < CLASSIFIER_BUDGET_NANOS,
                "Classifier overhead exceeded budget: " + elapsedNanos + " ns");
    }

    @Test
    public void strategyPathHappyPathTypeAndClickWithinBudget() {
        SHAFT.GUI.WebDriver shaft = driver.get();
        shaft.browser().navigateToURL(TestPageServer.url("clickTypeMicrobenchFixture.html"));
        dismissOverlay(shaft.getDriver());

        // Warm once so the timed section is action-dominated (sync/screenshot already paid).
        shaft.element().type(NAME, "warmup");
        shaft.element().click(GO);
        Assert.assertEquals(shaft.getDriver().findElement(RESULT).getAttribute("data-status"), "clicked");

        shaft.browser().navigateToURL(TestPageServer.url("clickTypeMicrobenchFixture.html"));
        dismissOverlay(shaft.getDriver());

        long typeStart = System.nanoTime();
        shaft.element().type(NAME, SAMPLE);
        long typeMillis = (System.nanoTime() - typeStart) / 1_000_000L;

        long clickStart = System.nanoTime();
        shaft.element().click(GO);
        long clickMillis = (System.nanoTime() - clickStart) / 1_000_000L;

        WebElement result = shaft.getDriver().findElement(RESULT);
        Assert.assertEquals(result.getAttribute("data-status"), "clicked");
        Assert.assertEquals(result.getText(), "clicked:" + SAMPLE);

        ReportManager.log("Wave F strategy happy-path timing (headless Chrome, overlay dismissed): type="
                + typeMillis + " ms, click=" + clickMillis + " ms, total="
                + (typeMillis + clickMillis) + " ms");
        Assert.assertTrue(typeMillis < ACTION_BUDGET_MILLIS,
                "Happy-path type exceeded budget: " + typeMillis + " ms");
        Assert.assertTrue(clickMillis < ACTION_BUDGET_MILLIS,
                "Happy-path click exceeded budget: " + clickMillis + " ms");
    }

    @Test
    public void strategyPathClickThroughOverlayRecoversWithJsFallback() {
        SHAFT.GUI.WebDriver shaft = driver.get();
        shaft.browser().navigateToURL(TestPageServer.url("clickTypeMicrobenchFixture.html"));

        shaft.element().type(NAME, SAMPLE);

        // Overlay still present: native click is intercepted; Wave B ladder + JS flag recovers.
        long clickStart = System.nanoTime();
        shaft.element().click(GO);
        long clickMillis = (System.nanoTime() - clickStart) / 1_000_000L;

        WebElement result = shaft.getDriver().findElement(RESULT);
        Assert.assertEquals(result.getAttribute("data-status"), "clicked");
        Assert.assertEquals(result.getText(), "clicked:" + SAMPLE);

        ReportManager.log("Wave F strategy click-through-overlay timing (JS fallback): "
                + clickMillis + " ms");
        Assert.assertTrue(clickMillis < ACTION_BUDGET_MILLIS,
                "Overlay click exceeded budget: " + clickMillis + " ms");
    }

    @Test
    public void rawSeleniumFloorRemainsAvailableForBaselineComparison() {
        SHAFT.GUI.WebDriver shaft = driver.get();
        WebDriver webDriver = shaft.getDriver();
        webDriver.navigate().to(TestPageServer.url("clickTypeMicrobenchFixture.html"));

        WebElement name = webDriver.findElement(NAME);
        WebElement go = webDriver.findElement(GO);

        long typeStart = System.nanoTime();
        name.clear();
        name.sendKeys(SAMPLE);
        long typeMillis = (System.nanoTime() - typeStart) / 1_000_000L;

        ((JavascriptExecutor) webDriver).executeScript("window.__shaftDismissOverlay();");

        long clickStart = System.nanoTime();
        go.click();
        long clickMillis = (System.nanoTime() - clickStart) / 1_000_000L;

        WebElement result = webDriver.findElement(RESULT);
        Assert.assertEquals(result.getAttribute("data-status"), "clicked");
        Assert.assertEquals(result.getText(), "clicked:" + SAMPLE);

        ReportManager.log("Wave F raw Selenium floor timing: type=" + typeMillis
                + " ms, click=" + clickMillis + " ms (no SHAFT sync/screenshot/classify)");
        Assert.assertTrue(typeMillis < ACTION_BUDGET_MILLIS);
        Assert.assertTrue(clickMillis < ACTION_BUDGET_MILLIS);
    }

    private static void dismissOverlay(WebDriver webDriver) {
        ((JavascriptExecutor) webDriver).executeScript("window.__shaftDismissOverlay();");
    }
}
