package testPackage.unitTests;

import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.tools.io.JSONFileManager;
import com.shaft.tools.io.JSONFileManagerTestAccessor;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

/**
 * Unit tests for ThreadLocal memory-leak fixes in {@link JSONFileManager}
 * and {@link LocatorBuilder}.
 */
public class MemoryLeakFixTests {

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        // Ensure LocatorBuilder state is clean after each test
        LocatorBuilder.cleanup();
    }

    // ── JSONFileManager ──────────────────────────────────────────────────────

    @Test(description = "JSONFileManager should not leave reader ThreadLocal set after getTestData()")
    public void jsonFileManagerShouldClearReaderThreadLocalAfterRead() {
        JSONFileManager testData = new JSONFileManager("simpleJSON.json");

        // The constructor should close and remove the reader immediately
        Assert.assertNull(JSONFileManagerTestAccessor.getReader(),
                "reader ThreadLocal should be null after the constructor closes it.");

        testData.getTestData("name");

        Assert.assertNull(JSONFileManagerTestAccessor.getReader(),
                "reader ThreadLocal should be null after getTestData() closes it in the finally block.");
    }

    @Test(description = "JSONFileManager should read correct value even after ThreadLocal is cleared between calls")
    public void jsonFileManagerShouldReturnCorrectValueAfterThreadLocalCleanup() {
        JSONFileManager testData = new JSONFileManager("simpleJSON.json");

        // First call – opens, reads, and closes reader
        String firstValue = testData.getTestData("name");
        Assert.assertNotNull(firstValue,
                "First getTestData() call should return a non-null value.");

        // Ensure ThreadLocal was cleared between calls
        Assert.assertNull(JSONFileManagerTestAccessor.getReader(),
                "reader ThreadLocal should be null between calls.");

        // Second call – should open a fresh reader and return the same value
        String secondValue = testData.getTestData("name");
        Assert.assertEquals(secondValue, firstValue,
                "Second getTestData() call should return the same value as the first.");

        Assert.assertNull(JSONFileManagerTestAccessor.getReader(),
                "reader ThreadLocal should be null after the second getTestData() call.");
    }

    // ── LocatorBuilder ───────────────────────────────────────────────────────

    @Test(description = "LocatorBuilder.cleanup() should remove iFrameLocator ThreadLocal")
    public void locatorBuilderCleanupShouldClearIFrameLocator() {
        // Simulate iFrame state being set (as happens after switchToIframe)
        LocatorBuilder.getIFrameLocator().set(By.id("myFrame"));
        Assert.assertNotNull(LocatorBuilder.getIFrameLocator().get(),
                "iFrameLocator should be set before cleanup.");

        LocatorBuilder.cleanup();

        Assert.assertNull(LocatorBuilder.getIFrameLocator().get(),
                "iFrameLocator ThreadLocal should be null after LocatorBuilder.cleanup().");
    }

    @Test(description = "LocatorBuilder.cleanup() should remove shadowDomLocator ThreadLocal")
    public void locatorBuilderCleanupShouldClearShadowDomLocator() {
        // Simulate shadow-DOM state being set
        LocatorBuilder.getShadowDomLocator().set(By.cssSelector("#shadow-host"));
        Assert.assertNotNull(LocatorBuilder.getShadowDomLocator().get(),
                "shadowDomLocator should be set before cleanup.");

        LocatorBuilder.cleanup();

        Assert.assertNull(LocatorBuilder.getShadowDomLocator().get(),
                "shadowDomLocator ThreadLocal should be null after LocatorBuilder.cleanup().");
    }

    @Test(description = "LocatorBuilder.cleanup() should be safe to call when nothing was set")
    public void locatorBuilderCleanupShouldBeIdempotent() {
        // No state set – calling cleanup() should not throw
        LocatorBuilder.cleanup();
        Assert.assertNull(LocatorBuilder.getIFrameLocator().get(),
                "iFrameLocator ThreadLocal should be null when cleanup() is called on clean state.");
        Assert.assertNull(LocatorBuilder.getShadowDomLocator().get(),
                "shadowDomLocator ThreadLocal should be null when cleanup() is called on clean state.");
    }
}
