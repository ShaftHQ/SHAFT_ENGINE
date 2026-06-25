package com.shaft.gui.internal.natural;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.properties.internal.Properties;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class PlaywrightNaturalActionExecutorTest {
    private PlaywrightSession session;
    private Page page;
    private Locator locator;

    @BeforeMethod(alwaysRun = true)
    public void setUp() {
        SHAFTProperties();
        session = mock(PlaywrightSession.class);
        page = mock(Page.class);
        locator = mock(Locator.class);
        when(session.page()).thenReturn(page);
        when(page.locator(anyString())).thenReturn(locator);
        when(locator.count()).thenReturn(1);
        when(locator.isVisible()).thenReturn(true);
        when(locator.isEnabled()).thenReturn(true);
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void enabledBrowserIntentShouldExecuteThroughPlaywright() {
        PlaywrightNaturalActionExecutor.perform(session, "refresh page");

        verify(page).reload();
    }

    @Test
    public void enabledElementIntentShouldExecuteThroughPlaywrightLocator() {
        PlaywrightNaturalActionExecutor.perform(session, "click Submit");

        verify(locator).click();
    }

    @Test
    public void touchIntentShouldFailWithSpecificUnsupportedReason() {
        RuntimeException exception = Assert.expectThrows(
                RuntimeException.class,
                () -> PlaywrightNaturalActionExecutor.perform(session, "tap Submit"));

        Assert.assertTrue(exception.getMessage().contains("Touch natural actions are not available"));
        verify(locator, never()).click();
    }

    private void SHAFTProperties() {
        com.shaft.driver.SHAFT.Properties.naturalActions.set()
                .enabled(true)
                .minimumTrustPercentage(85)
                .allowedActions("browser,element,touch");
    }
}
