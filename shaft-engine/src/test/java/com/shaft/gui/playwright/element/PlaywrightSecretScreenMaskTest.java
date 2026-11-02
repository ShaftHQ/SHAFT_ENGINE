package com.shaft.gui.playwright.element;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.shaft.gui.internal.image.SecretScreenMask;
import com.shaft.gui.playwright.browser.BrowserActions;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import org.mockito.ArgumentCaptor;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class PlaywrightSecretScreenMaskTest {
    @AfterMethod(alwaysRun = true)
    public void clear() {
        SecretScreenMask.clear();
    }

    @Test
    public void securelyTypedLocatorIsMaskedInLaterPlaywrightScreenshots() {
        Page page = mock(Page.class);
        PlaywrightSession session = mock(PlaywrightSession.class);
        when(session.page()).thenReturn(page);
        Locator secret = mock(Locator.class);
        Locator plain = mock(Locator.class);
        when(secret.page()).thenReturn(page);
        when(plain.page()).thenReturn(page);
        when(secret.screenshot(any())).thenReturn(new byte[]{1});
        when(page.screenshot(any())).thenReturn(new byte[]{1});
        ElementActions actions = new ElementActions(session);

        actions.type(plain, "hello");
        actions.typeSecure(secret, "pw");
        actions.typeAppend(plain, "x");
        actions.captureScreenshot(secret);
        new BrowserActions(session).captureScreenshot();

        ArgumentCaptor<Locator.ScreenshotOptions> element = ArgumentCaptor.forClass(Locator.ScreenshotOptions.class);
        verify(secret).screenshot(element.capture());
        Assert.assertEquals(element.getValue().mask, java.util.List.of(secret));
        ArgumentCaptor<Page.ScreenshotOptions> full = ArgumentCaptor.forClass(Page.ScreenshotOptions.class);
        verify(page).screenshot(full.capture());
        Assert.assertEquals(full.getValue().mask, java.util.List.of(secret));
    }
}
