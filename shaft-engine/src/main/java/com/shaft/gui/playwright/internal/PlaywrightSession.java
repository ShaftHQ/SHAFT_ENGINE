package com.shaft.gui.playwright.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Dialog;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Playwright;
import com.shaft.tools.io.ReportManager;
import org.apache.logging.log4j.Level;

import java.util.IdentityHashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Owns a single Playwright runtime session for one SHAFT GUI driver instance.
 */
public final class PlaywrightSession implements AutoCloseable {
    private final Playwright playwright;
    private final Browser browser;
    private final BrowserContext browserContext;
    private Page page;
    private final PlaywrightTraceManager traceManager;
    private final AtomicReference<String> lastDialogText = new AtomicReference<>();
    private final AtomicBoolean dialogSeen = new AtomicBoolean();
    private final AtomicReference<DialogAction> nextDialogAction = new AtomicReference<>();
    private final AtomicReference<String> nextPromptText = new AtomicReference<>("");
    private final Map<Page, String> pageHandles = new IdentityHashMap<>();
    private int nextPageHandleIndex = 1;

    PlaywrightSession(Playwright playwright, Browser browser, BrowserContext browserContext, Page page,
                      PlaywrightTraceManager traceManager) {
        this.playwright = playwright;
        this.browser = browser;
        this.browserContext = browserContext;
        this.page = page;
        this.traceManager = traceManager;
        registerDialogBridge(page);
    }

    public Playwright playwright() {
        return playwright;
    }

    public Browser browser() {
        return browser;
    }

    public BrowserContext browserContext() {
        return browserContext;
    }

    public Page page() {
        return page;
    }

    public void setPage(Page page) {
        this.page = page;
        registerDialogBridge(page);
    }

    public PlaywrightTraceManager traceManager() {
        return traceManager;
    }

    public boolean isDialogSeen() {
        return dialogSeen.get();
    }

    public String lastDialogText() {
        return lastDialogText.get();
    }

    public void acceptNextDialog() {
        dialogSeen.set(false);
        nextDialogAction.set(DialogAction.ACCEPT);
    }

    public void dismissNextDialog() {
        dialogSeen.set(false);
        nextDialogAction.set(DialogAction.DISMISS);
    }

    public void typeIntoNextPrompt(String text) {
        dialogSeen.set(false);
        nextPromptText.set(text);
        nextDialogAction.set(DialogAction.PROMPT);
    }

    public synchronized String pageHandle(Page targetPage) {
        return pageHandles.computeIfAbsent(targetPage, ignored -> "page-" + nextPageHandleIndex++);
    }

    public synchronized Page pageByHandle(String handle) {
        for (Page candidate : browserContext.pages()) {
            if (pageHandle(candidate).equals(handle)) {
                return candidate;
            }
        }
        return null;
    }

    @Override
    public void close() {
        if (traceManager != null) {
            traceManager.stopAndAttach();
        }
        closeQuietly(page);
        closeQuietly(browserContext);
        closeQuietly(browser);
        closeQuietly(playwright);
    }

    private void registerDialogBridge(Page targetPage) {
        if (targetPage == null) {
            return;
        }
        targetPage.onDialog(dialog -> {
            lastDialogText.set(dialog.message());
            dialogSeen.set(true);
            DialogAction action = nextDialogAction.getAndSet(null);
            try {
                if (action == DialogAction.ACCEPT) {
                    dialog.accept();
                } else if (action == DialogAction.PROMPT) {
                    dialog.accept(nextPromptText.getAndSet(""));
                } else {
                    dialog.dismiss();
                }
            } catch (RuntimeException e) {
                ReportManager.logDiscrete("Failed to handle Playwright dialog: " + e.getMessage(), Level.WARN);
            }
        });
    }

    private static void closeQuietly(Object resource) {
        if (resource == null) {
            return;
        }
        try {
            if (resource instanceof AutoCloseable closeable) {
                closeable.close();
            }
        } catch (Exception e) {
            ReportManager.logDiscrete("Failed to close Playwright resource: " + e.getMessage(), Level.WARN);
        }
    }

    private enum DialogAction {
        ACCEPT,
        DISMISS,
        PROMPT
    }
}
