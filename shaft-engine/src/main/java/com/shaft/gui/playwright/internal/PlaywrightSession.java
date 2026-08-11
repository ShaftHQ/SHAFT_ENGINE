package com.shaft.gui.playwright.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Dialog;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Playwright;
import com.shaft.gui.browser.internal.PlaywrightNetworkInterceptor;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import org.apache.logging.log4j.Level;

import java.util.IdentityHashMap;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Owns a single Playwright runtime session for one SHAFT GUI driver instance.
 */
public final class PlaywrightSession implements AutoCloseable {
    private static final int CONSOLE_EVENT_LIMIT = 1000;
    private final Playwright playwright;
    private final Browser browser;
    private final BrowserContext browserContext;
    private Page page;
    private final PlaywrightTraceManager traceManager;
    private final PlaywrightNetworkInterceptor networkInterceptor;
    private final AtomicReference<String> lastDialogText = new AtomicReference<>();
    private final AtomicBoolean dialogSeen = new AtomicBoolean();
    private final AtomicReference<DialogAction> nextDialogAction = new AtomicReference<>();
    private final AtomicReference<String> nextPromptText = new AtomicReference<>("");
    private final Map<Page, String> pageHandles = new IdentityHashMap<>();
    private final Set<Page> observedPages = Collections.newSetFromMap(new IdentityHashMap<>());
    private final List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> consoleEvents = new ArrayList<>();
    private int nextPageHandleIndex = 1;

    PlaywrightSession(Playwright playwright, Browser browser, BrowserContext browserContext, Page page,
                      PlaywrightTraceManager traceManager) {
        this.playwright = playwright;
        this.browser = browser;
        this.browserContext = browserContext;
        this.page = page;
        this.traceManager = traceManager;
        this.networkInterceptor = new PlaywrightNetworkInterceptor(browserContext);
        registerDialogBridge(page);
        registerConsoleBridge(page);
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
        registerConsoleBridge(page);
    }

    public PlaywrightTraceManager traceManager() {
        return traceManager;
    }

    public PlaywrightNetworkInterceptor networkInterceptor() {
        return networkInterceptor;
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
        clearConsole();
        networkInterceptor.clear();
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

    private void registerConsoleBridge(Page targetPage) {
        if (targetPage == null || !observedPages.add(targetPage)) {
            return;
        }
        targetPage.onConsoleMessage(message -> recordConsole(message.type(), message.text()));
        targetPage.onPageError(message -> recordConsole("pageerror", message));
    }

    /** @return immutable console observations owned by this session, oldest first */
    public synchronized List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> consoleSnapshot() {
        return List.copyOf(consoleEvents);
    }

    /** Clears only this Playwright session's console observations. */
    public synchronized void clearConsole() {
        consoleEvents.clear();
    }

    /** Atomically transfers this session's console observations to failure-trace storage. */
    public void drainConsoleToRecorder() {
        List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> snapshot;
        synchronized (this) {
            snapshot = List.copyOf(consoleEvents);
            consoleEvents.clear();
        }
        for (BrowserObservabilityRecorder.ConsoleSnapshotEntry entry : snapshot) {
            BrowserObservabilityRecorder.recordConsole(entry.source(), entry.level(), entry.message(), entry.timestamp());
        }
    }

    private synchronized void recordConsole(String level, String message) {
        if (consoleEvents.size() >= CONSOLE_EVENT_LIMIT) {
            consoleEvents.removeFirst();
        }
        consoleEvents.add(BrowserObservabilityRecorder.consoleEntry(
                "playwright", level, message, System.currentTimeMillis()));
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
