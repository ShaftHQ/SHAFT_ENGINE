package com.shaft.gui.playwright.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Download;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Playwright;
import com.microsoft.playwright.options.ColorScheme;
import com.microsoft.playwright.options.Media;
import com.microsoft.playwright.options.ReducedMotion;
import com.microsoft.playwright.options.ViewportSize;
import com.shaft.gui.browser.internal.PlaywrightNetworkInterceptor;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import com.shaft.tools.io.internal.FailureTraceReporter;
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
    private final Map<Page, ViewportSize> initialViewports = new IdentityHashMap<>();
    private final Map<Page, MediaState> mediaStates = new IdentityHashMap<>();
    private final Set<Page> observedPages = Collections.newSetFromMap(new IdentityHashMap<>());
    private final Set<Page> downloadObservedPages = Collections.newSetFromMap(new IdentityHashMap<>());
    private final List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> consoleEvents = new ArrayList<>();
    private boolean oldestConsoleEventOmitted;
    private final BrowserObservabilityRecorder.ObservationBinding observationBinding;
    private final List<Download> downloads = new ArrayList<>();
    private int nextPageHandleIndex = 1;

    PlaywrightSession(Playwright playwright, Browser browser, BrowserContext browserContext, Page page,
                      PlaywrightTraceManager traceManager) {
        this.playwright = playwright;
        this.browser = browser;
        this.browserContext = browserContext;
        this.page = page;
        this.traceManager = traceManager;
        this.observationBinding = BrowserObservabilityRecorder.captureBinding();
        this.networkInterceptor = new PlaywrightNetworkInterceptor(browserContext);
        registerDownloadContextBridge();
        registerDialogBridge(page);
        registerConsoleBridge(page);
        registerDownloadBridge(page);
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
        registerDownloadBridge(page);
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
        try {
            clearConsole();
            clearDownloadHandles();
            initialViewports.clear();
            mediaStates.clear();
            networkInterceptor.close();
            if (traceManager != null) {
                traceManager.stopAndAttach();
            }
            closeQuietly(page);
            closeQuietly(browserContext);
            closeQuietly(browser);
            closeQuietly(playwright);
        } finally {
            FailureTraceReporter.clearPersistentSensitiveBrowserState(this);
        }
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

    private synchronized void registerDownloadBridge(Page targetPage) {
        if (targetPage == null || !downloadObservedPages.add(targetPage)) {
            return;
        }
        targetPage.onDownload(this::trackDownload);
    }

    private void registerDownloadContextBridge() {
        if (browserContext == null) {
            return;
        }
        browserContext.onPage(this::registerDownloadBridge);
        for (Page existingPage : browserContext.pages()) {
            registerDownloadBridge(existingPage);
        }
    }

    /** @return immutable console observations owned by this session, oldest first */
    public synchronized List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> consoleSnapshot() {
        return List.copyOf(consoleEvents);
    }

    /** Clears only this Playwright session's console observations. */
    public synchronized void clearConsole() {
        consoleEvents.clear();
        oldestConsoleEventOmitted = false;
    }

    /** @return immutable native download handles owned by this session, oldest first */
    public synchronized List<Download> downloadSnapshot() {
        return List.copyOf(downloads);
    }

    /** Retains one native download handle without duplicating listener and wait-for observations. */
    public synchronized void trackDownload(Download download) {
        if (download != null && downloads.stream().noneMatch(existing -> existing == download)) {
            downloads.add(download);
        }
    }

    /** Removes a deleted native download handle from this session. */
    public synchronized void forgetDownload(Download download) {
        downloads.removeIf(existing -> existing == download);
    }

    /** Clears retained handles after their native files have been removed. */
    public synchronized void clearDownloadHandles() {
        downloads.clear();
    }

    /** Applies a page viewport override while retaining the pre-SHAFT value for a later reset. */
    public synchronized void setViewport(Page targetPage, int width, int height) {
        if (!initialViewports.containsKey(targetPage)) {
            initialViewports.put(targetPage, targetPage.viewportSize());
        }
        targetPage.setViewportSize(width, height);
    }

    /** Restores and forgets the page viewport captured before SHAFT's first override. */
    public synchronized void clearViewport(Page targetPage) {
        if (!initialViewports.containsKey(targetPage) || initialViewports.get(targetPage) == null) {
            throw new UnsupportedOperationException(
                    "The original Playwright viewport was disabled and cannot be restored on a live page; recreate the context.");
        }
        ViewportSize original = initialViewports.get(targetPage);
        targetPage.setViewportSize(original.width, original.height);
        initialViewports.remove(targetPage);
    }

    /** Applies a media type without clearing the page's other SHAFT-owned media overrides. */
    public synchronized void setMediaType(Page targetPage, Media media) {
        applyMedia(targetPage, mediaState(targetPage).withMedia(media));
    }

    /** Applies a color scheme without clearing the page's other SHAFT-owned media overrides. */
    public synchronized void setColorScheme(Page targetPage, ColorScheme colorScheme) {
        applyMedia(targetPage, mediaState(targetPage).withColorScheme(colorScheme));
    }

    /** Applies reduced-motion preference without clearing the page's other SHAFT-owned media overrides. */
    public synchronized void setReducedMotion(Page targetPage, ReducedMotion reducedMotion) {
        applyMedia(targetPage, mediaState(targetPage).withReducedMotion(reducedMotion));
    }

    /** Clears every SHAFT-owned media override for one page. */
    public synchronized void resetMedia(Page targetPage) {
        targetPage.emulateMedia(new Page.EmulateMediaOptions());
        mediaStates.remove(targetPage);
    }

    private MediaState mediaState(Page targetPage) {
        return mediaStates.getOrDefault(targetPage, MediaState.EMPTY);
    }

    private void applyMedia(Page targetPage, MediaState state) {
        Page.EmulateMediaOptions options = new Page.EmulateMediaOptions();
        if (state.media() != null) {
            options.setMedia(state.media());
        }
        if (state.colorScheme() != null) {
            options.setColorScheme(state.colorScheme());
        }
        if (state.reducedMotion() != null) {
            options.setReducedMotion(state.reducedMotion());
        }
        targetPage.emulateMedia(options);
        mediaStates.put(targetPage, state);
    }

    /** Atomically transfers this session's console observations to failure-trace storage. */
    public void drainConsoleToRecorder() {
        BrowserObservabilityRecorder.ObservationSession owner =
                BrowserObservabilityRecorder.resolveSession(observationBinding);
        List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> snapshot;
        boolean oldestOmitted;
        synchronized (this) {
            snapshot = List.copyOf(consoleEvents);
            consoleEvents.clear();
            oldestOmitted = oldestConsoleEventOmitted;
            oldestConsoleEventOmitted = false;
        }
        if (oldestOmitted) {
            BrowserObservabilityRecorder.recordConsoleOmission(owner);
        }
        for (BrowserObservabilityRecorder.ConsoleSnapshotEntry entry : snapshot) {
            BrowserObservabilityRecorder.recordConsole(owner,
                    entry.source(), entry.level(), entry.message(), entry.timestamp());
        }
    }

    private synchronized void recordConsole(String level, String message) {
        if (consoleEvents.size() >= CONSOLE_EVENT_LIMIT) {
            consoleEvents.removeFirst();
            oldestConsoleEventOmitted = true;
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

    private record MediaState(Media media, ColorScheme colorScheme, ReducedMotion reducedMotion) {
        private static final MediaState EMPTY = new MediaState(null, null, null);

        private MediaState withMedia(Media value) {
            return new MediaState(value, colorScheme, reducedMotion);
        }

        private MediaState withColorScheme(ColorScheme value) {
            return new MediaState(media, value, reducedMotion);
        }

        private MediaState withReducedMotion(ReducedMotion value) {
            return new MediaState(media, colorScheme, value);
        }
    }
}
