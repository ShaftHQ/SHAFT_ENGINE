package com.shaft.gui.playwright.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Playwright;

/**
 * Thread-local access to the active Playwright session.
 */
public final class PlaywrightSessionManager {
    private static final ThreadLocal<PlaywrightSession> SESSION = new ThreadLocal<>();

    private PlaywrightSessionManager() {
        throw new IllegalStateException("Utility class");
    }

    public static boolean hasSession() {
        return SESSION.get() != null;
    }

    public static PlaywrightSession currentSession() {
        return SESSION.get();
    }

    public static Playwright currentPlaywright() {
        PlaywrightSession session = currentSession();
        return session == null ? null : session.playwright();
    }

    public static Browser currentBrowser() {
        PlaywrightSession session = currentSession();
        return session == null ? null : session.browser();
    }

    public static BrowserContext currentBrowserContext() {
        PlaywrightSession session = currentSession();
        return session == null ? null : session.browserContext();
    }

    public static Page currentPage() {
        PlaywrightSession session = currentSession();
        return session == null ? null : session.page();
    }

    static PlaywrightSession setSession(PlaywrightSession session) {
        SESSION.set(session);
        return session;
    }

    public static void clearSession() {
        PlaywrightSession session = currentSession();
        if (session != null) {
            session.close();
        }
        SESSION.remove();
    }
}
