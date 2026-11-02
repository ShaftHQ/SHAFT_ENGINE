package com.shaft.driver.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.testng.ITestResult;
import org.testng.Reporter;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Keeps one browser per thread and test class when {@code reuseBrowserSession=true}.
 * {@code quit()} resets browser state; the real quit happens when the class (or execution) finishes.
 */
public final class BrowserSessionReuse {
    private record Session(String testClass, DriverFactoryHelper helper) {
    }

    private static final Map<Long, Session> SESSIONS = new ConcurrentHashMap<>();

    private BrowserSessionReuse() {
    }

    /**
     * Returns the thread's reusable helper for the current test class, closing a stale one from another class.
     *
     * @return the reusable helper, or {@code null} when reuse is off or nothing is cached
     */
    public static DriverFactoryHelper reusable() {
        if (!SHAFT.Properties.flags.reuseBrowserSession()) {
            return null;
        }
        Session session = SESSIONS.get(Thread.currentThread().threadId());
        if (session == null) {
            return null;
        }
        if (session.testClass().equals(currentTestClass()) && session.helper().getDriver() != null) {
            return session.helper();
        }
        close(session);
        SESSIONS.remove(Thread.currentThread().threadId());
        return null;
    }

    /**
     * Registers a freshly created helper for reuse when the flag is on.
     *
     * @param helper the new driver helper
     */
    public static void register(DriverFactoryHelper helper) {
        if (helper != null && SHAFT.Properties.flags.reuseBrowserSession()) {
            SESSIONS.put(Thread.currentThread().threadId(), new Session(currentTestClass(), helper));
        }
    }

    /**
     * Resets a reused browser instead of quitting it.
     *
     * @param helper the helper being quit
     * @return {@code true} when the helper is reused and was only reset
     */
    public static boolean resetInsteadOfQuit(DriverFactoryHelper helper) {
        Session session = SESSIONS.get(Thread.currentThread().threadId());
        if (session == null || session.helper() != helper || helper.getDriver() == null) {
            return false;
        }
        WebDriver driver = helper.getDriver();
        String main = driver.getWindowHandle();
        for (String handle : driver.getWindowHandles()) {
            if (!handle.equals(main)) {
                driver.switchTo().window(handle).close();
            }
        }
        driver.switchTo().window(main);
        if (driver instanceof JavascriptExecutor js) {
            js.executeScript("try { window.localStorage.clear(); window.sessionStorage.clear(); } catch (e) {}");
        }
        driver.manage().deleteAllCookies();
        driver.navigate().to("about:blank");
        return true;
    }

    /**
     * Quits every reused browser that belongs to the finished class.
     *
     * @param testClass fully qualified class name
     */
    public static void closeForClass(String testClass) {
        SESSIONS.entrySet().removeIf(entry -> {
            if (entry.getValue().testClass().equals(testClass)) {
                close(entry.getValue());
                return true;
            }
            return false;
        });
    }

    /**
     * Quits every reused browser.
     */
    public static void closeAll() {
        SESSIONS.values().forEach(BrowserSessionReuse::close);
        SESSIONS.clear();
    }

    private static void close(Session session) {
        try {
            session.helper().closeDriver();
        } catch (RuntimeException ignored) {
            // the browser may already be gone; nothing else to release
        }
    }

    private static String currentTestClass() {
        ITestResult result = Reporter.getCurrentTestResult();
        return result == null || result.getTestClass() == null ? "" : result.getTestClass().getName();
    }
}
