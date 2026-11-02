package com.shaft.gui.browser.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.driver.BrowserConsoleMessage;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.gui.playwright.internal.PlaywrightSessionManager;
import com.shaft.tools.io.internal.AttachmentReporter;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import org.openqa.selenium.WebDriver;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Optional end-of-test gate (#6400): fails an otherwise passing test when the browser console captured
 * error-level messages outside {@code browserConsoleErrorAllowlist}. Reuses the BiDi and Playwright
 * console capture that already backs {@code browser().console()}.
 */
public final class BrowserConsoleErrorGate {
    private BrowserConsoleErrorGate() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Checks the current thread's browser session when {@code failOnBrowserConsoleErrors} is on, attaches
     * offending messages, and clears the buffer so the next test in a shared session starts clean.
     *
     * @return the failure to apply to the test, or {@code null} when the gate is off or the console is clean
     */
    public static AssertionError checkCurrentTest() {
        if (!SHAFT.Properties.web.failOnBrowserConsoleErrors()) {
            return null;
        }
        List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> entries;
        WebDriver driver = DriverFactoryHelper.getActiveDriver();
        PlaywrightSession session = PlaywrightSessionManager.currentSession();
        if (driver != null && BidiConsoleLogSource.isHealthy(driver)) {
            entries = BidiConsoleLogSource.snapshot(driver);
            BidiConsoleLogSource.clear(driver);
        } else if (session != null) {
            entries = session.consoleSnapshot();
            session.clearConsole();
        } else {
            entries = BrowserObservabilityRecorder.snapshotConsole();
            BrowserObservabilityRecorder.clearConsole();
        }
        AssertionError failure = failureFor(entries, SHAFT.Properties.web.browserConsoleErrorAllowlist());
        if (failure != null) {
            ByteArrayOutputStream content = new ByteArrayOutputStream();
            content.writeBytes(failure.getMessage().getBytes(StandardCharsets.UTF_8));
            AttachmentReporter.attachBasedOnFileType("Browser console errors", "console-errors.txt", content,
                    "Browser console errors");
        }
        return failure;
    }

    static AssertionError failureFor(List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> entries, String allowlist) {
        Pattern allowed = allowlist == null || allowlist.isBlank() ? null : Pattern.compile(allowlist);
        String errors = entries.stream()
                .map(entry -> new BrowserConsoleMessage(entry.source(), entry.level(), entry.message(), entry.timestamp()))
                .filter(BrowserConsoleMessage::isError)
                .filter(message -> allowed == null || !allowed.matcher(message.message()).find())
                .map(message -> "[" + message.level() + "] " + message.message())
                .collect(Collectors.joining(System.lineSeparator()));
        return errors.isEmpty() ? null : new AssertionError(
                "Browser console captured errors (failOnBrowserConsoleErrors=true):" + System.lineSeparator() + errors);
    }
}
