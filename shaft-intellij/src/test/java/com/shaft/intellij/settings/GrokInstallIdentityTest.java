package com.shaft.intellij.settings;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import javax.swing.SwingUtilities;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;

/** Issue #6634: the Grok label probe runs once and never blocks the EDT. */
class GrokInstallIdentityTest {

    @AfterEach
    void restoreLiveProbe() {
        GrokInstallIdentity.useHelp(null);
    }

    @Test
    void probesOnceAndCachesTheLabel() {
        AtomicInteger probes = new AtomicInteger();
        GrokInstallIdentity.useHelp(() -> {
            probes.incrementAndGet();
            return "Grok Build TUI\n";
        });

        for (int i = 0; i < 5; i++) {
            assertEquals(GrokInstallIdentity.GROK_BUILD, GrokInstallIdentity.detectedLabel());
        }
        assertEquals(1, probes.get());
    }

    @Test
    void edtCallReturnsTheDefaultWithoutWaitingForTheProbe() throws Exception {
        GrokInstallIdentity.useHelp(() -> "Grok Build TUI\n");
        AtomicReference<String> onEdt = new AtomicReference<>();

        SwingUtilities.invokeAndWait(() -> onEdt.set(GrokInstallIdentity.detectedLabel()));

        assertEquals(GrokInstallIdentity.GROK_CLI, onEdt.get());
        assertEquals(GrokInstallIdentity.GROK_BUILD, GrokInstallIdentity.detectedLabel());
    }

    @Test
    void resettingTheProbeClearsTheCache() {
        GrokInstallIdentity.useHelp(() -> "grok 0.1\n");
        assertEquals(GrokInstallIdentity.GROK_CLI, GrokInstallIdentity.detectedLabel());

        GrokInstallIdentity.useHelp(() -> "Grok Build TUI\n");
        assertEquals(GrokInstallIdentity.GROK_BUILD, GrokInstallIdentity.detectedLabel());
    }
}
