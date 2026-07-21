package com.shaft.mcp;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

/**
 * {@code driver_initialize}/{@code driver_quit}'s PLAYWRIGHT-engine dispatch and error paths --
 * covered here without any real browser, since {@link EngineService}'s package-private constructor
 * accepts an injectable {@link PlaywrightService} (mirroring the bridge-based pattern in
 * {@link EngineServiceCaptureBridgeTest}/{@link EngineServiceMobileInitBridgeTest}). The WEB-engine
 * happy path itself (a real WebDriver launch) is out of scope for a unit test and stays covered by
 * the {@code external-e2e}-tagged {@link EngineServiceTest}.
 */
class EngineServiceDriverInitializeTest {

    @AfterEach
    void resetActiveEngine() {
        EngineService.setActiveEngine(null);
    }

    @Test
    void initializeDriverDispatchesToPlaywrightServiceWhenEnginePlaywright() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        EngineService engineService = new EngineService(playwrightService);

        engineService.initializeDriver(BrowserType.CHROME, ActiveEngine.PLAYWRIGHT, null);

        verify(playwrightService).initialize("CHROME", com.shaft.driver.SHAFT.Properties.web.headlessExecution());
    }

    @Test
    void initializeDriverPassesNullBrowserNameToPlaywrightServiceWhenTargetBrowserOmitted() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        EngineService engineService = new EngineService(playwrightService);

        engineService.initializeDriver(null, ActiveEngine.PLAYWRIGHT, null);

        verify(playwrightService).initialize(null, com.shaft.driver.SHAFT.Properties.web.headlessExecution());
    }

    @Test
    void initializeDriverThrowsAndLogsWhenTargetBrowserIsNullForTheDefaultWebEngine() {
        EngineService engineService = new EngineService();

        // engine omitted (null) defaults to WEB; a null targetBrowser fails fast on
        // `targetBrowser.name()` before any real driver is touched, exercising the catch-log-rethrow
        // branch without launching a browser.
        assertThrows(NullPointerException.class, () -> engineService.initializeDriver(null, null, null));
    }

    /**
     * Regression for a masking bug found while writing the test above: the catch block's own
     * diagnostic log line re-evaluated {@code targetBrowser.name()} on the very value already known
     * to be null, throwing a second NPE from inside the {@code catch} that silently replaced
     * whatever the try block actually failed with and skipped the log line entirely. The fix logs
     * {@code targetBrowser} directly (SLF4J's {@code {}} placeholder null-safely stringifies it)
     * instead of re-deriving its name.
     */
    @Test
    void initializeDriverPropagatesTheOriginalFailureInsteadOfAMaskingNpeFromItsOwnLogging() {
        EngineService engineService = new EngineService();

        NullPointerException failure = assertThrows(NullPointerException.class,
                () -> engineService.initializeDriver(null, null, null));

        StackTraceElement topFrame = failure.getStackTrace()[0];
        assertEquals("com.shaft.mcp.EngineService", topFrame.getClassName());
        assertEquals("initializeDriver", topFrame.getMethodName());
        // The masking bug always surfaced at the catch block's own logger.error call; asserting the
        // failure is NOT attributed there proves it's the original try-block failure propagating.
        org.junit.jupiter.api.Assertions.assertNotEquals(231, topFrame.getLineNumber(),
                "exception must propagate from the try block, not get replaced by the catch block's "
                        + "own logging re-dereferencing the already-null targetBrowser: " + failure);
    }

    @Test
    void quitDriverDispatchesToPlaywrightServiceWhenActiveEngineIsPlaywright() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        EngineService engineService = new EngineService(playwrightService);
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        engineService.quitDriver();

        verify(playwrightService).quit();
    }

    @Test
    void quitDriverResetsActiveEngineToNoneWhenNoDriverIsActive() {
        EngineService engineService = new EngineService();
        EngineService.setActiveEngine(ActiveEngine.WEB);

        // No `driver` was ever initialized in this unit test JVM, so this exercises the
        // `driver == null` branch instead of attempting to quit a real WebDriver.
        engineService.quitDriver();

        assertThrows(IllegalStateException.class, EngineService::getDriver);
    }

    @Test
    void getPageSourceThrowsIllegalStateWhenNoSessionIsActive() {
        EngineService engineService = new EngineService();

        assertThrows(IllegalStateException.class, engineService::getPageSource);
    }
}
