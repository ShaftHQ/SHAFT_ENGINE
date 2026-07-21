package com.shaft.mcp;

import com.shaft.driver.SHAFT;
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
        SHAFT.Properties.clearForCurrentThread();
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

    /**
     * design doc Decision 2/3: {@code targetBrowser} has a deterministic default
     * ({@code SHAFT.Properties.web.targetBrowserName()}, {@code @DefaultValue("chrome")}), so it must
     * be optional rather than required. This is exercised against the {@code resolveBrowserName}
     * seam directly -- not through {@code initializeDriver} -- because a null {@code targetBrowser}
     * on the WEB engine no longer fails fast: it now resolves to the configured property and
     * proceeds to a real {@code SHAFT.GUI.WebDriver()} launch, which is out of scope for a unit test
     * (see the class Javadoc).
     */
    @Test
    void resolveBrowserNameDefersToTheConfiguredPropertyWhenTargetBrowserIsOmitted() {
        SHAFT.Properties.web.set().targetBrowserName("firefox");

        String resolved = EngineService.resolveBrowserName(null);

        assertEquals("firefox", resolved);
        // Proves the omitted case is a read-only default lookup, not a hardcoded "chrome" fallback,
        // and that it does NOT overwrite an already-configured non-default preference.
        assertEquals("firefox", SHAFT.Properties.web.targetBrowserName());
    }

    @Test
    void resolveBrowserNameSetsThePropertyAndReturnsItsNameWhenTargetBrowserIsProvided() {
        String resolved = EngineService.resolveBrowserName(BrowserType.EDGE);

        assertEquals("EDGE", resolved);
        assertEquals("EDGE", SHAFT.Properties.web.targetBrowserName());
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
