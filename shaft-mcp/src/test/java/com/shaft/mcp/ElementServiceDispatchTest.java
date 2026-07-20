package com.shaft.mcp;

import com.microsoft.playwright.Page;
import com.shaft.driver.SHAFT;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.element.internal.Actions;
import com.shaft.gui.playwright.browser.BrowserActions;
import com.shaft.gui.playwright.element.ElementActions;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;
import org.openqa.selenium.By;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Per-engine dispatch matrix for the unified {@code element_click} tool (design doc amendment A2):
 * asserts the tool reaches the correct underlying implementation for each {@link ActiveEngine} and
 * that a recording-active mobile/Playwright session records the step (amendment A1). Uses
 * {@code mockStatic(EngineService.class, CALLS_REAL_METHODS)} so the real, pure
 * {@code EngineService.getLocator(...)}/{@code activeEngine()} helpers still run while only
 * {@code getDriver()} is stubbed -- the same pattern {@link MobileServiceDriverBackedTest} uses.
 */
class ElementServiceDispatchTest {
    @TempDir
    Path temp;

    @AfterEach
    void resetActiveEngine() {
        EngineService.setActiveEngine(null);
    }

    @Test
    void webEngineSingleModeDispatchesToElementClickAndDoesNotTouchPlaywrightOrMobile() {
        Actions element = mock(Actions.class);
        TouchActions touch = mock(TouchActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element, touch);
        MobileService mobileService = mock(MobileService.class);
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        ElementService service = new ElementService(playwrightService, mobileService);
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            ElementActionResult result = service.click(locatorStrategy.ID, "submit", null);

            verify(element).click(any(By.class));
            verifyNoInteractions(touch, playwrightService, mobileService);
            assertEquals("WEB", result.activeEngine());
        }
    }

    @Test
    void webEngineDoubleAndLongModesDispatchToDoubleClickAndClickAndHold() {
        Actions element = mock(Actions.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element, mock(TouchActions.class));
        ElementService service = new ElementService(mock(PlaywrightService.class), mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            service.click(locatorStrategy.ID, "submit", ClickMode.DOUBLE);
            service.click(locatorStrategy.ID, "submit", ClickMode.LONG);

            verify(element).doubleClick(any(By.class));
            verify(element).clickAndHold(any(By.class));
        }
    }

    @Test
    void mobileNativeEngineDispatchesToTouchTapNotElementClick() {
        Actions element = mock(Actions.class);
        TouchActions touch = mock(TouchActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element, touch);
        MobileService mobileService = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));
        ElementService service = new ElementService(mock(PlaywrightService.class), mobileService);
        EngineService.setActiveEngine(ActiveEngine.MOBILE_NATIVE);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            ElementActionResult result = service.click(locatorStrategy.ACCESSIBILITY_ID, "login", null);

            verify(touch).tap(any(By.class));
            verify(element, org.mockito.Mockito.never()).click(any(By.class));
            assertEquals("MOBILE_NATIVE", result.activeEngine());
        }
    }

    @Test
    void mobileNativeEngineRecordsTheStepWhenAMobileRecordingIsActive() {
        Actions element = mock(Actions.class);
        TouchActions touch = mock(TouchActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element, touch);
        MobileService mobileService = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));
        ElementService service = new ElementService(mock(PlaywrightService.class), mobileService);
        EngineService.setActiveEngine(ActiveEngine.MOBILE_NATIVE);
        mobileService.recordStart(temp.resolve("recordings/native-dispatch.json").toString(), "native", true);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            ElementActionResult result = service.click(locatorStrategy.ACCESSIBILITY_ID, "login", null);

            assertTrue(result.recorded(), "unified element_click must record the step into the active mobile recording");
            assertEquals(1, mobileService.recordStatus().actionCount());
        }
    }

    @Test
    void playwrightEngineDispatchesToPlaywrightClickAndRecordsWhenRecordingIsActive() {
        BrowserActions browser = mock(BrowserActions.class);
        ElementActions element = mock(ElementActions.class);
        Page page = mock(Page.class);
        SHAFT.GUI.Playwright playwrightDriver = mock(SHAFT.GUI.Playwright.class);
        when(playwrightDriver.browser()).thenReturn(browser);
        when(playwrightDriver.element()).thenReturn(element);
        when(playwrightDriver.getDriver()).thenReturn(page);
        PlaywrightService playwrightService = new PlaywrightService(McpWorkspacePolicy.of(temp));
        playwrightService.setDriverForTesting(playwrightDriver);
        playwrightService.recordStart(temp.resolve("recordings/pw-dispatch.json").toString(), "playwright", true);
        ElementService service = new ElementService(playwrightService, mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        ElementActionResult result = service.click(locatorStrategy.ID, "submit", null);

        verify(element).click(any(org.openqa.selenium.By.class));
        assertTrue(result.recorded(), "unified element_click must record the step into the active Playwright recording");
        assertEquals("PLAYWRIGHT", result.activeEngine());
        assertEquals(1, playwrightService.recordStatus().actionCount());
    }

    private static SHAFT.GUI.WebDriver mockShaftDriver(Actions element, TouchActions touch) {
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.element()).thenReturn(element);
        when(shaftDriver.touch()).thenReturn(touch);
        return shaftDriver;
    }
}
