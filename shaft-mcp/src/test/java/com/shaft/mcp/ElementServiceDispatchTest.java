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

    @Test
    void webEngineTypeDispatchesToElementTypeWithAppendAndClearSemantics() {
        Actions element = mock(Actions.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element, mock(TouchActions.class));
        ElementService service = new ElementService(mock(PlaywrightService.class), mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            ElementActionResult replace = service.type(locatorStrategy.ID, "email", "alice");
            service.type(locatorStrategy.ID, "email", "bob", true, null);
            service.type(locatorStrategy.ID, "email", "carol", null, false);

            verify(element).type(any(By.class), org.mockito.ArgumentMatchers.eq("alice"));
            verify(element, org.mockito.Mockito.times(2)).typeAppend(any(By.class), org.mockito.ArgumentMatchers.anyString());
            assertEquals("WEB", replace.activeEngine());
        }
    }

    @Test
    void mobileNativeEngineTypeRecordsIntoActiveMobileRecordingAndSupportsAppend() {
        Actions element = mock(Actions.class);
        TouchActions touch = mock(TouchActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element, touch);
        MobileService mobileService = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));
        ElementService service = new ElementService(mock(PlaywrightService.class), mobileService);
        EngineService.setActiveEngine(ActiveEngine.MOBILE_NATIVE);
        mobileService.recordStart(temp.resolve("recordings/native-type.json").toString(), "native", true);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            ElementActionResult result = service.type(locatorStrategy.ACCESSIBILITY_ID, "email", "alice");
            ElementActionResult appended = service.type(locatorStrategy.ACCESSIBILITY_ID, "email", "bob", true, null);

            verify(element).type(any(By.class), org.mockito.ArgumentMatchers.eq("alice"));
            verify(element).typeAppend(any(By.class), org.mockito.ArgumentMatchers.eq("bob"));
            assertTrue(result.recorded(), "unified element_type must record the step into the active mobile recording");
            assertTrue(appended.recorded());
            assertEquals("MOBILE_NATIVE", result.activeEngine());
            assertEquals(2, mobileService.recordStatus().actionCount());
        }
    }

    @Test
    void playwrightEngineTypeDispatchesToPlaywrightTypeOrAppendAndRecordsWhenActive() {
        BrowserActions browser = mock(BrowserActions.class);
        ElementActions element = mock(ElementActions.class);
        Page page = mock(Page.class);
        SHAFT.GUI.Playwright playwrightDriver = mock(SHAFT.GUI.Playwright.class);
        when(playwrightDriver.browser()).thenReturn(browser);
        when(playwrightDriver.element()).thenReturn(element);
        when(playwrightDriver.getDriver()).thenReturn(page);
        PlaywrightService playwrightService = new PlaywrightService(McpWorkspacePolicy.of(temp));
        playwrightService.setDriverForTesting(playwrightDriver);
        playwrightService.recordStart(temp.resolve("recordings/pw-type.json").toString(), "playwright", true);
        ElementService service = new ElementService(playwrightService, mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        ElementActionResult result = service.type(locatorStrategy.ID, "email", "alice");
        ElementActionResult appended = service.type(locatorStrategy.ID, "email", "bob", true, null);

        verify(element).type(any(org.openqa.selenium.By.class), org.mockito.ArgumentMatchers.eq("alice"));
        verify(element).typeAppend(any(org.openqa.selenium.By.class), org.mockito.ArgumentMatchers.eq("bob"));
        assertTrue(result.recorded(), "unified element_type must record the step into the active Playwright recording");
        assertTrue(appended.recorded());
        assertEquals("PLAYWRIGHT", result.activeEngine());
        assertEquals(2, playwrightService.recordStatus().actionCount());
    }

    @Test
    void mobileNativeEngineClearDispatchesToMobileServiceClearAndRecords() {
        Actions element = mock(Actions.class);
        TouchActions touch = mock(TouchActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element, touch);
        MobileService mobileService = new MobileService(mock(EngineService.class), McpWorkspacePolicy.of(temp));
        ElementService service = new ElementService(mock(PlaywrightService.class), mobileService);
        EngineService.setActiveEngine(ActiveEngine.MOBILE_NATIVE);
        mobileService.recordStart(temp.resolve("recordings/native-clear.json").toString(), "native", true);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            ElementActionResult result = service.clear(locatorStrategy.ACCESSIBILITY_ID, "email");

            verify(element).clear(any(By.class));
            assertTrue(result.recorded());
            assertEquals("MOBILE_NATIVE", result.activeEngine());
        }
    }

    @Test
    void playwrightEngineClearDispatchesToPlaywrightClear() {
        ElementActions element = mock(ElementActions.class);
        SHAFT.GUI.Playwright playwrightDriver = mock(SHAFT.GUI.Playwright.class);
        when(playwrightDriver.element()).thenReturn(element);
        PlaywrightService playwrightService = new PlaywrightService(McpWorkspacePolicy.of(temp));
        playwrightService.setDriverForTesting(playwrightDriver);
        ElementService service = new ElementService(playwrightService, mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        ElementActionResult result = service.clear(locatorStrategy.ID, "email");

        verify(element).clear(any(org.openqa.selenium.By.class));
        assertEquals("PLAYWRIGHT", result.activeEngine());
    }

    @Test
    void webEngineHoverDispatchesToElementHoverAndPlaywrightEngineDispatchesToPlaywrightHover() {
        Actions element = mock(Actions.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element, mock(TouchActions.class));
        ElementService webService = new ElementService(mock(PlaywrightService.class), mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);
        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            ElementActionResult result = webService.hover(locatorStrategy.ID, "menu");

            verify(element).hover(any(By.class));
            assertEquals("WEB", result.activeEngine());
        }

        ElementActions playwrightElement = mock(ElementActions.class);
        SHAFT.GUI.Playwright playwrightDriver = mock(SHAFT.GUI.Playwright.class);
        when(playwrightDriver.element()).thenReturn(playwrightElement);
        PlaywrightService playwrightService = new PlaywrightService(McpWorkspacePolicy.of(temp));
        playwrightService.setDriverForTesting(playwrightDriver);
        ElementService pwService = new ElementService(playwrightService, mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        ElementActionResult pwResult = pwService.hover(locatorStrategy.ID, "menu");

        verify(playwrightElement).hover(any(org.openqa.selenium.By.class));
        assertEquals("PLAYWRIGHT", pwResult.activeEngine());
    }

    @Test
    void dragAndDropDispatchesTargetOrOffsetOnWebAndPlaywright() {
        Actions element = mock(Actions.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element, mock(TouchActions.class));
        ElementService webService = new ElementService(mock(PlaywrightService.class), mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);
        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            webService.dragAndDrop(locatorStrategy.ID, "source", locatorStrategy.ID, "target", null, null);
            webService.dragAndDrop(locatorStrategy.ID, "source", null, null, 10, 20);

            verify(element).dragAndDrop(any(By.class), any(By.class));
            verify(element).dragAndDropByOffset(any(By.class), org.mockito.ArgumentMatchers.eq(10), org.mockito.ArgumentMatchers.eq(20));
        }

        ElementActions playwrightElement = mock(ElementActions.class);
        SHAFT.GUI.Playwright playwrightDriver = mock(SHAFT.GUI.Playwright.class);
        when(playwrightDriver.element()).thenReturn(playwrightElement);
        PlaywrightService playwrightService = new PlaywrightService(McpWorkspacePolicy.of(temp));
        playwrightService.setDriverForTesting(playwrightDriver);
        ElementService pwService = new ElementService(playwrightService, mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        ElementActionResult pwTarget = pwService.dragAndDrop(locatorStrategy.ID, "source", locatorStrategy.ID, "target", null, null);
        ElementActionResult pwOffset = pwService.dragAndDrop(locatorStrategy.ID, "source", null, null, 5, 6);

        verify(playwrightElement).dragAndDrop(any(org.openqa.selenium.By.class), any(org.openqa.selenium.By.class));
        verify(playwrightElement).dragAndDropByOffset(any(org.openqa.selenium.By.class),
                org.mockito.ArgumentMatchers.eq(5), org.mockito.ArgumentMatchers.eq(6));
        assertEquals("PLAYWRIGHT", pwTarget.activeEngine());
        assertEquals("PLAYWRIGHT", pwOffset.activeEngine());
    }

    @Test
    void dragAndDropRequiresEitherTargetOrOffset() {
        ElementService service = new ElementService(mock(PlaywrightService.class), mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        org.junit.jupiter.api.Assertions.assertThrows(IllegalArgumentException.class,
                () -> service.dragAndDrop(locatorStrategy.ID, "source", null, null, null, null));
    }

    @Test
    void uploadFileDispatchesToWebAndPlaywrightUploadImplementations() {
        Actions element = mock(Actions.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element, mock(TouchActions.class));
        ElementService webService = new ElementService(mock(PlaywrightService.class), mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);
        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            webService.dropFileToUpload(locatorStrategy.ID, "upload", "file.txt");

            verify(element).dropFileToUpload(any(By.class), org.mockito.ArgumentMatchers.eq("file.txt"));
        }

        ElementActions playwrightElement = mock(ElementActions.class);
        SHAFT.GUI.Playwright playwrightDriver = mock(SHAFT.GUI.Playwright.class);
        when(playwrightDriver.element()).thenReturn(playwrightElement);
        PlaywrightService playwrightService = new PlaywrightService(McpWorkspacePolicy.of(temp));
        playwrightService.setDriverForTesting(playwrightDriver);
        ElementService pwService = new ElementService(playwrightService, mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        ElementActionResult result = pwService.dropFileToUpload(locatorStrategy.ID, "upload", "file.txt");

        verify(playwrightElement).typeFileLocationForUpload(any(org.openqa.selenium.By.class), org.mockito.ArgumentMatchers.eq("file.txt"));
        assertEquals("PLAYWRIGHT", result.activeEngine());
    }

    @Test
    void queryToolsDispatchToWebAndPlaywrightAndEchoActiveEngine() {
        Actions element = mock(Actions.class);
        Actions.GetElementInformation info = mock(Actions.GetElementInformation.class);
        when(element.get()).thenReturn(info);
        when(info.isDisplayed(any(By.class))).thenReturn(true);
        when(info.isEnabled(any(By.class))).thenReturn(false);
        when(info.isSelected(any(By.class))).thenReturn(true);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element, mock(TouchActions.class));
        ElementService webService = new ElementService(mock(PlaywrightService.class), mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);
        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            ElementQueryResult displayed = webService.isDisplayed(locatorStrategy.ID, "banner");
            ElementQueryResult enabled = webService.isEnabled(locatorStrategy.ID, "banner");
            ElementQueryResult selected = webService.isSelected(locatorStrategy.ID, "banner");

            assertTrue(displayed.value());
            assertEquals("WEB", displayed.activeEngine());
            assertEquals(false, enabled.value());
            assertTrue(selected.value());
        }

        PlaywrightService playwrightService = mock(PlaywrightService.class);
        when(playwrightService.isDisplayed(locatorStrategy.ID, "banner")).thenReturn(true);
        when(playwrightService.isEnabled(locatorStrategy.ID, "banner")).thenReturn(false);
        when(playwrightService.isSelected(locatorStrategy.ID, "banner")).thenReturn(true);
        ElementService pwService = new ElementService(playwrightService, mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        ElementQueryResult pwDisplayed = pwService.isDisplayed(locatorStrategy.ID, "banner");
        ElementQueryResult pwSelected = pwService.isSelected(locatorStrategy.ID, "banner");

        assertTrue(pwDisplayed.value());
        assertEquals("PLAYWRIGHT", pwDisplayed.activeEngine());
        assertTrue(pwSelected.value());
        assertEquals("PLAYWRIGHT", pwSelected.activeEngine());
    }

    private static SHAFT.GUI.WebDriver mockShaftDriver(Actions element, TouchActions touch) {
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.element()).thenReturn(element);
        when(shaftDriver.touch()).thenReturn(touch);
        return shaftDriver;
    }
}
