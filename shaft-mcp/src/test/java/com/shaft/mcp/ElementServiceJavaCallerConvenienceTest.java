package com.shaft.mcp;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.internal.Actions;
import com.shaft.gui.element.TouchActions;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.openqa.selenium.By;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Covers {@link ElementService}'s Java-caller-only convenience surface (not exposed as MCP tools)
 * that {@link ElementServiceDispatchTest} doesn't reach: the 2-arg {@code click} overload, the
 * semantic-locator aliases, the WEB branch of {@code clear}, the 4-arg {@code dragAndDrop} overload,
 * the DOM/CSS value getters, and the Playwright branch of {@code isEnabled}. Reuses the same
 * {@code mockStatic(EngineService.class, CALLS_REAL_METHODS)} fake-driver pattern as
 * {@link ElementServiceDispatchTest} -- no real browser is launched.
 */
class ElementServiceJavaCallerConvenienceTest {

    @AfterEach
    void resetActiveEngine() {
        EngineService.setActiveEngine(null);
    }

    @Test
    void twoArgClickDefaultsToSingleClickMode() {
        Actions element = mock(Actions.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element);
        ElementService service = new ElementService(mock(PlaywrightService.class), mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            ElementActionResult result = service.click(locatorStrategy.ID, "submit");

            verify(element).click(any(By.class));
            assertEquals("WEB", result.activeEngine());
        }
    }

    @Test
    void clickSemanticAndItsDeprecatedAliasDelegateToTheRealDriver() {
        Actions element = mock(Actions.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element);
        ElementService service = new ElementService(mock(PlaywrightService.class), mock(MobileService.class));

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            service.clickSemantic("Submit button");
            service.clickUsingAI("Submit button");

            verify(element, org.mockito.Mockito.times(2)).click("Submit button");
        }
    }

    @Test
    void typeSemanticAndItsDeprecatedAliasDelegateToTheRealDriver() {
        Actions element = mock(Actions.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element);
        ElementService service = new ElementService(mock(PlaywrightService.class), mock(MobileService.class));

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            service.typeSemantic("Email field", "alice@example.com");
            service.typeUsingAI("Email field", "alice@example.com");

            verify(element, org.mockito.Mockito.times(2)).type("Email field", "alice@example.com");
        }
    }

    @Test
    void typeSemanticToleratesANullVarargsArray() {
        Actions element = mock(Actions.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element);
        ElementService service = new ElementService(mock(PlaywrightService.class), mock(MobileService.class));

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            service.typeSemantic("Email field", (CharSequence[]) null);

            verify(element).type("Email field", (CharSequence[]) null);
        }
    }

    @Test
    void webEngineClearDispatchesToElementClear() {
        Actions element = mock(Actions.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element);
        ElementService service = new ElementService(mock(PlaywrightService.class), mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            ElementActionResult result = service.clear(locatorStrategy.ID, "email");

            verify(element).clear(any(By.class));
            assertEquals("WEB", result.activeEngine());
        }
    }

    @Test
    void fourArgDragAndDropOverloadForwardsToTheSixArgVersionWithoutAnOffset() {
        Actions element = mock(Actions.class);
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element);
        ElementService service = new ElementService(mock(PlaywrightService.class), mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            service.dragAndDrop(locatorStrategy.ID, "source", locatorStrategy.ID, "target");

            verify(element).dragAndDrop(any(By.class), any(By.class));
        }
    }

    @Test
    void domAndCssValueGettersReturnValuesFromTheRealDriver() {
        Actions element = mock(Actions.class);
        Actions.GetElementInformation info = mock(Actions.GetElementInformation.class);
        when(element.get()).thenReturn(info);
        when(info.domAttribute(any(By.class), anyString())).thenReturn("attr-value");
        when(info.domProperty(any(By.class), anyString())).thenReturn("prop-value");
        when(info.cssValue(any(By.class), anyString())).thenReturn("12px");
        when(info.text(any(By.class))).thenReturn("Hello");
        SHAFT.GUI.WebDriver shaftDriver = mockShaftDriver(element);
        ElementService service = new ElementService(mock(PlaywrightService.class), mock(MobileService.class));

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            assertEquals("Hello", service.getText(locatorStrategy.ID, "greeting"));
            assertEquals("attr-value", service.getDomAttribute(locatorStrategy.ID, "el", "data-x"));
            assertEquals("prop-value", service.getDomProperty(locatorStrategy.ID, "el", "value"));
            assertEquals("12px", service.getCssValue(locatorStrategy.ID, "el", "font-size"));
        }
    }

    @Test
    void playwrightEngineIsEnabledDispatchesToPlaywrightService() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        when(playwrightService.isEnabled(locatorStrategy.ID, "banner")).thenReturn(false);
        ElementService service = new ElementService(playwrightService, mock(MobileService.class));
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        ElementQueryResult result = service.isEnabled(locatorStrategy.ID, "banner");

        assertEquals("PLAYWRIGHT", result.activeEngine());
        assertEquals(false, result.value());
    }

    private static SHAFT.GUI.WebDriver mockShaftDriver(Actions element) {
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.element()).thenReturn(element);
        when(shaftDriver.touch()).thenReturn(mock(TouchActions.class));
        return shaftDriver;
    }
}
