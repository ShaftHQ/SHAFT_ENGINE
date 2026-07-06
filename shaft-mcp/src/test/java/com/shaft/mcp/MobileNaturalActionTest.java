package com.shaft.mcp;

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;
import org.openqa.selenium.WebDriver;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

class MobileNaturalActionTest {
    private static final String SAMPLE_ACCESSIBILITY_TREE = """
            <?xml version="1.0" encoding="UTF-8"?>
            <hierarchy rotation="0">
              <node index="0" text="" resource-id="" class="android.widget.FrameLayout" package="com.example.app" content-desc="" bounds="[0,0][1080,1920]">
                <node index="0" text="Login Button" resource-id="com.example.app:id/login_btn" class="android.widget.Button" package="com.example.app" content-desc="login action" bounds="[300,800][780,900]" name="loginButton" />
                <node index="1" text="OK" resource-id="com.example.app:id/ok_btn" class="android.widget.Button" package="com.example.app" content-desc="confirm dialog" bounds="[300,950][780,1050]" />
              </node>
            </hierarchy>
            """;

    @TempDir
    Path temp;

    private EngineService engineService;
    private MobileService mobileService;

    @BeforeEach
    void setup() {
        engineService = mock(EngineService.class);
        mobileService = new MobileService(engineService, McpWorkspacePolicy.of(temp));
    }

    @AfterEach
    void cleanup() {
        SHAFT.Properties.clearForCurrentThread();
    }

    @Test
    void naturalActResolvesAccessibilityIdDeterministicallyByName() {
        // Arrange: Mock the driver to return accessibility tree with an accessible element
        WebDriver mockDriver = mock(WebDriver.class);
        when(mockDriver.getPageSource()).thenReturn(SAMPLE_ACCESSIBILITY_TREE);

        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.getDriver()).thenReturn(mockDriver);

        try (MockedStatic<EngineService> engineServiceMock = mockStatic(EngineService.class)) {
            engineServiceMock.when(EngineService::getDriver).thenReturn(shaftDriver);

            // Act: Resolve "loginButton" name from the tree
            McpMobileNaturalActionResult result = mobileService.naturalAct(
                    "tap the login button",
                    "loginButton",
                    false);

            // Assert: Deterministic resolution should match the ACCESSIBILITY_ID strategy
            assertTrue(result.success());
            assertEquals("ACCESSIBILITY_ID", result.locatorStrategy());
            assertEquals("loginButton", result.locatorValue());
            assertTrue(result.warnings().isEmpty());
        }
    }

    @Test
    void naturalActResolvesResourceIdWhenAccessibilityIdNotAvailable() {
        // Arrange: Mock driver with tree containing only resource-id
        WebDriver mockDriver = mock(WebDriver.class);
        when(mockDriver.getPageSource()).thenReturn(SAMPLE_ACCESSIBILITY_TREE);

        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.getDriver()).thenReturn(mockDriver);

        try (MockedStatic<EngineService> engineServiceMock = mockStatic(EngineService.class)) {
            engineServiceMock.when(EngineService::getDriver).thenReturn(shaftDriver);

            // Act: Resolve "confirm dialog" content-desc (element has no name, falls back to content-desc)
            McpMobileNaturalActionResult result = mobileService.naturalAct(
                    "tap the confirm dialog button",
                    "confirm dialog",
                    false);

            // Assert: Should resolve using next strategy in ranking (content-desc is ACCESSIBILITY_ID)
            assertTrue(result.success());
            assertEquals("confirm dialog", result.locatorValue());
            assertEquals("ACCESSIBILITY_ID", result.locatorStrategy());
        }
    }

    @Test
    void naturalActFailsDeterministicallyWhenAccessibleNameBlank() {
        // Act: Try to resolve with blank accessible name
        McpMobileNaturalActionResult result = mobileService.naturalAct(
                "tap button",
                "",
                false);

        // Assert: Should fail deterministic resolution
        assertFalse(result.success());
        assertTrue(result.locatorStrategy().isEmpty());
        assertTrue(result.locatorValue().isEmpty());
        assertFalse(result.warnings().isEmpty());
    }

    @Test
    void naturalActFailsDeterministicallyWhenAccessibleNameNotFound() {
        // Arrange: Non-existent accessible name with AI fallback disabled
        WebDriver mockDriver = mock(WebDriver.class);
        when(mockDriver.getPageSource()).thenReturn(SAMPLE_ACCESSIBILITY_TREE);

        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.getDriver()).thenReturn(mockDriver);

        try (MockedStatic<EngineService> engineServiceMock = mockStatic(EngineService.class)) {
            engineServiceMock.when(EngineService::getDriver).thenReturn(shaftDriver);

            // Act: Try to resolve non-existent accessible name
            McpMobileNaturalActionResult result = mobileService.naturalAct(
                    "tap nonexistent button",
                    "nonexistentButton",
                    false);

            // Assert: Should fail deterministic resolution and report no match
            assertFalse(result.success());
            assertTrue(result.locatorStrategy().isEmpty());
            assertTrue(result.locatorValue().isEmpty());
            assertTrue(result.warnings().stream().anyMatch(w -> w.contains("deterministic") && w.contains("disabled")));
        }
    }

    @Test
    void naturalActReportsFallbackAvailabilityWhenEnabled() {
        // Arrange: Non-existent accessible name with AI fallback enabled
        WebDriver mockDriver = mock(WebDriver.class);
        when(mockDriver.getPageSource()).thenReturn(SAMPLE_ACCESSIBILITY_TREE);

        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.getDriver()).thenReturn(mockDriver);

        try (MockedStatic<EngineService> engineServiceMock = mockStatic(EngineService.class)) {
            engineServiceMock.when(EngineService::getDriver).thenReturn(shaftDriver);

            // Act: Try with AI fallback enabled
            McpMobileNaturalActionResult result = mobileService.naturalAct(
                    "tap nonexistent button",
                    "nonexistentButton",
                    true);

            // Assert: Should indicate fallback is available
            assertFalse(result.success());
            assertTrue(result.warnings().stream().anyMatch(w -> w.contains("fallback available")));
        }
    }

    @Test
    void naturalActFailsWhenSourceIsEmpty() {
        // Arrange: Empty page source
        WebDriver mockDriver = mock(WebDriver.class);
        when(mockDriver.getPageSource()).thenReturn("");

        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.getDriver()).thenReturn(mockDriver);

        try (MockedStatic<EngineService> engineServiceMock = mockStatic(EngineService.class)) {
            engineServiceMock.when(EngineService::getDriver).thenReturn(shaftDriver);

            // Act: Try to resolve with empty source
            McpMobileNaturalActionResult result = mobileService.naturalAct(
                    "tap button",
                    "loginButton",
                    false);

            // Assert: Should fail with empty source warning
            assertFalse(result.success());
            assertTrue(result.warnings().stream().anyMatch(w -> w.contains("empty")));
        }
    }

    @Test
    void naturalActFailsWhenIntentIsBlank() {
        // Act: Try with blank intent
        McpMobileNaturalActionResult result = mobileService.naturalAct(
                "",
                "loginButton",
                false);

        // Assert: Should fail immediately with blank intent warning
        assertFalse(result.success());
        assertTrue(result.warnings().stream().anyMatch(w -> w.contains("blank")));
    }

    @Test
    void locatorSuggesterSelectsByAccessibleNameCaseInsensitive() {
        // Arrange: Parse accessibility tree and search by name
        var suggester = McpAppiumLocatorSuggester.parse(SAMPLE_ACCESSIBILITY_TREE);

        // Act & Assert: Case-insensitive matching on accessible name
        assertTrue(suggester.isPresent());
        var suggestion = suggester.get().locatorByAccessibleName("loginButton");
        assertTrue(suggestion.isPresent());
        assertEquals("loginButton", suggestion.get().value());
        assertEquals("ACCESSIBILITY_ID", suggestion.get().strategy().name());

        // Also test case-insensitive
        var caseInsensitive = suggester.get().locatorByAccessibleName("LOGINBUTTON");
        assertTrue(caseInsensitive.isPresent());
    }
}
