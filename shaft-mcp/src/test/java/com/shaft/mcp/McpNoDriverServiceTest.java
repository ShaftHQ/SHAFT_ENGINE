package com.shaft.mcp;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertThrows;

class McpNoDriverServiceTest {
    @TempDir
    Path temp;

    private EngineService engineService;

    @BeforeEach
    void resetDriver() {
        engineService = new EngineService();
        engineService.quitDriver();
    }

    @Test
    void browserToolsShouldFailWhenNoDriverSessionExists() {
        BrowserService service = new BrowserService(McpWorkspacePolicy.of(temp));

        assertNoDriver(() -> service.navigate("https://example.test"));
        assertNoDriver(() -> service.navigateWithBasicAuth("https://example.test", "u", "p", "https://example.test"));
        assertNoDriver(service::refreshPage);
        assertNoDriver(service::navigateBack);
        assertNoDriver(service::navigateForward);
        assertNoDriver(service::maximizeWindow);
        assertNoDriver(() -> service.setWindowSize(800, 600));
        assertNoDriver(service::fullscreenWindow);
        assertNoDriver(service::deleteAllCookies);
        assertNoDriver(() -> service.deleteCookie("sid"));
        assertNoDriver(() -> service.addCookie("sid", "value"));
        assertNoDriver(() -> service.getCookie("sid"));
        assertNoDriver(service::getAllCookies);
        assertNoDriver(service::getCurrentUrl);
        assertNoDriver(service::getTitle);
        assertNoDriver(() -> service.getPageDom(10));
        assertNoDriver(() -> service.takeScreenshot("shot.png", false));
    }

    @Test
    void elementToolsShouldFailWhenNoDriverSessionExists() {
        ElementService service = new ElementService();

        assertNoDriver(() -> service.hover(locatorStrategy.ID, "submit"));
        assertNoDriver(() -> service.click(locatorStrategy.ID, "submit"));
        assertNoDriver(() -> service.clickSemantic("Submit"));
        assertNoDriver(() -> service.clickUsingAI("Submit"));
        assertNoDriver(() -> service.clickUsingJavaScript(locatorStrategy.ID, "submit"));
        assertNoDriver(() -> service.doubleClick(locatorStrategy.ID, "submit"));
        assertNoDriver(() -> service.clickAndHold(locatorStrategy.ID, "submit"));
        assertNoDriver(() -> service.type(locatorStrategy.ID, "name", "value"));
        assertNoDriver(() -> service.appendText(locatorStrategy.ID, "name", "value"));
        assertNoDriver(() -> service.typeSemantic("Name", "value"));
        assertNoDriver(() -> service.typeUsingAI("Name", "value"));
        assertNoDriver(() -> service.setValueUsingJavaScript(locatorStrategy.ID, "name", "value"));
        assertNoDriver(() -> service.clear(locatorStrategy.ID, "name"));
        assertNoDriver(() -> service.dropFileToUpload(locatorStrategy.ID, "upload", "file.txt"));
        assertNoDriver(() -> service.dragAndDrop(locatorStrategy.ID, "source", locatorStrategy.ID, "target"));
        assertNoDriver(() -> service.dragAndDropByOffset(locatorStrategy.ID, "source", 1, 2));
        assertNoDriver(() -> service.getText(locatorStrategy.ID, "message"));
        assertNoDriver(() -> service.getDomAttribute(locatorStrategy.ID, "message", "data-id"));
        assertNoDriver(() -> service.getDomProperty(locatorStrategy.ID, "message", "value"));
        assertNoDriver(() -> service.getCssValue(locatorStrategy.ID, "message", "color"));
        assertNoDriver(() -> service.isDisplayed(locatorStrategy.ID, "message"));
        assertNoDriver(() -> service.isEnabled(locatorStrategy.ID, "message"));
        assertNoDriver(() -> service.isSelected(locatorStrategy.ID, "message"));
    }

    @Test
    void mobileAndNaturalToolsShouldFailWhenNoDriverSessionExists() {
        MobileService mobile = new MobileService(engineService, McpWorkspacePolicy.of(temp));
        NaturalActionService natural = new NaturalActionService();

        assertNoDriver(() -> natural.act("click submit", List.of(), 50, "deterministic", false, "element"));
        assertNoDriver(() -> mobile.getContexts(10));
        assertNoDriver(() -> mobile.getAccessibilityTree(10));
        assertNoDriver(() -> mobile.takeScreenshot("mobile.png", false));
        assertNoDriver(() -> mobile.switchContext("NATIVE_APP"));
        assertNoDriver(() -> mobile.tap(locatorStrategy.ID, "submit"));
        assertNoDriver(() -> mobile.doubleTap(locatorStrategy.ID, "submit"));
        assertNoDriver(() -> mobile.longTap(locatorStrategy.ID, "submit"));
        assertNoDriver(() -> mobile.type(locatorStrategy.ID, "name", "value"));
        assertNoDriver(() -> mobile.clear(locatorStrategy.ID, "name"));
        assertNoDriver(() -> mobile.swipeByOffset(locatorStrategy.ID, "list", 1, 2));
        assertNoDriver(() -> mobile.swipeElementIntoView(locatorStrategy.ID, "target", "DOWN"));
        assertNoDriver(() -> mobile.swipeTextIntoView("target", "VERTICAL"));
        assertNoDriver(() -> mobile.tapCoordinates(1, 2));
        assertNoDriver(() -> mobile.swipeCoordinates(1, 2, 3, 4, 100));
        assertNoDriver(() -> mobile.rotate("PORTRAIT"));
        assertNoDriver(mobile::hideKeyboard);
        assertNoDriver(() -> mobile.keyboardKey("DONE"));
        assertNoDriver(() -> mobile.backgroundApp(1));
        assertNoDriver(() -> mobile.activateApp("com.example"));
    }

    @Test
    void engineSupportMethodsShouldFailWhenNoDriverSessionExists() {
        assertNoDriver(engineService::getPageSource);
    }

    private static void assertNoDriver(Executable executable) {
        assertThrows(IllegalStateException.class, executable);
    }
}
