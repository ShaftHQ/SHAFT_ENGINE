package com.shaft.gui.element.internal;

import com.shaft.gui.browser.internal.FluentBrowserActions;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.SikuliActions;
import com.shaft.validation.internal.ValidationsBuilder;
import com.shaft.validation.internal.WebDriverElementValidationsBuilder;
import org.openqa.selenium.By;
import org.sikuli.script.App;
import org.sikuli.script.Screen;
import org.testng.annotations.Test;

import java.lang.reflect.InvocationTargetException;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.testng.Assert.assertNull;
import static org.testng.Assert.assertThrows;
import static org.utbot.runtime.utils.java.UtUtils.getFieldValue;
import static org.utbot.runtime.utils.java.UtUtils.setField;

public final class FluentElementActionsTest {
    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.alert

    ///region

    @Test
    public void testAlert1() {
        org.mockito.MockedConstruction mockedConstruction = null;
        try {
            mockedConstruction = mockConstruction(AlertActions.class, (AlertActions alertActionsMock, org.mockito.MockedConstruction.Context context) -> {
            });
            FluentElementActions fluentElementActions = new FluentElementActions();

            AlertActions actual = fluentElementActions.alert();

            AlertActions expectedMock = mock(AlertActions.class);
        } finally {
            mockedConstruction.close();
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.and

    ///region

    @Test
    public void testAnd1() {
        FluentElementActions fluentElementActions = new FluentElementActions();

        FluentElementActions actual = fluentElementActions.and();

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.assertThat

    ///region

    @Test
    public void testAssertThat1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.validation.Validations.class);
            ValidationsBuilder validationsBuilderMock = mock(ValidationsBuilder.class);
            (when(validationsBuilderMock.element(any()))).thenReturn(((WebDriverElementValidationsBuilder) null));
            (mockedStatic.when(com.shaft.validation.Validations::assertThat)).thenReturn(validationsBuilderMock);
            FluentElementActions fluentElementActions = new FluentElementActions();

            WebDriverElementValidationsBuilder actual = fluentElementActions.assertThat(null);

            assertNull(actual);
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.browser

    ///region

    @Test
    public void testBrowser1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(FluentBrowserActions.class);
            (mockedStatic.when(FluentBrowserActions::getInstance)).thenReturn(((FluentBrowserActions) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            FluentBrowserActions actual = fluentElementActions.browser();

            assertNull(actual);
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.captureScreenshot

    ///region

    @Test
    public void testCaptureScreenshot1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.captureScreenshot(null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for captureScreenshot

    public void testCaptureScreenshot_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 1 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.click

    ///region

    @Test
    public void testClick1() {
        org.mockito.MockedConstruction mockedConstruction = null;
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedConstruction = mockConstruction(com.shaft.gui.element.TouchActions.class, (com.shaft.gui.element.TouchActions touchActionsMock, org.mockito.MockedConstruction.Context context) -> {
            });
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::isMobileNativeExecution)).thenReturn(true);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.click(null));
        } finally {
            mockedConstruction.close();
            mockedStatic.close();
        }
    }

    @Test
    public void testClick2() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::isMobileNativeExecution)).thenReturn(false);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.click(null));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for click

    public void testClick_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 4 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.clickAndHold

    ///region

    @Test
    public void testClickAndHold1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.clickAndHold(null));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for clickAndHold

    public void testClickAndHold_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.clickUsingJavascript

    ///region

    @Test
    public void testClickUsingJavascript1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null), ((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.clickUsingJavascript(null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for clickUsingJavascript

    public void testClickUsingJavascript_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 4 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.clipboardActions

    ///region

    @Test
    public void testClipboardActions1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null), ((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.clipboardActions(null, null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for clipboardActions

    public void testClipboardActions_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 2 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.doubleClick

    ///region

    @Test
    public void testDoubleClick1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.doubleClick(null));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for doubleClick

    public void testDoubleClick_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 2 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.dragAndDrop

    ///region

    @Test
    public void testDragAndDrop1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.dragAndDrop(null, null));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for dragAndDrop

    public void testDragAndDrop_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.dragAndDropByOffset

    ///region

    @Test
    public void testDragAndDropByOffset1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null), ((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.dragAndDropByOffset(null, -255, -255));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for dragAndDropByOffset

    public void testDragAndDropByOffset_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 5 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.executeNativeMobileCommand

    ///region

    @Test
    public void testExecuteNativeMobileCommand1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null), ((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.executeNativeMobileCommand(null, null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for executeNativeMobileCommand

    public void testExecuteNativeMobileCommand_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 1 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.getAttribute

    ///region

    @Test(enabled = false, description = "Disabled due to sandbox")
    public void testGetAttribute1() {
        FluentElementActions fluentElementActions = new FluentElementActions();
        By elementLocatorMock = mock(By.class);
        String attributeName = "";

        assertThrows(ExceptionInInitializerError.class, () -> fluentElementActions.getAttribute(elementLocatorMock, attributeName));
    }

    @Test
    public void testGetAttribute2() {
        FluentElementActions fluentElementActions = new FluentElementActions();

        assertThrows(AssertionError.class, () -> fluentElementActions.getAttribute(null, null));
    }
    ///endregion

    ///region OTHER: SECURITY for method getAttribute(org.openqa.selenium.By, java.lang.String)

    @Test(enabled = false, description = "Disabled due to sandbox")
    public void testGetAttribute11() {
        FluentElementActions fluentElementActions = new FluentElementActions();
        By elementLocatorMock = mock(By.class);
        String attributeName = "";

        assertThrows(ExceptionInInitializerError.class, () -> fluentElementActions.getAttribute(elementLocatorMock, attributeName));
    }
    ///endregion

    ///region OTHER: ERROR SUITE for method getAttribute(org.openqa.selenium.By, java.lang.String)

    @Test
    public void testGetAttribute21() {
        FluentElementActions fluentElementActions = new FluentElementActions();

        assertThrows(AssertionError.class, () -> fluentElementActions.getAttribute(null, null));
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.getContext

    ///region

    @Test
    public void testGetContext1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.getContext());
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for getContext

    public void testGetContext_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.getContextHandles

    ///region

    @Test
    public void testGetContextHandles1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.getContextHandles());
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for getContextHandles

    public void testGetContextHandles_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 2 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.getCSSProperty

    ///region

    @Test
    public void testGetCSSProperty1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.getCSSProperty(null, null));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for getCSSProperty

    public void testGetCSSProperty_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.getElementsCount

    ///region

    @Test
    public void testGetElementsCount1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.getElementsCount(null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for getElementsCount

    public void testGetElementsCount_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.getInstance

    ///region Errors report for getInstance

    public void testGetInstance_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 1 occurrences of:
        /* Unable to make field private static final sun.misc.Unsafe java.util.concurrent.atomic.AtomicInteger.U accessible:
        module java.base does not "opens java.util.concurrent.atomic" to unnamed module @7ea37dbf */

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.getSelectedText

    ///region

    @Test
    public void testGetSelectedText1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.getSelectedText(null));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for getSelectedText

    public void testGetSelectedText_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.getText

    ///region

    @Test
    public void testGetText1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.getText(null));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for getText

    public void testGetText_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.getWindowHandle

    ///region

    @Test
    public void testGetWindowHandle1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.getWindowHandle());
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for getWindowHandle

    public void testGetWindowHandle_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.getWindowHandles

    ///region

    @Test
    public void testGetWindowHandles1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.getWindowHandles());
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for getWindowHandles

    public void testGetWindowHandles_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.hover

    ///region

    @Test
    public void testHover1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.hover(null));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for hover

    public void testHover_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.hoverAndClick

    ///region

    @Test
    public void testHoverAndClick1() {
        FluentElementActions fluentElementActions = new FluentElementActions();

        assertThrows(NullPointerException.class, () -> fluentElementActions.hoverAndClick(null, null));
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.isElementClickable

    ///region

    @Test
    public void testIsElementClickable1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null), ((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.isElementClickable(null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for isElementClickable

    public void testIsElementClickable_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 5 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.isElementDisplayed

    ///region

    @Test
    public void testIsElementDisplayed1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.isElementDisplayed(null));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for isElementDisplayed

    public void testIsElementDisplayed_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 4 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.keyPress

    ///region Errors report for keyPress

    public void testKeyPress_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 1 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.performAlertAction

    ///region

    @Test
    public void testPerformAlertAction1() {
        org.mockito.MockedConstruction mockedConstruction = null;
        try {
            mockedConstruction = mockConstruction(AlertActions.class, (AlertActions alertActionsMock, org.mockito.MockedConstruction.Context context) -> {
            });
            FluentElementActions fluentElementActions = new FluentElementActions();

            AlertActions actual = fluentElementActions.performAlertAction();

            AlertActions expectedMock = mock(AlertActions.class);
        } finally {
            mockedConstruction.close();
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.performBrowserAction

    ///region

    @Test
    public void testPerformBrowserAction1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(FluentBrowserActions.class);
            (mockedStatic.when(FluentBrowserActions::getInstance)).thenReturn(((FluentBrowserActions) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            FluentBrowserActions actual = fluentElementActions.performBrowserAction();

            assertNull(actual);
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.performSikuliAction

    ///region

    @Test
    public void testPerformSikuliAction1() throws ClassNotFoundException, IllegalAccessException, NoSuchFieldException, InvocationTargetException, NoSuchMethodException {
        org.mockito.MockedConstruction mockedConstruction = null;
        try {
            mockedConstruction = mockConstruction(SikuliActions.class, (SikuliActions sikuliActionsMock, org.mockito.MockedConstruction.Context context) -> {
            });
            FluentElementActions fluentElementActions = new FluentElementActions();

            SikuliActions actual = fluentElementActions.performSikuliAction();

            SikuliActions expectedMock = mock(SikuliActions.class);
            setField(expectedMock, "com.shaft.gui.element.SikuliActions", "screen", null);
            setField(expectedMock, "com.shaft.gui.element.SikuliActions", "applicationWindow", null);
            Screen actualScreen = ((Screen) getFieldValue(actual, "com.shaft.gui.element.SikuliActions", "screen"));
            assertNull(actualScreen);

            App actualApplicationWindow = ((App) getFieldValue(actual, "com.shaft.gui.element.SikuliActions", "applicationWindow"));
            assertNull(actualApplicationWindow);

        } finally {
            mockedConstruction.close();
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.performSikuliAction

    ///region

    @Test
    public void testPerformSikuliAction2() throws ClassNotFoundException, IllegalAccessException, NoSuchFieldException, InvocationTargetException, NoSuchMethodException {
        org.mockito.MockedConstruction mockedConstruction = null;
        try {
            mockedConstruction = mockConstruction(SikuliActions.class, (SikuliActions sikuliActionsMock, org.mockito.MockedConstruction.Context context) -> {
            });
            FluentElementActions fluentElementActions = new FluentElementActions();

            SikuliActions actual = fluentElementActions.performSikuliAction(null);

            SikuliActions expectedMock = mock(SikuliActions.class);
            setField(expectedMock, "com.shaft.gui.element.SikuliActions", "screen", null);
            setField(expectedMock, "com.shaft.gui.element.SikuliActions", "applicationWindow", null);
            Screen actualScreen = ((Screen) getFieldValue(actual, "com.shaft.gui.element.SikuliActions", "screen"));
            assertNull(actualScreen);

            App actualApplicationWindow = ((App) getFieldValue(actual, "com.shaft.gui.element.SikuliActions", "applicationWindow"));
            assertNull(actualApplicationWindow);

        } finally {
            mockedConstruction.close();
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.performTouchAction

    ///region Errors report for performTouchAction

    public void testPerformTouchAction_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 1 occurrences of:
        // Default concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.scrollToElement

    ///region

    @Test
    public void testScrollToElement1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::isMobileNativeExecution)).thenReturn(false);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null), ((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.scrollToElement(null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for scrollToElement

    public void testScrollToElement_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 4 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.select

    ///region

    @Test
    public void testSelect1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.select(null, null));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for select

    public void testSelect_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 2 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.setContext

    ///region

    @Test
    public void testSetContext1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.setContext(null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for setContext

    public void testSetContext_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 2 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.setValueUsingJavaScript

    ///region

    @Test
    public void testSetValueUsingJavaScript1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null), ((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.setValueUsingJavaScript(null, null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for setValueUsingJavaScript

    public void testSetValueUsingJavaScript_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 1 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.sikulix

    ///region

    @Test
    public void testSikulix1() throws ClassNotFoundException, IllegalAccessException, NoSuchFieldException, InvocationTargetException, NoSuchMethodException {
        org.mockito.MockedConstruction mockedConstruction = null;
        try {
            mockedConstruction = mockConstruction(SikuliActions.class, (SikuliActions sikuliActionsMock, org.mockito.MockedConstruction.Context context) -> {
            });
            FluentElementActions fluentElementActions = new FluentElementActions();

            SikuliActions actual = fluentElementActions.sikulix();

            SikuliActions expectedMock = mock(SikuliActions.class);
            setField(expectedMock, "com.shaft.gui.element.SikuliActions", "screen", null);
            setField(expectedMock, "com.shaft.gui.element.SikuliActions", "applicationWindow", null);
            Screen actualScreen = ((Screen) getFieldValue(actual, "com.shaft.gui.element.SikuliActions", "screen"));
            assertNull(actualScreen);

            App actualApplicationWindow = ((App) getFieldValue(actual, "com.shaft.gui.element.SikuliActions", "applicationWindow"));
            assertNull(actualApplicationWindow);

        } finally {
            mockedConstruction.close();
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.sikulix

    ///region

    @Test
    public void testSikulix2() throws ClassNotFoundException, IllegalAccessException, NoSuchFieldException, InvocationTargetException, NoSuchMethodException {
        org.mockito.MockedConstruction mockedConstruction = null;
        try {
            mockedConstruction = mockConstruction(SikuliActions.class, (SikuliActions sikuliActionsMock, org.mockito.MockedConstruction.Context context) -> {
            });
            FluentElementActions fluentElementActions = new FluentElementActions();

            SikuliActions actual = fluentElementActions.sikulix(null);

            SikuliActions expectedMock = mock(SikuliActions.class);
            setField(expectedMock, "com.shaft.gui.element.SikuliActions", "screen", null);
            setField(expectedMock, "com.shaft.gui.element.SikuliActions", "applicationWindow", null);
            Screen actualScreen = ((Screen) getFieldValue(actual, "com.shaft.gui.element.SikuliActions", "screen"));
            assertNull(actualScreen);

            App actualApplicationWindow = ((App) getFieldValue(actual, "com.shaft.gui.element.SikuliActions", "applicationWindow"));
            assertNull(actualApplicationWindow);

        } finally {
            mockedConstruction.close();
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.submitFormUsingJavaScript

    ///region

    @Test
    public void testSubmitFormUsingJavaScript1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null), ((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.submitFormUsingJavaScript(null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for submitFormUsingJavaScript

    public void testSubmitFormUsingJavaScript_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 2 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.switchToDefaultContent

    ///region

    @Test
    public void testSwitchToDefaultContent1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            FluentElementActions actual = fluentElementActions.switchToDefaultContent();

        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for switchToDefaultContent

    public void testSwitchToDefaultContent_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.switchToIframe

    ///region

    @Test
    public void testSwitchToIframe1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.switchToIframe(null));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for switchToIframe

    public void testSwitchToIframe_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 2 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.touch

    ///region Errors report for touch

    public void testTouch_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 1 occurrences of:
        // Default concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.type

    ///region

    @Test
    public void testType1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.type(null, null));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for type

    public void testType_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 2 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.typeAppend

    ///region

    @Test
    public void testTypeAppend1() {
        FluentElementActions fluentElementActions = new FluentElementActions();

        FluentElementActions actual = fluentElementActions.typeAppend(null, null);

    }

    @Test
    public void testTypeAppend2() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            FluentElementActions fluentElementActions = new FluentElementActions();
            String text = "";

            assertThrows(NullPointerException.class, () -> fluentElementActions.typeAppend(null, text));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for typeAppend

    public void testTypeAppend_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.typeFileLocationForUpload

    ///region

    @Test
    public void testTypeFileLocationForUpload1() {
        FluentElementActions fluentElementActions = new FluentElementActions();

        assertThrows(NullPointerException.class, () -> fluentElementActions.typeFileLocationForUpload(null, null));
    }

    @Test
    public void testTypeFileLocationForUpload2() {
        FluentElementActions fluentElementActions = new FluentElementActions();
        String filePath = "";

        assertThrows(AssertionError.class, () -> fluentElementActions.typeFileLocationForUpload(null, filePath));
    }
    ///endregion

    ///region OTHER: ERROR SUITE for method typeFileLocationForUpload(org.openqa.selenium.By, java.lang.String)

    @Test
    public void testTypeFileLocationForUpload21() {
        FluentElementActions fluentElementActions = new FluentElementActions();
        String filePath = "";

        assertThrows(AssertionError.class, () -> fluentElementActions.typeFileLocationForUpload(null, filePath));
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.typeSecure

    ///region

    @Test
    public void testTypeSecure1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.typeSecure(null, null));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for typeSecure

    public void testTypeSecure_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 2 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.verifyThat

    ///region

    @Test
    public void testVerifyThat1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.validation.Validations.class);
            ValidationsBuilder validationsBuilderMock = mock(ValidationsBuilder.class);
            (when(validationsBuilderMock.element(any()))).thenReturn(((WebDriverElementValidationsBuilder) null));
            (mockedStatic.when(com.shaft.validation.Validations::verifyThat)).thenReturn(validationsBuilderMock);
            FluentElementActions fluentElementActions = new FluentElementActions();

            WebDriverElementValidationsBuilder actual = fluentElementActions.verifyThat(null);

            assertNull(actual);
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.waitForTextToChange

    ///region

    @Test
    public void testWaitForTextToChange1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null), ((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.waitForTextToChange(null, null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for waitForTextToChange

    public void testWaitForTextToChange_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 2 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.internal.FluentElementActions.waitToAttribute

    ///region

    @Test
    public void testWaitToAttribute1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null), ((ThreadLocal) null));
            FluentElementActions fluentElementActions = new FluentElementActions();

            assertThrows(NullPointerException.class, () -> fluentElementActions.waitToAttribute(null, null, null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for waitToAttribute

    public void testWaitToAttribute_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 2 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion
}
