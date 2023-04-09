package com.shaft.gui.element;

import com.shaft.gui.element.TouchActions.SwipeDirection;
import com.shaft.gui.element.internal.FluentElementActions;
import com.shaft.validation.internal.ValidationsBuilder;
import com.shaft.validation.internal.WebDriverElementValidationsBuilder;
import org.openqa.selenium.By;
import org.testng.annotations.Test;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.testng.Assert.assertNull;
import static org.testng.Assert.assertThrows;

public final class TouchActionsTest {
    ///region Test suites for executable com.shaft.gui.element.TouchActions.swipeElementIntoView

    ///region Errors report for swipeElementIntoView

    public void testSwipeElementIntoView_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 144 occurrences of:
        // Default concrete execution failed

        // 1 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.swipeElementIntoView

    ///region

    @Test
    public void testSwipeElementIntoView1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null), ((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            TouchActions touchActions = new TouchActions();

            assertThrows(NullPointerException.class, () -> touchActions.swipeElementIntoView(((By) null), ((By) null), ((SwipeDirection) null)));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for swipeElementIntoView

    public void testSwipeElementIntoView_errors1() {
        // Couldn't generate some tests. List of errors:
        // 
        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.swipeElementIntoView

    ///region Errors report for swipeElementIntoView

    public void testSwipeElementIntoView_errors2() {
        // Couldn't generate some tests. List of errors:
        // 
        // 4 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.swipeElementIntoView

    ///region

    @Test
    public void testSwipeElementIntoView2() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null), ((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            TouchActions touchActions = new TouchActions();

            assertThrows(NullPointerException.class, () -> touchActions.swipeElementIntoView(((By) null), ((String) null), ((SwipeDirection) null)));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for swipeElementIntoView

    public void testSwipeElementIntoView_errors3() {
        // Couldn't generate some tests. List of errors:
        // 
        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.nativeKeyboardKeyPress

    ///region

    @Test
    public void testNativeKeyboardKeyPress1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null), ((ThreadLocal) null));
            TouchActions touchActions = new TouchActions();

            assertThrows(NullPointerException.class, () -> touchActions.nativeKeyboardKeyPress(null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for nativeKeyboardKeyPress

    public void testNativeKeyboardKeyPress_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 147 occurrences of:
        // Default concrete execution failed

        // 4 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.pinchToZoom

    ///region Errors report for pinchToZoom

    public void testPinchToZoom_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 144 occurrences of:
        // Default concrete execution failed

        // 1 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.activateAppFromBackground

    ///region

    @Test
    public void testActivateAppFromBackground1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::isMobileNativeExecution)).thenReturn(false);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            TouchActions touchActions = new TouchActions();

            assertThrows(NullPointerException.class, () -> touchActions.activateAppFromBackground(null));
        } finally {
            mockedStatic.close();
        }
    }

    @Test
    public void testActivateAppFromBackground2() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::isMobileNativeExecution)).thenReturn(true);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            TouchActions touchActions = new TouchActions();

            assertThrows(NullPointerException.class, () -> touchActions.activateAppFromBackground(null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for activateAppFromBackground

    public void testActivateAppFromBackground_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 144 occurrences of:
        // Default concrete execution failed

        // 5 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.and

    ///region Errors report for and

    public void testAnd_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 145 occurrences of:
        // Default concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.assertThat

    ///region

    @Test
    public void testAssertThat1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.validation.Validations.class);
            ValidationsBuilder validationsBuilderMock = mock(ValidationsBuilder.class);
            (when(validationsBuilderMock.element(any()))).thenReturn(((WebDriverElementValidationsBuilder) null));
            (mockedStatic.when(com.shaft.validation.Validations::assertThat)).thenReturn(validationsBuilderMock);
            TouchActions touchActions = new TouchActions();

            WebDriverElementValidationsBuilder actual = touchActions.assertThat(null);

            assertNull(actual);
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.doubleTap

    ///region

    @Test
    public void testDoubleTap1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null), ((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            TouchActions touchActions = new TouchActions();

            assertThrows(NullPointerException.class, () -> touchActions.doubleTap(null));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for doubleTap

    public void testDoubleTap_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 8 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.element

    ///region

    @Test
    public void testElement1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(FluentElementActions.class);
            (mockedStatic.when(FluentElementActions::getInstance)).thenReturn(((FluentElementActions) null));
            TouchActions touchActions = new TouchActions();

            FluentElementActions actual = touchActions.element();

            assertNull(actual);
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for element

    public void testElement_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 144 occurrences of:
        // Default concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.getInstance

    ///region Errors report for getInstance

    public void testGetInstance_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 1 occurrences of:
        // Default concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.hideNativeKeyboard

    ///region

    @Test
    public void testHideNativeKeyboard1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null), ((ThreadLocal) null));
            TouchActions touchActions = new TouchActions();

            assertThrows(NullPointerException.class, () -> touchActions.hideNativeKeyboard());
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for hideNativeKeyboard

    public void testHideNativeKeyboard_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 144 occurrences of:
        // Default concrete execution failed

        // 8 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.longTap

    ///region

    @Test
    public void testLongTap1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null), ((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            TouchActions touchActions = new TouchActions();

            assertThrows(NullPointerException.class, () -> touchActions.longTap(null));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for longTap

    public void testLongTap_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 8 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.performElementAction

    ///region

    @Test
    public void testPerformElementAction1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(FluentElementActions.class);
            (mockedStatic.when(FluentElementActions::getInstance)).thenReturn(((FluentElementActions) null));
            TouchActions touchActions = new TouchActions();

            FluentElementActions actual = touchActions.performElementAction();

            assertNull(actual);
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for performElementAction

    public void testPerformElementAction_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 142 occurrences of:
        // Default concrete execution failed

    }
    ///endregion

    ///endregion

    public void testResetApp_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 144 occurrences of:
        // Default concrete execution failed

        // 5 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    public void testRestartApp_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 142 occurrences of:
        // Default concrete execution failed

        // 5 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.sendAppToBackground

    ///region Errors report for sendAppToBackground

    public void testSendAppToBackground_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 143 occurrences of:
        // Default concrete execution failed

        // 4 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.sendAppToBackground

    ///region

    @Test
    public void testSendAppToBackground1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::isMobileNativeExecution)).thenReturn(false);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            TouchActions touchActions = new TouchActions();

            assertThrows(NullPointerException.class, () -> touchActions.sendAppToBackground(-255));
        } finally {
            mockedStatic.close();
        }
    }

    @Test
    public void testSendAppToBackground2() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::isMobileNativeExecution)).thenReturn(true);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            TouchActions touchActions = new TouchActions();

            assertThrows(NullPointerException.class, () -> touchActions.sendAppToBackground(-255));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for sendAppToBackground

    public void testSendAppToBackground_errors1() {
        // Couldn't generate some tests. List of errors:
        // 
        // 143 occurrences of:
        // Default concrete execution failed

        // 5 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.swipeByOffset

    ///region

    @Test
    public void testSwipeByOffset1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            TouchActions touchActions = new TouchActions();

            assertThrows(NullPointerException.class, () -> touchActions.swipeByOffset(null, 0, 2));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for swipeByOffset

    public void testSwipeByOffset_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 7 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.swipeElementIntoView

    ///region

    @Test
    public void testSwipeElementIntoView3() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            TouchActions touchActions = new TouchActions();

            assertThrows(NullPointerException.class, () -> touchActions.swipeElementIntoView(null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for swipeElementIntoView

    public void testSwipeElementIntoView_errors4() {
        // Couldn't generate some tests. List of errors:
        // 
        // 143 occurrences of:
        // Default concrete execution failed

        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.swipeToElement

    ///region

    @Test
    public void testSwipeToElement1() {
        org.mockito.MockedStatic mockedStatic = null;
        org.mockito.MockedStatic mockedStatic1 = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            mockedStatic1 = mockStatic(com.google.common.base.Throwables.class);
            (mockedStatic1.when(() -> com.google.common.base.Throwables.getRootCause(any()))).thenReturn(((Throwable) null));
            TouchActions touchActions = new TouchActions();

            assertThrows(NullPointerException.class, () -> touchActions.swipeToElement(null, null));
        } finally {
            mockedStatic.close();
            mockedStatic1.close();
        }
    }
    ///endregion

    ///region Errors report for swipeToElement

    public void testSwipeToElement_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 6 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Errors report for tap

    public void testTap_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 10 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.tap

    ///region

    @Test
    public void testTap2() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            TouchActions touchActions = new TouchActions();

            assertThrows(NullPointerException.class, () -> touchActions.tap(((String) null)));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for tap

    public void testTap_errors1() {
        // Couldn't generate some tests. List of errors:
        // 
        // 325 occurrences of:
        // Default concrete execution failed

        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.verifyThat

    ///region

    @Test
    public void testVerifyThat1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.validation.Validations.class);
            ValidationsBuilder validationsBuilderMock = mock(ValidationsBuilder.class);
            (when(validationsBuilderMock.element(any()))).thenReturn(((WebDriverElementValidationsBuilder) null));
            (mockedStatic.when(com.shaft.validation.Validations::verifyThat)).thenReturn(validationsBuilderMock);
            TouchActions touchActions = new TouchActions();

            WebDriverElementValidationsBuilder actual = touchActions.verifyThat(null);

            assertNull(actual);
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///endregion

    ///region Test suites for executable com.shaft.gui.element.TouchActions.waitUntilElementIsVisible

    ///region

    @Test
    public void testWaitUntilElementIsVisible1() {
        org.mockito.MockedStatic mockedStatic = null;
        try {
            mockedStatic = mockStatic(com.shaft.driver.internal.DriverFactoryHelper.class);
            (mockedStatic.when(com.shaft.driver.internal.DriverFactoryHelper::getDriver)).thenReturn(((ThreadLocal) null));
            TouchActions touchActions = new TouchActions();

            assertThrows(NullPointerException.class, () -> touchActions.waitUntilElementIsVisible(null));
        } finally {
            mockedStatic.close();
        }
    }
    ///endregion

    ///region Errors report for waitUntilElementIsVisible

    public void testWaitUntilElementIsVisible_errors() {
        // Couldn't generate some tests. List of errors:
        // 
        // 251 occurrences of:
        // Default concrete execution failed

        // 3 occurrences of:
        // Concrete execution failed

    }
    ///endregion

    ///endregion
}
