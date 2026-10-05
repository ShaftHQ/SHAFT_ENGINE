package com.shaft.gui.element.internal;

import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;

/**
 * Firefox Grid timed out {@code TableTests} because a failed accessible-name
 * lookup fell through to {@code getDomProperty("text")} (#6552).
 */
public class ReportingAccessibleNameTest {

    @Test
    public void firefoxAccessibleNameFailureDoesNotReadTheTextDomProperty() {
        WebElement element = mock(WebElement.class);
        when(element.getAccessibleName()).thenThrow(new TimeoutException("getElementDomProperty text hung"));

        assertEquals(Actions.reportingAccessibleName(element, false), "");

        verify(element, never()).getDomProperty("text");
    }

    @Test
    public void accessibleNameIsReturnedWhenTheDriverProvidesIt() {
        WebElement element = mock(WebElement.class);
        when(element.getAccessibleName()).thenReturn("Roland Mendel");

        assertEquals(Actions.reportingAccessibleName(element, false), "Roland Mendel");
    }
}
