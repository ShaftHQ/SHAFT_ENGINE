package com.shaft.gui.element.internal;

import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.testng.Assert;
import org.testng.annotations.Test;

public class ActionsExceptionDetailsUnitTest {

    @Test
    public void createDragAndDropDestinationNotFoundExceptionShouldWrapRootCause() {
        NoSuchElementException rootCause = new NoSuchElementException("Cannot locate drop target");

        NoSuchElementException exception = Actions.createDragAndDropDestinationNotFoundException(
                By.id("drop-l2"), rootCause);

        Assert.assertTrue(exception.getMessage().contains("drop-l2"),
                "Expected destination locator in exception message.");
        Assert.assertSame(exception.getCause(), rootCause,
                "Expected original exception to be preserved as cause.");
    }

    @Test
    public void createFailureMessageWithCausedByShouldIncludeCauseSection() {
        RuntimeException rootCause = new RuntimeException("Root issue");
        RuntimeException exception = new RuntimeException("Wrapper issue", rootCause);

        String message = Actions.createFailureMessageWithCausedBy(exception);

        Assert.assertTrue(message.contains("Root issue"), "Expected root cause details in message.");
        Assert.assertTrue(message.contains("Caused by: java.lang.RuntimeException: Root issue"),
                "Expected explicit caused-by section in message.");
    }
}
