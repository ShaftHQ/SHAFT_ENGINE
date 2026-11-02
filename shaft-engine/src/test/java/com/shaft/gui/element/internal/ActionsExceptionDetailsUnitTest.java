package com.shaft.gui.element.internal;

import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.interactions.Sequence;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Map;
import java.util.List;
import java.util.Collections;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

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

    @Test
    public void buildTouchDragAndDropSequenceShouldUseSourceAndDestinationCenters() {
        Rectangle sourceRectangle = new Rectangle(10, 20, 40, 60);
        Rectangle destinationRectangle = new Rectangle(100, 200, 80, 20);

        Sequence sequence = Actions.buildTouchDragAndDropSequence(sourceRectangle, destinationRectangle);
        List<?> interactions = sequence.toJson() instanceof Map<?, ?> sequenceAsMap
                && sequenceAsMap.get("actions") instanceof List<?> actions ? actions : List.of();

        Assert.assertEquals(interactions.size(), 5, "Expected five interactions for drag and drop gesture.");
        Assert.assertTrue(interactions.get(0) instanceof Map<?, ?>, "First interaction should be a pointer move.");
        Assert.assertEquals(((Map<?, ?>) interactions.get(0)).get("x"),
                sourceRectangle.getX() + sourceRectangle.getWidth() / 2, "Source X should be centered.");
        Assert.assertEquals(((Map<?, ?>) interactions.get(0)).get("y"),
                sourceRectangle.getY() + sourceRectangle.getHeight() / 2, "Source Y should be centered.");
        Assert.assertTrue(interactions.get(2) instanceof Map<?, ?>
                        && "pause".equals(((Map<?, ?>) interactions.get(2)).get("type")),
                "Expected a pause interaction before moving to destination.");
        Assert.assertEquals(((Map<?, ?>) interactions.get(3)).get("x"),
                destinationRectangle.getX() + destinationRectangle.getWidth() / 2, "Destination X should be centered.");
        Assert.assertEquals(((Map<?, ?>) interactions.get(3)).get("y"),
                destinationRectangle.getY() + destinationRectangle.getHeight() / 2, "Destination Y should be centered.");
    }

    @Test
    public void chooseDragAndDropElementShouldReturnDisplayedElement() {
        WebElement hiddenElement = mock(WebElement.class);
        WebElement displayedElement = mock(WebElement.class);
        when(hiddenElement.isDisplayed()).thenReturn(false);
        when(displayedElement.isDisplayed()).thenReturn(true);

        WebElement selected = Actions.chooseDragAndDropElement(
                List.of(hiddenElement, displayedElement), By.id("drop-l2"), "destination");

        Assert.assertSame(selected, displayedElement, "Expected first displayed element to be selected.");
    }

    @Test
    public void chooseDragAndDropElementShouldFailWhenNoDisplayedElementsExist() {
        WebElement hiddenElement = mock(WebElement.class);
        when(hiddenElement.isDisplayed()).thenReturn(false);

        NoSuchElementException exception = Assert.expectThrows(NoSuchElementException.class,
                () -> Actions.chooseDragAndDropElement(Collections.singletonList(hiddenElement), By.id("drop-l2"), "destination"));

        Assert.assertTrue(exception.getMessage().contains("destination element could not be located"),
                "Expected clear destination element error message.");
        Assert.assertTrue(exception.getCause() instanceof NoSuchElementException,
                "Expected original not-found reason as cause.");
    }

    @Test
    public void hasDisplayedElementShouldReturnTrueOnlyWhenAtLeastOneElementIsDisplayed() {
        WebElement hiddenElement = mock(WebElement.class);
        WebElement displayedElement = mock(WebElement.class);
        when(hiddenElement.isDisplayed()).thenReturn(false);
        when(displayedElement.isDisplayed()).thenReturn(true);

        Assert.assertTrue(Actions.hasDisplayedElement(List.of(hiddenElement, displayedElement)),
                "Expected true when at least one element is displayed.");
        Assert.assertFalse(Actions.hasDisplayedElement(List.of(hiddenElement)),
                "Expected false when all elements are hidden.");
        Assert.assertFalse(Actions.hasDisplayedElement(Collections.emptyList()),
                "Expected false when no elements are available.");
    }

    @Test
    public void createDragGestureParametersShouldContainElementIdAndDestinationCenter() {
        Rectangle destinationRectangle = new Rectangle(100, 200, 80, 20);

        Map<String, Object> params = Actions.createDragGestureParameters("sourceElementId", destinationRectangle);

        Assert.assertEquals(params.get("elementId"), "sourceElementId", "Expected source element id in params.");
        Assert.assertEquals(params.get("endX"),
                destinationRectangle.getX() + destinationRectangle.getWidth() / 2, "Expected centered destination X.");
        Assert.assertEquals(params.get("endY"),
                destinationRectangle.getY() + destinationRectangle.getHeight() / 2, "Expected centered destination Y.");
    }

    @Test
    public void chooseBestEffortDisplayedElementPrefersDisplayedElement() {
        WebElement hiddenElement = mock(WebElement.class);
        WebElement displayedElement = mock(WebElement.class);
        when(hiddenElement.isDisplayed()).thenReturn(false);
        when(displayedElement.isDisplayed()).thenReturn(true);

        WebElement selected = Actions.chooseBestEffortDisplayedElement(List.of(hiddenElement, displayedElement));

        Assert.assertSame(selected, displayedElement, "Expected displayed element to be preferred.");
    }

    @Test
    public void chooseBestEffortDisplayedElementFallsBackToFirstElementWhenNoneDisplayed() {
        WebElement firstElement = mock(WebElement.class);
        WebElement secondElement = mock(WebElement.class);
        when(firstElement.isDisplayed()).thenReturn(false);
        when(secondElement.isDisplayed()).thenReturn(false);

        WebElement selected = Actions.chooseBestEffortDisplayedElement(List.of(firstElement, secondElement));

        Assert.assertSame(selected, firstElement, "Expected fallback to first element when none are displayed.");
    }
}
