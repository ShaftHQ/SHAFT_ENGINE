package com.shaft.test.unittests;

import com.shaft.gui.element.TouchActions;
import com.shaft.gui.element.internal.IosScrollView;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class IosScrollViewTest {

    @Test
    public void pickTallestPrefersTallerScrollViewOverShortToolbar() {
        WebElement toolbar = scrollView(0, 0, 390, 80, true);
        WebElement list = scrollView(0, 100, 390, 700, true);

        WebElement picked = IosScrollView.pickTallest(List.of(toolbar, list), 390, 844);

        Assert.assertSame(picked, list);
    }

    @Test
    public void pickTallestReturnsNullWhenNothingUsable() {
        WebElement hidden = scrollView(0, 0, 390, 700, false);
        WebElement inverted = mock(WebElement.class);
        when(inverted.isDisplayed()).thenReturn(true);
        when(inverted.getRect()).thenReturn(new Rectangle(0, 0, -10, 100));

        Assert.assertNull(IosScrollView.pickTallest(List.of(hidden, inverted), 390, 844));
        Assert.assertNull(IosScrollView.pickTallest(List.of(), 390, 844));
        Assert.assertNull(IosScrollView.pickTallest(null, 390, 844));
    }

    @Test
    public void pickTallestSkipsCandidatesFullyOutsideScreen() {
        WebElement outside = scrollView(0, 2000, 390, 700, true);
        Assert.assertNull(IosScrollView.pickTallest(List.of(outside), 390, 844));
    }

    @DataProvider(name = "dragDirections")
    public Object[][] dragDirections() {
        return new Object[][]{
                {TouchActions.SwipeDirection.DOWN},
                {TouchActions.SwipeDirection.UP},
                {TouchActions.SwipeDirection.LEFT},
                {TouchActions.SwipeDirection.RIGHT},
        };
    }

    @Test(dataProvider = "dragDirections")
    public void dragParametersAreDirectionAware(TouchActions.SwipeDirection direction) {
        // Selenium Rectangle(x, y, height, width)
        Rectangle rect = new Rectangle(10, 100, 500, 300);
        Map<String, Object> drag = IosScrollView.dragParameters(direction, rect);

        Assert.assertEquals(drag.get("duration"), IosScrollView.DRAG_DURATION_SECONDS);
        int fromX = ((Number) drag.get("fromX")).intValue();
        int fromY = ((Number) drag.get("fromY")).intValue();
        int toX = ((Number) drag.get("toX")).intValue();
        int toY = ((Number) drag.get("toY")).intValue();

        int left = rect.getX();
        int top = rect.getY();
        int width = rect.getWidth();
        int height = rect.getHeight();
        int insetTop = top + height * 15 / 100;
        int usableHeight = Math.max(height - height * 15 / 100, 1);
        int insetLeft = left + width * 15 / 100;
        int usableWidth = Math.max(width - width * 15 / 100, 1);
        int midX = left + Math.max(width / 2, 1);
        int midY = insetTop + Math.max(usableHeight / 2, 1);

        switch (direction) {
            case DOWN -> {
                Assert.assertEquals(fromX, midX);
                Assert.assertEquals(toX, midX);
                Assert.assertEquals(fromY, insetTop + usableHeight * 80 / 100);
                Assert.assertEquals(toY, insetTop + usableHeight * 20 / 100);
                Assert.assertTrue(fromY > toY, "DOWN reveals lower rows: finger moves up");
            }
            case UP -> {
                Assert.assertEquals(fromX, midX);
                Assert.assertEquals(toX, midX);
                Assert.assertEquals(fromY, insetTop + usableHeight * 20 / 100);
                Assert.assertEquals(toY, insetTop + usableHeight * 80 / 100);
                Assert.assertTrue(fromY < toY, "UP reveals upper rows: finger moves down");
            }
            case RIGHT -> {
                Assert.assertEquals(fromY, midY);
                Assert.assertEquals(toY, midY);
                Assert.assertEquals(fromX, insetLeft + usableWidth * 80 / 100);
                Assert.assertEquals(toX, insetLeft + usableWidth * 20 / 100);
                Assert.assertTrue(fromX > toX, "RIGHT reveals content to the right: finger moves left");
            }
            case LEFT -> {
                Assert.assertEquals(fromY, midY);
                Assert.assertEquals(toY, midY);
                Assert.assertEquals(fromX, insetLeft + usableWidth * 20 / 100);
                Assert.assertEquals(toX, insetLeft + usableWidth * 80 / 100);
                Assert.assertTrue(fromX < toX, "LEFT reveals content to the left: finger moves right");
            }
        }
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void dragParametersRejectNullDirection() {
        IosScrollView.dragParameters(null, new Rectangle(0, 0, 10, 10));
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void dragParametersRejectNullRect() {
        IosScrollView.dragParameters(TouchActions.SwipeDirection.DOWN, null);
    }

    private static WebElement scrollView(int x, int y, int width, int height, boolean displayed) {
        WebElement element = mock(WebElement.class);
        when(element.isDisplayed()).thenReturn(displayed);
        when(element.getRect()).thenReturn(new Rectangle(x, y, height, width));
        return element;
    }
}
