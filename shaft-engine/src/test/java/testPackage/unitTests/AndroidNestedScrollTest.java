package testPackage.unitTests;

import com.shaft.gui.element.internal.AndroidNestedScroll;

import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class AndroidNestedScrollTest {

    @Test
    public void inspectFlagsInvertedBoundsAsClipped() {
        WebElement element = mock(WebElement.class);
        when(element.isDisplayed()).thenReturn(false);
        // Selenium Rectangle(x, y, height, width): negative height => inverted clip.
        when(element.getRect()).thenReturn(new Rectangle(40, 2200, -80, 200));

        AndroidNestedScroll.TargetPresence presence = AndroidNestedScroll.inspect(element);

        Assert.assertTrue(presence.invertedOrEmpty());
        Assert.assertTrue(presence.clippedOutOfView());
        Assert.assertTrue(presence.diagnosis().contains("inverted / non-displayed bounds"));
        Assert.assertTrue(presence.diagnosis().contains("displayed=false"));
    }

    @Test
    public void pickScrollablePrefersNestedScrollResourceId() {
        WebElement large = scrollable("com.app:id/otherScroll", 10, 100, 900, 800, true);
        WebElement nested = scrollable("com.app:id/nestedScrollView", 20, 200, 400, 300, true);

        WebElement picked = AndroidNestedScroll.pickScrollable(List.of(large, nested), 1080, 2400);

        Assert.assertSame(picked, nested);
    }

    @Test
    public void pickScrollableFallsBackToLargestDisplayedArea() {
        WebElement small = scrollable("com.app:id/a", 10, 100, 100, 100, true);
        WebElement large = scrollable("com.app:id/b", 10, 100, 900, 800, true);
        WebElement hidden = scrollable("com.app:id/nestedScrollView", 10, 100, 900, 800, false);

        WebElement picked = AndroidNestedScroll.pickScrollable(List.of(small, large, hidden), 1080, 2400);

        Assert.assertSame(picked, large);
    }

    @Test
    public void pickScrollableReturnsNullWhenNothingUsable() {
        WebElement inverted = mock(WebElement.class);
        when(inverted.isDisplayed()).thenReturn(true);
        when(inverted.getRect()).thenReturn(new Rectangle(10, 10, -5, 20));
        when(inverted.getDomAttribute("resource-id")).thenReturn("com.app:id/nestedScrollView");

        Assert.assertNull(AndroidNestedScroll.pickScrollable(List.of(inverted), 1080, 2400));
        Assert.assertNull(AndroidNestedScroll.pickScrollable(List.of(), 1080, 2400));
        Assert.assertNull(AndroidNestedScroll.pickScrollable(null, 1080, 2400));
    }

    @Test
    public void looksLikeNestedScrollMatchesResourceIdFragment() {
        Assert.assertTrue(AndroidNestedScroll.looksLikeNestedScroll("pkg:id/nestedScrollView"));
        Assert.assertTrue(AndroidNestedScroll.looksLikeNestedScroll("NESTEDSCROLL_HOST"));
        Assert.assertFalse(AndroidNestedScroll.looksLikeNestedScroll("pkg:id/recyclerView"));
        Assert.assertFalse(AndroidNestedScroll.looksLikeNestedScroll(""));
        Assert.assertFalse(AndroidNestedScroll.looksLikeNestedScroll(null));
    }


    @Test
    public void diagnosisIsNullWhenTargetLooksFine() {
        AndroidNestedScroll.TargetPresence presence =
                new AndroidNestedScroll.TargetPresence(true, 10, 20, 100, 50);
        Assert.assertFalse(presence.clippedOutOfView());
        Assert.assertNull(presence.diagnosis());
    }

    @Test
    public void inspectTreatsDisplayAndRectFailuresAsClipped() {
        WebElement displayFails = mock(WebElement.class);
        when(displayFails.isDisplayed()).thenThrow(new RuntimeException("stale"));
        when(displayFails.getRect()).thenReturn(new Rectangle(1, 2, 3, 4));
        AndroidNestedScroll.TargetPresence fromDisplay = AndroidNestedScroll.inspect(displayFails);
        Assert.assertFalse(fromDisplay.displayed());
        Assert.assertEquals(fromDisplay.width(), 4);

        WebElement rectFails = mock(WebElement.class);
        when(rectFails.isDisplayed()).thenReturn(true);
        when(rectFails.getRect()).thenThrow(new RuntimeException("gone"));
        AndroidNestedScroll.TargetPresence fromRect = AndroidNestedScroll.inspect(rectFails);
        Assert.assertTrue(fromRect.invertedOrEmpty());
        Assert.assertTrue(fromRect.diagnosis().contains("inverted / non-displayed"));
    }

    @Test
    public void resourceIdFallsBackWhenDomAttributeBlankOrThrows() {
        WebElement blankDom = mock(WebElement.class);
        when(blankDom.getDomAttribute("resource-id")).thenReturn("  ");
        when(blankDom.getAttribute("resource-id")).thenReturn("app:id/fromAttribute");
        Assert.assertEquals(AndroidNestedScroll.resourceId(blankDom), "app:id/fromAttribute");

        WebElement nullAttr = mock(WebElement.class);
        when(nullAttr.getDomAttribute("resource-id")).thenThrow(new RuntimeException("dom"));
        when(nullAttr.getAttribute("resource-id")).thenReturn(null);
        Assert.assertEquals(AndroidNestedScroll.resourceId(nullAttr), "");

        WebElement bothFail = mock(WebElement.class);
        when(bothFail.getDomAttribute("resource-id")).thenThrow(new RuntimeException("dom"));
        when(bothFail.getAttribute("resource-id")).thenThrow(new RuntimeException("attr"));
        Assert.assertEquals(AndroidNestedScroll.resourceId(bothFail), "");
    }

    @Test
    public void pickScrollableSkipsCandidatesFullyOutsideScreen() {
        WebElement outside = scrollable("app:id/nestedScrollView", 2000, 3000, 100, 100, true);
        Assert.assertNull(AndroidNestedScroll.pickScrollable(List.of(outside), 1080, 2400));
    }

    private static WebElement scrollable(String resourceId, int x, int y, int width, int height, boolean displayed) {
        WebElement element = mock(WebElement.class);
        when(element.isDisplayed()).thenReturn(displayed);
        when(element.getRect()).thenReturn(new Rectangle(x, y, height, width));
        when(element.getDomAttribute("resource-id")).thenReturn(resourceId);
        when(element.getAttribute("resource-id")).thenReturn(resourceId);
        return element;
    }
}
