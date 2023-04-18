

import com.shaft.gui.element.TouchActions;

import org.mockito.Mock;

import org.testng.annotations.Test;

import static org.mockito.Mockito.doReturn;

import static org.mockito.Mockito.verify;

public class TouchActionsTest {

    @Mock

    private TouchDevice touchDevice;

    @Test

    public void testDown() {

        doReturn(true).when(touchDevice).down(10, 10);

        TouchActions touchActions = new TouchActions(touchDevice);

        touchActions.down(10, 10);

        verify(touchDevice).down(10, 10);

    }

    @Test

    public void testUp() {

        doReturn(true).when(touchDevice).up(10, 10);

        TouchActions touchActions = new TouchActions(touchDevice);

        touchActions.up(10, 10);

        verify(touchDevice).up(10, 10);

    }

    @Test

    public void testMove() {

        doReturn(true).when(touchDevice).move(10, 10);

        TouchActions touchActions = new TouchActions(touchDevice);

        touchActions.move(10, 10);

        verify(touchDevice).move(10, 10);

    }

    @Test

    public void testClick() {

        doReturn(true).when(touchDevice).click();

        TouchActions touchActions = new TouchActions(touchDevice);

        touchActions.click();

        verify(touchDevice).click();

    }

    @Test

    public void testDoubleClick() {

        doReturn(true).when(touchDevice).doubleClick();

        TouchActions touchActions = new TouchActions(touchDevice);

        touchActions.doubleClick();

        verify(touchDevice).doubleClick();

    }

    @Test

    public void testLongPress() {

        doReturn(true).when(touchDevice).longPress(1000);

        TouchActions touchActions = new TouchActions(touchDevice);

        touchActions.longPress(1000);

        verify(touchDevice).longPress(1000);

    }

    @Test

    public void testFlick() {

        doReturn(true).when(touchDevice).flick(10, 10);

        TouchActions touchActions = new TouchActions(touchDevice);

        touchActions.flick(10, 10);

        verify(touchDevice).flick(10, 10);

    }

}
