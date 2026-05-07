package com.shaft.gui.element;

import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.enums.internal.ClipboardAction;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Field;
import java.util.ArrayList;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@Test(singleThreaded = true)
public class AsyncElementActionsCoverageUnitTest {

    @Test
    public void asyncActionsShouldDelegateToElementActionsAndClipboardHelper() {
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        WebDriver driver = mock(WebDriver.class);
        when(helper.getDriver()).thenReturn(driver);

        By locatorA = By.id("a");
        By locatorB = By.id("b");

        try (MockedConstruction<ElementActions> elementActionsConstruction = Mockito.mockConstruction(ElementActions.class)) {
            AsyncElementActions asyncElementActions = new AsyncElementActions(helper);

            asyncElementActions
                    .type(locatorA, "text")
                    .click(locatorA)
                    .select(locatorA, "option")
                    .clear(locatorA)
                    .doubleClick(locatorA)
                    .clickUsingJavascript(locatorA)
                    .captureScreenshot(locatorA)
                    .hover(locatorA)
                    .typeAppend(locatorA, "append")
                    .clickAndHold(locatorA)
                    .setValueUsingJavaScript(locatorA, "value")
                    .typeSecure(locatorA, "secret")
                    .submitFormUsingJavaScript(locatorA)
                    .typeFileLocationForUpload(locatorA, "/tmp/file.txt")
                    .dragAndDrop(locatorA, locatorB)
                    .clipboardActions(locatorA, ClipboardAction.COPY)
                    .join();

            ElementActions elementActions = elementActionsConstruction.constructed().get(0);
            verify(elementActions).type(locatorA, "text");
            verify(elementActions).click(locatorA);
            verify(elementActions).select(locatorA, "option");
            verify(elementActions).clear(locatorA);
            verify(elementActions).doubleClick(locatorA);
            verify(elementActions).clickUsingJavascript(locatorA);
            verify(elementActions).captureScreenshot(locatorA);
            verify(elementActions).hover(locatorA);
            verify(elementActions).typeAppend(locatorA, "append");
            verify(elementActions).clickAndHold(locatorA);
            verify(elementActions).setValueUsingJavaScript(locatorA, "value");
            verify(elementActions).typeSecure(locatorA, "secret");
            verify(elementActions).submitFormUsingJavaScript(locatorA);
            verify(elementActions).typeFileLocationForUpload(locatorA, "/tmp/file.txt");
            verify(elementActions).dragAndDrop(locatorA, locatorB);
            verify(helper, atLeast(2)).getDriver();
        }
    }

    @Test
    public void synchronizeShouldCatchInterruptedExceptionAndReturnSameInstance() throws Exception {
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(mock(WebDriver.class));

        try (MockedConstruction<ElementActions> ignored = Mockito.mockConstruction(ElementActions.class)) {
            AsyncElementActions asyncElementActions = new AsyncElementActions(helper);
            Thread actionThread = Thread.ofVirtual().start(() -> {
                try {
                    Thread.sleep(50);
                } catch (InterruptedException ignoredInterruptedException) {
                    Thread.currentThread().interrupt();
                }
            });

            Field actionThreadsField = AsyncElementActions.class.getDeclaredField("actionThreads");
            actionThreadsField.setAccessible(true);
            @SuppressWarnings("unchecked")
            ArrayList<Thread> actionThreads = (ArrayList<Thread>) actionThreadsField.get(asyncElementActions);
            actionThreads.add(actionThread);

            Thread.currentThread().interrupt();
            Assert.assertSame(asyncElementActions.synchronize(), asyncElementActions);
            Thread.interrupted();
            actionThread.join();
        }
    }

    @Test
    public void syncShouldBeAliasForSynchronize() {
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(mock(WebDriver.class));

        try (MockedConstruction<ElementActions> ignored = Mockito.mockConstruction(ElementActions.class)) {
            AsyncElementActions asyncElementActions = new AsyncElementActions(helper);
            Assert.assertSame(asyncElementActions.sync(), asyncElementActions);
        }
    }
}
