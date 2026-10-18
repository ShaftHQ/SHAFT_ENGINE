package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.gui.driver.ElementActionsContract;
import com.shaft.gui.driver.ElementAssertions;
import com.shaft.gui.driver.ElementTarget;
import com.shaft.gui.driver.ShaftLocator;
import com.shaft.gui.internal.locator.SmartLocators;
import com.shaft.gui.ocr.OcrTarget;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.mock;

public class ElementActionsContractOfflineTest {
    private final ElementAssertions assertions = mock(ElementAssertions.class);
    private final RecordingActions actions = new RecordingActions(assertions);
    private final ShaftLocator save = SHAFT.GUI.Locator.role("button", "Save");
    private final ShaftLocator cancel = SHAFT.GUI.Locator.role("button", "Cancel");

    @Test
    public void shaftLocatorsDelegateToTheByOverload() {
        Assert.assertSame(actions.and(), actions);
        Assert.assertSame(actions.assertThat(save), assertions);
        Assert.assertEquals(actions.lastName, "assertThat");
        Assert.assertEquals(actions.lastLocator.toString(), save.toBy().toString());
        Assert.assertSame(actions.verifyThat(save), assertions);
        Assert.assertSame(actions.assertThat(ElementTarget.located(cancel)), assertions);
        Assert.assertEquals(actions.lastLocator.toString(), ElementTarget.located(cancel).toBy().toString());
        Assert.assertSame(actions.verifyThat(ElementTarget.located(save)), assertions);

        Assert.assertEquals(actions.getElementsCount(save), 4);
        Assert.assertEquals(actions.lastName, "getElementsCount");
        Assert.assertEquals(actions.lastLocator.toString(), save.toBy().toString());

        assertDelegated(actions.click(save), "click");
        assertDelegated(actions.clickUsingJavascript(save), "clickUsingJavascript");
        assertDelegated(actions.scrollToElement(save), "scrollToElement");
        assertDelegated(actions.clickAndHold(save), "clickAndHold");
        assertDelegated(actions.doubleClick(save), "doubleClick");
        assertDelegated(actions.hover(save), "hover");
        assertDelegated(actions.select(save, "Yes"), "select");
        Assert.assertEquals(actions.lastText, "Yes");
        assertDelegated(actions.setValueUsingJavaScript(save, "kept"), "setValueUsingJavaScript");
        Assert.assertEquals(actions.lastText, "kept");
        assertDelegated(actions.submitFormUsingJavaScript(save), "submitFormUsingJavaScript");
        assertDelegated(actions.switchToIframe(save), "switchToIframe");
        assertDelegated(actions.type(save, "ada"), "type");
        Assert.assertEquals(actions.lastText, "ada");
        assertDelegated(actions.clear(save), "clear");
        assertDelegated(actions.typeAppend(save, "more"), "typeAppend");
        assertDelegated(actions.typeFileLocationForUpload(save, "notes.txt"), "typeFileLocationForUpload");
        Assert.assertEquals(actions.lastText, "notes.txt");
        assertDelegated(actions.typeSecure(save, "secret"), "typeSecure");
        Assert.assertEquals(actions.lastText, "secret");
        assertDelegated(actions.captureScreenshot(save), "captureScreenshot");
        Assert.assertEquals(actions.getTableRowsData(save), List.of(Map.of("Name", "Ada")));
        Assert.assertEquals(actions.ariaSnapshot(save), "role: button");

        Assert.assertSame(actions.dragAndDrop(save, cancel), actions);
        Assert.assertEquals(actions.lastLocator.toString(), save.toBy().toString());
        Assert.assertEquals(actions.secondLocator.toString(), cancel.toBy().toString());
        Assert.assertSame(actions.dragAndDropByOffset(save, 3, 4), actions);
        Assert.assertEquals(actions.offsetX, 3);
        Assert.assertEquals(actions.offsetY, 4);
    }

    @Test
    public void namedFieldsUseTheSmartLocatorAndOcrStaysUnsupported() {
        Assert.assertSame(actions.click("Save"), actions);
        Assert.assertEquals(actions.lastLocator.toString(), SmartLocators.clickableField("Save").toString());
        Assert.assertSame(actions.type("Email", "ada"), actions);
        Assert.assertEquals(actions.lastLocator.toString(), SmartLocators.inputField("Email").toString());
        Assert.assertEquals(actions.lastText, "ada");

        UnsupportedOperationException click = Assert.expectThrows(
                UnsupportedOperationException.class,
                () -> actions.click(OcrTarget.exact("Save")));
        Assert.assertTrue(click.getMessage().contains("OCR click is not supported"), click.getMessage());
        UnsupportedOperationException doubleClick = Assert.expectThrows(
                UnsupportedOperationException.class,
                () -> actions.doubleClick(OcrTarget.exact("Save")));
        Assert.assertTrue(doubleClick.getMessage().contains("OCR double-click is not supported"), doubleClick.getMessage());
        UnsupportedOperationException hover = Assert.expectThrows(
                UnsupportedOperationException.class,
                () -> actions.hover(OcrTarget.exact("Save")));
        Assert.assertTrue(hover.getMessage().contains("OCR hover is not supported"), hover.getMessage());
        UnsupportedOperationException parent = Assert.expectThrows(
                UnsupportedOperationException.class,
                actions::switchToParentFrame);
        Assert.assertTrue(parent.getMessage().contains("switchToParentFrame is not supported"), parent.getMessage());
    }

    private void assertDelegated(ElementActionsContract returned, String name) {
        Assert.assertSame(returned, actions);
        Assert.assertEquals(actions.lastName, name);
        Assert.assertEquals(actions.lastLocator.toString(), save.toBy().toString());
    }

    private static final class RecordingActions implements ElementActionsContract {
        private final ElementAssertions assertions;
        private String lastName;
        private By lastLocator;
        private By secondLocator;
        private String lastText;
        private int offsetX;
        private int offsetY;

        private RecordingActions(ElementAssertions assertions) {
            this.assertions = assertions;
        }

        private ElementActionsContract record(String name, By locator) {
            lastName = name;
            lastLocator = locator;
            return this;
        }

        @Override
        public ElementActionsContract and() {
            lastName = "and";
            return this;
        }

        @Override
        public ElementAssertions assertThat(By elementLocator) {
            lastName = "assertThat";
            lastLocator = elementLocator;
            return assertions;
        }

        @Override
        public ElementAssertions verifyThat(By elementLocator) {
            lastName = "verifyThat";
            lastLocator = elementLocator;
            return assertions;
        }

        @Override
        public int getElementsCount(By elementLocator) {
            lastName = "getElementsCount";
            lastLocator = elementLocator;
            return 4;
        }

        @Override
        public ElementActionsContract executeNativeMobileCommand(String command, Map<String, String> parameters) {
            return record("executeNativeMobileCommand", null);
        }

        @Override
        public ElementActionsContract click(By elementLocator) {
            return record("click", elementLocator);
        }

        @Override
        public ElementActionsContract clickUsingJavascript(By elementLocator) {
            return record("clickUsingJavascript", elementLocator);
        }

        @Override
        public ElementActionsContract scrollToElement(By elementLocator) {
            return record("scrollToElement", elementLocator);
        }

        @Override
        public ElementActionsContract clickAndHold(By elementLocator) {
            return record("clickAndHold", elementLocator);
        }

        @Override
        public ElementActionsContract doubleClick(By elementLocator) {
            return record("doubleClick", elementLocator);
        }

        @Override
        public ElementActionsContract dragAndDrop(By sourceElementLocator, By destinationElementLocator) {
            lastName = "dragAndDrop";
            lastLocator = sourceElementLocator;
            secondLocator = destinationElementLocator;
            return this;
        }

        @Override
        public ElementActionsContract dragAndDropByOffset(By sourceElementLocator, int xOffset, int yOffset) {
            lastName = "dragAndDropByOffset";
            lastLocator = sourceElementLocator;
            offsetX = xOffset;
            offsetY = yOffset;
            return this;
        }

        @Override
        public ElementActionsContract hover(By elementLocator) {
            return record("hover", elementLocator);
        }

        @Override
        public ElementActionsContract hoverAndClick(List<By> hoverElementLocators, By clickableElementLocator) {
            return record("hoverAndClick", clickableElementLocator);
        }

        @Override
        public ElementActionsContract select(By elementLocator, String valueOrVisibleText) {
            lastText = valueOrVisibleText;
            return record("select", elementLocator);
        }

        @Override
        public ElementActionsContract setValueUsingJavaScript(By elementLocator, String value) {
            lastText = value;
            return record("setValueUsingJavaScript", elementLocator);
        }

        @Override
        public ElementActionsContract submitFormUsingJavaScript(By elementLocator) {
            return record("submitFormUsingJavaScript", elementLocator);
        }

        @Override
        public ElementActionsContract switchToIframe(By elementLocator) {
            return record("switchToIframe", elementLocator);
        }

        @Override
        public ElementActionsContract switchToDefaultContent() {
            return record("switchToDefaultContent", null);
        }

        @Override
        public String getCurrentFrame() {
            lastName = "getCurrentFrame";
            return "parent";
        }

        @Override
        public ElementActionsContract type(By elementLocator, CharSequence... text) {
            lastText = String.join("", text);
            return record("type", elementLocator);
        }

        @Override
        public ElementActionsContract clear(By elementLocator) {
            return record("clear", elementLocator);
        }

        @Override
        public ElementActionsContract typeAppend(By elementLocator, CharSequence... text) {
            lastText = String.join("", text);
            return record("typeAppend", elementLocator);
        }

        @Override
        public ElementActionsContract typeFileLocationForUpload(By elementLocator, String filePath) {
            lastText = filePath;
            return record("typeFileLocationForUpload", elementLocator);
        }

        @Override
        public ElementActionsContract typeSecure(By elementLocator, CharSequence... text) {
            lastText = String.join("", text);
            return record("typeSecure", elementLocator);
        }

        @Override
        public List<Map<String, String>> getTableRowsData(By tableLocator) {
            lastName = "getTableRowsData";
            lastLocator = tableLocator;
            return List.of(Map.of("Name", "Ada"));
        }

        @Override
        public ElementActionsContract captureScreenshot(By elementLocator) {
            return record("captureScreenshot", elementLocator);
        }

        @Override
        public String ariaSnapshot(By elementLocator) {
            lastName = "ariaSnapshot";
            lastLocator = elementLocator;
            return "role: button";
        }
    }
}
