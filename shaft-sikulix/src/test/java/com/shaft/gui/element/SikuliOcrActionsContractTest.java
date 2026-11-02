package com.shaft.gui.element;

import com.shaft.gui.ocr.OcrTarget;
import org.testng.Assert;
import org.testng.annotations.Test;

public class SikuliOcrActionsContractTest {
    @Test
    public void desktopActionsExposeOcrClickDoubleClickAndHover() throws Exception {
        Assert.assertNotNull(SikuliActions.class.getMethod("click", OcrTarget.class));
        Assert.assertNotNull(SikuliActions.class.getMethod("doubleClick", OcrTarget.class));
        Assert.assertNotNull(SikuliActions.class.getMethod("hover", OcrTarget.class));
    }
}
