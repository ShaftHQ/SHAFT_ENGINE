package com.shaft.gui.internal.ocr;

import com.shaft.gui.ocr.OcrMatch;
import com.shaft.gui.ocr.OcrRectangle;
import org.testng.Assert;
import org.testng.annotations.Test;

public class OcrCoordinateMapperTest {
    @Test
    public void independentlyScalesScreenshotPixelsAndAddsScreenOrigin() {
        OcrMatch match = new OcrMatch("Pay", new OcrRectangle(800, 300, 200, 100), 0.9);

        OcrPoint point = OcrCoordinateMapper.toPointerCenter(match, 2000, 1000,
                1000, 500, 25, 40);

        Assert.assertEquals(point.x(), 475);
        Assert.assertEquals(point.y(), 215);
    }

    @Test
    public void invalidDimensionsFailBeforePointerDispatch() {
        OcrMatch match = new OcrMatch("Pay", new OcrRectangle(1, 1, 2, 2), 0.9);
        Assert.expectThrows(IllegalArgumentException.class,
                () -> OcrCoordinateMapper.toPointerCenter(match, 0, 100, 100, 100, 0, 0));
    }
}
