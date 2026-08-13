package com.shaft.gui.internal.image;

import com.shaft.gui.image.ImageMatch;
import com.shaft.gui.image.ImageMatchingAlgorithm;
import com.shaft.gui.image.ImageRectangle;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Map;

public class ImageCoordinateMapperTest {
    @Test
    public void preservesNativeScreenshotPixelsWhenTargetDimensionsMatch() {
        ImageMatch match = new ImageMatch(new ImageRectangle(200, 300, 40, 20), 0.95, 1,
                ImageMatchingAlgorithm.TEMPLATE_COLOR, Map.of());

        Assert.assertEquals(ImageCoordinateMapper.toPointerCenter(match, 1080, 1920, 1080, 1920),
                new int[]{220, 310});
    }

    @Test
    public void scalesBrowserScreenshotPixelsToInnerViewportAndClampsEdges() {
        ImageMatch match = new ImageMatch(new ImageRectangle(1900, 1000, 20, 20), 0.95, 1,
                ImageMatchingAlgorithm.TEMPLATE_COLOR, Map.of());

        Assert.assertEquals(ImageCoordinateMapper.toPointerCenter(match, 1920, 1080, 960, 540),
                new int[]{955, 505});
    }
}
