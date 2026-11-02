package com.shaft.gui.internal.image;

import com.shaft.gui.internal.healing.HealingVisualProvider;
import org.testng.Assert;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.util.ServiceLoader;

public class OpenCvHealingVisualProviderTest {
    @Test
    public void shouldDiscoverProviderThroughServiceLoader() {
        HealingVisualProvider provider = ServiceLoader.load(HealingVisualProvider.class)
                .findFirst()
                .orElseThrow();

        Assert.assertEquals(provider.id(), "opencv");
    }

    @Test
    public void shouldScoreIdenticalImagesHigherThanDifferentImages() throws Exception {
        OpenCvHealingVisualProvider provider = new OpenCvHealingVisualProvider();
        byte[] black = png(Color.BLACK);
        byte[] white = png(Color.WHITE);

        Assert.assertEquals(provider.similarity(black, black), 1.0, 0.0001);
        Assert.assertTrue(provider.similarity(black, white) < 0.05);
        Assert.assertEquals(provider.similarity(new byte[0], black), 0.0);
    }

    private static byte[] png(Color color) throws Exception {
        BufferedImage image = new BufferedImage(8, 8, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = image.createGraphics();
        graphics.setColor(color);
        graphics.fillRect(0, 0, image.getWidth(), image.getHeight());
        graphics.dispose();
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        ImageIO.write(image, "png", output);
        return output.toByteArray();
    }
}
