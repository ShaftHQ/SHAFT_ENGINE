package com.shaft.gui.image;

import org.testng.Assert;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.OptionalDouble;

public class ImagePublicContractTest {
    @Test
    public void imageTargetAndRichMatchTypesShouldExposeTheTypedVisualContract() throws IOException {
        try {
            Class<?> targetType = Class.forName("com.shaft.gui.image.ImageTarget");
            Class<?> matchType = Class.forName("com.shaft.gui.image.ImageMatch");
            Class<?> rectangleType = Class.forName("com.shaft.gui.image.ImageRectangle");
            Class<?> modeType = Class.forName("com.shaft.gui.image.ImageMatchingMode");

            Assert.assertNotNull(targetType.getMethod("fromPath", Path.class));
            Assert.assertNotNull(targetType.getMethod("fromBytes", byte[].class));
            Assert.assertNotNull(targetType.getMethod("minimumConfidence", double.class));
            Assert.assertNotNull(targetType.getMethod("occurrence", int.class));
            Assert.assertNotNull(targetType.getMethod("within", rectangleType));
            Assert.assertNotNull(targetType.getMethod("matchingMode", modeType));

            Method bytesAccessor = targetType.getMethod("imageBytes");
            Object target = targetType.getMethod("fromBytes", byte[].class)
                    .invoke(null, (Object) createPng());
            byte[] firstRead = (byte[]) bytesAccessor.invoke(target);
            firstRead[0] = 99;
            byte[] secondRead = (byte[]) bytesAccessor.invoke(target);
            Assert.assertNotEquals(secondRead[0], firstRead[0], "ImageTarget must own defensive byte copies.");

            Assert.assertNotNull(matchType.getMethod("bounds"));
            Assert.assertNotNull(matchType.getMethod("centerX"));
            Assert.assertNotNull(matchType.getMethod("centerY"));
            Assert.assertNotNull(matchType.getMethod("confidence"));
            Assert.assertNotNull(matchType.getMethod("scale"));
            Assert.assertNotNull(matchType.getMethod("algorithm"));
            Assert.assertNotNull(matchType.getMethod("diagnostics"));
        } catch (ReflectiveOperationException exception) {
            Assert.fail("Typed image identification contract is missing or incomplete.", exception);
        }
    }

    @Test
    public void imageTargetShouldRejectInvalidAndOversizedEncodedInput() throws IOException {
        Assert.expectThrows(IllegalArgumentException.class, () -> ImageTarget.fromBytes(new byte[]{1, 2, 3}));
        byte[] validPng = createPng();
        Assert.expectThrows(IllegalArgumentException.class,
                () -> ImageTarget.fromBytes(Arrays.copyOf(validPng, validPng.length / 2)));

        Path oversized = Files.createTempFile("shaft-image-target", ".png");
        try {
            Files.write(oversized, new byte[ImageTarget.MAX_ENCODED_IMAGE_BYTES + 1]);
            Assert.expectThrows(IllegalArgumentException.class, () -> ImageTarget.fromPath(oversized));
        } finally {
            Files.deleteIfExists(oversized);
        }
    }

    @Test
    public void imageTargetShouldPreserveConfiguredConfidencePrecedence() throws IOException {
        ImageTarget defaultTarget = ImageTarget.fromBytes(createPng());
        Assert.assertEquals(defaultTarget.minimumConfidence(), OptionalDouble.empty());
        Assert.assertEquals(defaultTarget.minimumConfidence(0.73).minimumConfidence(), OptionalDouble.of(0.73));
    }

    @Test
    public void imageRectangleShouldRejectOverflowingEdges() {
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new ImageRectangle(Integer.MAX_VALUE, 0, 1, 1));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new ImageRectangle(0, Integer.MAX_VALUE, 1, 1));
    }

    private byte[] createPng() throws IOException {
        BufferedImage image = new BufferedImage(2, 2, BufferedImage.TYPE_INT_ARGB);
        try (ByteArrayOutputStream output = new ByteArrayOutputStream()) {
            ImageIO.write(image, "png", output);
            return output.toByteArray();
        }
    }
}
