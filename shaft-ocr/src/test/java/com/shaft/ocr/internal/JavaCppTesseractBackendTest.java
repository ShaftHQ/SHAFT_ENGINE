package com.shaft.ocr.internal;

import com.shaft.gui.ocr.OcrBlockLevel;
import com.shaft.gui.ocr.OcrOptions;
import com.shaft.gui.ocr.OcrPageSegmentationMode;
import com.shaft.gui.ocr.OcrRectangle;
import com.shaft.gui.ocr.OcrResult;
import com.shaft.gui.ocr.OcrTextBlock;
import org.testng.Assert;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.bytedeco.tesseract.global.tesseract.PSM_AUTO;
import static org.bytedeco.tesseract.global.tesseract.PSM_SINGLE_BLOCK;
import static org.bytedeco.tesseract.global.tesseract.PSM_SINGLE_LINE;
import static org.bytedeco.tesseract.global.tesseract.PSM_SINGLE_WORD;
import static org.bytedeco.tesseract.global.tesseract.PSM_SPARSE_TEXT;

/**
 * Deterministic tests for the JavaCPP backend: geometry, text normalisation, mode mapping and the
 * failure paths that need no tessdata model (#7076). Real recognition stays in TesseractRealEngineTest.
 */
public class JavaCppTesseractBackendTest {
    private final JavaCppTesseractBackend backend = new JavaCppTesseractBackend();

    @Test
    public void everyPageSegmentationModeMapsToItsTesseractConstant() {
        Assert.assertEquals(mode(OcrPageSegmentationMode.AUTO), PSM_AUTO);
        Assert.assertEquals(mode(OcrPageSegmentationMode.SINGLE_BLOCK), PSM_SINGLE_BLOCK);
        Assert.assertEquals(mode(OcrPageSegmentationMode.SINGLE_LINE), PSM_SINGLE_LINE);
        Assert.assertEquals(mode(OcrPageSegmentationMode.SINGLE_WORD), PSM_SINGLE_WORD);
        Assert.assertEquals(mode(OcrPageSegmentationMode.SPARSE_TEXT), PSM_SPARSE_TEXT);
    }

    @Test
    public void normalizeStripsAndCollapsesLineBreaks() {
        Assert.assertEquals(JavaCppTesseractBackend.normalize("  one\r\n\r\ntwo\n\nthree \n"), "one\ntwo\nthree");
        Assert.assertEquals(JavaCppTesseractBackend.normalize(null), "");
    }

    @Test
    public void quarterTurnSwapsDimensionsAndMapsBoundsBackToTheOriginalPage() throws IOException {
        var original = new JavaCppTesseractBackend.Correction(png(40, 10), new AffineTransform());

        var rotated = JavaCppTesseractBackend.rotate(original, 90);

        BufferedImage image = ImageIO.read(new ByteArrayInputStream(rotated.image()));
        // ceil() over sin/cos of 90 degrees can add one pixel of padding; the axes must still swap.
        Assert.assertTrue(image.getWidth() >= 10 && image.getWidth() <= 11, "width " + image.getWidth());
        Assert.assertTrue(image.getHeight() >= 40 && image.getHeight() <= 41, "height " + image.getHeight());
        // The whole corrected page maps back onto the whole original page.
        OcrResult mapped = JavaCppTesseractBackend.transform(result(new OcrRectangle(0, 0, image.getWidth(), image.getHeight())),
                rotated.correctedToOriginal());
        OcrRectangle bounds = mapped.blocks().getFirst().bounds();
        Assert.assertTrue(bounds.x() <= 1 && bounds.y() <= 1, bounds.toString());
        Assert.assertTrue(Math.abs(bounds.width() - 40) <= 2 && Math.abs(bounds.height() - 10) <= 2, bounds.toString());
        Assert.assertEquals(mapped.fullText(), "text");
        Assert.assertEquals(mapped.blocks().getFirst().level(), OcrBlockLevel.WORD);
    }

    @Test
    public void negligibleRotationsReturnTheSameCorrection() {
        var original = new JavaCppTesseractBackend.Correction(png(4, 4), new AffineTransform());
        Assert.assertSame(JavaCppTesseractBackend.rotate(original, 0.0001), original);
        Assert.assertSame(JavaCppTesseractBackend.rotate(original, 360), original);
    }

    @Test
    public void rotatingUndecodableBytesFailsLoudly() {
        var broken = new JavaCppTesseractBackend.Correction(new byte[]{1, 2, 3}, new AffineTransform());
        Assert.assertThrows(IllegalArgumentException.class, () -> JavaCppTesseractBackend.rotate(broken, 90));
    }

    @Test
    public void identityTransformReturnsTheSameResult() {
        OcrResult result = result(new OcrRectangle(1, 2, 3, 4));
        Assert.assertSame(JavaCppTesseractBackend.transform(result, new AffineTransform()), result);
    }

    @Test
    public void emptyOrNullImagesAreRejectedBeforeTesseractStarts() {
        Assert.assertThrows(IllegalArgumentException.class,
                () -> backend.recognize(null, Path.of("unused"), "eng", OcrOptions.defaults()));
        Assert.assertThrows(IllegalArgumentException.class,
                () -> backend.recognize(new byte[0], Path.of("unused"), "eng", OcrOptions.defaults()));
    }

    @Test
    public void missingLanguageModelsFailWithTheDirectoryInTheMessage() throws IOException {
        Path empty = Files.createTempDirectory("shaft-ocr-empty-tessdata");
        IllegalStateException failure = Assert.expectThrows(IllegalStateException.class,
                () -> backend.recognize(png(8, 8), empty, "eng", OcrOptions.defaults()));
        Assert.assertTrue(failure.getMessage().contains("'eng'") && failure.getMessage().contains(empty.toString()),
                failure.getMessage());
    }

    @Test
    public void documentAnalysisWithoutModelsFailsInsteadOfReturningAnEmptyPage() throws IOException {
        Path empty = Files.createTempDirectory("shaft-ocr-empty-tessdata");
        Assert.assertThrows(IllegalStateException.class,
                () -> backend.analyzeDocumentPage(png(8, 8), empty, "eng", OcrOptions.defaults(), true, false));
        // Deskew without models degrades to 0 degrees, then recognition reports the missing model.
        Assert.assertThrows(IllegalStateException.class,
                () -> backend.analyzeDocumentPage(png(8, 8), empty, "eng", OcrOptions.defaults(), false, true));
    }

    private static int mode(OcrPageSegmentationMode mode) {
        return JavaCppTesseractBackend.pageSegmentationMode(OcrOptions.defaults().withPageSegmentationMode(mode));
    }

    private static OcrResult result(OcrRectangle bounds) {
        return new OcrResult("text", List.of(new OcrTextBlock("text", bounds, 0.9, OcrBlockLevel.WORD)));
    }

    private static byte[] png(int width, int height) {
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        var graphics = image.createGraphics();
        graphics.setColor(Color.WHITE);
        graphics.fillRect(0, 0, width, height);
        graphics.dispose();
        try (ByteArrayOutputStream output = new ByteArrayOutputStream()) {
            ImageIO.write(image, "png", output);
            return output.toByteArray();
        } catch (IOException exception) {
            throw new IllegalStateException(exception);
        }
    }
}
