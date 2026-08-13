package com.shaft.ocr.internal;

import com.shaft.gui.internal.ocr.OcrDocumentPageAnalysis;
import com.shaft.gui.ocr.OcrBlockLevel;
import com.shaft.gui.ocr.OcrOptions;
import com.shaft.gui.ocr.OcrRectangle;
import com.shaft.gui.ocr.OcrResult;
import com.shaft.gui.ocr.OcrTextBlock;
import org.bytedeco.javacpp.BytePointer;
import org.bytedeco.javacpp.FloatPointer;
import org.bytedeco.javacpp.IntPointer;
import org.bytedeco.javacpp.PointerPointer;
import org.bytedeco.tesseract.ResultIterator;
import org.bytedeco.tesseract.TessBaseAPI;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.bytedeco.tesseract.global.tesseract.OEM_LSTM_ONLY;
import static org.bytedeco.tesseract.global.tesseract.PSM_AUTO;
import static org.bytedeco.tesseract.global.tesseract.PSM_SINGLE_BLOCK;
import static org.bytedeco.tesseract.global.tesseract.PSM_SINGLE_LINE;
import static org.bytedeco.tesseract.global.tesseract.PSM_SINGLE_WORD;
import static org.bytedeco.tesseract.global.tesseract.PSM_SPARSE_TEXT;
import static org.bytedeco.tesseract.global.tesseract.PSM_OSD_ONLY;
import static org.bytedeco.tesseract.global.tesseract.RIL_BLOCK;
import static org.bytedeco.tesseract.global.tesseract.RIL_PARA;
import static org.bytedeco.tesseract.global.tesseract.RIL_TEXTLINE;
import static org.bytedeco.tesseract.global.tesseract.RIL_WORD;

final class JavaCppTesseractBackend implements TesseractBackend {
    private static final double ORIENTATION_CONFIDENCE_THRESHOLD = 14;

    @Override
    public OcrResult recognize(byte[] image, Path tessdataDirectory, String languageCodes, OcrOptions options) {
        if (image == null || image.length == 0) {
            throw new IllegalArgumentException("OCR image bytes cannot be null or empty.");
        }
        byte[] processedImage = OcrImagePreprocessor.apply(image, options.preprocessingMode());
        try (TessBaseAPI api = new TessBaseAPI()) {
            int initialization = api.Init(tessdataDirectory.toString(), languageCodes, OEM_LSTM_ONLY);
            if (initialization != 0) {
                throw new IllegalStateException("Tesseract could not initialize languages '" + languageCodes
                        + "' from " + tessdataDirectory + ".");
            }
            api.SetPageSegMode(pageSegmentationMode(options));
            return recognizeConfigured(api, processedImage, options);
        }
    }

    @Override
    public OcrDocumentPageAnalysis analyzeDocumentPage(byte[] image, Path tessdataDirectory, String languageCodes,
                                                       OcrOptions options, boolean detectOrientation,
                                                       boolean deskew) {
        Correction correction = new Correction(OcrImagePreprocessor.apply(image, options.preprocessingMode()),
                new AffineTransform());
        List<String> warnings = new ArrayList<>();
        int rotation = 0;
        if (detectOrientation) {
            Orientation orientation = detectOrientation(correction.image(), tessdataDirectory);
            if (orientation.detected() && orientation.rotationDegrees() != 0) {
                rotation = orientation.rotationDegrees();
                correction = rotate(correction, rotation);
            }
            if (orientation.detected() && orientation.confidence() < ORIENTATION_CONFIDENCE_THRESHOLD) {
                warnings.add("Tesseract orientation confidence " + orientation.confidence()
                        + " was below " + ORIENTATION_CONFIDENCE_THRESHOLD
                        + "; the detected cardinal correction was applied as a best effort.");
            }
        }
        double deskewDegrees = 0;
        if (deskew) {
            double candidate = detectDeskew(correction.image(), tessdataDirectory, languageCodes);
            if (Math.abs(candidate) >= 0.5 && Math.abs(candidate) <= 15) {
                deskewDegrees = candidate;
                correction = rotate(correction, -candidate);
            }
        }
        OcrOptions finalOptions = options.withPreprocessingMode(com.shaft.gui.ocr.OcrPreprocessingMode.NONE);
        OcrResult result = recognize(correction.image(), tessdataDirectory, languageCodes, finalOptions);
        return new OcrDocumentPageAnalysis(transform(result, correction.correctedToOriginal()), rotation,
                deskewDegrees, warnings);
    }

    private static OcrResult recognizeConfigured(TessBaseAPI api, byte[] image, OcrOptions options) {
        try (org.bytedeco.leptonica.PIX pix = org.bytedeco.leptonica.global.leptonica
                .pixReadMemPng(image, image.length)) {
            requireDecoded(pix);
            api.SetImage(pix);
            applyRegion(api, pix, options.region());
            if (api.Recognize(null) != 0) {
                throw new IllegalStateException("Tesseract failed to recognize the OCR image.");
            }
            return readResult(api, imageWidth(pix), imageHeight(pix));
        } finally {
            api.End();
        }
    }

    private static void requireDecoded(org.bytedeco.leptonica.PIX pix) {
        if (pix == null || pix.isNull()) {
            throw new IllegalArgumentException("Tesseract could not decode the OCR image as PNG.");
        }
    }

    private static void applyRegion(TessBaseAPI api, org.bytedeco.leptonica.PIX pix, OcrRectangle region) {
        if (region == null) {
            return;
        }
        if (region.right() > imageWidth(pix) || region.bottom() > imageHeight(pix)) {
            throw new IllegalArgumentException("OCR region " + region + " exceeds the decoded image bounds "
                    + imageWidth(pix) + "x" + imageHeight(pix) + ".");
        }
        api.SetRectangle(region.x(), region.y(), region.width(), region.height());
    }

    private static OcrResult readResult(TessBaseAPI api, int width, int height) {
        String fullText;
        try (BytePointer text = api.GetUTF8Text()) {
            fullText = text == null || text.isNull() ? "" : normalize(text.getString());
        }
        List<OcrTextBlock> blocks = new ArrayList<>();
        if (!fullText.isBlank()) {
            blocks.add(new OcrTextBlock(fullText, new OcrRectangle(0, 0, width, height),
                    Math.clamp(api.MeanTextConf() / 100.0, 0, 1), OcrBlockLevel.PAGE));
        }
        try (ResultIterator iterator = api.GetIterator()) {
            if (iterator != null && !iterator.isNull()) {
                collect(iterator, RIL_BLOCK, OcrBlockLevel.BLOCK, blocks);
                collect(iterator, RIL_PARA, OcrBlockLevel.PARAGRAPH, blocks);
                collect(iterator, RIL_TEXTLINE, OcrBlockLevel.LINE, blocks);
                collect(iterator, RIL_WORD, OcrBlockLevel.WORD, blocks);
            }
        }
        return new OcrResult(fullText, blocks);
    }

    private static Orientation detectOrientation(byte[] image, Path tessdataDirectory) {
        try (TessBaseAPI api = new TessBaseAPI();
             org.bytedeco.leptonica.PIX pix = org.bytedeco.leptonica.global.leptonica
                     .pixReadMemPng(image, image.length)) {
            requireDecoded(pix);
            if (api.Init(tessdataDirectory.toString(), "osd") != 0) {
                throw new IllegalStateException("Tesseract could not initialize the orientation model from "
                        + tessdataDirectory + ".");
            }
            api.SetPageSegMode(PSM_OSD_ONLY);
            api.SetImage(pix);
            try (IntPointer orientationDegrees = new IntPointer(1);
                 FloatPointer orientationConfidence = new FloatPointer(1);
                 PointerPointer<BytePointer> scriptName = new PointerPointer<>(1);
                 FloatPointer scriptConfidence = new FloatPointer(1)) {
                boolean detected = api.DetectOrientationScript(orientationDegrees, orientationConfidence,
                        scriptName, scriptConfidence);
                int rotation = detected ? Math.floorMod(orientationDegrees.get(0), 360) : 0;
                return new Orientation(detected, rotation, orientationConfidence.get(0));
            } finally {
                api.End();
            }
        }
    }

    private static double detectDeskew(byte[] image, Path tessdataDirectory, String languageCodes) {
        try (TessBaseAPI api = new TessBaseAPI();
             org.bytedeco.leptonica.PIX pix = org.bytedeco.leptonica.global.leptonica
                     .pixReadMemPng(image, image.length)) {
            requireDecoded(pix);
            if (api.Init(tessdataDirectory.toString(), languageCodes, OEM_LSTM_ONLY) != 0) {
                return 0;
            }
            api.SetPageSegMode(PSM_AUTO);
            api.SetImage(pix);
            try (var layout = api.AnalyseLayout()) {
                if (layout == null || layout.isNull()) {
                    return 0;
                }
                int[] orientation = {0};
                int[] writingDirection = {0};
                int[] textlineOrder = {0};
                float[] deskewAngle = {0};
                layout.Orientation(orientation, writingDirection, textlineOrder, deskewAngle);
                return Math.toDegrees(deskewAngle[0]);
            } finally {
                api.End();
            }
        }
    }

    private static Correction rotate(Correction correction, double degrees) {
        if (Math.abs(degrees) < 0.001 || Math.abs(degrees - 360) < 0.001) {
            return correction;
        }
        try {
            BufferedImage source = ImageIO.read(new ByteArrayInputStream(correction.image()));
            if (source == null) {
                throw new IllegalArgumentException("Tesseract could not decode the document page for rotation.");
            }
            double radians = Math.toRadians(degrees);
            double sine = Math.abs(Math.sin(radians));
            double cosine = Math.abs(Math.cos(radians));
            int width = (int) Math.ceil(source.getWidth() * cosine + source.getHeight() * sine);
            int height = (int) Math.ceil(source.getHeight() * cosine + source.getWidth() * sine);
            BufferedImage target = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
            Graphics2D graphics = target.createGraphics();
            AffineTransform transform = new AffineTransform();
            try {
                graphics.setColor(Color.WHITE);
                graphics.fillRect(0, 0, width, height);
                graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                        RenderingHints.VALUE_INTERPOLATION_BICUBIC);
                transform.translate(width / 2.0, height / 2.0);
                transform.rotate(radians);
                transform.translate(-source.getWidth() / 2.0, -source.getHeight() / 2.0);
                graphics.drawImage(source, transform, null);
            } finally {
                graphics.dispose();
                source.flush();
            }
            ByteArrayOutputStream output = new ByteArrayOutputStream();
            ImageIO.write(target, "png", output);
            target.flush();
            AffineTransform correctedToOriginal = new AffineTransform(correction.correctedToOriginal());
            correctedToOriginal.concatenate(transform.createInverse());
            return new Correction(output.toByteArray(), correctedToOriginal);
        } catch (IOException | NoninvertibleTransformException exception) {
            throw new IllegalStateException("Could not rotate the OCR document page.", exception);
        }
    }

    private static OcrResult transform(OcrResult result, AffineTransform correctedToOriginal) {
        if (correctedToOriginal.isIdentity()) {
            return result;
        }
        return new OcrResult(result.fullText(), result.blocks().stream().map(block -> new OcrTextBlock(block.text(),
                transform(block.bounds(), correctedToOriginal), block.confidence(), block.level())).toList());
    }

    private static OcrRectangle transform(OcrRectangle rectangle, AffineTransform correctedToOriginal) {
        Point2D[] corners = {
                new Point2D.Double(rectangle.x(), rectangle.y()),
                new Point2D.Double(rectangle.right(), rectangle.y()),
                new Point2D.Double(rectangle.x(), rectangle.bottom()),
                new Point2D.Double(rectangle.right(), rectangle.bottom())
        };
        double minX = Double.POSITIVE_INFINITY;
        double minY = Double.POSITIVE_INFINITY;
        double maxX = Double.NEGATIVE_INFINITY;
        double maxY = Double.NEGATIVE_INFINITY;
        for (Point2D corner : corners) {
            Point2D transformed = correctedToOriginal.transform(corner, null);
            minX = Math.min(minX, transformed.getX());
            minY = Math.min(minY, transformed.getY());
            maxX = Math.max(maxX, transformed.getX());
            maxY = Math.max(maxY, transformed.getY());
        }
        int x = Math.max(0, (int) Math.floor(minX));
        int y = Math.max(0, (int) Math.floor(minY));
        return new OcrRectangle(x, y, Math.max(1, (int) Math.ceil(maxX) - x),
                Math.max(1, (int) Math.ceil(maxY) - y));
    }

    private record Orientation(boolean detected, int rotationDegrees, double confidence) {
    }

    private record Correction(byte[] image, AffineTransform correctedToOriginal) {
    }

    private static int imageWidth(org.bytedeco.leptonica.PIX pix) {
        return org.bytedeco.leptonica.global.leptonica.pixGetWidth(pix);
    }

    private static int imageHeight(org.bytedeco.leptonica.PIX pix) {
        return org.bytedeco.leptonica.global.leptonica.pixGetHeight(pix);
    }

    private static void collect(ResultIterator iterator, int level, OcrBlockLevel blockLevel,
                                List<OcrTextBlock> output) {
        iterator.Begin();
        do {
            try (BytePointer recognized = iterator.GetUTF8Text(level)) {
                String text = recognized == null || recognized.isNull() ? "" : normalize(recognized.getString());
                int[] left = {0};
                int[] top = {0};
                int[] right = {0};
                int[] bottom = {0};
                if (!text.isBlank() && iterator.BoundingBox(level, left, top, right, bottom)
                        && right[0] > left[0] && bottom[0] > top[0]) {
                    output.add(new OcrTextBlock(text,
                            new OcrRectangle(left[0], top[0], right[0] - left[0], bottom[0] - top[0]),
                            Math.clamp(iterator.Confidence(level) / 100.0, 0, 1), blockLevel));
                }
            }
        } while (iterator.Next(level));
    }

    private static int pageSegmentationMode(OcrOptions options) {
        return switch (options.pageSegmentationMode()) {
            case AUTO -> PSM_AUTO;
            case SINGLE_BLOCK -> PSM_SINGLE_BLOCK;
            case SINGLE_LINE -> PSM_SINGLE_LINE;
            case SINGLE_WORD -> PSM_SINGLE_WORD;
            case SPARSE_TEXT -> PSM_SPARSE_TEXT;
        };
    }

    private static String normalize(String text) {
        return text == null ? "" : text.strip().replaceAll("\\R+", "\n");
    }
}
