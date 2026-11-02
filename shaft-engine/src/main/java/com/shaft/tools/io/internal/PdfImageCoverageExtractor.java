package com.shaft.tools.io.internal;

import org.apache.pdfbox.contentstream.PDFGraphicsStreamEngine;
import org.apache.pdfbox.cos.COSName;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.graphics.image.PDImage;

import java.awt.geom.Point2D;
import java.io.IOException;

/** Measures displayed image area, including image transforms and nested form XObjects. */
final class PdfImageCoverageExtractor extends PDFGraphicsStreamEngine {
    private double displayedArea;

    private PdfImageCoverageExtractor(PDPage page) {
        super(page);
    }

    static double coverage(PDPage page) throws IOException {
        PdfImageCoverageExtractor extractor = new PdfImageCoverageExtractor(page);
        extractor.processPage(page);
        double pageArea = page.getCropBox().getWidth() * page.getCropBox().getHeight();
        return Math.min(1, extractor.displayedArea / Math.max(1, pageArea));
    }

    @Override
    public void drawImage(PDImage image) {
        var transform = getGraphicsState().getCurrentTransformationMatrix().createAffineTransform();
        Point2D origin = transform.transform(new Point2D.Double(0, 0), null);
        Point2D horizontal = transform.transform(new Point2D.Double(1, 0), null);
        Point2D vertical = transform.transform(new Point2D.Double(0, 1), null);
        displayedArea += Math.abs((horizontal.getX() - origin.getX()) * (vertical.getY() - origin.getY())
                - (horizontal.getY() - origin.getY()) * (vertical.getX() - origin.getX()));
    }

    @Override public void appendRectangle(Point2D p0, Point2D p1, Point2D p2, Point2D p3) { }
    @Override public void clip(int windingRule) { }
    @Override public void moveTo(float x, float y) { }
    @Override public void lineTo(float x, float y) { }
    @Override public void curveTo(float x1, float y1, float x2, float y2, float x3, float y3) { }
    @Override public Point2D getCurrentPoint() { return new Point2D.Float(); }
    @Override public void closePath() { }
    @Override public void endPath() { }
    @Override public void strokePath() { }
    @Override public void fillPath(int windingRule) { }
    @Override public void fillAndStrokePath(int windingRule) { }
    @Override public void shadingFill(COSName shadingName) { }
}
