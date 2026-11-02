package com.shaft.gui.internal.image;

import com.shaft.gui.internal.healing.HealingVisualProvider;
import nu.pattern.OpenCV;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;

/**
 * OpenCV-backed optional visual evidence for SHAFT Heal.
 */
public class OpenCvHealingVisualProvider implements HealingVisualProvider {
    /**
     * Creates the ServiceLoader provider.
     */
    public OpenCvHealingVisualProvider() {
    }

    @Override
    public String id() {
        return "opencv";
    }

    @Override
    public double similarity(byte[] referenceImage, byte[] candidateImage) {
        if (referenceImage == null || candidateImage == null
                || referenceImage.length == 0 || candidateImage.length == 0) {
            return 0;
        }
        OpenCV.loadLocally();
        MatOfByte referenceBuffer = new MatOfByte(referenceImage);
        MatOfByte candidateBuffer = new MatOfByte(candidateImage);
        Mat reference = Imgcodecs.imdecode(referenceBuffer, Imgcodecs.IMREAD_GRAYSCALE);
        Mat decodedCandidate = Imgcodecs.imdecode(candidateBuffer, Imgcodecs.IMREAD_GRAYSCALE);
        Mat resizedCandidate = new Mat();
        Mat difference = new Mat();
        try {
            if (reference.empty() || decodedCandidate.empty()) {
                return 0;
            }
            Mat candidate = decodedCandidate;
            if (reference.width() != candidate.width() || reference.height() != candidate.height()) {
                Imgproc.resize(candidate, resizedCandidate, new Size(reference.width(), reference.height()));
                candidate = resizedCandidate;
            }
            Core.absdiff(reference, candidate, difference);
            double normalizedDifference = Core.mean(difference).val[0] / 255.0;
            return Math.max(0, Math.min(1, 1 - normalizedDifference));
        } finally {
            difference.release();
            resizedCandidate.release();
            decodedCandidate.release();
            reference.release();
            candidateBuffer.release();
            referenceBuffer.release();
        }
    }
}
