package com.shaft.gui.image;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;
import java.util.Optional;
import java.util.OptionalDouble;
import java.util.OptionalInt;

/**
 * An immutable screenshot target and its matching constraints.
 */
public final class ImageTarget {
    static final int MAX_ENCODED_IMAGE_BYTES = 20 * 1024 * 1024;
    private static final long MAX_IMAGE_PIXELS = 100_000_000L;

    private final byte[] imageBytes;
    private final Path sourcePath;
    private final Double minimumConfidence;
    private final Integer occurrence;
    private final ImageRectangle searchRegion;
    private final ImageMatchingMode matchingMode;

    private ImageTarget(byte[] imageBytes, Path sourcePath, Double minimumConfidence, Integer occurrence,
                        ImageRectangle searchRegion, ImageMatchingMode matchingMode) {
        this.imageBytes = imageBytes.clone();
        this.sourcePath = sourcePath;
        this.minimumConfidence = minimumConfidence;
        this.occurrence = occurrence;
        this.searchRegion = searchRegion;
        this.matchingMode = matchingMode;
    }

    public static ImageTarget fromPath(Path imagePath) {
        Objects.requireNonNull(imagePath, "Image path cannot be null.");
        Path normalizedPath = imagePath.toAbsolutePath().normalize();
        try {
            byte[] bytes = readBounded(normalizedPath);
            validateEncodedImage(bytes);
            return new ImageTarget(bytes, normalizedPath, null,
                    null, null, ImageMatchingMode.AUTO);
        } catch (IOException exception) {
            throw new IllegalArgumentException("Could not read image target: " + normalizedPath, exception);
        }
    }

    public static ImageTarget fromPath(String imagePath) {
        Objects.requireNonNull(imagePath, "Image path cannot be null.");
        return fromPath(Path.of(imagePath));
    }

    public static ImageTarget fromBytes(byte[] imageBytes) {
        if (imageBytes == null || imageBytes.length == 0) {
            throw new IllegalArgumentException("Image target bytes cannot be null or empty.");
        }
        if (imageBytes.length > MAX_ENCODED_IMAGE_BYTES) {
            throw new IllegalArgumentException("Image target exceeds the 20 MiB encoded-size limit.");
        }
        validateEncodedImage(imageBytes);
        return new ImageTarget(imageBytes, null, null, null, null,
                ImageMatchingMode.AUTO);
    }

    public ImageTarget minimumConfidence(double confidence) {
        if (!Double.isFinite(confidence) || confidence < 0 || confidence > 1) {
            throw new IllegalArgumentException("Minimum confidence must be a finite value from 0 through 1.");
        }
        return new ImageTarget(imageBytes, sourcePath, confidence, occurrence, searchRegion, matchingMode);
    }

    public ImageTarget occurrence(int zeroBasedOccurrence) {
        if (zeroBasedOccurrence < 0) {
            throw new IllegalArgumentException("Image occurrence cannot be negative.");
        }
        return new ImageTarget(imageBytes, sourcePath, minimumConfidence, zeroBasedOccurrence, searchRegion,
                matchingMode);
    }

    public ImageTarget within(ImageRectangle region) {
        return new ImageTarget(imageBytes, sourcePath, minimumConfidence, occurrence,
                Objects.requireNonNull(region, "Image search region cannot be null."), matchingMode);
    }

    public ImageTarget matchingMode(ImageMatchingMode mode) {
        return new ImageTarget(imageBytes, sourcePath, minimumConfidence, occurrence, searchRegion,
                Objects.requireNonNull(mode, "Image matching mode cannot be null."));
    }

    public byte[] imageBytes() {
        return imageBytes.clone();
    }

    public Optional<Path> sourcePath() {
        return Optional.ofNullable(sourcePath);
    }

    public OptionalDouble minimumConfidence() {
        return minimumConfidence == null ? OptionalDouble.empty() : OptionalDouble.of(minimumConfidence);
    }

    public OptionalInt occurrence() {
        return occurrence == null ? OptionalInt.empty() : OptionalInt.of(occurrence);
    }

    public Optional<ImageRectangle> searchRegion() {
        return Optional.ofNullable(searchRegion);
    }

    public ImageMatchingMode matchingMode() {
        return matchingMode;
    }

    private static byte[] readBounded(Path imagePath) throws IOException {
        if (Files.size(imagePath) > MAX_ENCODED_IMAGE_BYTES) {
            throw new IllegalArgumentException("Image target exceeds the 20 MiB encoded-size limit: " + imagePath);
        }
        try (InputStream input = Files.newInputStream(imagePath)) {
            byte[] bytes = input.readNBytes(MAX_ENCODED_IMAGE_BYTES + 1);
            if (bytes.length > MAX_ENCODED_IMAGE_BYTES) {
                throw new IllegalArgumentException("Image target exceeds the 20 MiB encoded-size limit: " + imagePath);
            }
            return bytes;
        }
    }

    private static void validateEncodedImage(byte[] imageBytes) {
        try (ImageInputStream input = ImageIO.createImageInputStream(new ByteArrayInputStream(imageBytes))) {
            if (input == null) {
                throw new IllegalArgumentException("Image target is not a supported encoded image.");
            }
            var readers = ImageIO.getImageReaders(input);
            if (!readers.hasNext()) {
                throw new IllegalArgumentException("Image target is not a supported encoded image.");
            }
            ImageReader reader = readers.next();
            try {
                reader.setInput(input, true, true);
                int width = reader.getWidth(0);
                int height = reader.getHeight(0);
                if (width <= 0 || height <= 0 || (long) width * height > MAX_IMAGE_PIXELS) {
                    throw new IllegalArgumentException("Image target dimensions are invalid or exceed 100 megapixels.");
                }
                if (reader.read(0) == null) {
                    throw new IllegalArgumentException("Image target could not be decoded.");
                }
            } finally {
                reader.dispose();
            }
        } catch (IOException | RuntimeException exception) {
            if (exception instanceof IllegalArgumentException illegalArgumentException) {
                throw illegalArgumentException;
            }
            throw new IllegalArgumentException("Image target is corrupt or uses an unsupported encoding.", exception);
        }
    }
}
