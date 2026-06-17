package com.shaft.gui.internal.image;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.imgscalr.Scalr;

import javax.imageio.*;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.metadata.IIOMetadataNode;
import javax.imageio.stream.FileImageOutputStream;
import javax.imageio.stream.ImageOutputStream;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.io.*;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Iterator;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.regex.Pattern;

/**
 * Manages the creation, frame-appending, and finalisation of animated GIF files that visualise
 * test execution steps in the SHAFT framework.
 *
 * <p>Each test thread gets its own GIF session through {@link ThreadLocal}, making this class safe
 * for parallel test execution. A new GIF is started automatically when the first screenshot is
 * captured; subsequent screenshots are appended as additional frames.
 *
 * <p>GIF creation is controlled by the {@code SHAFT.Properties.visuals.createAnimatedGif()} flag.
 * When disabled, all methods in this class are effectively no-ops.
 *
 * <p>Example (managed automatically by the SHAFT framework):
 * <pre>{@code
 * AnimatedGifManager.startOrAppendToAnimatedGif(screenshotBytes);
 * // ... test steps ...
 * String gifPath = AnimatedGifManager.attachAnimatedGif();
 * }</pre>
 */
@SuppressWarnings("ConstantValue")
public class AnimatedGifManager {
    protected static final Boolean DETAILED_GIF = true;
    protected static final Pattern LIGHTWEIGHT_GIF_PATTERN = Pattern.compile(
            "(.*validation.*)|(.*verify.*)|(.*assert.*)|(.*click.*)|(.*tap.*)|(.*key.*)|(.*navigate.*)|(.*type.*)");
    private static final int GIF_SIZE = 1280;
    private static final ThreadLocal<GifSession> gifSession = new ThreadLocal<>();
    private static final ThreadLocal<String> gifRelativePathWithFileName = ThreadLocal.withInitial(() -> "");
    private static final DateTimeFormatter GIF_FILENAME_FORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss");
    private final ImageWriter gifWriter;
    private final ImageWriteParam imageWriteParam;
    private final IIOMetadata imageMetaData;

    /**
     * Creates a new GifSequenceWriter
     *
     * @param outputStream        the ImageOutputStream to be written to
     * @param imageType           one of the imageTypes specified in BufferedImage
     * @param timeBetweenFramesMS the time between frames in milliseconds
     * @throws IOException if no gif ImageWriters are found
     */
    protected AnimatedGifManager(ImageOutputStream outputStream, int imageType, int timeBetweenFramesMS) throws IOException {
        gifWriter = getWriter();
        imageWriteParam = gifWriter.getDefaultWriteParam();
        var imageTypeSpecifier = ImageTypeSpecifier.createFromBufferedImageType(imageType);

        imageMetaData = gifWriter.getDefaultImageMetadata(imageTypeSpecifier, imageWriteParam);

        String metaFormatName = imageMetaData.getNativeMetadataFormatName();

        IIOMetadataNode root = (IIOMetadataNode) imageMetaData.getAsTree(metaFormatName);

        IIOMetadataNode graphicsControlExtensionNode = getNode(root, "GraphicControlExtension");

        graphicsControlExtensionNode.setAttribute("disposalMethod", "none");
        graphicsControlExtensionNode.setAttribute("userInputFlag", "FALSE");
        graphicsControlExtensionNode.setAttribute("transparentColorFlag", "FALSE");
        graphicsControlExtensionNode.setAttribute("delayTime", Integer.toString(Math.round(timeBetweenFramesMS / 10.0f)));
        graphicsControlExtensionNode.setAttribute("transparentColorIndex", "0");

        IIOMetadataNode commentsNode = getNode(root, "CommentExtensions");
        commentsNode.setAttribute("CommentExtension", "Created by MAH");

        IIOMetadataNode appExtensionsNode = getNode(root, "ApplicationExtensions");

        var child = new IIOMetadataNode("ApplicationExtension");

        child.setAttribute("applicationID", "NETSCAPE");
        child.setAttribute("authenticationCode", "2.0");

        var loop = 0;

        child.setUserObject(new byte[]{0x1, (byte) (loop & 0xFF), (byte) ((loop >> 8) & 0xFF)});
        appExtensionsNode.appendChild(child);

        imageMetaData.setFromTree(metaFormatName, root);

        gifWriter.setOutput(outputStream);

        gifWriter.prepareWriteSequence(null);
    }

    /**
     * Finalises and attaches the current thread's animated GIF to the Allure report, then
     * resets the thread-local GIF state so a new GIF can be started for the next test.
     *
     * <p>This method is a no-op when {@code SHAFT.Properties.visuals.createAnimatedGif()}
     * is {@code false} or when no GIF has been started for the current thread.
     *
     * <p>Example:
     * <pre>{@code
     * String gifPath = AnimatedGifManager.attachAnimatedGif();
     * }</pre>
     *
     * @return the relative file-system path of the attached GIF, or an empty string if nothing was attached
     */
    public static String attachAnimatedGif() {
        // stop and attach
        GifSession session = gifSession.get();
        if (session != null && !gifRelativePathWithFileName.get().isEmpty()) {
            try {
                session.finish();
                if (!SHAFT.Properties.visuals.createAnimatedGif() || !session.hasFrames()) {
                    return "";
                }

                String gifRelativePath = session.gifPath();
                try (InputStream in = Files.newInputStream(Paths.get(gifRelativePath))) {
                    ReportManagerHelper.attach("Animated Gif", String.valueOf(System.currentTimeMillis()), in);
                }
                return gifRelativePath;
            } catch (IOException | NullPointerException | IllegalStateException e) {
                ReportManagerHelper.logDiscrete(e);
            } finally {
                gifSession.remove();
                gifRelativePathWithFileName.remove();
            }
        }
        return "";
    }

    /**
     * Appends a screenshot frame to the current thread's animated GIF, or starts a new GIF
     * if one has not yet been created for the current thread.
     *
     * <p>This method is a no-op when {@code SHAFT.Properties.visuals.createAnimatedGif()}
     * is {@code false}.
     *
     * <p>Example:
     * <pre>{@code
     * byte[] screenshot = ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
     * AnimatedGifManager.startOrAppendToAnimatedGif(screenshot);
     * }</pre>
     *
     * @param screenshot the raw PNG screenshot bytes to append as the next GIF frame;
     *                   if {@code null} the call is silently ignored
     */
    public static void startOrAppendToAnimatedGif(byte[] screenshot) {
        startOrAppendToAnimatedGif(screenshot, false);
    }

    /**
     * Appends a screenshot frame to the current thread's animated GIF, optionally skipping
     * SHAFT watermark overlay when the screenshot bytes already include it.
     *
     * @param screenshot          the raw PNG screenshot bytes to append as the next GIF frame;
     *                            if {@code null} the call is silently ignored
     * @param alreadyWatermarked  {@code true} when the frame already contains the SHAFT watermark
     */
    public static void startOrAppendToAnimatedGif(byte[] screenshot, boolean alreadyWatermarked) {
        // ensure that animatedGif is started, else force start it
        if (SHAFT.Properties.visuals.createAnimatedGif() && screenshot != null) {
            if (gifRelativePathWithFileName.get().isEmpty()) {
                startAnimatedGif(screenshot, alreadyWatermarked);
                return;
            }
            appendToAnimatedGif(screenshot, alreadyWatermarked);
        }
    }

    private static void appendToAnimatedGif(byte[] screenshot, boolean alreadyWatermarked) {
        GifSession session = gifSession.get();
        if (session != null) {
            session.enqueue(screenshot, shouldApplyWatermark(alreadyWatermarked));
        }
    }

    protected static void startAnimatedGif(byte[] screenshot) {
        startAnimatedGif(screenshot, false);
    }

    private static void startAnimatedGif(byte[] screenshot, boolean alreadyWatermarked) {
        if (SHAFT.Properties.visuals.createAnimatedGif() && screenshot != null) {
            String gifFileName = FileSystems.getDefault().getSeparator() + System.currentTimeMillis() + ".gif";
            String gifPath = SHAFT.Properties.paths.allureResults() + "/screenshots/" + GIF_FILENAME_FORMATTER.format(ZonedDateTime.now()) + gifFileName;
            GifSession session = new GifSession(gifPath, SHAFT.Properties.visuals.animatedGifFrameDelay());
            gifSession.set(session);
            gifRelativePathWithFileName.set(gifPath);
            session.enqueue(screenshot, shouldApplyWatermark(alreadyWatermarked));
        }
    }

    private static boolean shouldApplyWatermark(boolean alreadyWatermarked) {
        return !alreadyWatermarked && SHAFT.Properties.visuals.screenshotParamsWatermark();
    }

    /**
     * Returns the first available GIF ImageWriter using
     * ImageIO.getImageWritersBySuffix("gif").
     *
     * @return a GIF ImageWriter object
     * @throws IIOException if no GIF image writers are returned
     */
    private static ImageWriter getWriter() throws IIOException {
        Iterator<ImageWriter> iterator = ImageIO.getImageWritersBySuffix("gif");
        if (!iterator.hasNext()) {
            throw new IIOException("No GIF Image Writers Exist");
        } else {
            return iterator.next();
        }
    }

    /**
     * Returns an existing child node, or creates and returns a new child node (if
     * the requested node does not exist).
     *
     * @param rootNode the <tt>IIOMetadataNode</tt> to search for the child node.
     * @param nodeName the name of the child node.
     * @return the child node, if found or a new node created with the given name.
     */
    private static IIOMetadataNode getNode(IIOMetadataNode rootNode, String nodeName) {
        int nNodes = rootNode.getLength();
        for (var i = 0; i < nNodes; i++) {
            if (rootNode.item(i).getNodeName().compareToIgnoreCase(nodeName) == 0) {
                return ((IIOMetadataNode) rootNode.item(i));
            }
        }
        var node = new IIOMetadataNode(nodeName);
        rootNode.appendChild(node);
        return (node);
    }

    /**
     * Writes the given image as the next frame in the GIF sequence using the current thread's
     * {@link ImageWriter} and metadata.
     *
     * <p>Example:
     * <pre>{@code
     * gifManager.writeToSequence(bufferedImage);
     * }</pre>
     *
     * @param img the image frame to write to the GIF sequence
     * @throws IOException if an I/O error occurs while writing the frame
     */
    protected void writeToSequence(RenderedImage img) throws IOException {
        gifWriter.writeToSequence(new IIOImage(img, null, imageMetaData), imageWriteParam);
    }

    /**
     * Close this GifSequenceWriter object. This does not close the underlying
     * stream, just finishes off the GIF.
     *
     * @throws IOException if an error occurs during writing.
     */
    protected void close() throws IOException {
        gifWriter.endWriteSequence();
    }

    static BufferedImage prepareFrame(byte[] screenshot, boolean applyWatermark) throws IOException {
        BufferedImage image = ImageIO.read(new ByteArrayInputStream(screenshot));
        if (image == null) {
            return null;
        }
        image = resizeForGif(image);
        if (applyWatermark) {
            ScreenshotHelper.overlayShaftEngineLogo(image);
        }
        return image;
    }

    private static BufferedImage resizeForGif(BufferedImage image) {
        if (image.getWidth() > GIF_SIZE || image.getHeight() > GIF_SIZE) {
            image = Scalr.resize(image, Scalr.Method.BALANCED, GIF_SIZE);
        }
        if (image.getType() != BufferedImage.TYPE_CUSTOM) {
            return image;
        }
        BufferedImage converted = new BufferedImage(image.getWidth(), image.getHeight(), BufferedImage.TYPE_INT_ARGB);
        Graphics2D graphics = converted.createGraphics();
        graphics.drawImage(image, 0, 0, null);
        graphics.dispose();
        return converted;
    }

    private static BufferedImage blankFrameLike(BufferedImage image) {
        BufferedImage initialImage = new BufferedImage(image.getWidth(), image.getHeight(), image.getType());
        Graphics2D initialImageGraphics = initialImage.createGraphics();
        initialImageGraphics.setBackground(Color.WHITE);
        initialImageGraphics.setColor(Color.WHITE);
        initialImageGraphics.clearRect(0, 0, image.getWidth(), image.getHeight());
        initialImageGraphics.dispose();
        return initialImage;
    }

    private static Throwable unwrap(Throwable throwable) {
        return throwable instanceof CompletionException && throwable.getCause() != null
                ? throwable.getCause()
                : throwable;
    }

    private static final class GifSession {
        private final String gifPath;
        private final int frameDelay;
        private CompletableFuture<Void> frames = CompletableFuture.completedFuture(null);
        private AnimatedGifManager manager;
        private ImageOutputStream outputStream;
        private boolean hasFrames;

        private GifSession(String gifPath, int frameDelay) {
            this.gifPath = gifPath;
            this.frameDelay = frameDelay;
        }

        private String gifPath() {
            return gifPath;
        }

        private synchronized void enqueue(byte[] screenshot, boolean applyWatermark) {
            frames = frames.thenRunAsync(() -> writeFrame(screenshot, applyWatermark))
                    .exceptionally(throwable -> {
                        ReportManagerHelper.logDiscrete(unwrap(throwable));
                        return null;
                    });
        }

        private void finish() throws IOException {
            CompletableFuture<Void> pendingFrames;
            synchronized (this) {
                pendingFrames = frames;
            }
            pendingFrames.join();
            close();
        }

        private synchronized boolean hasFrames() {
            return hasFrames;
        }

        private void writeFrame(byte[] screenshot, boolean applyWatermark) {
            try {
                BufferedImage image = prepareFrame(screenshot, applyWatermark);
                if (image == null) {
                    return;
                }
                synchronized (this) {
                    if (manager == null) {
                        open(image);
                    }
                    manager.writeToSequence(image);
                    hasFrames = true;
                }
            } catch (IOException | RuntimeException e) {
                ReportManagerHelper.logDiscrete(e);
            }
        }

        private void open(BufferedImage firstImage) throws IOException {
            File gifFile = new File(gifPath);
            Files.createDirectories(gifFile.toPath().getParent());
            outputStream = new FileImageOutputStream(gifFile);
            manager = new AnimatedGifManager(outputStream, firstImage.getType(), frameDelay);
            manager.writeToSequence(blankFrameLike(firstImage));
        }

        private synchronized void close() throws IOException {
            IOException failure = null;
            if (manager != null) {
                try {
                    manager.close();
                } catch (IOException e) {
                    failure = e;
                }
            }
            if (outputStream != null) {
                try {
                    outputStream.close();
                } catch (IOException e) {
                    if (failure == null) {
                        failure = e;
                    } else {
                        failure.addSuppressed(e);
                    }
                }
            }
            if (failure != null) {
                throw failure;
            }
        }
    }
}
