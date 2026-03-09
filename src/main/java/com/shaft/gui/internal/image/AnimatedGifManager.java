package com.shaft.gui.internal.image;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.imgscalr.Scalr;
import org.openqa.selenium.NoSuchSessionException;
import org.openqa.selenium.WebDriverException;

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
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;

/**
 * Manages the creation, frame-appending, and finalisation of animated GIF files that visualise
 * test execution steps in the SHAFT framework.
 *
 * <p>Each test thread gets its own GIF writer state through {@link ThreadLocal} fields, making
 * this class safe for parallel test execution. A new GIF is started automatically when the first
 * screenshot is captured; subsequent screenshots are appended as additional frames.
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
    protected static final String LIGHTWEIGHT_GIF_REGEX = "(.*validation.*)|(.*verify.*)|(.*assert.*)|(.*click.*)|(.*tap.*)|(.*key.*)|(.*navigate.*)|(.*type.*)";
    private static final ThreadLocal<ImageWriter> gifWriter = new ThreadLocal<>();
    private static final ThreadLocal<ImageWriteParam> imageWriteParam = new ThreadLocal<>();
    private static final ThreadLocal<IIOMetadata> imageMetaData = new ThreadLocal<>();
    private static final int GIF_SIZE = 1280;
    private static final ThreadLocal<ImageOutputStream> gifOutputStream = new ThreadLocal<>();
    private static final ThreadLocal<AnimatedGifManager> gifManager = new ThreadLocal<>();
    private static final ThreadLocal<String> gifRelativePathWithFileName = ThreadLocal.withInitial(() -> "");

    /**
     * Creates a new GifSequenceWriter
     *
     * @param outputStream        the ImageOutputStream to be written to
     * @param imageType           one of the imageTypes specified in BufferedImage
     * @param timeBetweenFramesMS the time between frames in milliseconds
     * @throws IOException if no gif ImageWriters are found
     */
    protected AnimatedGifManager(ImageOutputStream outputStream, int imageType, int timeBetweenFramesMS) throws IOException {
        initialize(outputStream, imageType, timeBetweenFramesMS);
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
        if (SHAFT.Properties.visuals.createAnimatedGif() && !gifRelativePathWithFileName.get().isEmpty()) {
            try {
                ReportManagerHelper.attach("Animated Gif", String.valueOf(System.currentTimeMillis()), new FileInputStream(gifRelativePathWithFileName.get()));
                if (gifWriter.get() != null) {
                    gifManager.get().close();
                }
                if (gifOutputStream.get() != null) {
                    gifOutputStream.get().close();
                }

                gifOutputStream.remove();
                gifManager.remove();
                String gifRelativePath = gifRelativePathWithFileName.get();
                gifRelativePathWithFileName.set("");
                return gifRelativePath;
            } catch (FileNotFoundException e) {
                // this happens when the gif fails to start, maybe the browser window was
                // already closed
            } catch (IOException | NullPointerException | IllegalStateException e) {
                ReportManagerHelper.logDiscrete(e);
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
        // ensure that animatedGif is started, else force start it
        if (SHAFT.Properties.visuals.createAnimatedGif()) {
            if (gifRelativePathWithFileName.get().isEmpty()) {
                startAnimatedGif(screenshot);
            } else {
                appendToAnimatedGif(screenshot);
            }
        }
    }

    private static void appendToAnimatedGif(byte[] screenshot) {
        try {
            BufferedImage image;
            if (screenshot != null && gifManager.get() != null && gifWriter.get() != null) {
                image = ImageIO.read(new ByteArrayInputStream(screenshot));
                //scaling it down
                image = Scalr.resize(image, Scalr.Method.BALANCED, GIF_SIZE);
                gifManager.get().writeToSequence(ScreenshotHelper.overlayShaftEngineLogo(image));
            }
        } catch (NoSuchSessionException e) {
            // this happens when attempting to append to a non-existing gif, expected
            // solution is to recreate the gif
            // removed the old solution, the new fix is to ignore this exception, this will
            // leave the gif intact and will attach it even after failing to append to it
        } catch (WebDriverException | IOException | IllegalStateException | IllegalArgumentException |
                 NullPointerException e) {
            ReportManagerHelper.logDiscrete(e);
        }
    }

    protected static void startAnimatedGif(byte[] screenshot) {
        if (SHAFT.Properties.visuals.createAnimatedGif() && screenshot != null) {
            try {
                String gifFileName = FileSystems.getDefault().getSeparator() + System.currentTimeMillis() + ".gif";
                gifRelativePathWithFileName.set(SHAFT.Properties.paths.allureResults() + "/screenshots/" + new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date()) + gifFileName);

                // grab the output image type from the first image in the sequence
                BufferedImage firstImage = ImageIO.read(new ByteArrayInputStream(screenshot));

                //scaling it down
                firstImage = Scalr.resize(firstImage, Scalr.Method.BALANCED, GIF_SIZE);

                // create a new BufferedOutputStream
                FileActions.getInstance(true).createFile(gifRelativePathWithFileName.get().replace(gifFileName, ""), gifFileName);
                gifOutputStream.set(new FileImageOutputStream(new File(gifRelativePathWithFileName.get())));

                // create a gif sequence with the type of the first image, 500 milliseconds
                // between frames, which loops infinitely
                gifManager.set(
                        new AnimatedGifManager(gifOutputStream.get(), firstImage.getType(), SHAFT.Properties.visuals.animatedGifFrameDelay()));

                // draw initial blank image to set the size of the GIF...
                BufferedImage initialImage = new BufferedImage(firstImage.getWidth(), firstImage.getHeight(), firstImage.getType());
                Graphics2D initialImageGraphics = initialImage.createGraphics();
                initialImageGraphics.setBackground(Color.WHITE);
                initialImageGraphics.setColor(Color.WHITE);
                initialImageGraphics.clearRect(0, 0, firstImage.getWidth(), firstImage.getHeight());

                // write out initialImage to the sequence...
                gifManager.get().writeToSequence(initialImage);
                initialImageGraphics.dispose();

                // write out first image to the sequence...
                gifManager.get().writeToSequence(ScreenshotHelper.overlayShaftEngineLogo(ScreenshotHelper.toBufferedImage(firstImage)));
            } catch (NullPointerException | NoSuchSessionException e) {
                // this happens in case the start animated Gif is triggered in a none-test
                // method
                // or this happens when the window is already closed
            } catch (IOException | WebDriverException e) {
                ReportManagerHelper.logDiscrete(e);
            }
        }
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
        gifWriter.get().writeToSequence(new IIOImage(img, null, imageMetaData.get()), imageWriteParam.get());
    }

    /**
     * Close this GifSequenceWriter object. This does not close the underlying
     * stream, just finishes off the GIF.
     *
     * @throws IOException if an error occurs during writing.
     */
    protected void close() throws IOException {
        gifWriter.get().endWriteSequence();
    }

    private void initialize(ImageOutputStream outputStream, int imageType, int timeBetweenFramesMS) throws IOException {
        // my method to create a writer
        gifWriter.set(getWriter());
        imageWriteParam.set(gifWriter.get().getDefaultWriteParam());
        var imageTypeSpecifier = ImageTypeSpecifier.createFromBufferedImageType(imageType);

        imageMetaData.set(gifWriter.get().getDefaultImageMetadata(imageTypeSpecifier, imageWriteParam.get()));

        String metaFormatName = imageMetaData.get().getNativeMetadataFormatName();

        IIOMetadataNode root = (IIOMetadataNode) imageMetaData.get().getAsTree(metaFormatName);

        IIOMetadataNode graphicsControlExtensionNode = getNode(root, "GraphicControlExtension");

        graphicsControlExtensionNode.setAttribute("disposalMethod", "none");
        graphicsControlExtensionNode.setAttribute("userInputFlag", "FALSE");
        graphicsControlExtensionNode.setAttribute("transparentColorFlag", "FALSE");
        graphicsControlExtensionNode.setAttribute("delayTime", Integer.toString(timeBetweenFramesMS / 10));
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

        imageMetaData.get().setFromTree(metaFormatName, root);

        gifWriter.get().setOutput(outputStream);

        gifWriter.get().prepareWriteSequence(null);
    }
}