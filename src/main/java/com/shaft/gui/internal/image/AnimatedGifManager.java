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

@SuppressWarnings("ConstantValue")
public class AnimatedGifManager {
    protected static final Boolean DETAILED_GIF = true;
    protected static final String LIGHTWEIGHT_GIF_REGEX = "(.*validation.*)|(.*verify.*)|(.*assert.*)|(.*click.*)|(.*tap.*)|(.*key.*)|(.*navigate.*)|(.*type.*)";
    private static final ThreadLocal<ImageWriter> gifWriter = new ThreadLocal<>();
    private static final ThreadLocal<ImageWriteParam> imageWriteParam = new ThreadLocal<>();
    private static final ThreadLocal<IIOMetadata> imageMetaData = new ThreadLocal<>();
    private static final int GIF_SIZE = 1280;
    private static String gifRelativePathWithFileName = "";
    private static ThreadLocal<ImageOutputStream> gifOutputStream = new ThreadLocal<>();
    private static ThreadLocal<AnimatedGifManager> gifManager = new ThreadLocal<>();

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

    public static String attachAnimatedGif() {
        // stop and attach
        if (Boolean.TRUE.equals(SHAFT.Properties.visuals.createAnimatedGif()) && !"".equals(gifRelativePathWithFileName)) {
            try {
                ReportManagerHelper.attach("Animated Gif", String.valueOf(System.currentTimeMillis()), new FileInputStream(gifRelativePathWithFileName));
                if (!gifWriter.equals(new ThreadLocal<>())) {
                    gifManager.get().close();
                }
                if (!gifOutputStream.equals(new ThreadLocal<>())) {
                    gifOutputStream.get().close();
                }

                gifOutputStream = new ThreadLocal<>();
                gifManager = new ThreadLocal<>();
                String gifRelativePath = gifRelativePathWithFileName;
                gifRelativePathWithFileName = "";
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

    public static void startOrAppendToAnimatedGif(byte[] screenshot) {
        // ensure that animatedGif is started, else force start it
        if (Boolean.TRUE.equals(SHAFT.Properties.visuals.createAnimatedGif())) {
            if (gifRelativePathWithFileName.isEmpty()) {
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
        if (Boolean.TRUE.equals(SHAFT.Properties.visuals.createAnimatedGif()) && screenshot != null) {
            try {
                String gifFileName = FileSystems.getDefault().getSeparator() + System.currentTimeMillis() + ".gif";
                gifRelativePathWithFileName = SHAFT.Properties.paths.allureResults() + "/screenshots/" + new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date()) + gifFileName;

                // grab the output image type from the first image in the sequence
                BufferedImage firstImage = ImageIO.read(new ByteArrayInputStream(screenshot));

                //scaling it down
                firstImage = Scalr.resize(firstImage, Scalr.Method.BALANCED, GIF_SIZE);

                // create a new BufferedOutputStream
                FileActions.getInstance(true).createFile(gifRelativePathWithFileName.replace(gifFileName, ""), gifFileName);
                gifOutputStream.set(new FileImageOutputStream(new File(gifRelativePathWithFileName)));

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