package com.shaft.gui.image;

import java.awt.image.RenderedImage;
import java.io.IOException;
import java.util.Iterator;

import javax.imageio.IIOException;
import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.metadata.IIOMetadataNode;
import javax.imageio.stream.ImageOutputStream;

public class AnimatedGifManager {
    private static ThreadLocal<ImageWriter> gifWriter = new ThreadLocal<ImageWriter>();
    private static ThreadLocal<ImageWriteParam> imageWriteParam = new ThreadLocal<ImageWriteParam>();
    private static ThreadLocal<IIOMetadata> imageMetaData = new ThreadLocal<IIOMetadata>();

    /**
     * Creates a new GifSequenceWriter
     * 
     * @param outputStream        the ImageOutputStream to be written to
     * @param imageType           one of the imageTypes specified in BufferedImage
     * @param timeBetweenFramesMS the time between frames in milliseconds
     * @param loopContinuously    whether the gif should loop repeatedly
     * @throws IOException if no gif ImageWriters are found
     *
     */
    protected AnimatedGifManager(ImageOutputStream outputStream, int imageType, int timeBetweenFramesMS,
	    boolean loopContinuously) throws IOException {
	initialize(outputStream, imageType, timeBetweenFramesMS, loopContinuously);
    }

    private synchronized void initialize(ImageOutputStream outputStream, int imageType, int timeBetweenFramesMS,
	    boolean loopContinuously) throws IOException {
	// my method to create a writer
	gifWriter.set(getWriter());
	imageWriteParam.set(gifWriter.get().getDefaultWriteParam());
	ImageTypeSpecifier imageTypeSpecifier = ImageTypeSpecifier.createFromBufferedImageType(imageType);

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

	IIOMetadataNode appEntensionsNode = getNode(root, "ApplicationExtensions");

	IIOMetadataNode child = new IIOMetadataNode("ApplicationExtension");

	child.setAttribute("applicationID", "NETSCAPE");
	child.setAttribute("authenticationCode", "2.0");

	int loop = loopContinuously ? 0 : 1;

	child.setUserObject(new byte[] { 0x1, (byte) (loop & 0xFF), (byte) ((loop >> 8) & 0xFF) });
	appEntensionsNode.appendChild(child);

	imageMetaData.get().setFromTree(metaFormatName, root);

	gifWriter.get().setOutput(outputStream);

	gifWriter.get().prepareWriteSequence(null);
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

    /**
     * Returns the first available GIF ImageWriter using
     * ImageIO.getImageWritersBySuffix("gif").
     * 
     * @return a GIF ImageWriter object
     * @throws IIOException if no GIF image writers are returned
     */
    private static ImageWriter getWriter() throws IIOException {
	Iterator<ImageWriter> iter = ImageIO.getImageWritersBySuffix("gif");
	if (!iter.hasNext()) {
	    throw new IIOException("No GIF Image Writers Exist");
	} else {
	    return iter.next();
	}
    }

    /**
     * Returns an existing child node, or creates and returns a new child node (if
     * the requested node does not exist).
     * 
     * @param rootNode the <tt>IIOMetadataNode</tt> to search for the child node.
     * @param nodeName the name of the child node.
     * 
     * @return the child node, if found or a new node created with the given name.
     */
    private static IIOMetadataNode getNode(IIOMetadataNode rootNode, String nodeName) {
	int nNodes = rootNode.getLength();
	for (int i = 0; i < nNodes; i++) {
	    if (rootNode.item(i).getNodeName().compareToIgnoreCase(nodeName) == 0) {
		return ((IIOMetadataNode) rootNode.item(i));
	    }
	}
	IIOMetadataNode node = new IIOMetadataNode(nodeName);
	rootNode.appendChild(node);
	return (node);
    }
}