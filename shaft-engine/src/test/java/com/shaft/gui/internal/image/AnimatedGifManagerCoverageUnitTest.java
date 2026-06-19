package com.shaft.gui.internal.image;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import javax.imageio.stream.ImageOutputStream;
import javax.imageio.stream.MemoryCacheImageOutputStream;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;

public class AnimatedGifManagerCoverageUnitTest {
    private static final FileActions FILE_ACTIONS = FileActions.getInstance(true);
    private static final Path TEMP_BASE = Path.of("target", "temp", "animatedGifManagerCoverageUnitTest");
    private static final String GIF_SESSION_FIELD = "gifSession";
    private static final String GIF_PATH_FIELD = "gifRelativePathWithFileName";

    private boolean originalCreateAnimatedGif;
    private String originalAllureResults;
    private String originalVideo;
    private boolean originalWatermark;
    private int originalAnimatedGifFrameDelay;
    private Path tempRoot;
    private Path allureResults;
    private Path videoFolder;

    @BeforeMethod(alwaysRun = true)
    public void setup(Method method) {
        originalCreateAnimatedGif = SHAFT.Properties.visuals.createAnimatedGif();
        originalAllureResults = SHAFT.Properties.paths.allureResults();
        originalVideo = SHAFT.Properties.paths.video();
        originalWatermark = SHAFT.Properties.visuals.screenshotParamsWatermark();
        originalAnimatedGifFrameDelay = SHAFT.Properties.visuals.animatedGifFrameDelay();

        tempRoot = TEMP_BASE.resolve(method.getName() + "-" + Thread.currentThread().threadId());
        allureResults = tempRoot.resolve("allure-results");
        videoFolder = tempRoot.resolve("videos");
        FILE_ACTIONS.deleteFolder(tempRoot.toString());
        FILE_ACTIONS.createFolder(tempRoot.toString());
        SHAFT.Properties.visuals.set().createAnimatedGif(true).screenshotParamsWatermark(false).animatedGifFrameDelay(100);
        SHAFT.Properties.paths.set().allureResults(allureResults.toAbsolutePath().toString());
        SHAFT.Properties.paths.set().video(videoFolder.toAbsolutePath().toString());
        clearAnimatedGifState();
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        clearAnimatedGifState();
        SHAFT.Properties.visuals.set()
                .createAnimatedGif(originalCreateAnimatedGif)
                .screenshotParamsWatermark(originalWatermark)
                .animatedGifFrameDelay(originalAnimatedGifFrameDelay);
        SHAFT.Properties.paths.set().allureResults(originalAllureResults);
        SHAFT.Properties.paths.set().video(originalVideo);
        if (tempRoot != null) {
            FILE_ACTIONS.deleteFolder(tempRoot.toString());
        }
        Properties.clearForCurrentThread();
    }

    @Test
    public void attachAnimatedGifShouldReturnEmptyWhenNoGifWasStarted() {
        Assert.assertEquals(AnimatedGifManager.attachAnimatedGif(), "");
    }

    @Test
    public void startOrAppendToAnimatedGifShouldCreateAppendAndAttachGif() {
        byte[] screenshot = createPng(40, 30, Color.BLUE);

        AnimatedGifManager.startOrAppendToAnimatedGif(screenshot);
        Assert.assertFalse(getGifRelativePath().isEmpty(), "Expected start to initialize GIF path.");

        AnimatedGifManager.startOrAppendToAnimatedGif(screenshot);
        String gifPath = AnimatedGifManager.attachAnimatedGif();

        Assert.assertFalse(gifPath.isEmpty(), "Expected attached GIF path.");
        Assert.assertTrue(new File(gifPath).exists(), "Expected generated GIF file to exist.");
        Assert.assertTrue(Path.of(gifPath).startsWith(videoFolder.toAbsolutePath()),
                "Expected generated GIF file under video.folder.");
        Assert.assertEquals(getGifRelativePath(), "", "Expected thread-local GIF path state to be reset after attach.");
    }

    @Test
    public void startAnimatedGifShouldIgnoreNullScreenshot() {
        AnimatedGifManager.startAnimatedGif(null);
        Assert.assertEquals(getGifRelativePath(), "");
    }

    @Test
    public void appendToAnimatedGifShouldHandleInvalidImageBytesWithoutThrowing() {
        AnimatedGifManager.startOrAppendToAnimatedGif(createPng(30, 30, Color.GREEN));
        AnimatedGifManager.startOrAppendToAnimatedGif("invalid-image".getBytes(StandardCharsets.UTF_8));

        String gifPath = AnimatedGifManager.attachAnimatedGif();
        Assert.assertFalse(gifPath.isEmpty(), "Expected GIF to remain attachable after invalid append.");
    }

    @Test
    public void invalidFirstFrameShouldNotPreventLaterValidFrame() {
        AnimatedGifManager.startOrAppendToAnimatedGif("invalid-image".getBytes(StandardCharsets.UTF_8));
        AnimatedGifManager.startOrAppendToAnimatedGif(createPng(30, 30, Color.GREEN));

        String gifPath = AnimatedGifManager.attachAnimatedGif();
        Assert.assertFalse(gifPath.isEmpty(), "Expected later valid frame to create an attachable GIF.");
        Assert.assertTrue(new File(gifPath).exists(), "Expected generated GIF file to exist.");
    }

    @Test
    public void prepareFrameShouldApplyWatermarkOnlyWhenNeeded() throws IOException {
        byte[] screenshot = createPng(30, 30, Color.GREEN);

        try (MockedStatic<ScreenshotHelper> screenshotHelper = Mockito.mockStatic(ScreenshotHelper.class)) {
            BufferedImage alreadyWatermarked = AnimatedGifManager.prepareFrame(screenshot, false);
            Assert.assertNotNull(alreadyWatermarked);
            screenshotHelper.verify(() -> ScreenshotHelper.overlayShaftEngineLogo(Mockito.any(BufferedImage.class)), Mockito.never());

            screenshotHelper.when(() -> ScreenshotHelper.overlayShaftEngineLogo(Mockito.any(BufferedImage.class)))
                    .thenAnswer(invocation -> invocation.getArgument(0));
            BufferedImage needsWatermark = AnimatedGifManager.prepareFrame(screenshot, true);
            Assert.assertNotNull(needsWatermark);
            screenshotHelper.verify(() -> ScreenshotHelper.overlayShaftEngineLogo(Mockito.any(BufferedImage.class)), Mockito.times(1));
        }
    }

    @Test
    public void constructorShouldInitializeWriterAndAllowWriteAndClose() throws Exception {
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        try (ImageOutputStream imageOutputStream = new MemoryCacheImageOutputStream(byteArrayOutputStream)) {
            AnimatedGifManager gifManager = new AnimatedGifManager(imageOutputStream, BufferedImage.TYPE_INT_RGB, 100);
            gifManager.writeToSequence(new BufferedImage(20, 20, BufferedImage.TYPE_INT_RGB));
            gifManager.close();
        }
        byte[] gifBytes = byteArrayOutputStream.toByteArray();
        Assert.assertTrue(gifBytes.length > 0);
        String gifHeader = new String(gifBytes, 0, 6, StandardCharsets.US_ASCII);
        Assert.assertTrue(gifHeader.equals("GIF89a") || gifHeader.equals("GIF87a"),
                "Expected generated bytes to be a valid GIF header.");
    }

    private static byte[] createPng(int width, int height, Color color) {
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = image.createGraphics();
        graphics.setColor(color);
        graphics.fillRect(0, 0, width, height);
        graphics.dispose();

        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try {
            ImageIO.write(image, "png", output);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        return output.toByteArray();
    }

    private static String getGifRelativePath() {
        try {
            Field field = AnimatedGifManager.class.getDeclaredField(GIF_PATH_FIELD);
            field.setAccessible(true);
            @SuppressWarnings("unchecked")
            ThreadLocal<String> threadLocal = (ThreadLocal<String>) field.get(null);
            return threadLocal.get();
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }

    private static void clearAnimatedGifState() {
        try {
            AnimatedGifManager.attachAnimatedGif();
            clearThreadLocal(GIF_SESSION_FIELD);
            clearThreadLocal(GIF_PATH_FIELD);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static void clearThreadLocal(String fieldName) throws ReflectiveOperationException {
        Field field = AnimatedGifManager.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        @SuppressWarnings("unchecked")
        ThreadLocal<Object> threadLocal = (ThreadLocal<Object>) field.get(null);
        threadLocal.remove();
    }
}
