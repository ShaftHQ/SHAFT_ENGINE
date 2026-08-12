package com.shaft.gui.internal.ocr;

import com.shaft.gui.ocr.OcrMatch;
import com.shaft.gui.ocr.OcrTarget;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.interactions.Interactive;
import org.openqa.selenium.interactions.PointerInput;
import org.openqa.selenium.interactions.Sequence;
import io.appium.java_client.AppiumDriver;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.time.Duration;
import java.util.List;

/** W3C pointer dispatch for screenshot-backed OCR targets. */
public final class OcrWebDriverPointerActions {
    private OcrWebDriverPointerActions() {
    }

    public static void click(WebDriver driver, OcrTarget target) {
        perform(driver, target, PointerGesture.CLICK);
    }

    public static void doubleClick(WebDriver driver, OcrTarget target) {
        perform(driver, target, PointerGesture.DOUBLE_CLICK);
    }

    public static void hover(WebDriver driver, OcrTarget target) {
        perform(driver, target, PointerGesture.HOVER);
    }

    private static void perform(WebDriver driver, OcrTarget target, PointerGesture gesture) {
        if (!(driver instanceof TakesScreenshot screenshotDriver)) {
            throw new UnsupportedOperationException("OCR element actions require a screenshot-capable WebDriver.");
        }
        if (!(driver instanceof Interactive interactive)) {
            throw new UnsupportedOperationException("OCR element actions require W3C pointer input support.");
        }
        byte[] screenshot = screenshotDriver.getScreenshotAs(OutputType.BYTES);
        BufferedImage image = decode(screenshot);
        OcrMatch match = OcrProcessingActions.find(screenshot, target);
        boolean nativeMobile = driver instanceof AppiumDriver;
        int[] viewportSize = nativeMobile
                ? new int[]{image.getWidth(), image.getHeight()}
                : viewportSize(driver, image);
        OcrPoint point = OcrCoordinateMapper.toPointerCenter(match, image.getWidth(), image.getHeight(),
                viewportSize[0], viewportSize[1], 0, 0);

        PointerInput.Kind kind = nativeMobile ? PointerInput.Kind.TOUCH : PointerInput.Kind.MOUSE;
        PointerInput pointer = new PointerInput(kind, nativeMobile ? "shaft-ocr-touch" : "shaft-ocr-mouse");
        Sequence sequence = new Sequence(pointer, 0)
                .addAction(pointer.createPointerMove(Duration.ZERO, PointerInput.Origin.viewport(), point.x(), point.y()));
        if (gesture != PointerGesture.HOVER) {
            addClick(sequence, pointer);
            if (gesture == PointerGesture.DOUBLE_CLICK) {
                addClick(sequence, pointer);
            }
        }
        interactive.perform(List.of(sequence));
    }

    private static int[] viewportSize(WebDriver driver, BufferedImage screenshot) {
        if (driver instanceof JavascriptExecutor javascriptExecutor) {
            Object dimensions = javascriptExecutor.executeScript("return [window.innerWidth, window.innerHeight];");
            if (dimensions instanceof List<?> values && values.size() == 2
                    && values.get(0) instanceof Number width && values.get(1) instanceof Number height
                    && width.intValue() > 0 && height.intValue() > 0) {
                return new int[]{width.intValue(), height.intValue()};
            }
        }
        return new int[]{screenshot.getWidth(), screenshot.getHeight()};
    }

    private static void addClick(Sequence sequence, PointerInput pointer) {
        sequence.addAction(pointer.createPointerDown(PointerInput.MouseButton.LEFT.asArg()));
        sequence.addAction(pointer.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));
    }

    private static BufferedImage decode(byte[] screenshot) {
        try {
            BufferedImage image = ImageIO.read(new ByteArrayInputStream(screenshot));
            if (image == null) {
                throw new IllegalArgumentException("WebDriver returned an unreadable OCR screenshot.");
            }
            return image;
        } catch (IOException exception) {
            throw new IllegalArgumentException("WebDriver returned an unreadable OCR screenshot.", exception);
        }
    }

    private enum PointerGesture {
        CLICK,
        DOUBLE_CLICK,
        HOVER
    }
}
