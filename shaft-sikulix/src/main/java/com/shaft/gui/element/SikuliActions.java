package com.shaft.gui.element;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.sikuli.basics.Settings;
import org.sikuli.script.App;
import org.sikuli.script.FindFailed;
import org.sikuli.script.Key;
import org.sikuli.script.Pattern;
import org.sikuli.script.Screen;

import javax.imageio.ImageIO;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * SHAFT action facade for SikuliX image-based desktop automation.
 */
@SuppressWarnings("unused")
public class SikuliActions {
    private final Screen screen;
    private final App applicationWindow;

    /**
     * Creates SikuliX actions for the current desktop screen.
     */
    public SikuliActions() {
        this(null);
    }

    /**
     * Creates SikuliX actions scoped to a specific desktop application window.
     *
     * @param applicationWindow SikuliX application window handle
     */
    public SikuliActions(App applicationWindow) {
        initializeSikuliEngineForCurrentScreen();
        this.screen = new Screen();
        this.screen.setAutoWaitTimeout(SHAFT.Properties.timeouts.defaultElementIdentificationTimeout());
        this.applicationWindow = applicationWindow;
    }

    /**
     * Clears existing text from the target image region, then types the supplied text.
     *
     * @param pathToTargetElementImage path to the target element reference image
     * @param text text to type
     * @return self reference for chaining
     */
    public SikuliActions type(String pathToTargetElementImage, String text) {
        return type(readImageFromFile(pathToTargetElementImage), text);
    }

    /**
     * Clears existing text from the target image region, then types the supplied text.
     *
     * @param targetElement target element reference image bytes
     * @param text text to type
     * @return self reference for chaining
     */
    public SikuliActions type(byte[] targetElement, String text) {
        Pattern element = null;
        try {
            element = prepareElementPattern(targetElement);
            clearAndType(element, text);
            passAction("type", formatTextForReport(text));
        } catch (IOException | FindFailed rootCauseException) {
            failAction("type", formatTextForReport(text), rootCauseException);
        }
        return this;
    }

    /**
     * Appends text into the target image region.
     *
     * @param pathToTargetElementImage path to the target element reference image
     * @param text text to append
     * @return self reference for chaining
     */
    public SikuliActions typeAppend(String pathToTargetElementImage, String text) {
        return typeAppend(readImageFromFile(pathToTargetElementImage), text);
    }

    /**
     * Appends text into the target image region.
     *
     * @param targetElement target element reference image bytes
     * @param text text to append
     * @return self reference for chaining
     */
    public SikuliActions typeAppend(byte[] targetElement, String text) {
        Pattern element = null;
        try {
            element = prepareElementPattern(targetElement);
            screen.wait(element).type(text);
            passAction("typeAppend", formatTextForReport(text));
        } catch (IOException | FindFailed rootCauseException) {
            failAction("typeAppend", formatTextForReport(text), rootCauseException);
        }
        return this;
    }

    /**
     * Clears existing text from the target image region, types sensitive text, and masks it in reports.
     *
     * @param pathToTargetElementImage path to the target element reference image
     * @param text sensitive text to type
     * @return self reference for chaining
     */
    public SikuliActions typeSecure(String pathToTargetElementImage, String text) {
        return typeSecure(readImageFromFile(pathToTargetElementImage), text);
    }

    /**
     * Clears existing text from the target image region, types sensitive text, and masks it in reports.
     *
     * @param targetElement target element reference image bytes
     * @param text sensitive text to type
     * @return self reference for chaining
     */
    public SikuliActions typeSecure(byte[] targetElement, String text) {
        Pattern element = null;
        try {
            element = prepareElementPattern(targetElement);
            clearAndType(element, text);
            passAction("typeSecure", "*".repeat(formatTextForReport(text).length()));
        } catch (IOException | FindFailed rootCauseException) {
            failAction("typeSecure", "********", rootCauseException);
        }
        return this;
    }

    /**
     * Clicks the target image region.
     *
     * @param pathToTargetElementImage path to the target element reference image
     * @return self reference for chaining
     */
    public SikuliActions click(String pathToTargetElementImage) {
        return click(readImageFromFile(pathToTargetElementImage));
    }

    /**
     * Clicks the target image region.
     *
     * @param targetElement target element reference image bytes
     * @return self reference for chaining
     */
    public SikuliActions click(byte[] targetElement) {
        Pattern element = null;
        String elementText = null;
        try {
            element = prepareElementPattern(targetElement);
            var match = screen.wait(element);
            elementText = match.getText();
            match.click();
            passAction("click", formatTextForReport(elementText));
        } catch (IOException | FindFailed rootCauseException) {
            failAction("click", formatTextForReport(elementText), rootCauseException);
        }
        return this;
    }

    /**
     * Reads text from the target image region.
     *
     * @param pathToTargetElementImage path to the target element reference image
     * @return detected text
     */
    public String getText(String pathToTargetElementImage) {
        return getText(readImageFromFile(pathToTargetElementImage));
    }

    /**
     * Reads text from the target image region.
     *
     * @param targetElement target element reference image bytes
     * @return detected text
     */
    public String getText(byte[] targetElement) {
        Pattern element = null;
        String elementText = null;
        try {
            element = prepareElementPattern(targetElement);
            elementText = screen.wait(element).getText();
            passAction("getText", formatTextForReport(elementText));
        } catch (IOException | FindFailed rootCauseException) {
            failAction("getText", null, rootCauseException);
        }
        return formatTextForReport(elementText);
    }

    /**
     * Moves the mouse pointer over the target image region.
     *
     * @param pathToTargetElementImage path to the target element reference image
     * @return self reference for chaining
     */
    public SikuliActions hover(String pathToTargetElementImage) {
        return hover(readImageFromFile(pathToTargetElementImage));
    }

    /**
     * Moves the mouse pointer over the target image region.
     *
     * @param targetElement target element reference image bytes
     * @return self reference for chaining
     */
    public SikuliActions hover(byte[] targetElement) {
        Pattern element = null;
        String elementText = null;
        try {
            element = prepareElementPattern(targetElement);
            var match = screen.wait(element);
            elementText = match.getText();
            match.hover();
            passAction("hover", formatTextForReport(elementText));
        } catch (IOException | FindFailed rootCauseException) {
            failAction("hover", formatTextForReport(elementText), rootCauseException);
        }
        return this;
    }

    /**
     * Double-clicks the target image region.
     *
     * @param pathToTargetElementImage path to the target element reference image
     * @return self reference for chaining
     */
    public SikuliActions doubleClick(String pathToTargetElementImage) {
        return doubleClick(readImageFromFile(pathToTargetElementImage));
    }

    /**
     * Double-clicks the target image region.
     *
     * @param targetElement target element reference image bytes
     * @return self reference for chaining
     */
    public SikuliActions doubleClick(byte[] targetElement) {
        Pattern element = null;
        String elementText = null;
        try {
            element = prepareElementPattern(targetElement);
            var match = screen.wait(element);
            elementText = match.getText();
            match.doubleClick();
            passAction("doubleClick", formatTextForReport(elementText));
        } catch (IOException | FindFailed rootCauseException) {
            failAction("doubleClick", formatTextForReport(elementText), rootCauseException);
        }
        return this;
    }

    /**
     * Right-clicks the target image region.
     *
     * @param pathToTargetElementImage path to the target element reference image
     * @return self reference for chaining
     */
    public SikuliActions rightClick(String pathToTargetElementImage) {
        return rightClick(readImageFromFile(pathToTargetElementImage));
    }

    /**
     * Right-clicks the target image region.
     *
     * @param targetElement target element reference image bytes
     * @return self reference for chaining
     */
    public SikuliActions rightClick(byte[] targetElement) {
        Pattern element = null;
        String elementText = null;
        try {
            element = prepareElementPattern(targetElement);
            var match = screen.wait(element);
            elementText = match.getText();
            match.rightClick();
            passAction("rightClick", formatTextForReport(elementText));
        } catch (IOException | FindFailed rootCauseException) {
            failAction("rightClick", formatTextForReport(elementText), rootCauseException);
        }
        return this;
    }

    /**
     * Drags one image region and drops it on another image region.
     *
     * @param pathToDraggableElementImage path to the draggable element reference image
     * @param pathToTargetElementImage path to the drop target reference image
     * @return self reference for chaining
     */
    public SikuliActions dragAndDrop(String pathToDraggableElementImage, String pathToTargetElementImage) {
        return dragAndDrop(readImageFromFile(pathToDraggableElementImage), readImageFromFile(pathToTargetElementImage));
    }

    /**
     * Drags one image region and drops it on another image region.
     *
     * @param draggableElement draggable element reference image bytes
     * @param targetElement drop target reference image bytes
     * @return self reference for chaining
     */
    public SikuliActions dragAndDrop(byte[] draggableElement, byte[] targetElement) {
        Pattern draggableElementPattern = null;
        String elementText = null;
        try {
            draggableElementPattern = prepareElementPattern(draggableElement);
            var targetElementPattern = prepareElementPattern(targetElement);
            var match = screen.wait(draggableElementPattern);
            elementText = match.getText();
            match.dragDrop(draggableElementPattern, targetElementPattern);
            passAction("dragAndDrop", formatTextForReport(elementText));
        } catch (IOException | FindFailed rootCauseException) {
            failAction("dragAndDrop", formatTextForReport(elementText), rootCauseException);
        }
        return this;
    }

    private void clearAndType(Pattern element, String text) throws FindFailed {
        String elementText = formatTextForReport(screen.wait(element).getText());
        for (int i = 0; i < elementText.length(); i++) {
            try {
                screen.wait(element).type(element, Key.BACKSPACE);
            } catch (FindFailed findFailed) {
                ReportManagerHelper.logDiscrete(findFailed);
            }
        }
        screen.wait(element).type(text);
    }

    private byte[] readImageFromFile(String pathToTargetElementImage) {
        try {
            return Files.readAllBytes(Path.of(pathToTargetElementImage));
        } catch (IOException rootCauseException) {
            failAction("readImageFromFile", pathToTargetElementImage, rootCauseException);
            return new byte[0];
        }
    }

    private Pattern prepareElementPattern(byte[] targetElement) throws IOException {
        if (applicationWindow != null) {
            applicationWindow.waitForWindow(SHAFT.Properties.timeouts.browserNavigationTimeout());
            applicationWindow.focus();
        }
        var bufferedImage = ImageIO.read(new ByteArrayInputStream(targetElement));
        if (bufferedImage == null) {
            throw new IOException("Target element image bytes could not be decoded.");
        }
        Pattern elementPattern = new Pattern();
        elementPattern.setBImage(bufferedImage);
        return elementPattern;
    }

    private void initializeSikuliEngineForCurrentScreen() {
        Settings.setShowActions(false);
        Settings.ActionLogs = true;
        Settings.InfoLogs = true;
        Settings.DebugLogs = true;
        Settings.LogTime = true;
    }

    private void passAction(String actionName, String testData) {
        ReportManager.log("SikuliX action \"" + actionName + "\" passed. Test data: \"" + formatTextForReport(testData) + "\".");
    }

    private void failAction(String actionName, String testData, Throwable rootCauseException) {
        var message = "SikuliX action \"" + actionName + "\" failed. Test data: \"" + formatTextForReport(testData) + "\".";
        ReportManager.log(message);
        throw new RuntimeException(message, rootCauseException);
    }

    private String formatTextForReport(String text) {
        if (text != null) {
            return text.replace("\n", "").trim();
        }
        return "NULL";
    }
}
