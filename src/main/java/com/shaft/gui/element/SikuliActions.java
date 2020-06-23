package com.shaft.gui.element;

import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.gui.video.RecordManager;
import com.shaft.tools.io.ReportManager;
import org.apache.commons.io.IOUtils;
import org.sikuli.basics.Settings;
import org.sikuli.script.*;

import javax.imageio.ImageIO;
import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.List;

@SuppressWarnings("unused")
public class SikuliActions {
    private Screen screen;
    private App applicationWindow;

    public SikuliActions() {
        initializeSikuliEngineForCurrentScreen();
    }

    public SikuliActions(App applicationWindow) {
        initializeSikuliEngineForCurrentScreen();
        this.applicationWindow = applicationWindow;
    }

    protected static List<Object> prepareElementScreenshotAttachment(Screen screen, App applicationWindow, Pattern element, String actionName, boolean passFailStatus) {
        return ScreenshotManager.captureScreenShotUsingSikuliX(screen, applicationWindow, element, actionName, passFailStatus);
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element.
     *
     * @param pathToTargetElementImage relative path to the desired element image following this example "src/test/resources/DynamicObjectRepository/" + "sikuli_googleHome_searchBox_text.PNG"
     * @param text                     the target text that needs to be typed into the target
     *                                 element
     * @return a self-reference to be used to chain actions
     */
    public SikuliActions type(String pathToTargetElementImage, String text) {
        return type(readImageFromFile(pathToTargetElementImage), text);
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element.
     *
     * @param targetElement the image of the desired element in the form of a byte[]
     * @param text          the target text that needs to be typed into the target
     *                      element
     * @return a self-reference to be used to chain actions
     */
    public SikuliActions type(byte[] targetElement, String text) {
        Pattern element = null;
        try {
            element = prepareElementPattern(targetElement);
            clearAndType(element, text);
        } catch (IOException | FindFailed rootCauseException) {
            ElementActions.failAction(screen, applicationWindow, element, formatTextForReport(text), rootCauseException);
        }
        ElementActions.passAction(screen, applicationWindow, element, formatTextForReport(text));
        return this;
    }

    /**
     * Types the required string into the target element.
     *
     * @param pathToTargetElementImage relative path to the desired element image following this example "src/test/resources/DynamicObjectRepository/" + "sikuli_googleHome_searchBox_text.PNG"
     * @param text                     the target text that needs to be typed into the target
     *                                 element
     * @return a self-reference to be used to chain actions
     */
    public SikuliActions typeAppend(String pathToTargetElementImage, String text) {
        return typeAppend(readImageFromFile(pathToTargetElementImage), text);
    }

    /**
     * Types the required string into the target element.
     *
     * @param targetElement the image of the desired element in the form of a byte[]
     * @param text          the target text that needs to be typed into the target
     *                      element
     * @return a self-reference to be used to chain actions
     */
    public SikuliActions typeAppend(byte[] targetElement, String text) {
        Pattern element = null;
        try {
            element = prepareElementPattern(targetElement);
            screen.wait(element).type(text);
        } catch (IOException | FindFailed rootCauseException) {
            ElementActions.failAction(screen, applicationWindow, element, formatTextForReport(text), rootCauseException);
        }
        ElementActions.passAction(screen, applicationWindow, element, formatTextForReport(text));
        return this;
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element.
     *
     * @param pathToTargetElementImage relative path to the desired element image following this example "src/test/resources/DynamicObjectRepository/" + "sikuli_googleHome_searchBox_text.PNG"
     * @param text                     the target text that needs to be typed into the target
     *                                 element
     * @return a self-reference to be used to chain actions
     */
    public SikuliActions typeSecure(String pathToTargetElementImage, String text) {
        return typeSecure(readImageFromFile(pathToTargetElementImage), text);
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element.
     *
     * @param targetElement the image of the desired element in the form of a byte[]
     * @param text          the target text that needs to be typed into the target
     *                      element
     * @return a self-reference to be used to chain actions
     */
    @SuppressWarnings("SuspiciousRegexArgument")
    public SikuliActions typeSecure(byte[] targetElement, String text) {
        Pattern element = null;
        try {
            element = prepareElementPattern(targetElement);
            clearAndType(element, text);
        } catch (IOException | FindFailed rootCauseException) {
            ElementActions.failAction(screen, applicationWindow, element, formatTextForReport(text), rootCauseException);
        }
        ElementActions.passAction(screen, applicationWindow, element, formatTextForReport(text).replaceAll(".", "•"));
        return this;
    }

    /**
     * Clicks on a certain element using SikuliX
     *
     * @param pathToTargetElementImage relative path to the desired element image following this example "src/test/resources/DynamicObjectRepository/" + "sikuli_googleHome_searchBox_text.PNG"
     * @return a self-reference to be used to chain actions
     */
    public SikuliActions click(String pathToTargetElementImage) {
        return click(readImageFromFile(pathToTargetElementImage));
    }

    /**
     * Clicks on a certain element using SikuliX
     *
     * @param targetElement the image of the desired element in the form of a byte[]
     * @return a self-reference to be used to chain actions
     */
    public SikuliActions click(byte[] targetElement) {
        Pattern element = null;
        String elementText = null;
        try {
            element = prepareElementPattern(targetElement);
            elementText = screen.wait(element).getText();
            screen.wait(element).click();
        } catch (IOException | FindFailed rootCauseException) {
            ElementActions.failAction(screen, applicationWindow, element, elementText, rootCauseException);
        }
        ElementActions.passAction(screen, applicationWindow, element, formatTextForReport(elementText));
        return this;
    }

    /**
     * Retrieves text from the target element and returns it as a string value.
     *
     * @param pathToTargetElementImage relative path to the desired element image following this example "src/test/resources/DynamicObjectRepository/" + "sikuli_googleHome_searchBox_text.PNG"
     * @return the text value of the target element
     */
    public String getText(String pathToTargetElementImage) {
        return getText(readImageFromFile(pathToTargetElementImage));
    }

    /**
     * Retrieves text from the target element and returns it as a string value.
     *
     * @param targetElement the image of the desired element in the form of a byte[]
     * @return the text value of the target element
     */
    public String getText(byte[] targetElement) {
        Pattern element = null;
        String elementText = null;
        try {
            element = prepareElementPattern(targetElement);
            elementText = screen.wait(element).getText().replace("\n", "").trim();
        } catch (IOException | FindFailed rootCauseException) {
            ElementActions.failAction(screen, applicationWindow, element, null, rootCauseException);
        }
        ElementActions.passAction(screen, applicationWindow, element, formatTextForReport(elementText));
        return elementText;
    }

    /**
     * Hovers over target element.
     *
     * @param pathToTargetElementImage relative path to the desired element image following this example "src/test/resources/DynamicObjectRepository/" + "sikuli_googleHome_searchBox_text.PNG"
     * @return a self-reference to be used to chain actions
     */
    public SikuliActions hover(String pathToTargetElementImage) {
        return hover(readImageFromFile(pathToTargetElementImage));
    }

    /**
     * Hovers over target element.
     *
     * @param targetElement the image of the desired element in the form of a byte[]
     * @return a self-reference to be used to chain actions
     */
    public SikuliActions hover(byte[] targetElement) {
        Pattern element = null;
        String elementText = null;
        try {
            element = prepareElementPattern(targetElement);
            elementText = screen.wait(element).getText().replace("\n", "").trim();
            screen.wait(element).hover(element);
        } catch (IOException | FindFailed rootCauseException) {
            ElementActions.failAction(screen, applicationWindow, element, elementText, rootCauseException);
        }
        ElementActions.passAction(screen, applicationWindow, element, formatTextForReport(elementText));
        return this;
    }

    /**
     * Double-clicks on an element using SikuliX
     *
     * @param pathToTargetElementImage relative path to the desired element image following this example "src/test/resources/DynamicObjectRepository/" + "sikuli_googleHome_searchBox_text.PNG"
     * @return a self-reference to be used to chain actions
     */
    public SikuliActions doubleClick(String pathToTargetElementImage) {
        return doubleClick(readImageFromFile(pathToTargetElementImage));
    }

    /**
     * Double-clicks on an element using SikuliX
     *
     * @param targetElement the image of the desired element in the form of a byte[]
     * @return a self-reference to be used to chain actions
     */
    public SikuliActions doubleClick(byte[] targetElement) {
        Pattern element = null;
        String elementText = null;
        try {
            element = prepareElementPattern(targetElement);
            elementText = screen.wait(element).getText().replace("\n", "").trim();
            screen.wait(element).doubleClick(element);
        } catch (IOException | FindFailed rootCauseException) {
            ElementActions.failAction(screen, applicationWindow, element, elementText, rootCauseException);
        }
        ElementActions.passAction(screen, applicationWindow, element, formatTextForReport(elementText));
        return this;
    }

    /**
     * Right-clicks on an element to trigger the context menu
     *
     * @param pathToTargetElementImage relative path to the desired element image following this example "src/test/resources/DynamicObjectRepository/" + "sikuli_googleHome_searchBox_text.PNG"
     * @return a self-reference to be used to chain actions
     */
    public SikuliActions rightClick(String pathToTargetElementImage) {
        return rightClick(readImageFromFile(pathToTargetElementImage));
    }

    /**
     * Right-clicks on an element to trigger the context menu
     *
     * @param targetElement the image of the desired element in the form of a byte[]
     * @return a self-reference to be used to chain actions
     */
    public SikuliActions rightClick(byte[] targetElement) {
        Pattern element = null;
        String elementText = null;
        try {
            element = prepareElementPattern(targetElement);
            elementText = screen.wait(element).getText().replace("\n", "").trim();
            screen.wait(element).rightClick(element);
        } catch (IOException | FindFailed rootCauseException) {
            ElementActions.failAction(screen, applicationWindow, element, elementText, rootCauseException);
        }
        ElementActions.passAction(screen, applicationWindow, element, formatTextForReport(elementText));
        return this;
    }

    /**
     * Drags the draggable element and drops it onto the target element
     *
     * @param pathToDraggableElementImage relative path to the desired element image following this example "src/test/resources/DynamicObjectRepository/" + "sikuli_googleHome_searchBox_text.PNG"
     * @param pathToTargetElementImage    relative path to the desired element image following this example "src/test/resources/DynamicObjectRepository/" + "sikuli_googleHome_searchBox_text.PNG"
     * @return a self-reference to be used to chain actions
     */
    public SikuliActions dragAndDrop(String pathToDraggableElementImage, String pathToTargetElementImage) {
        return dragAndDrop(readImageFromFile(pathToDraggableElementImage), readImageFromFile(pathToTargetElementImage));
    }

    /**
     * Drags the draggable element and drops it onto the target element
     *
     * @param draggableElement the image of the desired element in the form of a byte[]
     * @param targetElement    the image of the desired element in the form of a byte[]
     * @return a self-reference to be used to chain actions
     */
    public SikuliActions dragAndDrop(byte[] draggableElement, byte[] targetElement) {
        Pattern draggableElementPattern = null;
        Pattern targetElementPattern;
        String elementText = null;
        try {
            draggableElementPattern = prepareElementPattern(draggableElement);
            targetElementPattern = prepareElementPattern(targetElement);
            elementText = screen.wait(draggableElementPattern).getText().replace("\n", "").trim();
            screen.wait(draggableElementPattern).dragDrop(draggableElementPattern, targetElementPattern);
        } catch (IOException | FindFailed rootCauseException) {
            ElementActions.failAction(screen, applicationWindow, draggableElementPattern, elementText, rootCauseException);
        }
        ElementActions.passAction(screen, applicationWindow, draggableElementPattern, elementText);
        return this;
    }

    private void clearAndType(Pattern element, String text) throws FindFailed {
        String elementText = screen.wait(element).getText().replace("\n", "").trim();
        if (!elementText.isEmpty()) {
            //clear
            Collections.singletonList(elementText.toCharArray()).forEach(character -> {
                try {
                    screen.wait(element).type(element, Key.BACKSPACE);
                } catch (FindFailed findFailed) {
                    ReportManager.log(findFailed);
                }
            });
        }
        screen.wait(element).type(text);
    }

    private byte[] readImageFromFile(String pathToTargetElementImage) {
        try {
            return IOUtils.toByteArray(new FileInputStream(pathToTargetElementImage));
        } catch (IOException rootCauseException) {
            ElementActions.failAction(null, "Failed to initialize SikuliAction; couldn't read the target Element Image", null, rootCauseException);
            return new byte[0];
        }
    }

    private Pattern prepareElementPattern(byte[] targetElement) throws IOException {
        if (applicationWindow != null) {
            applicationWindow.waitForWindow(Integer.parseInt(System.getProperty("browserNavigationTimeout")));
            applicationWindow.focus();
        }
        Pattern elementPattern = new Pattern();
        ByteArrayInputStream targetElementImage = new ByteArrayInputStream(targetElement);
        elementPattern.setBImage(ImageIO.read(targetElementImage));
        return elementPattern;
    }

    private void initializeSikuliEngineForCurrentScreen() {
        if (BrowserFactory.isWebExecution()) {
            JavaScriptWaitManager.waitForLazyLoading();
        }
        Settings.setShowActions(false);
        Settings.ActionLogs = true;
        Settings.InfoLogs = true;
        Settings.DebugLogs = true;
        Settings.LogTime = true;
        screen = new Screen();
        screen.setAutoWaitTimeout(Double.parseDouble(System.getProperty("defaultElementIdentificationTimeout")));
        RecordManager.startVideoRecording();
    }

    private String formatTextForReport(String text) {
        if (text != null) {
            return text.replace("\n", "").trim();
        } else {
            return "NULL";
        }
    }

}
