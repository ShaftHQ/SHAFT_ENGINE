package com.shaft.sikulix;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.SikuliActions;
import com.shaft.properties.internal.Properties;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import java.awt.GraphicsEnvironment;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.event.InputEvent;
import java.awt.image.BufferedImage;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

public class SikuliWindowsDesktopE2ETest {
    private final AtomicReference<JFrame> frame = new AtomicReference<>();

    @AfterMethod(alwaysRun = true)
    public void tearDown() throws Exception {
        JFrame openFrame = frame.getAndSet(null);
        if (openFrame != null) {
            SwingUtilities.invokeAndWait(openFrame::dispose);
        }
        Properties.clearForCurrentThread();
    }

    @Test
    public void shouldClickDesktopButtonUsingSikuliImageMatching() throws Exception {
        skipUnlessWindowsDesktopE2EIsEnabled();
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(5);

        AtomicBoolean clicked = new AtomicBoolean(false);
        AtomicReference<JButton> button = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            JFrame testFrame = new JFrame("SHAFT SikuliX E2E");
            JButton targetButton = new JButton("SHAFT SikuliX Target");
            targetButton.addActionListener(event -> clicked.set(true));
            testFrame.add(targetButton);
            testFrame.setAlwaysOnTop(true);
            testFrame.setBounds(240, 180, 320, 140);
            testFrame.setVisible(true);
            frame.set(testFrame);
            button.set(targetButton);
        });

        Robot robot = new Robot();
        robot.waitForIdle();
        TimeUnit.MILLISECONDS.sleep(500);
        Path targetImage = captureButtonReferenceImage(robot, button.get());

        new SikuliActions().click(targetImage.toString());

        robot.mouseRelease(InputEvent.BUTTON1_DOWN_MASK);
        SHAFT.Validations.assertThat().object(clicked.get()).isEqualTo(true).perform();
    }

    private static void skipUnlessWindowsDesktopE2EIsEnabled() {
        if (!Boolean.getBoolean("runWindowsDesktopE2E")) {
            throw new SkipException("Windows desktop E2E is disabled. Set -DrunWindowsDesktopE2E=true to run it.");
        }
        if (!System.getProperty("os.name", "").toLowerCase().contains("windows")) {
            throw new SkipException("SikuliX desktop E2E requires Windows.");
        }
        if (GraphicsEnvironment.isHeadless()) {
            throw new SkipException("SikuliX desktop E2E requires a graphical desktop.");
        }
    }

    private static Path captureButtonReferenceImage(Robot robot, JButton button) throws Exception {
        Rectangle bounds = button.getBounds();
        var location = button.getLocationOnScreen();
        Rectangle screenBounds = new Rectangle(location.x, location.y, bounds.width, bounds.height);
        BufferedImage image = robot.createScreenCapture(screenBounds);
        Path targetDirectory = Path.of("target", "sikulix-e2e");
        Files.createDirectories(targetDirectory);
        Path targetImage = targetDirectory.resolve("button.png");
        ImageIO.write(image, "png", targetImage.toFile());
        return targetImage;
    }
}
