package com.shaft.gui.internal.image;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.tools.io.internal.ReportSecrets;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;

/**
 * Keeps typed secrets out of the screenshots SHAFT publishes (#7175).
 *
 * <p>An element that received a secret (typed with {@code typeSecure}, or text holding a registered
 * secret value or a JWT-shaped token) is registered for the current thread. Every evidence capture
 * then hides that element's text while the element is on the page: on the web its text is made
 * transparent for the capture and restored afterwards; on native mobile the element's rectangle is
 * painted black, and the capture is dropped when the field is scrollable or its text can wrap outside
 * that rectangle (#7282); in Playwright the locator is passed as a screenshot mask. When hiding fails while
 * a secret element is still live, the capture is dropped rather than published.
 */
public final class SecretScreenMask {
    private static final String MASK_SCRIPT = """
            const saved = [];
            for (const e of arguments[0]) {
              saved.push(e.getAttribute('style'));
              for (const p of ['color', '-webkit-text-fill-color', 'caret-color']) {
                e.style.setProperty(p, 'transparent', 'important');
              }
              e.style.setProperty('text-shadow', 'none', 'important');
            }
            return saved;""";
    private static final String RESTORE_SCRIPT = """
            arguments[0].forEach((e, i) => arguments[1][i] === null
                ? e.removeAttribute('style') : e.setAttribute('style', arguments[1][i]));""";
    private static final ThreadLocal<List<Secret>> SECRET_ELEMENTS = ThreadLocal.withInitial(ArrayList::new);
    private static final ThreadLocal<List<Locator>> SECRET_LOCATORS = ThreadLocal.withInitial(ArrayList::new);
    private static final ThreadLocal<int[]> DEPTH = ThreadLocal.withInitial(() -> new int[1]);
    private static final ThreadLocal<Boolean> VIDEO_WARNED = ThreadLocal.withInitial(() -> Boolean.FALSE);

    private record Secret(WebDriver driver, WebElement element) {
    }

    private SecretScreenMask() {
    }

    /**
     * Tells whether typed text is secret: a registered secret value or a JWT-shaped token.
     *
     * @param text the typed text
     * @return {@code true} when the text must stay out of screenshots
     */
    public static boolean isSecret(CharSequence... text) {
        if (text == null) {
            return false;
        }
        StringBuilder joined = new StringBuilder();
        for (CharSequence part : text) {
            joined.append(part == null ? "" : part);
        }
        return ReportSecrets.containsSecret(joined.toString());
    }

    /**
     * Registers a WebDriver element that received a secret.
     *
     * @param driver  the driver session the element belongs to
     * @param element the element whose text must be hidden in later screenshots of that session
     */
    public static void register(WebDriver driver, WebElement element) {
        if (driver == null || element == null) {
            return;
        }
        Secret secret = new Secret(unwrap(driver), element);
        if (!SECRET_ELEMENTS.get().contains(secret)) {
            SECRET_ELEMENTS.get().add(secret);
        }
        warnIfRecordingVideo();
    }

    /**
     * Forgets the secret elements of a driver session, for example when it closes.
     *
     * @param driver the closing driver
     */
    public static void forget(WebDriver driver) {
        if (driver != null) {
            WebDriver session = unwrap(driver);
            SECRET_ELEMENTS.get().removeIf(secret -> Objects.equals(secret.driver(), session));
        }
    }

    /**
     * Registers a Playwright locator that received a secret.
     *
     * @param locator the locator to mask in later screenshots
     */
    public static void register(Locator locator) {
        if (locator != null && !SECRET_LOCATORS.get().contains(locator)) {
            SECRET_LOCATORS.get().add(locator);
        }
        warnIfRecordingVideo();
    }

    /**
     * Returns the Playwright locators of this page to pass as a screenshot mask.
     *
     * @param page the page about to be captured
     * @return the registered locators on that page; empty when none
     */
    public static List<Locator> playwrightMask(Page page) {
        List<Locator> mask = new ArrayList<>();
        for (Locator locator : SECRET_LOCATORS.get()) {
            if (Objects.equals(locator.page(), page)) {
                mask.add(locator);
            }
        }
        return mask;
    }

    /** Forgets every registered element and locator for the current thread. */
    public static void clear() {
        SECRET_ELEMENTS.get().clear();
        SECRET_LOCATORS.get().clear();
        VIDEO_WARNED.set(Boolean.FALSE);
    }

    /**
     * Runs a viewport or full-page capture with every live secret element hidden.
     *
     * @param driver  the driver that takes the screenshot
     * @param capture the capture to run
     * @return the captured image, or an empty array when a live secret could not be hidden
     */
    public static byte[] masked(WebDriver driver, Supplier<byte[]> capture) {
        return masked(driver, capture, true);
    }

    /**
     * Runs an element capture with every live secret element hidden.
     *
     * @param driver  the driver that takes the screenshot
     * @param capture the capture to run
     * @return the captured image, or an empty array when a live secret could not be hidden
     */
    public static byte[] maskedElement(WebDriver driver, Supplier<byte[]> capture) {
        return masked(driver, capture, false);
    }

    private static byte[] masked(WebDriver driver, Supplier<byte[]> capture, boolean viewportCoordinates) {
        int[] depth = DEPTH.get();
        if (depth[0] > 0 || driver == null) {
            return capture.get();
        }
        List<WebElement> elements = live(unwrap(driver));
        if (elements.isEmpty()) {
            return capture.get();
        }
        depth[0]++;
        try {
            if (!DriverFactoryHelper.isMobileNativeExecution() && driver instanceof JavascriptExecutor js) {
                return hideWithScript(js, elements, capture);
            }
            return viewportCoordinates ? paintOver(driver, elements, capture.get()) : new byte[0];
        } finally {
            depth[0]--;
        }
    }

    private static List<WebElement> live(WebDriver session) {
        List<Secret> secrets = SECRET_ELEMENTS.get();
        secrets.removeIf(secret -> Objects.equals(secret.driver(), session) && isStale(secret.element()));
        List<WebElement> elements = new ArrayList<>();
        for (Secret secret : secrets) {
            if (Objects.equals(secret.driver(), session)) {
                elements.add(secret.element());
            }
        }
        return elements;
    }

    private static WebDriver unwrap(WebDriver driver) {
        WebDriver current = driver;
        for (int hops = 0; hops < 5; hops++) {
            if (current instanceof com.epam.healenium.SelfHealingDriver healing) {
                current = healing.getDelegate();
            } else if (current instanceof org.openqa.selenium.WrapsDriver wraps && wraps.getWrappedDriver() != null
                    && !Objects.equals(wraps.getWrappedDriver(), current)) {
                current = wraps.getWrappedDriver();
            } else {
                break;
            }
        }
        return current;
    }

    private static boolean isStale(WebElement element) {
        try {
            element.isEnabled();
            return false;
        } catch (StaleElementReferenceException | org.openqa.selenium.NoSuchSessionException e) {
            return true;
        } catch (RuntimeException e) {
            return false;
        }
    }

    private static byte[] hideWithScript(JavascriptExecutor js, List<WebElement> elements, Supplier<byte[]> capture) {
        Object saved;
        try {
            saved = js.executeScript(MASK_SCRIPT, elements);
        } catch (RuntimeException e) {
            return dropped(e);
        }
        try {
            return capture.get();
        } finally {
            try {
                js.executeScript(RESTORE_SCRIPT, elements, saved);
            } catch (RuntimeException e) {
                ReportManagerHelper.logDiscrete("Could not restore the style of a masked secret element: " + e.getMessage(), Level.WARN);
            }
        }
    }

    private static byte[] paintOver(WebDriver driver, List<WebElement> elements, byte[] image) {
        if (image == null || image.length == 0) {
            return image;
        }
        try {
            for (WebElement element : elements) {
                if (mayDrawOutsideRect(element)) {
                    return dropped(new IllegalStateException("multi-line or scrollable secret field"));
                }
            }
            BufferedImage picture = ImageIO.read(new ByteArrayInputStream(image));
            Dimension window = driver.manage().window().getSize();
            double scale = window.getWidth() > 0 ? (double) picture.getWidth() / window.getWidth() : 1.0;
            Graphics2D graphics = picture.createGraphics();
            graphics.setColor(Color.BLACK);
            for (WebElement element : elements) {
                Rectangle rect = element.getRect();
                graphics.fillRect((int) Math.floor(rect.getX() * scale), (int) Math.floor(rect.getY() * scale),
                        (int) Math.ceil(rect.getWidth() * scale), (int) Math.ceil(rect.getHeight() * scale));
            }
            graphics.dispose();
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            ImageIO.write(picture, "png", out);
            return out.toByteArray();
        } catch (IOException | RuntimeException e) {
            return dropped(e);
        }
    }

    /** Text longer than this may wrap or scroll past the rectangle a native driver reports (#7282). */
    static final int MAX_SINGLE_LINE_SECRET = 64;

    /**
     * Tells whether a native secret field may draw its text outside its reported rectangle: it is
     * scrollable, multi-line, or holds text long enough to wrap. Such captures fail closed (#7282).
     */
    static boolean mayDrawOutsideRect(WebElement element) {
        String scrollable = element.getAttribute("scrollable");
        String text = element.getText();
        return mayDrawOutsideRect(scrollable, text);
    }

    static boolean mayDrawOutsideRect(String scrollable, String text) {
        if ("true".equalsIgnoreCase(scrollable)) {
            return true;
        }
        return text != null && (text.indexOf('\n') >= 0 || text.length() > MAX_SINGLE_LINE_SECRET);
    }

    private static byte[] dropped(Exception cause) {
        ReportManagerHelper.logDiscrete("Dropped a screenshot because a typed secret could not be hidden in it: "
                + cause.getClass().getSimpleName(), Level.WARN);
        return new byte[0];
    }

    private static void warnIfRecordingVideo() {
        if (Boolean.FALSE.equals(VIDEO_WARNED.get()) && SHAFT.Properties.visuals.videoParamsRecordVideo()) {
            VIDEO_WARNED.set(Boolean.TRUE);
            ReportManagerHelper.logDiscrete("A secret was typed while videoParams_recordVideo is on. Screenshots hide it, "
                    + "but the video can show it: hold the video back, or turn video recording off for this test.", Level.WARN);
        }
    }
}
