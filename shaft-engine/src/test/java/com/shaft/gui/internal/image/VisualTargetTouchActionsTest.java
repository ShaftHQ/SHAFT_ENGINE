package com.shaft.gui.internal.image;

import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.gui.image.ImageMatch;
import com.shaft.gui.image.ImageMatchingAlgorithm;
import com.shaft.gui.image.ImageMatchingMode;
import com.shaft.gui.image.ImageRectangle;
import com.shaft.gui.image.ImageTarget;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;
import org.openqa.selenium.By;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.RemoteWebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class VisualTargetTouchActionsTest {
    @AfterMethod(alwaysRun = true)
    public void resetProvider() {
        VisualProcessingProviderRegistry.resetProviderForTesting();
    }

    @DataProvider
    public Object[][] directions() {
        return new Object[][]{
                {TouchActions.SwipeDirection.UP},
                {TouchActions.SwipeDirection.DOWN},
                {TouchActions.SwipeDirection.LEFT},
                {TouchActions.SwipeDirection.RIGHT}
        };
    }

    @Test(dataProvider = "directions")
    public void publicImageScrollShouldSearchGestureAndSearchAgainInEveryDirection(
            TouchActions.SwipeDirection direction) throws Exception {
        byte[] screenshot = image(false);
        SequencedProvider provider = new SequencedProvider(2);
        VisualProcessingProviderRegistry.setProviderForTesting(provider);
        AndroidDriver driver = driver();
        doReturn(true).when(driver).executeScript(eq("mobile: scrollGesture"), anyMap());
        TouchActions actions = actions(driver);
        ImageTarget target = ImageTarget.fromBytes(screenshot).matchingMode(ImageMatchingMode.TEMPLATE);

        try (MockedConstruction<ScreenshotManager> ignored = mockConstruction(ScreenshotManager.class,
                (manager, context) -> when(manager.takeViewportScreenshot(driver)).thenReturn(screenshot))) {
            actions.swipeElementIntoView(target, direction);
        }

        Assert.assertEquals(provider.targets.size(), 2);
        ArgumentCaptor<Map<Object, Object>> parameters = ArgumentCaptor.forClass(Map.class);
        verify(driver).executeScript(eq("mobile: scrollGesture"), parameters.capture());
        Assert.assertEquals(parameters.getValue().get("direction"), direction.name());
    }

    @Test
    public void containerShouldIntersectExistingImageRegionInScreenshotPixels() throws Exception {
        byte[] screenshot = image(false);
        SequencedProvider provider = new SequencedProvider(1);
        VisualProcessingProviderRegistry.setProviderForTesting(provider);
        AndroidDriver driver = driver();
        TouchActions actions = actions(driver);
        By container = By.id("container");
        container(actions, driver, container, new Rectangle(10, 20, 40, 30));
        ImageTarget target = ImageTarget.fromBytes(screenshot)
                .matchingMode(ImageMatchingMode.TEMPLATE)
                .within(new ImageRectangle(0, 10, 30, 40));

        try (MockedConstruction<ScreenshotManager> ignored = mockConstruction(ScreenshotManager.class,
                (manager, context) -> when(manager.takeViewportScreenshot(driver)).thenReturn(screenshot))) {
            actions.swipeElementIntoView(container, target, TouchActions.SwipeDirection.DOWN);
        }

        Assert.assertEquals(provider.targets.getFirst().searchRegion().orElseThrow(),
                new ImageRectangle(10, 20, 20, 30));
    }

    @Test
    public void publicIosImageScrollShouldReachTheDocumentedMobileScrollCommand() throws Exception {
        byte[] screenshot = image(false);
        SequencedProvider provider = new SequencedProvider(2);
        VisualProcessingProviderRegistry.setProviderForTesting(provider);
        IOSDriver driver = iosDriver();
        doReturn(null).when(driver).executeScript(eq("mobile: scroll"), anyMap());
        TouchActions actions = actions(driver);
        ImageTarget target = ImageTarget.fromBytes(screenshot).matchingMode(ImageMatchingMode.TEMPLATE);

        try (MockedConstruction<ScreenshotManager> ignored = mockConstruction(ScreenshotManager.class,
                (manager, context) -> when(manager.takeViewportScreenshot(driver)).thenReturn(screenshot))) {
            actions.swipeElementIntoView(target, TouchActions.SwipeDirection.DOWN);
        }

        verify(driver).executeScript("mobile: scroll", Map.of("direction", "down"));
        Assert.assertEquals(provider.targets.size(), 2);
    }

    @Test
    public void containerStableDetectionShouldIgnoreAnimationOutsideContainer() throws Exception {
        byte[] first = image(false);
        byte[] animatedOutside = image(true);
        SequencedProvider provider = new SequencedProvider(Integer.MAX_VALUE);
        VisualProcessingProviderRegistry.setProviderForTesting(provider);
        AndroidDriver driver = driver();
        doReturn(true).when(driver).executeScript(eq("mobile: scrollGesture"), anyMap());
        TouchActions actions = actions(driver);
        By container = By.id("container");
        container(actions, driver, container, new Rectangle(10, 10, 30, 30));
        ImageTarget target = ImageTarget.fromBytes(first).matchingMode(ImageMatchingMode.TEMPLATE);

        try (MockedConstruction<ScreenshotManager> ignored = mockConstruction(ScreenshotManager.class,
                (manager, context) -> when(manager.takeViewportScreenshot(driver))
                        .thenReturn(first, animatedOutside, first, animatedOutside))) {
            actions.swipeElementIntoView(container, target, TouchActions.SwipeDirection.DOWN);
        }

        verify(driver, times(2)).executeScript(eq("mobile: scrollGesture"), anyMap());
        Assert.assertEquals(provider.targets.size(), 3);
    }

    private static TouchActions actions(WebDriver driver) throws Exception {
        TouchActions actions = new TouchActions(driver);
        Field helper = FluentWebDriverAction.class.getDeclaredField("elementActionsHelper");
        helper.setAccessible(true);
        helper.set(actions, mock(ElementActionsHelper.class));
        return actions;
    }

    private static void container(TouchActions actions, AndroidDriver driver, By locator, Rectangle rectangle)
            throws Exception {
        Field helperField = FluentWebDriverAction.class.getDeclaredField("elementActionsHelper");
        helperField.setAccessible(true);
        ElementActionsHelper helper = (ElementActionsHelper) helperField.get(actions);
        RemoteWebElement element = mock(RemoteWebElement.class);
        when(element.getRect()).thenReturn(rectangle);
        when(helper.identifyUniqueElement(driver, locator)).thenReturn(List.of(locator.toString(), element));
    }

    private static AndroidDriver driver() {
        AndroidDriver driver = mock(AndroidDriver.class);
        WebDriver.Options options = mock(WebDriver.Options.class);
        WebDriver.Window window = mock(WebDriver.Window.class);
        when(driver.manage()).thenReturn(options);
        when(options.window()).thenReturn(window);
        when(window.getSize()).thenReturn(new Dimension(100, 100));
        return driver;
    }

    private static IOSDriver iosDriver() {
        IOSDriver driver = mock(IOSDriver.class);
        WebDriver.Options options = mock(WebDriver.Options.class);
        WebDriver.Window window = mock(WebDriver.Window.class);
        when(driver.manage()).thenReturn(options);
        when(options.window()).thenReturn(window);
        when(window.getSize()).thenReturn(new Dimension(100, 100));
        return driver;
    }

    private static byte[] image(boolean animateOutsideContainer) throws Exception {
        BufferedImage image = new BufferedImage(100, 100, BufferedImage.TYPE_INT_RGB);
        java.awt.Graphics2D graphics = image.createGraphics();
        try {
            graphics.setColor(Color.WHITE);
            graphics.fillRect(0, 0, 100, 100);
            graphics.setColor(Color.BLACK);
            graphics.fillRect(10, 10, 30, 30);
            if (animateOutsideContainer) {
                graphics.setColor(Color.RED);
                graphics.fillRect(80, 80, 10, 10);
            }
        } finally {
            graphics.dispose();
        }
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        ImageIO.write(image, "png", output);
        return output.toByteArray();
    }

    private static final class SequencedProvider implements VisualProcessingProvider {
        private final int matchOnCall;
        private final List<ImageTarget> targets = new ArrayList<>();

        private SequencedProvider(int matchOnCall) {
            this.matchOnCall = matchOnCall;
        }

        @Override
        public List<ImageMatch> findImageMatches(ImageTarget target, byte[] currentPageScreenshot) {
            targets.add(target);
            if (targets.size() != matchOnCall) {
                return List.of();
            }
            return List.of(new ImageMatch(new ImageRectangle(20, 20, 10, 10), 0.99, 1,
                    ImageMatchingAlgorithm.TEMPLATE_COLOR, Map.of("fixture", "sequenced")));
        }

        @Override
        public List<Integer> findImageWithinCurrentPage(String referenceImagePath, byte[] currentPageScreenshot) {
            return List.of();
        }

        @Override
        public Boolean compareAgainstBaseline(WebDriver driver, By elementLocator, byte[] elementScreenshot,
                                              ImageProcessingActions.VisualValidationEngine visualValidationEngine,
                                              String referenceImagePath, String differencesImagePath) {
            return true;
        }

        @Override
        public void load() {
        }
    }
}
