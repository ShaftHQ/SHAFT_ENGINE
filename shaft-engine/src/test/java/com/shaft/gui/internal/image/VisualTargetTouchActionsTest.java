package com.shaft.gui.internal.image;

import com.shaft.gui.element.TouchActions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.gui.image.ImageMatch;
import com.shaft.gui.internal.ocr.OcrProcessingActions;
import com.shaft.gui.ocr.OcrMatch;
import com.shaft.gui.ocr.OcrRectangle;
import com.shaft.gui.ocr.OcrTarget;
import com.shaft.gui.image.ImageMatchingAlgorithm;
import com.shaft.gui.image.ImageMatchingMode;
import com.shaft.gui.image.ImageRectangle;
import com.shaft.gui.image.ImageTarget;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.openqa.selenium.By;
import org.openqa.selenium.OutputType;
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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
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
        TestTouchActions actions = actions(driver);
        ImageTarget target = ImageTarget.fromBytes(screenshot).matchingMode(ImageMatchingMode.TEMPLATE);

        try (MockedConstruction<ScreenshotManager> ignored = mockConstruction(ScreenshotManager.class,
                (manager, context) -> when(manager.takeViewportScreenshot(driver)).thenReturn(screenshot))) {
            actions.swipeElementIntoView(target, direction);
        }

        Assert.assertEquals(provider.targets.size(), 2);
        ArgumentCaptor<Map<Object, Object>> parameters = ArgumentCaptor.forClass(Map.class);
        verify(driver).executeScript(eq("mobile: scrollGesture"), parameters.capture());
        Assert.assertEquals(parameters.getValue().get("direction"), direction.name().toLowerCase());
    }

    @Test
    public void containerHorizontalScrollGeometryShouldMirrorLeftAndRight() throws Exception {
        byte[] screenshot = image(false);
        AndroidDriver driver = driver();
        doReturn(true).when(driver).executeScript(eq("mobile: scrollGesture"), anyMap());
        TestTouchActions actions = actions(driver);
        By container = By.id("tabs");
        // Selenium Rectangle(x, y, height, width)
        Rectangle bounds = new Rectangle(10, 20, 40, 100);
        container(actions, driver, container, bounds);
        ImageTarget target = ImageTarget.fromBytes(screenshot).matchingMode(ImageMatchingMode.TEMPLATE);

        Map<Object, Object> right;
        Map<Object, Object> left;
        try (MockedConstruction<ScreenshotManager> ignored = mockConstruction(ScreenshotManager.class,
                (manager, context) -> when(manager.takeViewportScreenshot(driver)).thenReturn(screenshot))) {
            VisualProcessingProviderRegistry.setProviderForTesting(new SequencedProvider(2));
            actions.swipeElementIntoView(container, target, TouchActions.SwipeDirection.RIGHT);
            ArgumentCaptor<Map<Object, Object>> rightCap = ArgumentCaptor.forClass(Map.class);
            verify(driver).executeScript(eq("mobile: scrollGesture"), rightCap.capture());
            right = rightCap.getValue();

            VisualProcessingProviderRegistry.setProviderForTesting(new SequencedProvider(2));
            actions.swipeElementIntoView(container, target, TouchActions.SwipeDirection.LEFT);
            ArgumentCaptor<Map<Object, Object>> leftCap = ArgumentCaptor.forClass(Map.class);
            verify(driver, times(2)).executeScript(eq("mobile: scrollGesture"), leftCap.capture());
            left = leftCap.getAllValues().get(1);
        }

        Assert.assertEquals(right.get("direction"), "right");
        Assert.assertEquals(left.get("direction"), "left");
        Assert.assertEquals(right.get("elementId"), "container-element");
        Assert.assertEquals(left.get("elementId"), "container-element");
        Assert.assertEquals(right.get("width"), bounds.getWidth() * 70 / 100);
        Assert.assertEquals(left.get("width"), bounds.getWidth() * 70 / 100);
        Assert.assertEquals(right.get("left"), bounds.getX());
        Assert.assertEquals(left.get("left"), bounds.getX() + (bounds.getWidth() * 30 / 100));
    }

    @Test
    public void containerShouldIntersectExistingImageRegionInScreenshotPixels() throws Exception {
        byte[] screenshot = image(false);
        SequencedProvider provider = new SequencedProvider(1);
        VisualProcessingProviderRegistry.setProviderForTesting(provider);
        AndroidDriver driver = driver();
        TestTouchActions actions = actions(driver);
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
    public void appiumImageTapShouldScaleScreenshotPixelsToWindowPoints() throws Exception {
        byte[] screenshot = image(false);
        SequencedProvider provider = new SequencedProvider(1);
        VisualProcessingProviderRegistry.setProviderForTesting(provider);
        IOSDriver driver = iosDriver();
        when(driver.manage().window().getSize()).thenReturn(new Dimension(50, 50));
        TestTouchActions actions = actions(driver);
        ImageTarget target = ImageTarget.fromBytes(screenshot).matchingMode(ImageMatchingMode.TEMPLATE);

        try (MockedConstruction<ScreenshotManager> ignored = mockConstruction(ScreenshotManager.class,
                (manager, context) -> when(manager.takeViewportScreenshot(driver)).thenReturn(screenshot))) {
            actions.tap(target);
        }

        @SuppressWarnings("unchecked")
        ArgumentCaptor<java.util.Collection<org.openqa.selenium.interactions.Sequence>> sequences =
                ArgumentCaptor.forClass(java.util.Collection.class);
        verify(driver).perform(sequences.capture());
        Map<String, Object> move = ((List<Map<String, Object>>) sequences.getValue().iterator().next()
                .encode().get("actions")).getFirst();
        Assert.assertEquals(((Number) move.get("x")).intValue(), 13);
        Assert.assertEquals(((Number) move.get("y")).intValue(), 13);
    }

    @Test
    public void publicIosImageScrollShouldReachTheDocumentedMobileScrollCommand() throws Exception {
        byte[] screenshot = image(false);
        SequencedProvider provider = new SequencedProvider(2);
        VisualProcessingProviderRegistry.setProviderForTesting(provider);
        IOSDriver driver = iosDriver();
        doReturn(null).when(driver).executeScript(eq("mobile: scroll"), anyMap());
        TestTouchActions actions = actions(driver);
        ImageTarget target = ImageTarget.fromBytes(screenshot).matchingMode(ImageMatchingMode.TEMPLATE);

        try (MockedConstruction<ScreenshotManager> ignored = mockConstruction(ScreenshotManager.class,
                (manager, context) -> when(manager.takeViewportScreenshot(driver)).thenReturn(screenshot))) {
            actions.swipeElementIntoView(target, TouchActions.SwipeDirection.DOWN);
        }

        verify(driver).executeScript("mobile: scroll", Map.of("direction", "down"));
        Assert.assertEquals(provider.targets.size(), 2);
    }

    @Test
    public void containerSwipeShouldPreferElementScreenshotWithoutViewportScaleRegion() throws Exception {
        byte[] viewport = image(false);
        byte[] containerShot = image(false);
        SequencedProvider provider = new SequencedProvider(1);
        VisualProcessingProviderRegistry.setProviderForTesting(provider);
        AndroidDriver driver = driver();
        TestTouchActions actions = actions(driver);
        By container = By.id("container");
        ElementActionsHelper helper = actions.helper();
        RemoteWebElement element = mock(RemoteWebElement.class);
        when(element.getRect()).thenReturn(new Rectangle(10, 10, 30, 30));
        when(element.getScreenshotAs(OutputType.BYTES)).thenReturn(containerShot);
        when(helper.identifyUniqueElement(driver, container)).thenReturn(List.of(container.toString(), element));
        ImageTarget target = ImageTarget.fromBytes(viewport).matchingMode(ImageMatchingMode.TEMPLATE);

        try (MockedConstruction<ScreenshotManager> ignored = mockConstruction(ScreenshotManager.class,
                (manager, context) -> when(manager.takeViewportScreenshot(driver)).thenReturn(viewport))) {
            actions.swipeElementIntoView(container, target, TouchActions.SwipeDirection.DOWN);
        }

        // Element-local search must not apply a viewport-scaled within() region.
        Assert.assertTrue(provider.targets.getFirst().searchRegion().isEmpty());
        verify(element).getScreenshotAs(OutputType.BYTES);
    }

    @Test
    public void containerStableDetectionShouldIgnoreAnimationOutsideContainer() throws Exception {
        byte[] first = image(false);
        byte[] animatedOutside = image(true);
        SequencedProvider provider = new SequencedProvider(Integer.MAX_VALUE);
        VisualProcessingProviderRegistry.setProviderForTesting(provider);
        AndroidDriver driver = driver();
        doReturn(true).when(driver).executeScript(eq("mobile: scrollGesture"), anyMap());
        TestTouchActions actions = actions(driver);
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

    @Test
    public void ocrSwipeShouldKeepGesturingAfterAppiumReportsNoMoreScrollOnMiss() throws Exception {
        byte[] screenshot = image(false);
        AndroidDriver driver = driver();
        doReturn(false).when(driver).executeScript(eq("mobile: scrollGesture"), anyMap());
        TestTouchActions actions = actions(driver);
        By container = By.id("expandable");
        container(actions, driver, container, new Rectangle(10, 10, 80, 80));
        OcrTarget target = OcrTarget.exact("Group 1");
        OcrMatch hit = new OcrMatch("Group 1", new OcrRectangle(12, 12, 20, 10), 0.9);
        AtomicInteger finds = new AtomicInteger();
        try (MockedConstruction<ScreenshotManager> ignored = mockConstruction(ScreenshotManager.class,
                (manager, context) -> when(manager.takeViewportScreenshot(driver)).thenReturn(screenshot));
             MockedStatic<OcrProcessingActions> ocr = mockStatic(OcrProcessingActions.class)) {
            ocr.when(() -> OcrProcessingActions.find(any(), any())).thenAnswer(invocation -> {
                if (finds.incrementAndGet() < 3) {
                    throw new IllegalStateException("No OCR match for 'Group 1'. fullText=Group 8");
                }
                return hit;
            });
            actions.swipeElementIntoView(container, target, TouchActions.SwipeDirection.UP);
        }
        verify(driver, times(2)).executeScript(eq("mobile: scrollGesture"), anyMap());
        Assert.assertEquals(finds.get(), 3);
    }

    @Test
    public void ocrSwipeShouldKeepGesturingWhenExactMatchIsClippedAtTrailingEdge() throws Exception {
        byte[] screenshot = image(false);
        AndroidDriver driver = driver();
        doReturn(false).when(driver).executeScript(eq("mobile: scrollGesture"), anyMap());
        TestTouchActions actions = actions(driver);
        By container = By.id("tabs");
        container(actions, driver, container, new Rectangle(10, 10, 80, 80));
        OcrTarget target = OcrTarget.exact("TAB 1");
        OcrMatch clipped = new OcrMatch("TAB 1", new OcrRectangle(90, 10, 10, 10), 0.91);
        OcrMatch fullyInView = new OcrMatch("TAB 1", new OcrRectangle(12, 10, 20, 10), 0.91);
        AtomicInteger finds = new AtomicInteger();
        try (MockedConstruction<ScreenshotManager> ignored = mockConstruction(ScreenshotManager.class,
                (manager, context) -> when(manager.takeViewportScreenshot(driver)).thenReturn(screenshot));
             MockedStatic<OcrProcessingActions> ocr = mockStatic(OcrProcessingActions.class)) {
            ocr.when(() -> OcrProcessingActions.find(any(), any())).thenAnswer(invocation -> {
                if (finds.incrementAndGet() < 3) {
                    return clipped;
                }
                return fullyInView;
            });
            actions.swipeElementIntoView(container, target, TouchActions.SwipeDirection.LEFT);
        }
        verify(driver, times(2)).executeScript(eq("mobile: scrollGesture"), anyMap());
        Assert.assertEquals(finds.get(), 3);
    }

    @Test
    public void imageSwipeShouldKeepGesturingAfterAppiumReportsNoMoreScrollOnMiss() throws Exception {
        byte[] screenshot = image(false);
        byte[] crop = image(true);
        AndroidDriver driver = driver();
        doReturn(false).when(driver).executeScript(eq("mobile: scrollGesture"), anyMap());
        TestTouchActions actions = actions(driver);
        By container = By.id("expandable");
        container(actions, driver, container, new Rectangle(10, 10, 80, 80));
        ImageTarget target = ImageTarget.fromBytes(crop).matchingMode(ImageMatchingMode.AUTO);
        AtomicInteger finds = new AtomicInteger();
        try (MockedConstruction<ScreenshotManager> ignored = mockConstruction(ScreenshotManager.class,
                (manager, context) -> when(manager.takeViewportScreenshot(driver)).thenReturn(screenshot));
             MockedStatic<ImageProcessingActions> images = mockStatic(ImageProcessingActions.class)) {
            images.when(() -> ImageProcessingActions.isUniqueImageTargetInView(any(), any()))
                    .thenAnswer(invocation -> finds.incrementAndGet() >= 3);
            actions.swipeElementIntoView(container, target, TouchActions.SwipeDirection.DOWN);
        }
        verify(driver, times(2)).executeScript(eq("mobile: scrollGesture"), anyMap());
        Assert.assertEquals(finds.get(), 3);
    }

    private static TestTouchActions actions(WebDriver driver) {
        return new TestTouchActions(driver, mock(ElementActionsHelper.class));
    }

    private static void container(TestTouchActions actions, AndroidDriver driver, By locator, Rectangle rectangle) {
        ElementActionsHelper helper = actions.helper();
        RemoteWebElement element = mock(RemoteWebElement.class);
        when(element.getId()).thenReturn("container-element");
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
            // No native bootstrap is needed by this deterministic provider.
        }
    }

    private static final class TestTouchActions extends TouchActions {
        private TestTouchActions(WebDriver driver, ElementActionsHelper helper) {
            super(driver);
            this.elementActionsHelper = helper;
        }

        private ElementActionsHelper helper() {
            return elementActionsHelper;
        }
    }
}
