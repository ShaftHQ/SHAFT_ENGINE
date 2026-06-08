package com.shaft.gui.internal.image;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Optional;

public class VisualProcessingProvidersUnitTest {
    @AfterMethod(alwaysRun = true)
    public void resetProviderCache() {
        VisualProcessingProviders.resetCacheForTesting();
    }

    @Test
    public void selectProviderShouldUsePriorityThenClassNameDeterministically() {
        VisualProcessingProvider lowerPriority = new ZProvider(1);
        VisualProcessingProvider alphabeticallyFirst = new AProvider(5);
        VisualProcessingProvider alphabeticallyLast = new ZProvider(5);

        Optional<VisualProcessingProvider> selected = VisualProcessingProviders.selectProvider(
                List.of(lowerPriority, alphabeticallyLast, alphabeticallyFirst));

        Assert.assertTrue(selected.isPresent());
        Assert.assertSame(selected.get(), alphabeticallyFirst);
    }

    @Test
    public void optionalOperationShouldNameFutureVisualArtifactWhenProviderIsMissing() {
        VisualProcessingProviders.setProviderForTesting(Optional.empty());

        IllegalStateException exception = Assert.expectThrows(IllegalStateException.class,
                () -> ImageProcessingActions.findImageWithinCurrentPage("reference.png", new byte[0]));

        Assert.assertTrue(exception.getMessage().contains("io.github.shafthq:shaft-visual"));
    }

    @Test
    public void providerContractDescriptorsShouldNotExposeThirdPartyVisualTypes() {
        for (Method method : VisualProcessingProvider.class.getMethods()) {
            assertCoreType(method.getReturnType());
            for (Class<?> parameterType : method.getParameterTypes()) {
                assertCoreType(parameterType);
            }
        }
    }

    @Test
    public void coreImageActionsShouldLoadAndHighlightWhenOpenCvIsBlocked() throws Exception {
        ClassLoader loader = new OpenCvBlockingClassLoader(getClass().getClassLoader());
        Class<?> imageActions = Class.forName(ImageProcessingActions.class.getName(), true, loader);
        Method highlight = imageActions.getMethod("highlightElementInScreenshot", byte[].class,
                org.openqa.selenium.Rectangle.class, Color.class);

        byte[] highlighted = (byte[]) highlight.invoke(null, createPng(),
                new org.openqa.selenium.Rectangle(5, 5, 10, 10), Color.RED);

        Assert.assertTrue(highlighted.length > 0);
        Assert.assertNotNull(ImageIO.read(new java.io.ByteArrayInputStream(highlighted)));
    }

    private static void assertCoreType(Class<?> type) {
        String typeName = type.isArray() ? type.getComponentType().getName() : type.getName();
        Assert.assertFalse(typeName.startsWith("org.opencv"), typeName);
        Assert.assertFalse(typeName.startsWith("com.applitools"), typeName);
        Assert.assertFalse(typeName.startsWith("com.assertthat"), typeName);
    }

    private static byte[] createPng() throws IOException {
        BufferedImage image = new BufferedImage(30, 30, BufferedImage.TYPE_INT_RGB);
        var graphics = image.createGraphics();
        graphics.setColor(Color.WHITE);
        graphics.fillRect(0, 0, image.getWidth(), image.getHeight());
        graphics.dispose();
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        ImageIO.write(image, "png", output);
        return output.toByteArray();
    }

    private static class OpenCvBlockingClassLoader extends ClassLoader {
        private OpenCvBlockingClassLoader(ClassLoader parent) {
            super(parent);
        }

        @Override
        protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
            if (name.startsWith("org.opencv") || name.equals("nu.pattern.OpenCV")) {
                throw new ClassNotFoundException(name);
            }
            if (name.equals(ImageProcessingActions.class.getName())) {
                synchronized (getClassLoadingLock(name)) {
                    Class<?> loaded = findLoadedClass(name);
                    if (loaded == null) {
                        loaded = defineFromParentResource(name);
                    }
                    if (resolve) {
                        resolveClass(loaded);
                    }
                    return loaded;
                }
            }
            return super.loadClass(name, resolve);
        }

        private Class<?> defineFromParentResource(String name) throws ClassNotFoundException {
            String resourceName = name.replace('.', '/') + ".class";
            try (InputStream input = getParent().getResourceAsStream(resourceName)) {
                if (input == null) {
                    throw new ClassNotFoundException(name);
                }
                byte[] bytes = input.readAllBytes();
                return defineClass(name, bytes, 0, bytes.length);
            } catch (IOException e) {
                throw new ClassNotFoundException(name, e);
            }
        }
    }

    private abstract static class StubProvider implements VisualProcessingProvider {
        private final int priority;

        private StubProvider(int priority) {
            this.priority = priority;
        }

        @Override
        public int priority() {
            return priority;
        }

        @Override
        public List<Integer> findImageWithinCurrentPage(String referenceImagePath, byte[] currentPageScreenshot) {
            return List.of();
        }

        @Override
        public Boolean compareAgainstBaseline(WebDriver driver, By elementLocator, byte[] elementScreenshot,
                                              ImageProcessingActions.VisualValidationEngine visualValidationEngine) {
            return false;
        }
    }

    private static final class AProvider extends StubProvider {
        private AProvider(int priority) {
            super(priority);
        }
    }

    private static final class ZProvider extends StubProvider {
        private ZProvider(int priority) {
            super(priority);
        }
    }
}
