package com.shaft.gui.driver;

import org.testng.Assert;
import org.testng.annotations.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;

import java.lang.reflect.Modifier;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class GuiValidationContractTest {
    @Test
    public void elementTargetShouldBeAPublicImmutableValue() {
        Class<?> targetType;
        try {
            targetType = Class.forName("com.shaft.gui.driver.ElementTarget");
        } catch (ClassNotFoundException exception) {
            Assert.fail("ElementTarget public contract is missing.", exception);
            return;
        }

        Assert.assertTrue(Modifier.isPublic(targetType.getModifiers()));
        Assert.assertTrue(Modifier.isFinal(targetType.getModifiers()));
    }

    @Test
    public void elementRectangleShouldBeAPublicImmutableBackendNeutralValue() throws Exception {
        Class<?> rectangle = Class.forName("com.shaft.gui.driver.ElementRectangle");
        Assert.assertTrue(rectangle.isRecord());
        Assert.assertTrue(Modifier.isPublic(rectangle.getModifiers()));
        Assert.assertEquals(List.of(rectangle.getRecordComponents()).stream().map(component -> component.getName()).toList(),
                List.of("x", "y", "width", "height"));
        for (var component : rectangle.getRecordComponents()) {
            Assert.assertEquals(component.getType(), double.class);
        }
        var constructor = rectangle.getConstructor(double.class, double.class, double.class, double.class);
        Object value = constructor.newInstance(1.5, 2.5, 3.5, 4.5);
        Assert.assertEquals(rectangle.getMethod("x").invoke(value), 1.5d);
        Assert.assertEquals(rectangle.getMethod("y").invoke(value), 2.5d);
        Assert.assertEquals(rectangle.getMethod("width").invoke(value), 3.5d);
        Assert.assertEquals(rectangle.getMethod("height").invoke(value), 4.5d);
        Object boundaryValue = constructor.newInstance(-1.5, -2.5, 0, 0);
        Assert.assertEquals(rectangle.getMethod("x").invoke(boundaryValue), -1.5d);
        Assert.assertEquals(rectangle.getMethod("y").invoke(boundaryValue), -2.5d);
        Assert.assertEquals(rectangle.getMethod("width").invoke(boundaryValue), 0d);
        Assert.assertEquals(rectangle.getMethod("height").invoke(boundaryValue), 0d);
        for (double[] invalid : List.of(
                new double[]{Double.NaN, 0, 1, 1},
                new double[]{Double.NEGATIVE_INFINITY, 0, 1, 1},
                new double[]{0, Double.POSITIVE_INFINITY, 1, 1},
                new double[]{0, Double.NaN, 1, 1},
                new double[]{0, 0, Double.NaN, 1},
                new double[]{0, 0, Double.POSITIVE_INFINITY, 1},
                new double[]{0, 0, 1, Double.NaN},
                new double[]{0, 0, 1, Double.NEGATIVE_INFINITY},
                new double[]{0, 0, -1, 1},
                new double[]{0, 0, 1, -1})) {
            InvocationTargetException thrown = Assert.expectThrows(InvocationTargetException.class,
                    () -> constructor.newInstance(invalid[0], invalid[1], invalid[2], invalid[3]));
            Assert.assertTrue(thrown.getCause() instanceof IllegalArgumentException);
        }
    }

    @Test
    public void elementTargetOverloadsShouldBeCompatibilityDefaults() {
        assertDefaultMethod(DriverAssertions.class, "element", ElementTarget.class);
        assertDefaultMethod(DriverVerifications.class, "element", ElementTarget.class);
        assertDefaultMethod(ElementActionsContract.class, "assertThat", ElementTarget.class);
        assertDefaultMethod(ElementActionsContract.class, "verifyThat", ElementTarget.class);
    }

    @Test
    public void focusedElementCategoriesShouldBeCompatibilityDefaults() {
        assertDefaultMethod(ElementAssertions.class, "elementCount");
        assertDefaultMethod(ElementAssertions.class, "elementRectangle");
        assertDefaultMethod(ElementAssertions.class, "elementAccessibleName");
        assertDefaultMethod(ElementAssertions.class, "elementRole");
    }

    @Test
    public void focusedBrowserCoreCategoriesShouldBeCompatibilityDefaults() {
        assertBrowserDefaultMethod("pageSourceValue");
        assertBrowserDefaultMethod("windowHandleValue");
        assertBrowserDefaultMethod("windowPositionValue");
        assertBrowserDefaultMethod("windowSizeValue");
        assertBrowserDefaultMethod("browsingContextCountValue");
    }

    @Test
    public void focusedMobileContextAppAndDeviceCategoriesShouldBeCompatibilityDefaults() throws Exception {
        Class<?> mobileAssertions;
        try {
            mobileAssertions = Class.forName("com.shaft.gui.driver.MobileAssertions");
        } catch (ClassNotFoundException exception) {
            Assert.fail("MobileAssertions public contract is missing.", exception);
            return;
        }

        Assert.assertTrue(Modifier.isPublic(mobileAssertions.getModifiers()));
        Assert.assertTrue(mobileAssertions.isInterface());
        assertMobileRootDefault(DriverAssertions.class, mobileAssertions);
        assertMobileRootDefault(DriverVerifications.class, mobileAssertions);
        assertMobileValueDefault(mobileAssertions, "currentContextValue");
        assertMobileValueDefault(mobileAssertions, "contextCountValue");
        assertMobileValueDefault(mobileAssertions, "appInstalledValue", String.class);
        assertMobileValueDefault(mobileAssertions, "appStateValue", String.class);
        assertMobileValueDefault(mobileAssertions, "deviceLockedValue");
        assertMobileValueDefault(mobileAssertions, "deviceOrientationValue");
        assertMobileValueDefault(mobileAssertions, "deviceTimeValue");
        assertMobileValueDefault(mobileAssertions, "batteryValue");
        assertMobileValueDefault(mobileAssertions, "logMessageCountValue");
        assertMobileValueDefault(mobileAssertions, "logErrorCountValue");
        assertMobileValueDefault(mobileAssertions, "performanceSampleCountValue");
        assertMobileValueDefault(mobileAssertions, "recordingInProgressValue");
        assertMobileValueDefault(mobileAssertions, "retainedRecordingAvailableValue");
        assertMobileValueDefault(mobileAssertions, "retainedRecordingSizeValue");
        assertMobileValueDefault(mobileAssertions, "evidenceArtifactCountValue", MobileEvidenceBundle.class);
        assertMobileValueDefault(mobileAssertions, "evidenceOmissionCountValue", MobileEvidenceBundle.class);
        Assert.assertEquals(publicDescriptors(mobileAssertions), Set.of(
                "appInstalledValue[java.lang.String]->com.shaft.validation.internal.NativeValidationsBuilder",
                "appStateValue[java.lang.String]->com.shaft.validation.internal.NativeValidationsBuilder",
                "batteryValue[]->com.shaft.validation.internal.NativeValidationsBuilder",
                "contextCountValue[]->com.shaft.validation.internal.NativeValidationsBuilder",
                "currentContextValue[]->com.shaft.validation.internal.NativeValidationsBuilder",
                "deviceLockedValue[]->com.shaft.validation.internal.NativeValidationsBuilder",
                "deviceOrientationValue[]->com.shaft.validation.internal.NativeValidationsBuilder",
                "deviceTimeValue[]->com.shaft.validation.internal.NativeValidationsBuilder",
                "evidenceArtifactCountValue[com.shaft.gui.driver.MobileEvidenceBundle]->com.shaft.validation.internal.NativeValidationsBuilder",
                "evidenceOmissionCountValue[com.shaft.gui.driver.MobileEvidenceBundle]->com.shaft.validation.internal.NativeValidationsBuilder",
                "logErrorCountValue[]->com.shaft.validation.internal.NativeValidationsBuilder",
                "logMessageCountValue[]->com.shaft.validation.internal.NativeValidationsBuilder",
                "performanceSampleCountValue[]->com.shaft.validation.internal.NativeValidationsBuilder",
                "recordingInProgressValue[]->com.shaft.validation.internal.NativeValidationsBuilder",
                "retainedRecordingAvailableValue[]->com.shaft.validation.internal.NativeValidationsBuilder",
                "retainedRecordingSizeValue[]->com.shaft.validation.internal.NativeValidationsBuilder"));
        assertSingleMobileRootDescriptor(DriverAssertions.class, mobileAssertions);
        assertSingleMobileRootDescriptor(DriverVerifications.class, mobileAssertions);
    }

    @Test
    public void frozenOldDriverValidationConsumersShouldReceiveFailClosedMobileDefaults() throws Exception {
        Path output = Files.createTempDirectory("shaft-mobile-validation-old-consumer");
        var compiler = ToolProvider.getSystemJavaCompiler();
        List<SimpleJavaFileObject> sources = List.of(
                source("com.shaft.gui.driver.DriverAssertions", """
                        package com.shaft.gui.driver;
                        import org.openqa.selenium.By;
                        import com.shaft.validation.internal.NativeValidationsBuilder;
                        public interface DriverAssertions {
                            BrowserAssertions browser();
                            ElementAssertions element(By locator);
                            NativeValidationsBuilder object(Object value);
                        }
                        """),
                source("com.shaft.gui.driver.DriverVerifications", """
                        package com.shaft.gui.driver;
                        import org.openqa.selenium.By;
                        import com.shaft.validation.internal.NativeValidationsBuilder;
                        public interface DriverVerifications {
                            BrowserAssertions browser();
                            ElementAssertions element(By locator);
                            NativeValidationsBuilder object(Object value);
                        }
                        """),
                source("compat.OldMobileAssertions", """
                        package compat;
                        import com.shaft.gui.driver.*;
                        import com.shaft.validation.internal.NativeValidationsBuilder;
                        import org.openqa.selenium.By;
                        public final class OldMobileAssertions implements DriverAssertions {
                            public BrowserAssertions browser() { return null; }
                            public ElementAssertions element(By locator) { return null; }
                            public NativeValidationsBuilder object(Object value) { return null; }
                        }
                        """),
                source("compat.OldMobileVerifications", """
                        package compat;
                        import com.shaft.gui.driver.*;
                        import com.shaft.validation.internal.NativeValidationsBuilder;
                        import org.openqa.selenium.By;
                        public final class OldMobileVerifications implements DriverVerifications {
                            public BrowserAssertions browser() { return null; }
                            public ElementAssertions element(By locator) { return null; }
                            public NativeValidationsBuilder object(Object value) { return null; }
                        }
                        """));
        Assert.assertTrue(Boolean.TRUE.equals(compiler.getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, sources).call()));

        try (URLClassLoader loader = new URLClassLoader(new java.net.URL[]{output.toUri().toURL()},
                GuiValidationContractTest.class.getClassLoader())) {
            Object oldAssertions = Class.forName("compat.OldMobileAssertions", true, loader)
                    .getDeclaredConstructor().newInstance();
            Object oldVerifications = Class.forName("compat.OldMobileVerifications", true, loader)
                    .getDeclaredConstructor().newInstance();
            assertMobileRootFailsClosed(oldAssertions);
            assertMobileRootFailsClosed(oldVerifications);
        }
    }

    @Test
    public void partialMobileAssertionsImplementationsShouldFailClosedForEveryValue() throws Exception {
        Class<?> mobileAssertions = Class.forName("com.shaft.gui.driver.MobileAssertions");
        Object partial = org.mockito.Mockito.mock(mobileAssertions, org.mockito.Mockito.CALLS_REAL_METHODS);

        for (String methodName : List.of("currentContextValue", "contextCountValue", "deviceLockedValue",
                "deviceOrientationValue", "deviceTimeValue", "batteryValue")) {
            assertMobileValueFailsClosed(mobileAssertions, partial, methodName);
        }
        assertMobileValueFailsClosed(mobileAssertions, partial, "appInstalledValue", "com.example.app");
        assertMobileValueFailsClosed(mobileAssertions, partial, "appStateValue", "com.example.app");
        for (String methodName : List.of("logMessageCountValue", "logErrorCountValue",
                "performanceSampleCountValue", "recordingInProgressValue",
                "retainedRecordingAvailableValue", "retainedRecordingSizeValue")) {
            assertMobileValueFailsClosed(mobileAssertions, partial, methodName);
        }
        MobileEvidenceBundle evidence = mock(MobileEvidenceBundle.class);
        assertMobileValueFailsClosed(mobileAssertions, partial, "evidenceArtifactCountValue", evidence);
        assertMobileValueFailsClosed(mobileAssertions, partial, "evidenceOmissionCountValue", evidence);
        assertMobileValueFailsClosed(mobileAssertions, partial, "evidenceArtifactCountValue",
                new Class<?>[]{MobileEvidenceBundle.class}, new Object[]{null});
        assertMobileValueFailsClosed(mobileAssertions, partial, "evidenceOmissionCountValue",
                new Class<?>[]{MobileEvidenceBundle.class}, new Object[]{null});
    }

    @Test
    public void frozenPriorMobileAssertionsShouldReceiveFailClosedSnapshotDefaults() throws Exception {
        Path output = Files.createTempDirectory("shaft-mobile-assertions-old-consumer");
        var compiler = ToolProvider.getSystemJavaCompiler();
        List<SimpleJavaFileObject> sources = List.of(
                source("com.shaft.gui.driver.MobileAssertions", """
                        package com.shaft.gui.driver;
                        import com.shaft.validation.internal.NativeValidationsBuilder;
                        public interface MobileAssertions {
                            default NativeValidationsBuilder currentContextValue() { return null; }
                            default NativeValidationsBuilder contextCountValue() { return null; }
                            default NativeValidationsBuilder appInstalledValue(String appId) { return null; }
                            default NativeValidationsBuilder appStateValue(String appId) { return null; }
                            default NativeValidationsBuilder deviceLockedValue() { return null; }
                            default NativeValidationsBuilder deviceOrientationValue() { return null; }
                            default NativeValidationsBuilder deviceTimeValue() { return null; }
                            default NativeValidationsBuilder batteryValue() { return null; }
                        }
                        """),
                source("compat.OldMobileSnapshotAssertions", """
                        package compat;
                        public final class OldMobileSnapshotAssertions
                                implements com.shaft.gui.driver.MobileAssertions {}
                        """));
        Assert.assertTrue(Boolean.TRUE.equals(compiler.getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, sources).call()));

        try (URLClassLoader loader = new URLClassLoader(new java.net.URL[]{output.toUri().toURL()},
                GuiValidationContractTest.class.getClassLoader())) {
            Object oldConsumer = Class.forName("compat.OldMobileSnapshotAssertions", true, loader)
                    .getDeclaredConstructor().newInstance();
            Class<?> currentContract = Class.forName("com.shaft.gui.driver.MobileAssertions");
            for (String methodName : List.of("logMessageCountValue", "logErrorCountValue",
                    "performanceSampleCountValue", "recordingInProgressValue",
                    "retainedRecordingAvailableValue", "retainedRecordingSizeValue")) {
                assertMobileValueFailsClosed(currentContract, oldConsumer, methodName);
            }
            MobileEvidenceBundle evidence = mock(MobileEvidenceBundle.class);
            assertMobileValueFailsClosed(currentContract, oldConsumer, "evidenceArtifactCountValue", evidence);
            assertMobileValueFailsClosed(currentContract, oldConsumer, "evidenceOmissionCountValue", evidence);
            assertMobileValueFailsClosed(currentContract, oldConsumer, "evidenceArtifactCountValue",
                    new Class<?>[]{MobileEvidenceBundle.class}, new Object[]{null});
            assertMobileValueFailsClosed(currentContract, oldConsumer, "evidenceOmissionCountValue",
                    new Class<?>[]{MobileEvidenceBundle.class}, new Object[]{null});
        }
    }

    @Test
    public void naturalLegacyMobileAccessorsShouldRemainSourceCompatible() throws Exception {
        Assert.assertTrue(compileDriverValidationConsumer("NaturalMobileAssertionsAccessor", "DriverAssertions", """
                public boolean mobile() { return true; }
                """));
        Assert.assertTrue(compileDriverValidationConsumer("NaturalMobileVerificationsAccessor", "DriverVerifications", """
                public boolean mobile() { return true; }
                """));
        Assert.assertTrue(compileMobileAssertionsConsumer("NaturalMobileValueAccessors", """
                public String currentContext() { return ""; }
                public int contextCount() { return 0; }
                public boolean appInstalled(String appId) { return false; }
                public com.shaft.gui.driver.MobileApplicationState appState(String appId) {
                    return com.shaft.gui.driver.MobileApplicationState.NOT_RUNNING;
                }
                public boolean deviceLocked() { return false; }
                public org.openqa.selenium.ScreenOrientation deviceOrientation() {
                    return org.openqa.selenium.ScreenOrientation.PORTRAIT;
                }
                public String deviceTime() { return ""; }
                public com.shaft.gui.driver.MobileBatteryInfo battery() {
                    return new com.shaft.gui.driver.MobileBatteryInfo(0, "unknown");
                }
                public int logMessageCount() { return 0; }
                public int logErrorCount() { return 0; }
                public int performanceSampleCount() { return 0; }
                public boolean recordingInProgress() { return false; }
                public boolean retainedRecordingAvailable() { return false; }
                public long retainedRecordingSize() { return 0; }
                public int evidenceArtifactCount(com.shaft.gui.driver.MobileEvidenceBundle bundle) { return 0; }
                public int evidenceOmissionCount(com.shaft.gui.driver.MobileEvidenceBundle bundle) { return 0; }
                """));
    }

    @Test
    public void mobileValidationDefaultCollisionsShouldRequireCompatibleOverrides() throws Exception {
        Assert.assertFalse(compileMobileRootDefaultCollision("MissingMobileRootOverride", false));
        Assert.assertTrue(compileMobileRootDefaultCollision("CompatibleMobileRootOverride", true));
        Assert.assertFalse(compileDriverValidationConsumer("IncompatibleMobileRootOverride", "DriverAssertions", """
                public String mobileValues() { return ""; }
                """));
        Assert.assertFalse(compileMobileValueDefaultCollision("MissingMobileValueOverride", false));
        Assert.assertTrue(compileMobileValueDefaultCollision("CompatibleMobileValueOverride", true));
        Assert.assertFalse(compileMobileValueDefaultCollision("MissingMobileSnapshotValueOverride",
                "logMessageCountValue", false));
        Assert.assertTrue(compileMobileValueDefaultCollision("CompatibleMobileSnapshotValueOverride",
                "logMessageCountValue", true));
        Assert.assertFalse(compileMobileAssertionsConsumer("IncompatibleMobileValueOverride", """
                public String currentContextValue() { return ""; }
                """));
        Assert.assertFalse(compileMobileAssertionsConsumer("IncompatibleMobileSnapshotValueOverride", """
                public int logMessageCountValue() { return 0; }
                """));
    }

    @Test
    public void frozenOldBrowserAssertionsShouldReceiveFailClosedCoreCategoryDefaults() throws Exception {
        Path output = Files.createTempDirectory("shaft-browser-assertions-old-consumer");
        var compiler = ToolProvider.getSystemJavaCompiler();
        List<SimpleJavaFileObject> sources = List.of(
                source("com.shaft.gui.driver.BrowserAssertions", """
                        package com.shaft.gui.driver;
                        public interface BrowserAssertions {}
                        """),
                source("compat.OldBrowserAssertions", """
                        package compat;
                        public final class OldBrowserAssertions implements com.shaft.gui.driver.BrowserAssertions {}
                        """));
        Assert.assertTrue(Boolean.TRUE.equals(compiler.getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, sources).call()));

        try (URLClassLoader loader = new URLClassLoader(new java.net.URL[]{output.toUri().toURL()},
                GuiValidationContractTest.class.getClassLoader())) {
            Object oldConsumer = Class.forName("compat.OldBrowserAssertions", true, loader)
                    .getDeclaredConstructor().newInstance();
            for (String method : List.of("pageSourceValue", "windowHandleValue", "windowPositionValue", "windowSizeValue",
                    "browsingContextCountValue")) {
                assertBrowserCompatibilityDefaultFailsClosed(oldConsumer, method);
            }
        }
    }

    @Test
    public void naturalLegacyBrowserAccessorsShouldRemainSourceCompatible() throws Exception {
        Path output = Files.createTempDirectory("shaft-browser-assertions-collision");
        SimpleJavaFileObject consumer = source("compat.NaturalBrowserAccessors", """
                package compat;
                public abstract class NaturalBrowserAccessors implements com.shaft.gui.driver.BrowserAssertions {
                    public String pageSource() { return ""; }
                    public String windowHandle() { return ""; }
                    public String windowPosition() { return ""; }
                    public String windowSize() { return ""; }
                    public int browsingContextCount() { return 0; }
                }
                """);
        Assert.assertTrue(Boolean.TRUE.equals(ToolProvider.getSystemJavaCompiler().getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, List.of(consumer)).call()));
    }

    @Test
    public void browserCategoryDefaultCollisionsShouldRequireCompatibleOverrides() throws Exception {
        for (String method : List.of("pageSourceValue", "browsingContextCountValue")) {
            Assert.assertFalse(compileBrowserCategoryDefaultCollision("MissingBrowserCategoryOverride" + method, method, false));
            Assert.assertTrue(compileBrowserCategoryDefaultCollision("CompatibleBrowserCategoryOverride" + method, method, true));
        }
        Assert.assertFalse(compileBrowserAssertionsConsumer("IncompatibleBrowserCategoryOverride", """
                public String pageSourceValue() { return ""; }
                """));
        Assert.assertFalse(compileBrowserAssertionsConsumer("IncompatibleBrowserContextCountOverride", """
                public int browsingContextCountValue() { return 0; }
                """));
    }

    @Test
    public void frozenOldElementAssertionsShouldReceiveFailClosedCategoryDefaults() throws Exception {
        Path output = Files.createTempDirectory("shaft-element-assertions-old-consumer");
        var compiler = ToolProvider.getSystemJavaCompiler();
        List<SimpleJavaFileObject> sources = List.of(
                source("com.shaft.gui.driver.ElementAssertions", """
                        package com.shaft.gui.driver;
                        public interface ElementAssertions {}
                        """),
                source("compat.OldElementAssertions", """
                        package compat;
                        public final class OldElementAssertions implements com.shaft.gui.driver.ElementAssertions {}
                        """));
        Assert.assertTrue(Boolean.TRUE.equals(compiler.getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, sources).call()));

        try (URLClassLoader loader = new URLClassLoader(new java.net.URL[]{output.toUri().toURL()},
                GuiValidationContractTest.class.getClassLoader())) {
            Object oldConsumer = Class.forName("compat.OldElementAssertions", true, loader)
                    .getDeclaredConstructor().newInstance();
            assertCompatibilityDefaultFailsClosed(oldConsumer, "elementCount");
            assertCompatibilityDefaultFailsClosed(oldConsumer, "elementRectangle");
            assertCompatibilityDefaultFailsClosed(oldConsumer, "elementAccessibleName");
            assertCompatibilityDefaultFailsClosed(oldConsumer, "elementRole");
        }
    }

    @Test
    public void naturalLegacyAccessorsShouldRemainSourceCompatible() throws Exception {
        Assert.assertTrue(compileElementAssertionsConsumer("NaturalAccessors", """
                public int count() { return 1; }
                public org.openqa.selenium.Rectangle rectangle() { return null; }
                public String accessibleName() { return "name"; }
                public String role() { return "button"; }
                """));
    }

    @Test
    public void focusedCategoryDefaultCollisionsShouldRequireCompatibleOverrides() throws Exception {
        Assert.assertFalse(compileCategoryDefaultCollision("MissingCategoryOverride", false));
        Assert.assertTrue(compileCategoryDefaultCollision("CompatibleCategoryOverride", true));
        Assert.assertFalse(compileElementAssertionsConsumer("IncompatibleCategoryOverride", """
                public int elementCount() { return 1; }
                """));
    }

    @Test
    public void frozenOldConsumersShouldLinkAndRunAgainstTheNewDefaults() throws Exception {
        Path output = Files.createTempDirectory("shaft-validation-old-consumer");
        var compiler = ToolProvider.getSystemJavaCompiler();
        List<SimpleJavaFileObject> sources = List.of(
                source("com.shaft.gui.driver.DriverAssertions", """
                        package com.shaft.gui.driver;
                        import org.openqa.selenium.By;
                        import com.shaft.validation.internal.NativeValidationsBuilder;
                        public interface DriverAssertions {
                            BrowserAssertions browser();
                            ElementAssertions element(By locator);
                            NativeValidationsBuilder object(Object value);
                        }
                        """),
                source("com.shaft.gui.driver.DriverVerifications", """
                        package com.shaft.gui.driver;
                        import org.openqa.selenium.By;
                        import com.shaft.validation.internal.NativeValidationsBuilder;
                        public interface DriverVerifications {
                            BrowserAssertions browser();
                            ElementAssertions element(By locator);
                            NativeValidationsBuilder object(Object value);
                        }
                        """),
                source("com.shaft.gui.driver.ElementActionsContract", """
                        package com.shaft.gui.driver;
                        import org.openqa.selenium.By;
                        public interface ElementActionsContract {
                            ElementAssertions assertThat(By locator);
                            ElementAssertions verifyThat(By locator);
                        }
                        """),
                source("compat.OldAssertions", """
                        package compat;
                        import com.shaft.gui.driver.*;
                        import com.shaft.validation.internal.NativeValidationsBuilder;
                        import org.openqa.selenium.By;
                        public final class OldAssertions implements DriverAssertions {
                            public By last;
                            public BrowserAssertions browser() { return null; }
                            public ElementAssertions element(By locator) { last = locator; return sentinel(); }
                            public NativeValidationsBuilder object(Object value) { return null; }
                            private ElementAssertions sentinel() { return (ElementAssertions) java.lang.reflect.Proxy.newProxyInstance(
                                getClass().getClassLoader(), new Class[]{ElementAssertions.class}, (proxy, method, args) -> null); }
                        }
                        """),
                source("compat.OldVerifications", """
                        package compat;
                        import com.shaft.gui.driver.*;
                        import com.shaft.validation.internal.NativeValidationsBuilder;
                        import org.openqa.selenium.By;
                        public final class OldVerifications implements DriverVerifications {
                            public By last;
                            public BrowserAssertions browser() { return null; }
                            public ElementAssertions element(By locator) { last = locator; return sentinel(); }
                            public NativeValidationsBuilder object(Object value) { return null; }
                            private ElementAssertions sentinel() { return (ElementAssertions) java.lang.reflect.Proxy.newProxyInstance(
                                getClass().getClassLoader(), new Class[]{ElementAssertions.class}, (proxy, method, args) -> null); }
                        }
                        """),
                source("compat.OldElementActions", """
                        package compat;
                        import com.shaft.gui.driver.*;
                        import org.openqa.selenium.By;
                        public final class OldElementActions implements ElementActionsContract {
                            public By last;
                            public ElementAssertions assertThat(By locator) { last = locator; return sentinel(); }
                            public ElementAssertions verifyThat(By locator) { last = locator; return sentinel(); }
                            private ElementAssertions sentinel() { return (ElementAssertions) java.lang.reflect.Proxy.newProxyInstance(
                                getClass().getClassLoader(), new Class[]{ElementAssertions.class}, (proxy, method, args) -> null); }
                        }
                        """));
        Assert.assertTrue(Boolean.TRUE.equals(compiler.getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, sources).call()));

        ElementTarget target = ElementTarget.located(ShaftLocator.css("#save"));
        try (URLClassLoader loader = new URLClassLoader(new java.net.URL[]{output.toUri().toURL()},
                GuiValidationContractTest.class.getClassLoader())) {
            DriverAssertions assertions = (DriverAssertions) Class.forName("compat.OldAssertions", true, loader)
                    .getDeclaredConstructor().newInstance();
            DriverVerifications verifications = (DriverVerifications) Class.forName("compat.OldVerifications", true, loader)
                    .getDeclaredConstructor().newInstance();
            ElementActionsContract actions = (ElementActionsContract) Class.forName("compat.OldElementActions", true, loader)
                    .getDeclaredConstructor().newInstance();
            Assert.assertNotNull(assertions.element(target));
            assertForwardedTarget((By) assertions.getClass().getField("last").get(assertions));
            Assert.assertNotNull(verifications.element(target));
            assertForwardedTarget((By) verifications.getClass().getField("last").get(verifications));
            Assert.assertNotNull(actions.assertThat(target));
            assertForwardedTarget((By) actions.getClass().getField("last").get(actions));
            Assert.assertNotNull(actions.verifyThat(target));
            assertForwardedTarget((By) actions.getClass().getField("last").get(actions));
        }
    }

    @Test
    public void competingDefaultsShouldRequireAnExplicitOverrideAndIncompatibleReturnsShouldFail() throws Exception {
        Assert.assertTrue(compileCollision("CompatibleConsumer", "ElementAssertions", true));
        Assert.assertFalse(compileCollision("MissingOverrideConsumer", "ElementAssertions", false));
        Assert.assertFalse(compileCollision("IncompatibleConsumer", "String", false));
    }

    private static void assertDefaultMethod(Class<?> owner, String name, Class<?> parameterType) {
        Method method;
        try {
            method = owner.getMethod(name, parameterType);
        } catch (NoSuchMethodException exception) {
            Assert.fail("Missing compatibility method: " + owner.getSimpleName() + "." + name, exception);
            return;
        }
        Assert.assertTrue(method.isDefault(), owner.getSimpleName() + "." + name + " must be a default method.");
        Assert.assertEquals(method.getReturnType(), ElementAssertions.class);
    }

    private static void assertDefaultMethod(Class<?> owner, String name) {
        Method method;
        try {
            method = owner.getMethod(name);
        } catch (NoSuchMethodException exception) {
            Assert.fail("Missing compatibility method: " + owner.getSimpleName() + "." + name, exception);
            return;
        }
        Assert.assertTrue(method.isDefault(), owner.getSimpleName() + "." + name + " must be a default method.");
        Assert.assertEquals(method.getReturnType(), com.shaft.validation.internal.NativeValidationsBuilder.class);
    }

    private static void assertBrowserDefaultMethod(String name) {
        Method method;
        try {
            method = BrowserAssertions.class.getMethod(name);
        } catch (NoSuchMethodException exception) {
            Assert.fail("Missing compatibility method: BrowserAssertions." + name, exception);
            return;
        }
        Assert.assertTrue(method.isDefault(), "BrowserAssertions." + name + " must be a default method.");
        Assert.assertEquals(method.getReturnType(), com.shaft.validation.internal.NativeValidationsBuilder.class);
    }

    private static void assertMobileRootDefault(Class<?> owner, Class<?> mobileAssertions) throws Exception {
        Method method = owner.getMethod("mobileValues");
        Assert.assertTrue(method.isDefault(), owner.getSimpleName() + ".mobileValues must be a default method.");
        Assert.assertEquals(method.getReturnType(), mobileAssertions);
    }

    private static void assertSingleMobileRootDescriptor(Class<?> owner, Class<?> mobileAssertions) {
        List<Method> roots = List.of(owner.getDeclaredMethods()).stream()
                .filter(method -> Modifier.isPublic(method.getModifiers()))
                .filter(method -> method.getName().equals("mobileValues"))
                .toList();
        Assert.assertEquals(roots.size(), 1);
        Assert.assertEquals(roots.getFirst().getParameterCount(), 0);
        Assert.assertEquals(roots.getFirst().getReturnType(), mobileAssertions);
        Assert.assertTrue(roots.getFirst().isDefault());
    }

    private static Set<String> publicDescriptors(Class<?> owner) {
        return List.of(owner.getDeclaredMethods()).stream()
                .filter(method -> Modifier.isPublic(method.getModifiers()))
                .map(method -> method.getName() + List.of(method.getParameterTypes()).stream()
                        .map(Class::getName)
                        .toList() + "->" + method.getReturnType().getName())
                .collect(Collectors.toSet());
    }

    private static void assertMobileValueDefault(Class<?> owner, String name, Class<?>... parameterTypes)
            throws Exception {
        Method method = owner.getMethod(name, parameterTypes);
        Assert.assertTrue(method.isDefault(), "MobileAssertions." + name + " must be a default method.");
        Assert.assertEquals(method.getReturnType(), com.shaft.validation.internal.NativeValidationsBuilder.class);
    }

    private static void assertMobileRootFailsClosed(Object consumer) throws Exception {
        Method method = consumer.getClass().getMethod("mobileValues");
        InvocationTargetException thrown = Assert.expectThrows(InvocationTargetException.class,
                () -> method.invoke(consumer));
        Assert.assertTrue(thrown.getCause() instanceof UnsupportedOperationException);
        Assert.assertEquals(thrown.getCause().getMessage(),
                "mobileValues is not supported by this driver validation implementation.");
    }

    private static void assertMobileValueFailsClosed(Class<?> owner, Object consumer, String methodName,
                                                     Object... arguments) throws Exception {
        Class<?>[] parameterTypes = arguments.length == 0
                ? new Class<?>[0]
                : new Class<?>[]{arguments[0] instanceof MobileEvidenceBundle
                ? MobileEvidenceBundle.class : String.class};
        Method method = owner.getMethod(methodName, parameterTypes);
        InvocationTargetException thrown = Assert.expectThrows(InvocationTargetException.class,
                () -> method.invoke(consumer, arguments));
        Assert.assertTrue(thrown.getCause() instanceof UnsupportedOperationException);
        Assert.assertEquals(thrown.getCause().getMessage(),
                methodName + " is not supported by this mobile assertions implementation.");
    }

    private static void assertMobileValueFailsClosed(Class<?> owner, Object consumer, String methodName,
                                                      Class<?>[] parameterTypes, Object[] arguments) throws Exception {
        Method method = owner.getMethod(methodName, parameterTypes);
        InvocationTargetException thrown = Assert.expectThrows(InvocationTargetException.class,
                () -> method.invoke(consumer, arguments));
        Assert.assertTrue(thrown.getCause() instanceof UnsupportedOperationException);
        Assert.assertEquals(thrown.getCause().getMessage(),
                methodName + " is not supported by this mobile assertions implementation.");
    }

    private static void assertBrowserCompatibilityDefaultFailsClosed(Object oldConsumer, String methodName) throws Exception {
        Method method = BrowserAssertions.class.getMethod(methodName);
        InvocationTargetException thrown = Assert.expectThrows(InvocationTargetException.class,
                () -> method.invoke(oldConsumer));
        Assert.assertTrue(thrown.getCause() instanceof UnsupportedOperationException);
        Assert.assertEquals(thrown.getCause().getMessage(),
                methodName + " is not supported by this browser assertions implementation.");
    }

    private static void assertForwardedTarget(By forwarded) {
        SearchContext context = mock(SearchContext.class);
        WebElement expected = mock(WebElement.class);
        By suppliedLocator = By.cssSelector("#save");
        when(context.findElements(suppliedLocator)).thenReturn(List.of(expected));

        Assert.assertEquals(forwarded.findElements(context), List.of(expected));
        verify(context).findElements(suppliedLocator);
    }

    private static void assertCompatibilityDefaultFailsClosed(Object oldConsumer, String methodName) throws Exception {
        Method method = ElementAssertions.class.getMethod(methodName);
        InvocationTargetException thrown = Assert.expectThrows(InvocationTargetException.class,
                () -> method.invoke(oldConsumer));
        Assert.assertTrue(thrown.getCause() instanceof UnsupportedOperationException);
        Assert.assertEquals(thrown.getCause().getMessage(),
                methodName + " is not supported by this element assertions implementation.");
    }

    private static boolean compileElementAssertionsConsumer(String className, String methods) throws Exception {
        Path output = Files.createTempDirectory("shaft-element-assertions-collision");
        SimpleJavaFileObject consumer = source("compat." + className, """
                package compat;
                public abstract class %s implements com.shaft.gui.driver.ElementAssertions {
                    %s
                }
                """.formatted(className, methods));
        return Boolean.TRUE.equals(ToolProvider.getSystemJavaCompiler().getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, List.of(consumer)).call());
    }

    private static boolean compileBrowserAssertionsConsumer(String className, String methods) throws Exception {
        Path output = Files.createTempDirectory("shaft-browser-assertions-collision");
        SimpleJavaFileObject consumer = source("compat." + className, """
                package compat;
                public abstract class %s implements com.shaft.gui.driver.BrowserAssertions {
                    %s
                }
                """.formatted(className, methods));
        return Boolean.TRUE.equals(ToolProvider.getSystemJavaCompiler().getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, List.of(consumer)).call());
    }

    private static boolean compileDriverValidationConsumer(String className, String contract, String methods)
            throws Exception {
        Path output = Files.createTempDirectory("shaft-driver-mobile-validation-collision");
        SimpleJavaFileObject consumer = source("compat." + className, """
                package compat;
                import com.shaft.gui.driver.*;
                import com.shaft.validation.internal.NativeValidationsBuilder;
                import org.openqa.selenium.By;
                public abstract class %s implements %s {
                    %s
                }
                """.formatted(className, contract, methods));
        return Boolean.TRUE.equals(ToolProvider.getSystemJavaCompiler().getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, List.of(consumer)).call());
    }

    private static boolean compileMobileAssertionsConsumer(String className, String methods) throws Exception {
        Path output = Files.createTempDirectory("shaft-mobile-assertions-collision");
        SimpleJavaFileObject consumer = source("compat." + className, """
                package compat;
                public abstract class %s implements com.shaft.gui.driver.MobileAssertions {
                    %s
                }
                """.formatted(className, methods));
        return Boolean.TRUE.equals(ToolProvider.getSystemJavaCompiler().getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, List.of(consumer)).call());
    }

    private static boolean compileMobileRootDefaultCollision(String className, boolean override) throws Exception {
        Path output = Files.createTempDirectory("shaft-mobile-root-default-collision");
        SimpleJavaFileObject foreign = source("compat.ForeignMobileRoot", """
                package compat;
                public interface ForeignMobileRoot {
                    default com.shaft.gui.driver.MobileAssertions mobileValues() { return null; }
                }
                """);
        String explicitOverride = override
                ? "public com.shaft.gui.driver.MobileAssertions mobileValues() { "
                + "return com.shaft.gui.driver.DriverAssertions.super.mobileValues(); }"
                : "";
        SimpleJavaFileObject consumer = source("compat." + className, """
                package compat;
                public abstract class %s implements com.shaft.gui.driver.DriverAssertions, ForeignMobileRoot {
                    %s
                }
                """.formatted(className, explicitOverride));
        return Boolean.TRUE.equals(ToolProvider.getSystemJavaCompiler().getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, List.of(foreign, consumer)).call());
    }

    private static boolean compileMobileValueDefaultCollision(String className, boolean override) throws Exception {
        return compileMobileValueDefaultCollision(className, "currentContextValue", override);
    }

    private static boolean compileMobileValueDefaultCollision(String className, String methodName,
                                                               boolean override) throws Exception {
        Path output = Files.createTempDirectory("shaft-mobile-value-default-collision");
        SimpleJavaFileObject foreign = source("compat.ForeignMobileValue", """
                package compat;
                public interface ForeignMobileValue {
                    default com.shaft.validation.internal.NativeValidationsBuilder %s() { return null; }
                }
                """.formatted(methodName));
        String explicitOverride = override
                ? "public com.shaft.validation.internal.NativeValidationsBuilder " + methodName + "() { "
                + "return com.shaft.gui.driver.MobileAssertions.super." + methodName + "(); }"
                : "";
        SimpleJavaFileObject consumer = source("compat." + className, """
                package compat;
                public abstract class %s implements com.shaft.gui.driver.MobileAssertions, ForeignMobileValue {
                    %s
                }
                """.formatted(className, explicitOverride));
        return Boolean.TRUE.equals(ToolProvider.getSystemJavaCompiler().getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, List.of(foreign, consumer)).call());
    }

    private static boolean compileBrowserCategoryDefaultCollision(String className, String methodName,
                                                                  boolean override) throws Exception {
        Path output = Files.createTempDirectory("shaft-browser-category-default-collision");
        SimpleJavaFileObject foreign = source("compat.ForeignBrowserCategory", """
                package compat;
                public interface ForeignBrowserCategory {
                    default com.shaft.validation.internal.NativeValidationsBuilder %s() { return null; }
                }
                """.formatted(methodName));
        String explicitOverride = override
                ? "public com.shaft.validation.internal.NativeValidationsBuilder " + methodName + "() { "
                + "return com.shaft.gui.driver.BrowserAssertions.super." + methodName + "(); }"
                : "";
        SimpleJavaFileObject consumer = source("compat." + className, """
                package compat;
                public abstract class %s implements com.shaft.gui.driver.BrowserAssertions, ForeignBrowserCategory {
                    %s
                }
                """.formatted(className, explicitOverride));
        return Boolean.TRUE.equals(ToolProvider.getSystemJavaCompiler().getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, List.of(foreign, consumer)).call());
    }

    private static boolean compileCategoryDefaultCollision(String className, boolean override) throws Exception {
        Path output = Files.createTempDirectory("shaft-element-category-default-collision");
        SimpleJavaFileObject foreign = source("compat.ForeignCategory", """
                package compat;
                public interface ForeignCategory {
                    default com.shaft.validation.internal.NativeValidationsBuilder elementCount() { return null; }
                }
                """);
        String explicitOverride = override
                ? "public com.shaft.validation.internal.NativeValidationsBuilder elementCount() { "
                + "return com.shaft.gui.driver.ElementAssertions.super.elementCount(); }"
                : "";
        SimpleJavaFileObject consumer = source("compat." + className, """
                package compat;
                public abstract class %s implements com.shaft.gui.driver.ElementAssertions, ForeignCategory {
                    %s
                }
                """.formatted(className, explicitOverride));
        return Boolean.TRUE.equals(ToolProvider.getSystemJavaCompiler().getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, List.of(foreign, consumer)).call());
    }

    private static boolean compileCollision(String className, String returnType, boolean override) throws Exception {
        Path output = Files.createTempDirectory("shaft-validation-default-collision");
        String overrideMethod = override
                ? "public ElementAssertions assertThat(ElementTarget target) { return ElementActionsContract.super.assertThat(target); }"
                : "";
        var foreign = source("compat.Foreign", """
                package compat;
                import com.shaft.gui.driver.*;
                public interface Foreign {
                    default %s assertThat(ElementTarget target) { return null; }
                }
                """.formatted(returnType));
        var consumer = source("compat." + className, """
                package compat;
                import com.shaft.gui.driver.*;
                import org.openqa.selenium.By;
                public abstract class %s implements ElementActionsContract, Foreign {
                    %s
                }
                """.formatted(className, overrideMethod));
        return Boolean.TRUE.equals(ToolProvider.getSystemJavaCompiler().getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, List.of(foreign, consumer)).call());
    }

    private static SimpleJavaFileObject source(String className, String contents) {
        return new SimpleJavaFileObject(URI.create("string:///" + className.replace('.', '/') + ".java"),
                javax.tools.JavaFileObject.Kind.SOURCE) {
            @Override
            public CharSequence getCharContent(boolean ignoreEncodingErrors) {
                return contents;
            }
        };
    }
}
