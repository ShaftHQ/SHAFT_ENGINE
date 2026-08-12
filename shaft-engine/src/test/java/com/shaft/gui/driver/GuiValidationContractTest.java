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
