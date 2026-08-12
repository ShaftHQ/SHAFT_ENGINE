package com.shaft.gui.driver;

import org.testng.Assert;
import org.testng.annotations.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;

import java.lang.reflect.Modifier;
import java.lang.reflect.Method;
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
    public void elementTargetOverloadsShouldBeCompatibilityDefaults() {
        assertDefaultMethod(DriverAssertions.class, "element", ElementTarget.class);
        assertDefaultMethod(DriverVerifications.class, "element", ElementTarget.class);
        assertDefaultMethod(ElementActionsContract.class, "assertThat", ElementTarget.class);
        assertDefaultMethod(ElementActionsContract.class, "verifyThat", ElementTarget.class);
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

    private static void assertForwardedTarget(By forwarded) {
        SearchContext context = mock(SearchContext.class);
        WebElement expected = mock(WebElement.class);
        By suppliedLocator = By.cssSelector("#save");
        when(context.findElements(suppliedLocator)).thenReturn(List.of(expected));

        Assert.assertEquals(forwarded.findElements(context), List.of(expected));
        verify(context).findElements(suppliedLocator);
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
