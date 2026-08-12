package com.shaft.gui.driver;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import org.mockito.InOrder;
import org.openqa.selenium.By;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.argThat;

public class ElementTargetTest {
    @Test
    public void publicSurfaceShouldExposeImmutableComposition() throws Exception {
        Method locatedBy = method("located", By.class);
        Method locatedShaft = method("located", ShaftLocator.class);
        Method descendantBy = method("descendant", By.class);
        Method descendantShaft = method("descendant", ShaftLocator.class);
        Method nth = method("nth", int.class);
        Method toBy = method("toBy");
        Method toPlaywrightLocator = method("toPlaywrightLocator", Page.class);

        Assert.assertTrue(Modifier.isStatic(locatedBy.getModifiers()));
        Assert.assertTrue(Modifier.isStatic(locatedShaft.getModifiers()));
        Assert.assertEquals(descendantBy.getReturnType(), ElementTarget.class);
        Assert.assertEquals(descendantShaft.getReturnType(), ElementTarget.class);
        Assert.assertEquals(nth.getReturnType(), ElementTarget.class);
        Assert.assertEquals(toBy.getReturnType(), By.class);
        Assert.assertEquals(toPlaywrightLocator.getReturnType(), Locator.class);
    }

    @Test
    public void seleniumResolutionShouldPreserveCompositionOrderAndIndex() {
        SearchContext driver = mock(SearchContext.class);
        WebElement root = mock(WebElement.class);
        WebElement firstChild = mock(WebElement.class);
        WebElement secondChild = mock(WebElement.class);
        WebElement nested = mock(WebElement.class);
        By rootBy = By.id("root");
        By childrenBy = By.cssSelector(".child");
        By nestedBy = By.name("nested");

        String portableRoot = ShaftLocator.from(rootBy).toBy().toString();
        String portableChildren = ShaftLocator.from(childrenBy).toBy().toString();
        String portableNested = ShaftLocator.from(nestedBy).toBy().toString();
        when(driver.findElements(argThat(by -> portableRoot.equals(by.toString())))).thenReturn(List.of(root));
        when(root.findElements(argThat(by -> portableChildren.equals(by.toString()))))
                .thenReturn(List.of(firstChild, secondChild));
        when(secondChild.findElements(argThat(by -> portableNested.equals(by.toString()))))
                .thenReturn(List.of(nested));

        ElementTarget rootTarget = located(rootBy);
        ElementTarget composed = descendant(nth(descendant(rootTarget, childrenBy), 1), nestedBy);

        Assert.assertEquals(toBy(rootTarget).findElements(driver), List.of(root));
        Assert.assertEquals(toBy(composed).findElements(driver), List.of(nested));
        verifyNoInteractions(firstChild);
    }

    @Test
    public void playwrightResolutionShouldPreserveCompositionOrderAndIndex() {
        Page page = mock(Page.class);
        Locator root = mock(Locator.class);
        Locator children = mock(Locator.class);
        Locator secondChild = mock(Locator.class);
        Locator nested = mock(Locator.class);
        when(page.locator("#root")).thenReturn(root);
        when(root.locator(".child")).thenReturn(children);
        when(children.nth(1)).thenReturn(secondChild);
        when(secondChild.locator("[name=\"nested\"]")).thenReturn(nested);

        ElementTarget target = descendant(
                nth(descendant(located(ShaftLocator.css("#root")), ShaftLocator.css(".child")), 1),
                ShaftLocator.css("[name=\"nested\"]"));

        Assert.assertSame(toPlaywrightLocator(target, page), nested);
        InOrder order = inOrder(page, root, children, secondChild);
        order.verify(page).locator("#root");
        order.verify(root).locator(".child");
        order.verify(children).nth(1);
        order.verify(secondChild).locator("[name=\"nested\"]");
    }

    @Test
    public void invalidCompositionShouldFailBeforeResolution() {
        Assert.expectThrows(NullPointerException.class, () -> located((By) null));
        Assert.expectThrows(NullPointerException.class, () -> located((ShaftLocator) null));
        ElementTarget target = located(By.id("root"));
        Assert.expectThrows(NullPointerException.class, () -> descendant(target, (By) null));
        Assert.expectThrows(NullPointerException.class, () -> descendant(target, (ShaftLocator) null));
        Assert.expectThrows(IllegalArgumentException.class, () -> nth(target, -1));
    }

    @Test
    public void seleniumIndexShouldUseTheFlattenedMatchesAndReturnEmptyWhenOutOfRange() {
        SearchContext driver = mock(SearchContext.class);
        WebElement firstRoot = mock(WebElement.class);
        WebElement secondRoot = mock(WebElement.class);
        WebElement firstChild = mock(WebElement.class);
        WebElement secondChild = mock(WebElement.class);
        String root = ShaftLocator.css(".root").toBy().toString();
        String child = ShaftLocator.css(".child").toBy().toString();
        when(driver.findElements(argThat(by -> root.equals(by.toString()))))
                .thenReturn(List.of(firstRoot, secondRoot));
        when(firstRoot.findElements(argThat(by -> child.equals(by.toString())))).thenReturn(List.of(firstChild));
        when(secondRoot.findElements(argThat(by -> child.equals(by.toString())))).thenReturn(List.of(secondChild));

        ElementTarget children = descendant(located(ShaftLocator.css(".root")), ShaftLocator.css(".child"));
        Assert.assertEquals(toBy(nth(children, 1)).findElements(driver), List.of(secondChild));
        Assert.assertTrue(toBy(nth(children, 2)).findElements(driver).isEmpty());
    }

    private static Method method(String name, Class<?>... parameterTypes) {
        try {
            return ElementTarget.class.getMethod(name, parameterTypes);
        } catch (NoSuchMethodException exception) {
            Assert.fail("Missing ElementTarget method: " + name, exception);
            throw new AssertionError(exception);
        }
    }

    private static ElementTarget located(By locator) {
        return invoke(method("located", By.class), null, locator);
    }

    private static ElementTarget located(ShaftLocator locator) {
        return invoke(method("located", ShaftLocator.class), null, locator);
    }

    private static ElementTarget descendant(ElementTarget target, By locator) {
        return invoke(method("descendant", By.class), target, locator);
    }

    private static ElementTarget descendant(ElementTarget target, ShaftLocator locator) {
        return invoke(method("descendant", ShaftLocator.class), target, locator);
    }

    private static ElementTarget nth(ElementTarget target, int index) {
        return invoke(method("nth", int.class), target, index);
    }

    private static By toBy(ElementTarget target) {
        return invoke(method("toBy"), target);
    }

    private static Locator toPlaywrightLocator(ElementTarget target, Page page) {
        return invoke(method("toPlaywrightLocator", Page.class), target, page);
    }

    @SuppressWarnings("unchecked")
    private static <T> T invoke(Method method, Object target, Object... arguments) {
        try {
            return (T) method.invoke(target, arguments);
        } catch (InvocationTargetException exception) {
            if (exception.getCause() instanceof RuntimeException runtimeException) {
                throw runtimeException;
            }
            if (exception.getCause() instanceof Error error) {
                throw error;
            }
            throw new AssertionError(exception.getCause());
        } catch (IllegalAccessException exception) {
            throw new AssertionError(exception);
        }
    }
}
