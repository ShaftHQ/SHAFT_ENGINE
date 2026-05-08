package testPackage.unitTests;

import com.shaft.gui.internal.locator.SmartLocators;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class SmartLocatorsCoverageUnitTest {

    @Test
    public void inputFieldShouldBuildCombinedLocator() {
        By locator = SmartLocators.inputField("Username");
        String locatorAsString = locator.toString();

        Assert.assertNotNull(locator);
        Assert.assertTrue(locatorAsString.contains("placeholder"));
        Assert.assertTrue(locatorAsString.contains("textarea"));
        Assert.assertTrue(locatorAsString.contains("preceding::input"));
    }

    @Test
    public void clickableFieldShouldBuildCombinedLocator() {
        By locator = SmartLocators.clickableField("Login");
        String locatorAsString = locator.toString();

        Assert.assertNotNull(locator);
        Assert.assertTrue(locatorAsString.contains("//button"));
        Assert.assertTrue(locatorAsString.contains("//a"));
        Assert.assertTrue(locatorAsString.contains("following::button"));
    }

    @Test
    public void xpathBuilderShouldCoverAllPathStrategies() throws Exception {
        Class<?> pathStrategyClass = Class.forName("com.shaft.gui.internal.locator.SmartLocators$PathStrategy");
        Object[] strategies = pathStrategyClass.getEnumConstants();
        Method xpathBuilder = SmartLocators.class.getDeclaredMethod("xpathBuilder", pathStrategyClass, String.class);
        xpathBuilder.setAccessible(true);
        Object relativeByStrategy = getEnumConstant(pathStrategyClass, "INPUT_RELATIVE_STRAIGHT_RIGHT_OF_CONTAINS_TEXT");

        for (Object strategy : strategies) {
            By locator = (By) xpathBuilder.invoke(null, strategy, "UserName");
            String locatorAsString = locator.toString();

            Assert.assertNotNull(locator, "Locator should not be null for strategy: " + strategy);
            Assert.assertFalse(locatorAsString.isBlank(), "Locator string should not be blank for strategy: " + strategy);
            if (!strategy.equals(relativeByStrategy)) {
                Assert.assertTrue(locatorAsString.contains("By.xpath"), "Expected xpath strategy for: " + strategy);
                Assert.assertFalse(locatorAsString.endsWith("|"), "Locator should not end with separator for: " + strategy);
            }
        }
    }

    @Test
    public void nonNullArgumentsShouldBeEnforced() throws Exception {
        Assert.assertThrows(NullPointerException.class, () -> SmartLocators.inputField(null));
        Assert.assertThrows(NullPointerException.class, () -> SmartLocators.clickableField(null));

        Class<?> pathStrategyClass = Class.forName("com.shaft.gui.internal.locator.SmartLocators$PathStrategy");
        Method xpathBuilder = SmartLocators.class.getDeclaredMethod("xpathBuilder", pathStrategyClass, String.class);
        xpathBuilder.setAccessible(true);

        Assert.assertThrows(InvocationTargetException.class, () -> xpathBuilder.invoke(null, new Object[]{null, "Username"}));
        Assert.assertThrows(InvocationTargetException.class, () -> xpathBuilder.invoke(null, new Object[]{pathStrategyClass.getEnumConstants()[0], null}));
    }

    @SuppressWarnings({"rawtypes", "unchecked"})
    private static Object getEnumConstant(Class<?> enumClass, String constantName) {
        return Enum.valueOf((Class<? extends Enum>) enumClass.asSubclass(Enum.class), constantName);
    }
}
