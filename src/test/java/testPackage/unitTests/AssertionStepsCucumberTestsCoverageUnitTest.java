package testPackage.unitTests;

import com.shaft.cucumber.AssertionSteps;
import com.shaft.driver.SHAFT;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Field;

import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;

public class AssertionStepsCucumberTestsCoverageUnitTest {
    private ThreadLocal<SHAFT.GUI.WebDriver> threadLocalDriver;
    private AssertionSteps assertionSteps;

    @BeforeMethod(alwaysRun = true)
    public void setup() {
        threadLocalDriver = new ThreadLocal<>();
        threadLocalDriver.set(mock(SHAFT.GUI.WebDriver.class, RETURNS_DEEP_STUBS));
        assertionSteps = new AssertionSteps(threadLocalDriver);
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        threadLocalDriver.remove();
    }

    @Test
    public void shouldCoverAssertionStepsMethods() {
        assertionSteps.assertBrowserAttributeEquals("Title", "SHAFT");
        assertionSteps.assertBrowserAttributeDoesNotEqual("Title", "Selenium");
        assertionSteps.assertBrowserAttributeContains("CurrentUrl", "example");
        assertionSteps.assertBrowserAttributeDoesNotContain("PageSource", "forbidden");
        assertionSteps.assertBrowserAttributeMatches("Title", ".*");
        assertionSteps.assertBrowserAttributeDoesNotMatch("Title", "^$");

        assertionSteps.assertelementDomAttributeEquals("value", "id", "searchBox", "SHAFT");
        assertionSteps.assertElementAttributeDoesNotEqual("value", "id", "searchBox", "Selenium");
        assertionSteps.assertElementAttributeContains("value", "id", "searchBox", "SHA");
        assertionSteps.assertElementAttributeDoesNotContain("value", "id", "searchBox", "XYZ");
        assertionSteps.assertElementAttributeMatches("value", "id", "searchBox", ".*");
        assertionSteps.assertElementAttributeDoesNotMatch("value", "id", "searchBox", "^$");

        assertionSteps.assertElementExists("id", "searchBox");
        assertionSteps.assertElementDoesNotExist("id", "ghostElement");

        assertionSteps.assertElementCSSPropertyEquals("color", "id", "searchBox", "rgb(0, 0, 0)");
        assertionSteps.assertElementCSSPropertyDoesNotEqual("color", "id", "searchBox", "rgb(255, 255, 255)");
        assertionSteps.assertElementCSSPropertyContains("font-family", "id", "searchBox", "Arial");
        assertionSteps.assertElementCSSPropertyDoesNotContain("font-family", "id", "searchBox", "Monospace");
        assertionSteps.assertElementCSSPropertyMatches("font-size", "id", "searchBox", "\\d+px");
        assertionSteps.assertElementCSSPropertyDoesNotMatch("font-size", "id", "searchBox", "abc");

        assertionSteps.assertElementMatchesOpenCV("id", "logo");
        assertionSteps.assertElementDoesNotMatchOpenCV("id", "logo");
        assertionSteps.assertElementMatchesExactEyes("id", "logo");
        assertionSteps.assertElementDoesNotMatchExactEyes("id", "logo");
        assertionSteps.assertElementMatchesStrictEyes("id", "logo");
        assertionSteps.assertElementDoesNotMatchStrictEyes("id", "logo");
        assertionSteps.assertElementMatchesContentEyes("id", "logo");
        assertionSteps.assertElementDoesNotMatchContentEyes("id", "logo");
        assertionSteps.assertElementMatchesLayoutEyes("id", "logo");
        assertionSteps.assertElementDoesNotMatchLayoutEyes("id", "logo");
    }

    @Test
    public void shouldCreateNonNullThreadLocalWhenConstructorReceivesNull() throws Exception {
        AssertionSteps nullDriverAssertionSteps = new AssertionSteps(null);
        Field driverField = AssertionSteps.class.getDeclaredField("driver");
        driverField.setAccessible(true);
        Assert.assertNotNull(driverField.get(nullDriverAssertionSteps));
    }
}
