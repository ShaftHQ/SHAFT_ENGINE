package testPackage.unitTests;

import com.shaft.validation.Validations;
import com.shaft.validation.WebValidations;
import com.shaft.validation.internal.WebValidationsBuilder;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

public class WebValidationsEntryPointTest {

    @Test(groups = {"unit"})
    public void assertThatShouldReturnWebValidationsBuilder() {
        Object builder = WebValidations.assertThat();
        assertNotNull(builder);
        assertTrue(builder instanceof WebValidationsBuilder,
            "assertThat() must return WebValidationsBuilder, got: " + builder.getClass());
    }

    @Test(groups = {"unit"})
    public void verifyThatShouldReturnWebValidationsBuilder() {
        Object builder = WebValidations.verifyThat();
        assertNotNull(builder);
        assertTrue(builder instanceof WebValidationsBuilder,
            "verifyThat() must return WebValidationsBuilder, got: " + builder.getClass());
    }

    @Test(groups = {"unit"})
    public void webValidationsShouldBeDistinctFromCoreValidations() {
        Class<?> coreBuilderClass = Validations.assertThat().getClass();
        Class<?> webBuilderClass = WebValidations.assertThat().getClass();
        assertNotEquals(coreBuilderClass, webBuilderClass,
            "WebValidationsBuilder must be a distinct subclass of ValidationsBuilder");
    }

    @Test(groups = {"unit"})
    public void webValidationsBuilderShouldExposeElementAndBrowserMethods() {
        WebValidationsBuilder builder = WebValidations.assertThat();
        assertNotNull(builder);
        boolean hasElement = false, hasBrowser = false;
        for (java.lang.reflect.Method m : builder.getClass().getMethods()) {
            if ("element".equals(m.getName())) hasElement = true;
            if ("browser".equals(m.getName())) hasBrowser = true;
        }
        assertTrue(hasElement, "WebValidationsBuilder must expose element()");
        assertTrue(hasBrowser, "WebValidationsBuilder must expose browser()");
    }
}
