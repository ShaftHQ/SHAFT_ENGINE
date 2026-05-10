package com.shaft.api.validation;

import com.shaft.api.validation.internal.RestValidationsBuilder;
import com.shaft.tools.internal.support.JavaHelper;
import io.restassured.response.Response;
import org.mockito.Mockito;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertSame;

public class ApiValidationsEntryPointTest {

    @Test
    public void assertThatShouldReturnRestValidationsBuilder() {
        Response mock = Mockito.mock(Response.class);
        RestValidationsBuilder builder = ApiValidations.assertThat(mock);
        assertNotNull(builder, "assertThat must return a non-null RestValidationsBuilder");
        assertSame(builder.getClass(), RestValidationsBuilder.class);
    }

    @Test
    public void verifyThatShouldReturnRestValidationsBuilder() {
        Response mock = Mockito.mock(Response.class);
        RestValidationsBuilder builder = ApiValidations.verifyThat(mock);
        assertNotNull(builder, "verifyThat must return a non-null RestValidationsBuilder");
        assertSame(builder.getClass(), RestValidationsBuilder.class);
    }

    @Test
    public void apiValidationsShouldNotRequireSeleniumOnClasspath() {
        assertFalse(JavaHelper.isClassAvailable("org.openqa.selenium.WebDriver"),
            "WebDriver must not be on shaft-api test classpath — shaft-web is not a dependency");
    }
}
