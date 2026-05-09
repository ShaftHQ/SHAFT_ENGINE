package com.shaft.api.validation;

import com.shaft.api.validation.internal.RestValidationsBuilder;
import com.shaft.tools.internal.support.JavaHelper;
import io.restassured.response.Response;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.*;

class ApiValidationsEntryPointTest {

    @Test
    void assertThatShouldReturnRestValidationsBuilder() {
        Response mock = Mockito.mock(Response.class);
        RestValidationsBuilder builder = ApiValidations.assertThat(mock);
        assertNotNull(builder, "assertThat must return a non-null RestValidationsBuilder");
        assertSame(RestValidationsBuilder.class, builder.getClass());
    }

    @Test
    void verifyThatShouldReturnRestValidationsBuilder() {
        Response mock = Mockito.mock(Response.class);
        RestValidationsBuilder builder = ApiValidations.verifyThat(mock);
        assertNotNull(builder, "verifyThat must return a non-null RestValidationsBuilder");
        assertSame(RestValidationsBuilder.class, builder.getClass());
    }

    @Test
    void apiValidationsShouldNotRequireSeleniumOnClasspath() {
        assertFalse(JavaHelper.isClassAvailable("org.openqa.selenium.WebDriver"),
            "WebDriver must not be on shaft-api test classpath — shaft-web is not a dependency");
    }
}
