package com.shaft.api.validation;

import com.shaft.api.validation.internal.RestValidationsBuilder;
import io.restassured.response.Response;
import org.mockito.Mockito;
import org.testng.annotations.Test;

import static org.testng.Assert.assertNotNull;

public class ApiValidationsTest {

    @Test
    public void verifyThatReturnsNonNullBuilder() {
        Response mock = Mockito.mock(Response.class);
        RestValidationsBuilder builder = ApiValidations.verifyThat(mock);
        assertNotNull(builder);
    }

    @Test
    public void assertThatReturnsNonNullBuilder() {
        Response mock = Mockito.mock(Response.class);
        RestValidationsBuilder builder = ApiValidations.assertThat(mock);
        assertNotNull(builder);
    }
}
