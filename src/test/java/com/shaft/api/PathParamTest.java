package com.shaft.api;

import com.shaft.driver.SHAFT;
import org.testng.annotations.*;
import java.util.*;

public class PathParamTest {
    private final SHAFT.API api = new SHAFT.API("https://petstore.swagger.io/v2");
    private static final String GET_USER_BY_USERNAME = "/user/{username}";
    private static final String GET_RESOURCE_BY_USERNAME = "/{resource}/{username}";

    /**
     * Test fetching user details by username using a Map for path parameters.
     * This test verifies that the API correctly substitutes a path parameter provided as a Map.
     */
    @Test(description = "Validate fetching user details using a Map for path parameters")
    public void testGetUserByUsernameWithMap() {
        // Path parameter
        Map<String, Object> parameters = Map.of("username", "string");

        // Perform the GET request and validate
        api.get(GET_USER_BY_USERNAME)
                .setPathParameters(parameters)
                .perform();

        api.assertThatResponse().extractedJsonValue("username").isEqualTo("string");
    }

    /**
     * Test fetching user details by username using a single value for the placeholder.
     * This test ensures the API handles a single value substitution correctly.
     */
    @Test(description = "Validate fetching user details using a single value for the path parameter")
    public void testGetUserByUsernameWithValue() {
        api.get(GET_USER_BY_USERNAME)
                .setPathParameters("string") // Direct value substitution
                .perform();

        api.assertThatResponse().extractedJsonValue("username").isEqualTo("string");
    }

    /**
     * Test fetching a resource using multiple path parameters.
     * This test ensures the API supports multiple dynamic placeholders.
     */
    @Test(description = "Validate fetching a resource using multiple dynamic path parameters")
    public void testGetResourceWithMultiplePathParameters() {
        api.get(GET_RESOURCE_BY_USERNAME)
                .setPathParameters("user", "string") // Multiple values for placeholders
                .perform();

        api.assertThatResponse().extractedJsonValue("username").isEqualTo("string");
    }
}
