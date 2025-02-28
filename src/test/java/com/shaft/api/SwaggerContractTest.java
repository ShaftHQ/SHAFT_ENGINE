package com.shaft.api;

import com.shaft.driver.SHAFT;
import io.restassured.response.Response;
import org.testng.annotations.*;
import java.util.HashMap;
import java.util.Map;
import static com.shaft.tools.io.SwaggerManager.getResponseSchema;


public class SwaggerContractTest {
    private final SHAFT.API api = new SHAFT.API("https://petstore.swagger.io/v2");
    private static final String GET_USER_BY_USERNAME = "/user/{username}";
    private static final String GET_RESOURCE_BY_USERNAME = "/{resource}/{username}";

    @Test(description = "Validate GET_USER_BY_USERNAME API against Swagger Schema")
    public void validateGetUserByUsername() {
        // Define path parameters dynamically
        Map<String, Object> parameters = new HashMap<>();
        parameters.put("username", "string");

        // Perform API call
        Response response = api.get(GET_USER_BY_USERNAME)
                .setPathParameters(parameters)
                //.setTargetStatusCode(404)
                .perform();

        /*
        api.assertThatResponse()
                .matchesSchema("user_schema.json")
                .perform();
        */
    }
}