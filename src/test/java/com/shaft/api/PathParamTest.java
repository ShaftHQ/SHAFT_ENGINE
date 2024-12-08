package com.shaft.api;

import com.shaft.driver.SHAFT;
import org.testng.annotations.*;
import java.util.*;


public class PathParamTest {
    // Initialize SHAFT.API with the base URI
    SHAFT.API api = new SHAFT.API("https://petstore.swagger.io/v2");
    public static final String GET_USER_BY_USERNAME = "/user/{username}";     // Get user by username

    @Test
    public void GetUserByUsernameMap() {

        Map<String, Object> parameters = Map.of(
                "username", "string");
        // Perform the GET request
        api.get(GET_USER_BY_USERNAME) // Endpoint with a placeholder
                .setPathParameters(parameters) // Substitute the placeholder dynamically
                .setTargetStatusCode(200) // Expected status code
                .perform(); // Execute the request

        SHAFT.Report.log(">>>>>> Response Body:" + api.getResponseBody());
    }

    @Test
    public void GetUserByUsernameValue() {
        // Perform the GET request
        api.get("/user/{username}") // Endpoint with a placeholder
                .setPathParameters( "string") // Pass the value for the placeholder
                .setContentType("application/json")
                .setTargetStatusCode(200)
                .perform();

        SHAFT.Report.log(">>>>>> Response Body:" + api.getResponseBody());
    }

    @Test
    public void N_GetUserByUsernameTest3() {
        api.get("/{resource}/{username}")
                .setContentType("application/json")
                .setPathParameters("user", "string")
                .setTargetStatusCode(200)
                .perform();

        SHAFT.Report.log("Response Body: " + api.getResponseBody());
    }
}