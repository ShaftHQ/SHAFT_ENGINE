package com.shaft.api;

import com.shaft.driver.SHAFT;
import org.testng.annotations.*;
import java.util.*;


public class PathParamTest {
    // Initialize SHAFT.API with the base URI
    SHAFT.API api = new SHAFT.API("https://petstore.swagger.io/v2");
    public static final String GET_USER_BY_USERNAME = "/user/{username}";     // Get user by username

    @Test
    public void GetUserByUsernameTest() {
        // Define the path parameter as a key-value pair
        List<List<Object>> parameters = Arrays.asList(
                Arrays.asList("username", "string") // Replace "user1" with the desired test username
        );

        // Perform the GET request
        api.get(GET_USER_BY_USERNAME) // Endpoint with a placeholder
                .setPathParameters(parameters) // Substitute the placeholder dynamically
                .setTargetStatusCode(200) // Expected status code
                .perform(); // Execute the request

        SHAFT.Report.log(">>>>>> Response Body:" + api.getResponseBody());
    }

    @Test
    public void GetUserByUsernameTest2() {
        // Perform the GET request
        api.get(GET_USER_BY_USERNAME) // Endpoint with a placeholder
                .setPathParam("username", "string") // Substitute the placeholder dynamically
                .setContentType("application/json")
                .setTargetStatusCode(200) // Expected status code
                .perform(); // Execute the request

        SHAFT.Report.log(">>>>>> Response Body:" + api.getResponseBody());
    }
}