package com.shaft.api;

import com.shaft.driver.SHAFT;
import io.restassured.response.Response;
import org.testng.annotations.*;
import java.util.HashMap;
import java.util.Map;


public class SwaggerContractTest {

    @Test(description = "Validate GET_USER_BY_USERNAME API against Swagger Schema")
    public void validateGetUserByUsername() {
        final SHAFT.API api = new SHAFT.API("https://petstore.swagger.io/v2");
        final String GET_USER_BY_USERNAME = "/user/{username}";
        // Define path parameters dynamically
        Map<String, Object> parameters = new HashMap<>();
        parameters.put("username", "string");

        // Perform API call
        Response response = api.get(GET_USER_BY_USERNAME)
                .setPathParameters(parameters)
                .perform();
    }

    @Test(description = "Validate CREATE_USER_WITH_LIST API against Swagger Schema")
    public void validateCreateUserWithList() {
        final SHAFT.API api = new SHAFT.API("https://petstore.swagger.io/v2");
        final String CREATE_USER_WITH_LIST = "/user/createWithList";
        // Define request body
        String requestBody = "[\n" +
                "  {\n" +
                "    \"id\": 0,\n" +
                "    \"username\": \"string\",\n" +
                "    \"firstName\": \"string\",\n" +
                "    \"lastName\": \"string\",\n" +
                "    \"email\": \"string\",\n" +
                "    \"password\": \"string\",\n" +
                "    \"phone\": \"string\",\n" +
                "    \"userStatus\": 0\n" +
                "  }\n" +
                "]";
        // Perform API call
        Response response = api.post(CREATE_USER_WITH_LIST)
                .setContentType("application/json")
                .setRequestBody(requestBody)
                .perform();
    }


    @Test(description = "Validate GET_ACTIVITY_BY_ID API against Swagger Schema")
    public void validateGetActivityById() {
        final SHAFT.API api = new SHAFT.API("https://fakerestapi.azurewebsites.net/api/v1");
        final String GET_ACTIVITY_BY_ID = "/Activities/{id}";
        // Define path parameters dynamically
        Map<String, Object> parameters = new HashMap<>();
        parameters.put("id", "2");

        // Perform API call
        Response response = api.get(GET_ACTIVITY_BY_ID)
                .setPathParameters(parameters)
                .perform();
    }
}