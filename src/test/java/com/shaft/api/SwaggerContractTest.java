package com.shaft.api;

import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import io.restassured.response.Response;
import org.testng.annotations.*;

import java.util.HashMap;
import java.util.Map;

public class SwaggerContractTest {
    private final SHAFT.API api = new SHAFT.API("https://petstore.swagger.io/v2");
    private static final String GET_USER_BY_USERNAME = "/user/{username}";

    @Test(description = "Validate GET_USER_BY_USERNAME API against Swagger Schema")
    public void validateGetUserByUsername() {
        Map<String, Object> parameters = new HashMap<>();
        parameters.put("username", "string");

        Response response = api.get(GET_USER_BY_USERNAME)
                .setPathParameters(parameters)
                .perform();

        api.assertThatResponse()
                .extractedJsonValue("username")
                .isEqualTo("string")
                .perform();
    }

    @Test(description = "Validate CreateWithList API against Swagger Schema")
    public void validateCreateWithList() {
        String requestBody = "[\n" +
                " {\n" +
                " \"id\": 0,\n" +
                " \"username\": \"string\",\n" +
                " \"firstName\": \"string\",\n" +
                " \"lastName\": \"string\",\n" +
                " \"email\": test@SHAFT.com,\n" +
                " \"password\": \"string\",\n" +
                " \"phone\": \"string\",\n" +
                " \"userStatus\": 0\n" +
                " }\n" +
                "]";

        Response response = api.post("/user/createWithList")
                .setContentType("application/json")
                .setRequestBody(requestBody)
                .perform();

        api.assertThatResponse()
                .extractedJsonValue("username")
                .isEqualTo("string")
                .perform();
    }

    @Test(description = "Validate CreateWithList API with invalid payload")
    public void validateCreateWithListInvalidPayload() {
        SHAFT.Properties.api.set().swaggerValidationEnabled(false);

        String invalidRequestBody = "[\n" +
                " {\n" +
                " \"id\": 0,\n" +
                " \"username\": \"string\",\n" +
                " \"firstName\": \"string\",\n" +
                " \"lastName\": \"string\",\n" +
                " \"email\": 1,\n" +
                " \"password\": \"string\",\n" +
                " \"phone\": \"string\",\n" +
                " \"userStatus\": 0\n" +
                " }\n" +
                "]";

        Response response = api.post("/user/createWithList")
                .setContentType("application/json")
                .setRequestBody(invalidRequestBody)
                .setTargetStatusCode(400)
                .perform();
    }

}