package com.shaft.api;

import com.shaft.driver.SHAFT;
import io.restassured.RestAssured;
import io.restassured.builder.MultiPartSpecBuilder;
import io.restassured.http.ContentType;
import io.restassured.response.Response;
import io.restassured.specification.MultiPartSpecification;
import org.testng.annotations.Test;

import java.io.File;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;

public class TestUpload {
   SHAFT.API api;
    @Test
    public void testUploadWithList() {
        api = new SHAFT.API("http://localhost:3000/");
        SHAFT.Properties.api.set().swaggerValidationEnabled(false);

        List<List<Object>> parameters = Arrays.asList(
                Arrays.asList("image", new File("src/test/resources/search.PNG")),
                Arrays.asList("arabicText", "تست اوتوميشن")
        );

        api.post("upload")
                .setContentType("multipart/form-data; charset=UTF-8") // Ensure UTF-8 encoding
                .setParameters(parameters, RestActions.ParametersType.MULTIPART)
                .setTargetStatusCode(200)
                .perform();

        api.assertThatResponse()
                .extractedJsonValue("arabicText").isEqualTo("تست اوتوميشن");
        SHAFT.Report.log("Shaft API Response:" + api.getResponseBody());
    }

    @Test
    public void testUploadWithMap() {
        api = new SHAFT.API("http://localhost:3000/");
        SHAFT.Properties.api.set().swaggerValidationEnabled(false);

        Map<String, Object> parameters = new HashMap<>();
        parameters.put("image", new File("src/test/resources/search.PNG"));
        parameters.put("arabicText", "تست أوتوميشن");

        api.post("upload")
                .setContentType("multipart/form-data; charset=UTF-8") // Ensure UTF-8 encoding
                .setParameters(parameters, RestActions.ParametersType.MULTIPART)
                .setTargetStatusCode(200)
                .perform();

        api.assertThatResponse()
                .extractedJsonValue("arabicText").isEqualTo("تست أوتوميشن");
        SHAFT.Report.log("Shaft API Response:" + api.getResponseBody());
    }


}