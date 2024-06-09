package com.shaft.api;

import com.shaft.api.RestActions;
import com.shaft.driver.SHAFT;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.List;

public class testPostRequest {
    SHAFT.API api = new SHAFT.API("http://localhost:5000");
    String body = """
            {
              "title": "New Course 1",
              "description": "Full description of the new course in Arabic"
            }
            """;
    List<List<Object>> parameters = Arrays.asList(Arrays.asList("access_key", "your-access-key"));

   // @Test
    public void PostRequest(){

        api.post("/courses")
                .setRequestBody(body)
                .setParameters(parameters, RestActions.ParametersType.QUERY)
                .setContentType("application/json")
                .perform();

        /*
        api.get("/courses")
                .setParameters(parameters, RestActions.ParametersType.QUERY)
                .setContentType("application/json")
                .perform();
         */
        String body = api.getResponseBody();
        int st =api.getResponse().statusCode();
        Assert.assertEquals(st,201);

        SHAFT.Report.log(">>>>>>>>>>>>> Test result" +body);
    }
}