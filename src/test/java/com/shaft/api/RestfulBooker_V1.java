package com.shaft.api;

import com.shaft.driver.SHAFT;
import io.restassured.RestAssured;
import io.restassured.path.json.JsonPath;
import io.restassured.response.Response;
import org.testng.annotations.AfterSuite;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.List;

import static io.restassured.RestAssured.given;

/**
 * @author Kyrillos Nageh
 */
public class RestfulBooker_V1 {
        String token;
        SHAFT.API api = new SHAFT.API("https://restful-booker.herokuapp.com/");

        @Test
        public void testALogin() {

            Response response = api.post("auth").
                    setContentType("application/json").
                    setRequestBody("{\n" +
                            "    \"username\" : \"admin\",\n" +
                            "    \"password\" : \"password123\"\n" +
                            "}").
                    perform();


            JsonPath jsonPath = response.jsonPath();
            System.out.println(jsonPath.get("token").toString());


            token = response.path("token").toString();
            token = response.jsonPath().get("token").toString();
            System.out.println("The Tonken is: " + token);

        }

        @Test
        public void testGetALLBookingIds() {

           api.get("booking").
                    perform();

        }

        @Test
        public void testGetBookingByName() {
            List<List<Object>> parameters = Arrays.asList(Arrays.asList("firstname", "Jim"), Arrays.asList("lastname", "Brown"));

            api.get("booking").
                    setParameters(parameters, RestActions.ParametersType.QUERY).
                    perform();

        }
/*
        @Test
        public void testGetBookingByCheckInDateAndCheckOutDate() {
            List<List<Object>> parameters = Arrays.asList(Arrays.asList("checkin", "2022-01-01"), Arrays.asList("checkout", "2022-01-02"));

            api.get("/booking").
                    setParameters(parameters, RestActions.ParametersType.QUERY).
                    perform();

        }*/

        @Test
        public void testGetBooking() {

            api.get("booking/1").
                    perform();

        }

        @Test
        public void testCreateBookingJSON() {
           api.post("booking").
                    setContentType("application/json").
                    setRequestBody("{\"firstname\":\"Jim\",\"lastname\":\"Brown\",\"totalprice\":111,\n" +
                            " \"depositpaid\":true,\"bookingdates\":{\"checkin\":\"2018-01-01\",\"checkout\":\"2019-01-01\"},\n" +
                            " \"additionalneeds\":\"Breakfast\"}").
                    perform();
        }

        @Test
        public void testGetUsers() {
            SHAFT.API api = new SHAFT.API("https://reqres.in/");
            List<List<Object>> parameters = Arrays.asList(Arrays.asList("page", "2"));
            api.get("api/users")
                    .setParameters(parameters, RestActions.ParametersType.QUERY)
                    .perform();
        }

        @Test
        public void testGetUser() {
            SHAFT.API api = new SHAFT.API("https://reqres.in/");
            api.get("api/users/2").perform();
        }

        @Test
        public void testCreateUser() {
            SHAFT.API api = new SHAFT.API("https://reqres.in/");
            api.post("api/users")
                    .setContentType("application/json")
                    .setRequestBody("{\n" +
                            "  \"name\": \"morpheus\",\n" +
                            "  \"job\": \"leader\"\n" +
                            "}")
                    .perform();
        }

    @AfterSuite
    public void afterSuite() {
        RequestBuilder.generatePerformanceReport();
    }

    }
