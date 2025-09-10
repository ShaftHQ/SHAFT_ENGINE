package com.shaft.api;

import com.shaft.driver.SHAFT;
import io.restassured.path.json.JsonPath;
import io.restassured.response.Response;
import org.testng.annotations.AfterSuite;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;


public class ApiPerformanceReportTest {
    ThreadLocal<SHAFT.API> api = new ThreadLocal<>();

    @BeforeMethod(onlyForGroups = {"restful-booker"})
    public void setupRestfulBooker() {
        api.set(new SHAFT.API("https://restful-booker.herokuapp.com/"));
    }

    @Test(groups = {"restful-booker"})
    public void testALogin() {
        Response response = api.get().post("auth").
                setContentType("application/json").
                setRequestBody("{\n" +
                        "    \"username\" : \"admin\",\n" +
                        "    \"password\" : \"password123\"\n" +
                        "}").
                perform();


        JsonPath jsonPath = response.jsonPath();
        System.out.println(jsonPath.get("token").toString());


        var token = response.path("token").toString();
        token = response.jsonPath().get("token").toString();
        System.out.println("The Tonken is: " + token);

    }

    @Test(groups = {"restful-booker"})
    public void testGetALLBookingIds() {
        api.get().get("booking").
                perform();
    }

    @Test(groups = {"restful-booker"})
    public void testGetBookingByName() {
        List<List<Object>> parameters = Arrays.asList(Arrays.asList("firstname", "Jim"), Arrays.asList("lastname", "Brown"));
        api.get().get("booking").
                setParameters(parameters, RestActions.ParametersType.QUERY).
                perform();
    }

    @Test(groups = {"restful-booker"})
    public void testGetBooking() {
        api.get().get("booking/1").
                perform();
    }

    @Test(groups = {"restful-booker"})
    public void testCreateBookingJSON() {
        api.get().post("booking").
                setContentType("application/json").
                setRequestBody("{\"firstname\":\"Jim\",\"lastname\":\"Brown\",\"totalprice\":111,\n" +
                        " \"depositpaid\":true,\"bookingdates\":{\"checkin\":\"2018-01-01\",\"checkout\":\"2019-01-01\"},\n" +
                        " \"additionalneeds\":\"Breakfast\"}").
                perform();
    }

    @BeforeMethod(onlyForGroups = {"reqres"})
    public void setupRegress() {
        api.set(new SHAFT.API("https://reqres.in/"));
    }

    @Test(groups = {"reqres"})
    public void testGetUsers() {
        api.get().addHeader("x-api-key", "reqres-free-v1");
        List<List<Object>> parameters = List.of(Arrays.asList("page", "2"));
        api.get().get("api/users")
                .setParameters(parameters, RestActions.ParametersType.QUERY)
                .perform();
    }

    @Test(groups = {"reqres"})
    public void testGetUser() {
        api.get().addHeader("x-api-key", "reqres-free-v1");
        api.get().get("api/users/2").perform();
    }

    @Test(groups = {"reqres"})
    public void testCreateUser() {
        api.get().addHeader("x-api-key", "reqres-free-v1");
        api.get().post("api/users")
                .setContentType("application/json")
                .setRequestBody("{\n" +
                        "  \"name\": \"morpheus\",\n" +
                        "  \"job\": \"leader\"\n" +
                        "}")
                .perform();
    }

    @Test(groups = {"reqres"})
    public void testRegisterUser() {
        api.get().addHeader("x-api-key", "reqres-free-v1");
        api.get().post("api/register")
                .setContentType("application/json")
                .setRequestBody("{\n" +
                        "    \"email\": \"eve.holt@reqres.in\",\n" +
                        "    \"password\": \"pistol\"\n" +
                        "}")
                .perform();
    }

    @Test(groups = {"reqres"})
    public void testLoginUser() {
        api.get().addHeader("x-api-key", "reqres-free-v1");
        api.get().post("api/login")
                .setContentType("application/json")
                .setRequestBody("{\n" +
                        "    \"email\": \"eve.holt@reqres.in\",\n" +
                        "    \"password\": \"cityslicka\"\n" +
                        "}")
                .perform();
    }

    @Test(groups = {"reqres"})
    public void testDelayedResponse() {
        api.get().addHeader("x-api-key", "reqres-free-v1");
        List<List<Object>> parameters = List.of(Arrays.asList("delay", "3"));
        api.get().get("api/users")
                .setParameters(parameters, RestActions.ParametersType.QUERY)
                .perform();
    }

    @BeforeMethod(onlyForGroups = {"fakerestapi"})
    public void setupFakerestapi() {
        api.set(new SHAFT.API("https://fakerestapi.azurewebsites.net/api/v1/"));
    }

    @Test(groups = {"fakerestapi"})
    public void testGetActivities() {
        api.get().get("Activities/1").perform();
    }

    @Test(groups = {"fakerestapi"})
    public void testGetActivity() {
        api.get().get("Activities/1").perform();
    }

    @Test(groups = {"fakerestapi"})
    public void testGetAuthors() {
        api.get().get("Authors").perform();
    }

    @Test(groups = {"fakerestapi"})
    public void testGetAuthor() {
        api.get().get("Authors/1").perform();
    }

    @Test(groups = {"fakerestapi"})
    public void testGetAuthorBook() {
        api.get().get("Authors/authors/books/1").perform();
    }

    @Test(groups = {"fakerestapi"})
    public void testGetBooks() {
        api.get().get("Books").perform();
    }

    @BeforeMethod(onlyForGroups = {"demoblaze"})
    public void setupDemoblaze() {
        api.set(new SHAFT.API("https://api.demoblaze.com/"));
    }

    @Test(groups = {"demoblaze"})
    public void addToCart() {
        api.get().post("addtocart")
                .setContentType("application/json")
                .setRequestBody("{\n" +
                        "\"cookie\":\"dGVzdGNiYTEyMzE3MjI0MDI=\",\n" +
                        "\"flag\":true,\n" +
                        "\"id\" :\"995bed43-ab62-2940-c093-b42b2cc4d887\",\n" +
                        "\"prod_id\":1\n" +
                        "}")
                .perform();
    }

    @Test(groups = {"demoblaze"})
    public void deleteCart() {
        api.get().post("deletecart")
                .setContentType("application/json")
                .setRequestBody("{\"id\": \"995bed43-ab62-2940-c093-b42b2cc4d887\"}")
                .perform();
    }

    @Test(groups = {"demoblaze"})
    public  void viewCart() {
        api.get().post("viewcart")
                .setContentType("application/json")
                .setRequestBody("{\n" +
                        "    \"cookie\": \"dGVzdGNiYTEyMzE3MjI0MDI=\", \n" +
                        "    \"flag\": true\n" +
                        "}")
                .perform();
    }

    @BeforeMethod
    public void cleanup() {
        api = new ThreadLocal<>();
    }

    @AfterSuite
    public void afterSuite() throws IOException {
        //RequestBuilder.generatePerformanceReport("src/test/resources/report.html");
        //RequestBuilder.printPerformanceReport();
        // HTMLPerformanceReport.generatePerformanceReport();
    }

}
