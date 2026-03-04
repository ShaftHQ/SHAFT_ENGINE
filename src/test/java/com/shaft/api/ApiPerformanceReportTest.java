package com.shaft.api;

import com.shaft.driver.SHAFT;
import io.restassured.path.json.JsonPath;
import io.restassured.response.Response;
import org.testng.annotations.AfterSuite;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.AfterMethod;
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
                perform().getResponse();


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
        api.set(new SHAFT.API("https://jsonplaceholder.typicode.com/"));
    }

    @Test(groups = {"reqres"})
    public void testGetUsers() {
        List<List<Object>> parameters = List.of(Arrays.asList("_page", "2"));
        api.get().get("users")
                .setParameters(parameters, RestActions.ParametersType.QUERY)
                .perform();
    }

    @Test(groups = {"reqres"})
    public void testGetUser() {
        api.get().get("users/2").perform();
    }

    @Test(groups = {"reqres"})
    public void testCreateUser() {
        api.get().post("users")
                .setContentType("application/json")
                .setRequestBody("{\n" +
                        "  \"name\": \"morpheus\",\n" +
                        "  \"job\": \"leader\"\n" +
                        "}")
                .perform();
    }

    @Test(groups = {"reqres"})
    public void testCreateEmployee() {
        api.get().post("users")
                .setContentType("application/json")
                .setRequestBody("{\n" +
                        "    \"name\": \"eve.holt\",\n" +
                        "    \"job\": \"developer\"\n" +
                        "}")
                .perform();
    }

    @Test(groups = {"reqres"})
    public void testCreateQATester() {
        api.get().post("users")
                .setContentType("application/json")
                .setRequestBody("{\n" +
                        "    \"name\": \"eve.holt\",\n" +
                        "    \"job\": \"tester\"\n" +
                        "}")
                .perform();
    }

    @Test(groups = {"reqres"})
    public void testGetAllUsers() {
        api.get().get("users")
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

    @AfterMethod
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
