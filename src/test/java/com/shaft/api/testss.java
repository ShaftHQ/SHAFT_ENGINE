package com.shaft.api;

import com.shaft.driver.SHAFT;
import io.qameta.allure.TmsLinks;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.http.ContentType;
import io.restassured.response.Response;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

/**
 * @author Kyrillos Nageh
 */
public class testss {
    SHAFT.API api;


    @BeforeClass
    public void setUp() {
       // api = new SHAFT.API("http://www.dneonline.com");
    }

    @Test
    public void testCreateUser() {


    String requestBody= "    {\n" +
            "        \"name\": \"Test user1\",\n" +
            "        \"job\": \"leader\"\n" +
            "    }";

        api = new SHAFT.API("https://reqres.in/api/");
       //api.addHeader("Content-Type", "application/json");
        Response response = api.post("users")
                .setContentType("application/json")
                        .setRequestBody(requestBody)
                                .performRequest();
        System.out.println("Response Body: " + response.getBody().asString());
    }
    @Test
    public void testAddOperation() {
        /*
        AddRequest request = AddRequest.builder()
                .intA(5)
                .intB(10)
                .build();

        AddResponse response = calculatorService.add(request);
        Assert.assertEquals(response.getAddResult(), 15);

         */

        String requestXml = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\">\n" +
                "   <soapenv:Body>\n" +
                "      <tem:Multiply xmlns:tem=\"http://tempuri.org/\">\n" +
                "         <tem:intA>10</tem:intA>\n" +
                "         <tem:intB>2</tem:intB>\n" +
                "      </tem:Multiply>\n" +
                "   </soapenv:Body>\n" +
                "</soapenv:Envelope>";

        api = new SHAFT.API("");
       api.addHeader("Content-Type", "text/xml; charset=UTF-8");
        api.addHeader("SOAPAction", "http://tempuri.org/" + "Multiply");

        System.out.println("Request Body: " + requestXml);

        Response response = api.post("http://www.dneonline.com/calculator.asmx")
                .setRequestBody(requestXml)
                .performRequest();

        String Result =RestActions.getResponseXMLValue(response, "AddResponse/AddResult");

        SHAFT.Report.log(">>>>>>>>>>>> Test result >>>> " + Result);

/*
        Response response = given()
                .header("Content-Type", "text/xml")
                .contentType(ContentType.XML)
                .header("SOAPAction", "http://tempuri.org/" + "Multiply")
                .body(requestXml)
                .when()
                .post("http://www.dneonline.com/calculator.asmx")
                .then()
                .statusCode(200)  // Validate status code directly
                .extract().response();

        String Result =RestActions.getResponseXMLValue(response, "AddResponse/AddResult");

        SHAFT.Report.log(">>>>>>>>>>>> Test result >>>> " + Result);
*/
    }

}

