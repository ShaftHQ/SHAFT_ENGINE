package com.shaft.tools.tms;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import io.restassured.config.RestAssuredConfig;
import io.restassured.config.SSLConfig;
import io.restassured.http.ContentType;
import io.restassured.response.Response;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.Base64;
import java.util.Calendar;

import static io.restassured.RestAssured.*;
import static io.restassured.config.EncoderConfig.encoderConfig;

public class XrayIntegrationManager {
     public static String _JiraAuthorization=
             Base64.getEncoder().encodeToString(System.getProperty("authorization").getBytes());
     public static String _ProjectKey= System.getProperty("projectKey");
     public static String _TestExecutionID=null;

     public static void setup() {
         baseURI = System.getProperty("jiraUrl");
         given()
                 .config(RestAssuredConfig.config().sslConfig(SSLConfig.sslConfig().allowAllHostnames()))
                 .relaxedHTTPSValidation();
     }

    public static void importCucumberResults(String filepath) throws Exception {

        setup();
        String reportPath = System.getProperty("user.dir")+"\\"+filepath;
        ReportManager.logDiscrete("uploading file: "+reportPath);
        ReportManager.logDiscrete("Length: "+new File(reportPath).length());

        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        JsonElement je = JsonParser.parseString( new String(Files.readAllBytes(Paths.get(reportPath))));
        String prettyJsonString = gson.toJson(je);
        ReportManager.logDiscrete("Pretty: \n"+ prettyJsonString);

        try {
            Response response =given()
                    .contentType("application/json")
                    .header("Authorization", "Basic " + _JiraAuthorization)
                    .body(prettyJsonString)
                    .expect().statusCode(200)
                    .when()
                    .post( "/rest/raven/1.0/import/execution/cucumber").then().extract().response();
            ReportManager.logDiscrete("#################"+ response.asString());
            _TestExecutionID=response.jsonPath().get("testExecIssue.key").toString();
            ReportManager.logDiscrete("ExecutionID: "+_TestExecutionID);
        }catch (Exception e){
            ReportManagerHelper.log(e);
        }
    }

    public static void renameTestExecutionSuit(String executionName, String executionDescription) {
         if (_TestExecutionID==null) return;
         setup();
        Calendar cal = Calendar.getInstance();
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

         String body= "{\r\n    \"fields\" : {\r\n       " +
                 " \"summary\": " +
                 "\"Execution results "+executionName+" | "+sdf.format(cal.getTime())+"\",\r\n        " +
                 "\"description\": " +
                 "\""+executionDescription+"\"\r\n    }\r\n}";
        try {
            Response response =given()
                    .contentType("application/json")
                    .header("Authorization", "Basic " + _JiraAuthorization)
                    .body(body)
                    .expect().statusCode(204)
                    .when()
                    .put( "/rest/api/2/issue/" +_TestExecutionID).then().extract().response();
            ReportManager.logDiscrete("#################"+ response.asString());

        }catch (Exception e){
            ReportManagerHelper.log(e);
        }
    }

    public static void importTestNGResults(String filepath) {
        setup();
        String reportPath = System.getProperty("user.dir")+"\\"+filepath;
        try {
            Response response = given()
                    .config(config().encoderConfig(encoderConfig().encodeContentTypeAs("multipart/form-data", ContentType.TEXT)))
                    .relaxedHTTPSValidation().contentType("multipart/form-data")
                    .header("Authorization", "Basic " + _JiraAuthorization)
                    .multiPart(new File(reportPath))
                    .when()
                    .post("/rest/raven/1.0/import/execution/testng?projectKey="+_ProjectKey)
                    .then().log().all().extract().response();

            ReportManager.logDiscrete("#################"+ response.asString());
            _TestExecutionID=response.jsonPath().get("testExecIssue.key").toString();
            ReportManager.logDiscrete("ExecutionID: "+_TestExecutionID);

        }catch (Exception e){
            ReportManagerHelper.log(e);
        }
    }
}