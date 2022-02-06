package com.shaft.tools.tms;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import com.shaft.cli.FileActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import io.restassured.config.RestAssuredConfig;
import io.restassured.config.SSLConfig;
import io.restassured.http.ContentType;
import io.restassured.response.Response;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.Base64;
import java.util.Calendar;
import static io.restassured.RestAssured.*;
import static io.restassured.config.EncoderConfig.encoderConfig;


public class XrayIntegrationHelper {

     private static final String _JiraAuthorization =
                Base64.getEncoder().encodeToString(System.getProperty("authorization").trim().getBytes());
     private static final String _ProjectKey = System.getProperty("projectKey").trim();
     private static String _TestExecutionID = null;

     private static void setup() {
         baseURI = System.getProperty("jiraUrl");
         given()
                 .config(RestAssuredConfig.config().sslConfig(SSLConfig.sslConfig().allowAllHostnames()))
                 .relaxedHTTPSValidation();
     }

    /**
     * Import cucumber results recorded in cucumber.jsom file through /import/execution/cucumber endpoint
     * @param filepath > the report relative path
     *
     */
    public static void importCucumberResults(String filepath) throws Exception {

        setup();
        String reportPath = FileActions.getAbsolutePath(filepath);
        ReportManager.logDiscrete("uploading file: "+reportPath);
        ReportManager.logDiscrete("Length: "+new File(reportPath).length());

        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        JsonElement je = JsonParser.parseString( new String(Files.readAllBytes(Paths.get(reportPath))));
        String prettyJsonString = gson.toJson(je);

        try {
            Response response =given()
                    .contentType("application/json")
                    .header("Authorization", "Basic " + _JiraAuthorization)
                    .body(prettyJsonString)
                    .expect().statusCode(200)
                    .when()
                    .post( "/rest/raven/1.0/import/execution/cucumber").then().extract().response();

            _TestExecutionID=response.jsonPath().get("testExecIssue.key").toString();
            ReportManager.logDiscrete("ExecutionID: "+_TestExecutionID);
        }catch (Exception e){
            ReportManagerHelper.log(e);
        }
    }

    /**
     * Import TestNG results recorded in testng-results.jsom file through /import/execution/testng?projectKey=[projectKey] endpoint
     * @param  executionName  > The execution name mentioned in JiraXray.properties
     * @param  executionDescription > The execution Description mentioned in JiraXray.properties
     */
    public static void renameTestExecutionSuit(String executionName, String executionDescription) {

         if (_TestExecutionID==null) return;
         setup();
         SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

         String body= "{\r\n    \"fields\" : {\r\n       " +
                 " \"summary\": " +
                 "\"Execution results "+executionName+" | "+sdf.format(Calendar.getInstance().getTime())+"\",\r\n        " +
                 "\"description\": " +
                 "\""+executionDescription+"\"\r\n    }\r\n}";
        try {
            given()
                    .contentType("application/json")
                    .header("Authorization", "Basic " + _JiraAuthorization)
                    .body(body)
                    .expect().statusCode(204)
                    .when()
                    .put( "/rest/api/2/issue/" +_TestExecutionID).then().extract().response();

        }catch (Exception e){
            ReportManagerHelper.log(e);
        }
    }

    /**
     * Import TestNG results recorded in testng-results.jsom file through /import/execution/testng?projectKey=[projectKey] endpoint
     * @param filepath > the report relative path
     *
     */
    public static void importTestNGResults(String filepath) throws IOException {
        setup();
        String reportPath = FileActions.getAbsolutePath(filepath);
        ReportManager.logDiscrete("uploading file: "+reportPath);
        ReportManager.logDiscrete("Length: "+new File(reportPath).length());
        try {
            Response response = given()
                    .config(config().encoderConfig(encoderConfig().encodeContentTypeAs("multipart/form-data", ContentType.TEXT)))
                    .relaxedHTTPSValidation().contentType("multipart/form-data")
                    .header("Authorization", "Basic " + _JiraAuthorization)
                    .multiPart(new File(reportPath))
                    .when()
                    .post("/rest/raven/1.0/import/execution/testng?projectKey="+_ProjectKey)
                    .then().log().all().extract().response();

            _TestExecutionID=response.jsonPath().get("testExecIssue.key").toString();
            ReportManager.logDiscrete("ExecutionID: "+_TestExecutionID);

        }catch (Exception e){
            ReportManagerHelper.log(e);
        }
    }
}