package com.shaft.tools.internal.tms;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.restassured.config.RestAssuredConfig;
import io.restassured.config.SSLConfig;
import io.restassured.http.ContentType;
import io.restassured.response.Response;
import io.restassured.specification.RequestSpecification;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.Base64;
import java.util.Calendar;
import java.util.List;

import static io.restassured.RestAssured.*;
import static io.restassured.config.EncoderConfig.encoderConfig;


public class XrayIntegrationHelper {

    private static final String authType = SHAFT.Properties.jira.authType() + " ";
    private static final String _ProjectKey = SHAFT.Properties.jira.projectKey();
    private static String _JiraAuthorization = SHAFT.Properties.jira.authorization();
    private static String _TestExecutionID = null;

    private static void setup() {
        baseURI = SHAFT.Properties.jira.url();
        if (authType.equals("Basic "))
            _JiraAuthorization = Base64.getEncoder().encodeToString(_JiraAuthorization.getBytes());
        given()
                .config(RestAssuredConfig.config().sslConfig(SSLConfig.sslConfig().allowAllHostnames()))
                .relaxedHTTPSValidation();
    }

    /**
     * Import cucumber results recorded in cucumber.jsom file through /import/execution/cucumber endpoint
     *
     * @param filepath > the report relative path
     */
    public static void importCucumberResults(String filepath) throws Exception {

        setup();
        String reportPath = FileActions.getInstance(true).getAbsolutePath(filepath);
        ReportManager.logDiscrete("uploading file: " + reportPath);
        ReportManager.logDiscrete("Length: " + new File(reportPath).length());

        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        JsonElement je = JsonParser.parseString(new String(Files.readAllBytes(Paths.get(reportPath))));
        String prettyJsonString = gson.toJson(je);

        try {
            Response response = given()
                    .contentType("application/json")
                    .header("Authorization", authType + _JiraAuthorization)
                    .body(prettyJsonString)
                    .expect().statusCode(200)
                    .when()
                    .post("/rest/raven/1.0/import/execution/cucumber").then().extract().response();

            _TestExecutionID = response.jsonPath().get("testExecIssue.key").toString();
            ReportManager.logDiscrete("ExecutionID: " + _TestExecutionID);
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
        }
    }

    /**
     * Import TestNG results recorded in testng-results.jsom file through /import/execution/testng?projectKey=[projectKey] endpoint
     *
     * @param executionName        > The execution name mentioned in JiraXray.properties
     * @param executionDescription > The execution Description mentioned in JiraXray.properties
     */
    public static void renameTestExecutionSuit(String executionName, String executionDescription) {

        if (_TestExecutionID == null) return;
        setup();
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

        String body = "{\r\n    \"fields\" : {\r\n       " +
                " \"summary\": " +
                "\"Execution results " + executionName + " | " + sdf.format(Calendar.getInstance().getTime()) + "\",\r\n        " +
                "\"description\": " +
                "\"" + executionDescription + "\"\r\n    }\r\n}";
        try {
            given()
                    .contentType("application/json")
                    .header("Authorization", authType + _JiraAuthorization)
                    .body(body)
                    .expect().statusCode(204)
                    .when()
                    .put("/rest/api/2/issue/" + _TestExecutionID).then().extract().response();

        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
        }
    }

    /**
     * Import TestNG results recorded in testng-results.jsom file through /import/execution/testng?projectKey=[projectKey] endpoint
     *
     * @param filepath > the report relative path
     */
    public static void importTestNGResults(String filepath) {
        setup();
        String reportPath = FileActions.getInstance(true).getAbsolutePath(filepath);
        ReportManager.logDiscrete("uploading file: " + reportPath);
        ReportManager.logDiscrete("Length: " + new File(reportPath).length());
        try {
            Response response = given()
                    .config(config().encoderConfig(encoderConfig().encodeContentTypeAs("multipart/form-data", ContentType.TEXT)))
                    .relaxedHTTPSValidation().contentType("multipart/form-data")
                    .header("Authorization", authType + _JiraAuthorization)
                    .multiPart(new File(reportPath))
                    .when()
                    .post("/rest/raven/1.0/import/execution/testng?projectKey=" + _ProjectKey)
                    .then().log().all().extract().response();

            _TestExecutionID = response.jsonPath().get("testExecIssue.key").toString();
            ReportManager.logDiscrete("ExecutionID: " + _TestExecutionID);

        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
        }
    }

    /**
     * Create JIRA Bug to report execution failure.
     *
     * @param files        -> list of the failed testcase attachments.
     * @param testCaseName -> the failed testcase name
     * @param description  --> the failed test case execution log
     * @return String bugID
     */
    public static String createIssue(List<String> files, String testCaseName, String description) {
        setup();
        try {
            String issueRequestBody = """
                    {
                      "fields":{
                        "project":{
                          "key":"${PROJECT_KEY}"
                        },
                        "summary":"${BUG_SUMMERY}",
                        "description":"Reported By SHAFT Automation Engine|| Execution Log ${BUG_DESCRIPTION}",
                        "assignee":{
                          "name":"${ASSIGNEE_NAME}"
                        },
                        "issuetype":{
                          "name":"Bug"
                        }
                      }
                    }
                    """;
            Response response = given()
                    .config(config().encoderConfig(encoderConfig().encodeContentTypeAs("application/json", ContentType.JSON)))
                    .relaxedHTTPSValidation().contentType("application/json")
                    .header("Authorization", authType + _JiraAuthorization)
                    .when()
                    .body(issueRequestBody
                            .replace("${PROJECT_KEY}", _ProjectKey)
                            .replace("${BUG_SUMMERY}", "Execution Bug: " + testCaseName)
                            .replace("${BUG_DESCRIPTION}", description
                                    .replaceAll("[^a-zA-Z0-9.?=*$%@#&!<>|\\{\\}\\[\\]\"' /]", "")
                                    .replaceAll("\"", "'")
                            )
                            .replace("${ASSIGNEE_NAME}", SHAFT.Properties.jira.assignee())
                    )
                    .post("/rest/api/2/issue")
                    .then().log().all().extract().response();

            String id = response.jsonPath().get("key").toString();

            ReportManager.logDiscrete("BugID: " + id);
            attachFilesToIssue(id, files);
            return id;

        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
            return null;
        }
    }

    /**
     * Update the created issue with the attachments.
     *
     * @param issueID -> the created bug ID.
     * @param files   -> list of the failed testcase attachments.
     */
    @SuppressWarnings("SpellCheckingInspection")
    public static void attachFilesToIssue(String issueID, List<String> files) {
        setup();
        try {
            RequestSpecification req = given()
                    .relaxedHTTPSValidation().contentType(ContentType.MULTIPART)
                    .header("Authorization", authType + _JiraAuthorization)
                    .header("X-Atlassian-Token", "nocheck");
            for (String file : files)
                req.multiPart("file", new File(file));

            ReportManager.logDiscrete("BugID: " + issueID);
            req.when()
                    .post("/rest/api/2/issue/" + issueID + "/attachments")
                    .then().log().all().extract().response();
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
        }
    }

    /**
     * Link any jira 2 tickets using the tickets IDs through PUT API request. the covered relation is relates.
     *
     * @param ticketID   -> the main ticket ID.
     * @param linkedToID -> the one to be linked to.
     */
    public static void link2Tickets(String ticketID, String linkedToID) {
        setup();
        try {
            String linkJIRATicketRequestBody = """
                    {
                       "update":{
                         "issuelinks":[
                           {
                             "add":{
                               "type":{
                                 "name":"Relates"
                               },
                               "outwardIssue":{
                                 "key":"${TICKET_ID}"
                               }
                             }
                           }
                         ]
                       }
                     }
                    """;
            given()
                    .config(config().encoderConfig(encoderConfig().encodeContentTypeAs("application/json", ContentType.JSON)))
                    .relaxedHTTPSValidation().contentType("application/json")
                    .header("Authorization", authType + _JiraAuthorization)
                    .when()
                    .body(linkJIRATicketRequestBody.replace("${TICKET_ID}", linkedToID))
                    .put("/rest/api/2/issue/" + ticketID)
                    .then().log().all().extract().response();
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
        }
    }
}