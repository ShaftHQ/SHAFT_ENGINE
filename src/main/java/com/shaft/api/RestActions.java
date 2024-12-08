package com.shaft.api;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonParser;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.PathNotFoundException;
import com.jayway.jsonpath.spi.json.JsonOrgJsonProvider;
import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import eu.medsea.mimeutil.MimeUtil;
import eu.medsea.mimeutil.MimeUtil2;
import io.qameta.allure.restassured.AllureRestAssured;
import io.restassured.builder.MultiPartSpecBuilder;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.config.EncoderConfig;
import io.restassured.config.HttpClientConfig;
import io.restassured.config.RestAssuredConfig;
import io.restassured.http.ContentType;
import io.restassured.http.Cookie;
import io.restassured.http.Header;
import io.restassured.mapper.ObjectMapperType;
import io.restassured.path.json.exception.JsonPathException;
import io.restassured.path.xml.element.Node;
import io.restassured.path.xml.element.NodeChildren;
import io.restassured.response.Response;
import io.restassured.response.ResponseBody;
import io.restassured.specification.RequestSpecification;
import lombok.Getter;
import lombok.Setter;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.skyscreamer.jsonassert.JSONAssert;
import org.skyscreamer.jsonassert.JSONCompare;
import org.skyscreamer.jsonassert.JSONCompareMode;

import javax.xml.XMLConstants;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import java.io.*;
import java.net.URLConnection;
import java.util.*;
import java.util.concurrent.TimeUnit;

import static io.restassured.RestAssured.config;
import static io.restassured.RestAssured.given;

@SuppressWarnings("unused")
public class RestActions {
    private static final String ARGUMENT_SEPARATOR = "?";
    private static final String ERROR_NOT_FOUND = "Either actual value is \"null\" or couldn't find anything that matches with the desired ";
    private static final String ERROR_INCORRECT_JSONPATH = "Incorrect jsonPath ";
    private static final String ERROR_INCORRECT_XML_PATH = "Incorrect xmlPath ";
    private static final String ERROR_FAILED_TO_PARSE_JSON = "Failed to parse the JSON document";
    private static final String GRAPHQL_END_POINT = "graphql";
    static AllureRestAssured allureFilter = new AllureRestAssured()
            .setRequestAttachmentName("Request")
            .setResponseAttachmentName("Response");
    private static boolean AUTOMATICALLY_ASSERT_RESPONSE_STATUS_CODE = true;
    private static int HTTP_SOCKET_TIMEOUT;
    private static int HTTP_CONNECTION_TIMEOUT;
    private static int HTTP_CONNECTION_MANAGER_TIMEOUT;
    private final String serviceURI;
    private final Map<String, String> sessionHeaders;
    private final Map<String, Object> sessionCookies;
    private final RestAssuredConfig sessionConfig;
    @Getter
    @Setter
    private Response lastResponse;
    private String headerAuthorization;

    public RestActions(String serviceURI) {
        initializeSystemProperties();
        headerAuthorization = "";
        this.serviceURI = serviceURI;
        sessionCookies = new HashMap<>();
        sessionHeaders = new HashMap<>();
        sessionConfig = config();
    }

    public static RequestBuilder buildNewRequest(String serviceURI, String serviceName, RequestType requestType) {
        return new RequestBuilder(new RestActions(serviceURI), serviceName, requestType);
    }

    private static void passAction(String actionName, String testData, Object requestBody, RequestSpecification specs, Response response,
                                   Boolean isDiscrete, List<Object> expectedFileBodyAttachment) {
        reportActionResult(actionName, testData, requestBody, specs, response, isDiscrete, expectedFileBodyAttachment, true);
    }

    private static void failAction(String actionName, String testData, Object requestBody, RequestSpecification specs, Response response,
                                   Throwable... rootCauseException) {
        String message = reportActionResult(actionName, testData, requestBody, specs, response, false, null, false, rootCauseException);
        if (rootCauseException.length > 0) {
            FailureReporter.fail(RestActions.class, message, rootCauseException[0]);
        } else {
            FailureReporter.fail(message);
        }
    }

    protected static void passAction(String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(actionName, testData, null, null, null, true, null);
    }

    protected static void passAction(String testData, List<Object> expectedFileBodyAttachment) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(actionName, testData, null, null, null, true, expectedFileBodyAttachment);
    }

    static void passAction(String testData, Object requestBody, RequestSpecification specs, Response response) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(actionName, testData, requestBody, specs, response, false, null);
    }

    public static InputStream parseBodyToJson(Response response) {
        return parseBodyToJson(response.getBody());
    }

    public static InputStream parseBodyToJson(Object body) {
        try {
            return parseJsonBody(body);
        } catch (Exception e) {
            // response is not parsable to JSON
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            ObjectOutputStream oos;
            try {
                oos = new ObjectOutputStream(byteArrayOutputStream);
                oos.writeObject(body);
                oos.flush();
                oos.close();
                return new ByteArrayInputStream(byteArrayOutputStream.toByteArray());
            } catch (IOException ioe) {
                if (body.getClass().getName().toLowerCase().contains("restassured")) {
                    // if it's a string response body
                    return ((ResponseBody<?>) body).asInputStream();
                } else {
                    return new ByteArrayInputStream((body.toString()).getBytes());
                }
            }
        }
    }

    /**
     * Extracts the response body and returns it as a plain string
     *
     * @param response the target API response object
     * @return a string value that represents the response body
     */
    public static String getResponseBody(Response response) {
        return response.getBody().asString();
    }

    /**
     * Extracts a string value from the response body by parsing the target jsonpath
     *
     * @param response the full response object returned by 'performRequest()'
     *                 method
     * @param jsonPath the JSONPath expression that will be evaluated in order to
     *                 extract the desired value [without the trailing $.], please
     *                 refer to these urls for examples:
     *                 <a href="https://support.smartbear.com/alertsite/docs/monitors/api/endpoint/jsonpath.html">https://support.smartbear.com/alertsite/docs/monitors/api/endpoint/jsonpath.html</a>
     *                 <a href="http://jsonpath.com/">http://jsonpath.com/</a>
     * @return a string value that contains the extracted object
     */
    public static String getResponseJSONValue(Response response, String jsonPath) {
        String searchPool = "";
        String jsonResponse = response.asPrettyString();
        try {
            if (jsonPath.contains("?")) {
                List<String> jsonValueAsList = JsonPath.read(jsonResponse, jsonPath);
                searchPool = String.valueOf(jsonValueAsList.getFirst());
            } else {
                var jsonValue = JsonPath.read(jsonResponse, jsonPath);
                searchPool = String.valueOf(jsonValue);
            }
            // This implementation is to handle the *PathNotFoundException* that happens when we have json object but inside html or xml tags, so it's not represented as json object
        } catch (PathNotFoundException e) {
            String jsonObject = jsonResponse.substring(jsonResponse.indexOf("{"), jsonResponse.lastIndexOf("}") + 1);
            Configuration confOrgJsonProvider = Configuration.builder().jsonProvider(new JsonOrgJsonProvider()).build();
            try {
                if (jsonPath.contains("?")) {
                    JSONArray jsonValue = JsonPath.compile(jsonPath).read(new JSONObject(jsonObject), confOrgJsonProvider);
                    searchPool = String.valueOf(jsonValue.get(0));
                } else {
                    Object jsonValue = JsonPath.compile(jsonPath).read(new JSONObject(jsonObject), confOrgJsonProvider);
                    searchPool = String.valueOf(jsonValue);
                }
            } catch (JSONException rootCauseException) {
                ReportManager.log(ERROR_FAILED_TO_PARSE_JSON);
                failAction(jsonPath, rootCauseException);
            }
        } catch (ClassCastException rootCauseException) {
            ReportManager.log(ERROR_INCORRECT_JSONPATH + "\"" + jsonPath + "\"");
            failAction(jsonPath, rootCauseException);
        } catch (JsonPathException | IllegalArgumentException rootCauseException) {
            ReportManager.log(ERROR_FAILED_TO_PARSE_JSON);
            failAction(jsonPath, rootCauseException);
        }
        if (searchPool != null) {
            passAction(jsonPath);
            return searchPool;
        } else {
            ReportManager.logDiscrete(ERROR_NOT_FOUND + "jsonPath \"" + jsonPath + "\"");
            passAction(jsonPath);
            return null;
        }
    }

    public static String getResponseJSONValue(Object response, String jsonPath) {
        String searchPool = "";
        try {
            if (response instanceof HashMap<?, ?> hashMapResponse) {
                JSONObject obj = new JSONObject(hashMapResponse);
                searchPool = io.restassured.path.json.JsonPath.from(obj.toString()).getString(jsonPath);
            } else if (response instanceof Response responseObject) {
                String jsonResponse = responseObject.asPrettyString();
                try {
                    if (jsonPath.contains("?")) {
                        List<String> jsonValueAsList = JsonPath.read(jsonResponse, jsonPath);
                        searchPool = String.valueOf(jsonValueAsList.getFirst());
                    } else {
                        var jsonValue = JsonPath.read(jsonResponse, jsonPath);
                        searchPool = String.valueOf(jsonValue);
                    }
                } catch (PathNotFoundException e) {
                    String jsonObject = jsonResponse.substring(jsonResponse.indexOf("{"), jsonResponse.lastIndexOf("}") + 1);
                    Configuration confOrgJsonProvider = Configuration.builder().jsonProvider(new JsonOrgJsonProvider()).build();
                    try {
                        if (jsonPath.contains("?")) {
                            JSONArray jsonValue = JsonPath.compile(jsonPath).read(new JSONObject(jsonObject), confOrgJsonProvider);
                            searchPool = String.valueOf(jsonValue.get(0));
                        } else {
                            Object jsonValue = JsonPath.compile(jsonPath).read(new JSONObject(jsonObject), confOrgJsonProvider);
                            searchPool = String.valueOf(jsonValue);
                        }
                    } catch (JSONException rootCauseException) {
                        ReportManager.log(ERROR_FAILED_TO_PARSE_JSON);
                        failAction(jsonPath, rootCauseException);
                    }
                }
            }
        } catch (ClassCastException rootCauseException) {
            ReportManager.log(ERROR_INCORRECT_JSONPATH + "\"" + jsonPath + "\"");
            failAction(jsonPath, rootCauseException);
        } catch (JsonPathException | IllegalArgumentException rootCauseException) {
            ReportManager.log(ERROR_FAILED_TO_PARSE_JSON);
            failAction(jsonPath, rootCauseException);
        }
        if (searchPool != null) {
            passAction(jsonPath);
            return searchPool;
        } else {
            ReportManager.logDiscrete(ERROR_NOT_FOUND + "jsonPath \"" + jsonPath + "\"");
            passAction(jsonPath);
            return null;
        }
    }

    public static List<Object> getResponseJSONValueAsList(Response response, String jsonPath) {
        List<Object> searchPool = null;
        String jsonResponse = response.asPrettyString();
        try {
            searchPool = JsonPath.read(jsonResponse, jsonPath);
        } catch (PathNotFoundException e) {
            String jsonObject = jsonResponse.substring(jsonResponse.indexOf("{"), jsonResponse.lastIndexOf("}") + 1);
            Configuration confOrgJsonProvider = Configuration.builder().jsonProvider(new JsonOrgJsonProvider()).build();
            List<Object> jsonList = null;
            try {
                JSONArray jsonArray = JsonPath.compile(jsonPath).read(new JSONObject(jsonObject), confOrgJsonProvider);
                jsonList = new ObjectMapper().readValue(Objects.requireNonNull(jsonArray).toString(), new TypeReference<>() {
                });
            } catch (JSONException | JsonProcessingException rootCauseException) {
                ReportManager.log(ERROR_FAILED_TO_PARSE_JSON);
                failAction(jsonPath, rootCauseException);
            }
            searchPool = jsonList;
        } catch (ClassCastException rootCauseException) {
            ReportManager.log(ERROR_INCORRECT_JSONPATH + "\"" + jsonPath + "\"");
            failAction(jsonPath, rootCauseException);
        } catch (JsonPathException | IllegalArgumentException rootCauseException) {
            ReportManager.log(ERROR_FAILED_TO_PARSE_JSON);
            failAction(jsonPath, rootCauseException);
        }

        if (searchPool != null) {
            passAction(jsonPath);
            return searchPool;
        } else {
            ReportManager.logDiscrete(ERROR_NOT_FOUND + "jsonPath \"" + jsonPath + "\"");
            passAction(jsonPath);
            return null;
        }
    }

    /**
     * Extracts a string value from an object of a list by reference of another attribute inside the same object
     *
     * @param response                 The target API response object
     * @param jsonPathToList           The JSON path to the list of object inside the full response
     * @param jsonPathToValueNeeded    The JSON path to the attribute value you need to extract inside an object from the list. for example: id
     * @param jsonPathToValueReference The JSON path that refers to the needed attribute value inside an object from the list. for example: username
     * @param valueReference           The attribute value of the reference JSON path
     * @return A string value from the object of the list
     */
    public static String getResponseJSONValueFromList(Response response, String jsonPathToList, String jsonPathToValueNeeded,
                                                      String jsonPathToValueReference, String valueReference) {
        List<Object> list = getResponseJSONValueAsList(response, jsonPathToList);
        String value = "";
        for (Object res : Objects.requireNonNull(list)) {
            if (Objects.equals(getResponseJSONValue(res, jsonPathToValueReference), valueReference)) {
                value = getResponseJSONValue(res, jsonPathToValueNeeded);
            }
        }
        if (Objects.equals(value, "")) {
            failAction("Can't find the reference value [" + valueReference + "] in the list with the [" + jsonPathToValueReference + "] JSON Path");
        } else {
            passAction(value);
        }
        return value;
    }

    public static String getResponseXMLValue(Response response, String xmlPath) {
        String searchPool = "";
        try {
            searchPool = response.xmlPath().getString(xmlPath);
        } catch (ClassCastException rootCauseException) {
            ReportManager.log(ERROR_INCORRECT_XML_PATH + "\"" + xmlPath + "\"");
            failAction(xmlPath, rootCauseException);

        }
        if (searchPool != null) {
            passAction(xmlPath);
            return searchPool;
        } else {
            ReportManager.logDiscrete(ERROR_NOT_FOUND + "xmlPath \"" + xmlPath + "\"");
            passAction(xmlPath);
            return null;
        }
    }

    public static String getResponseXMLValue(Object response, String xmlPath) {
        String output = "";
        try {
            output = ((Node) response).getAttribute(xmlPath);
        } catch (ClassCastException rootCauseException) {
            ReportManager.log(ERROR_INCORRECT_XML_PATH + "\"" + xmlPath + "\"");
            failAction(xmlPath, rootCauseException);

        }
        if (output != null) {
            passAction(xmlPath);
            return output;
        } else {
            ReportManager.logDiscrete(ERROR_NOT_FOUND + "xmlPath \"" + xmlPath + "\"");
            passAction(xmlPath);
            return null;
        }
    }

    public static List<Object> getResponseXMLValueAsList(Response response, String xmlPath) {
        NodeChildren output = null;
        try {
            output = response.xmlPath().get(xmlPath);
        } catch (ClassCastException rootCauseException) {
            ReportManager.log(ERROR_INCORRECT_XML_PATH + "\"" + xmlPath + "\"");
            failAction(xmlPath, rootCauseException);

        }
        List<Node> nodes = null;
        if (output != null) {
            nodes = output.list();
        }
        List<Object> searchPool = null;
        if (nodes != null) {
            searchPool = Arrays.asList(nodes.toArray());
        }
        if (searchPool != null) {
            passAction(xmlPath);
            return searchPool;
        } else {
            ReportManager.logDiscrete(ERROR_NOT_FOUND + "xmlPath \"" + xmlPath + "\"");
            passAction(xmlPath);
            return null;
        }
    }

    public static int getResponseStatusCode(Response response) {
        int statusCode = response.getStatusCode();
        passAction(String.valueOf(statusCode));
        return statusCode;
    }

    public static long getResponseTime(Response response) {
        long time = response.timeIn(TimeUnit.MILLISECONDS);
        passAction(String.valueOf(time));
        return time;
    }

    /**
     * Compares the Response object against the content of the referenceJsonFilePath
     *
     * @param response              the full response object returned by
     *                              performRequest method.
     * @param referenceJsonFilePath the full absolute path to the test data file
     *                              that will be used as a reference for this
     *                              comparison
     * @param comparisonType        ComparisonType.EQUALS, CONTAINS, MATCHES,
     *                              EQUALS_STRICT; Note that MATCHES ignores the
     *                              content ordering inside the JSON
     * @return a boolean value that is TRUE in case the comparison passed, or FALSE
     * in case it failed
     */
    public static boolean compareJSON(Response response, String referenceJsonFilePath, ComparisonType comparisonType) {
        return compareJSON(response, referenceJsonFilePath, comparisonType, "");
    }

    /**
     * Compares the Response object against the content of the referenceJsonFilePath
     *
     * @param response              the full response object returned by
     *                              performRequest method.
     * @param referenceJsonFilePath the full absolute path to the test data file
     *                              that will be used as a reference for this
     *                              comparison
     * @param comparisonType        ComparisonType.EQUALS, CONTAINS; Note that
     *                              MATCHES ignores the content ordering inside the
     *                              JSON
     * @param jsonPathToTargetArray a jsonpath that will be parsed to point to the
     *                              target JSON Array
     * @return a boolean value that is TRUE in case the comparison passed, or FALSE
     * in case it failed
     */
    public static boolean compareJSON(Response response, String referenceJsonFilePath, ComparisonType comparisonType,
                                      String jsonPathToTargetArray) {
        if (jsonPathToTargetArray.isEmpty()) {
            ReportManager.logDiscrete("Comparing the provided API response with the file at this path \""
                    + referenceJsonFilePath + "\", comparison type \"" + comparisonType + "\"");
        } else {
            ReportManager.logDiscrete("Comparing the provided API response with the file at this path \""
                    + referenceJsonFilePath + "\", comparison type \"" + comparisonType
                    + "\", jsonPath to target array \"" + jsonPathToTargetArray + "\".");
        }
        boolean comparisonResult;
        JSONParser parser = new JSONParser();
        List<Object> expectedJSONAttachment = null;

        try {
            // parse actual JSON into Object or Array
            org.json.simple.JSONObject actualJsonObject = null;
            org.json.simple.JSONArray actualJsonArray = null;

            var actualObject = parser.parse(response.asString());
            if (actualObject instanceof org.json.simple.JSONObject) {
                actualJsonObject = (org.json.simple.JSONObject) parser.parse(response.asString());
            } else {
                // actualObject is an array org.json.simple.JSONArray
                actualJsonArray = (org.json.simple.JSONArray) parser.parse(response.asString());
            }

            // parse expected JSON into Object or Array
            org.json.simple.JSONObject expectedJsonObject = null;
            org.json.simple.JSONArray expectedJsonArray = null;

            var expectedObject = parser.parse(new FileReader(referenceJsonFilePath));
            if (expectedObject instanceof org.json.simple.JSONObject) {
                expectedJsonObject = (org.json.simple.JSONObject) parser.parse(new FileReader(referenceJsonFilePath));
                expectedJSONAttachment = Arrays.asList("File Content", "Expected JSON",
                        new GsonBuilder().setPrettyPrinting().create()
                                .toJson(JsonParser.parseString(expectedJsonObject.toJSONString())));
            } else {
                // expectedObject is an array org.json.simple.JSONArray
                expectedJsonArray = (org.json.simple.JSONArray) parser.parse(new FileReader(referenceJsonFilePath));
                expectedJSONAttachment = Arrays.asList("File Content", "Expected JSON", new GsonBuilder()
                        .setPrettyPrinting().create().toJson(JsonParser.parseString(expectedJsonArray.toJSONString())));
            }

            // handle different combinations of expected and actual (object vs array)
            // TODO: handle jsonPathToTargetArray and attempt to parse the actual result
            comparisonResult = switch (comparisonType) {
                case EQUALS -> compareJSONEquals(expectedJsonObject, expectedJsonArray, actualJsonObject,
                        actualJsonArray);
                case CONTAINS -> compareJSONContains(response, expectedJsonObject, expectedJsonArray,
                        actualJsonObject, jsonPathToTargetArray);
                case EQUALS_IGNORING_ORDER ->
                        compareJSONEqualsIgnoringOrder(expectedJsonObject, expectedJsonArray, actualJsonObject,
                                actualJsonArray);
            };
        } catch (IOException rootCauseException) {
            failAction("Couldn't find the desired file. \"" + referenceJsonFilePath + "\".", rootCauseException);
            comparisonResult = false;
        } catch (ParseException | JSONException rootCauseException) {
            failAction("Couldn't parse the desired file. \"" + referenceJsonFilePath + "\".", rootCauseException);
            comparisonResult = false;
        }
        passAction(referenceJsonFilePath, expectedJSONAttachment);
        return comparisonResult;
    }

    public static String formatXML(String input) {
        return prettyFormatXML(input);
    }

    protected static void failAction(String testData, Object requestBody, RequestSpecification specs, Response response,
                                     Throwable... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(actionName, testData, requestBody, specs, response, rootCauseException);
    }

    protected static void failAction(String testData, Throwable... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(actionName, testData, null, null, null, rootCauseException);
    }

    private static String reportActionResult(String actionName, String testData, Object requestBody, RequestSpecification specs, Response response,
                                             Boolean isDiscrete, List<Object> expectedFileBodyAttachment, Boolean passFailStatus, Throwable... rootCauseException) {
        actionName = JavaHelper.convertToSentenceCase(actionName);
        actionName = actionName.equals("Perform request") ? "Request performed" : actionName;
        String message;
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = actionName;
        } else {
            message = actionName + " failed";
        }

        List<List<Object>> attachments = new ArrayList<>();
        if (testData != null && testData.length() >= 500) {
            List<Object> actualValueAttachment = Arrays.asList("API Action Test Data - " + actionName, "Actual Value",
                    testData);
            attachments.add(actualValueAttachment);
        } else if (testData != null && !testData.isEmpty()) {
            message = message + "; " + testData.trim();
        }
        message = message + ".";

        Boolean initialLoggingState = ReportManagerHelper.getDiscreteLogging();
        attachments.add(expectedFileBodyAttachment);

        if (rootCauseException != null && rootCauseException.length >= 1) {
            List<Object> actualValueAttachment = Arrays.asList("API Action Exception - " + actionName,
                    "Stacktrace", ReportManagerHelper.formatStackTraceToLogEntry(rootCauseException[0]));
            attachments.add(actualValueAttachment);
        }

        if (Boolean.FALSE.equals(initialLoggingState)) {
            ReportManagerHelper.log(message, attachments);
        } else {
            ReportManager.logDiscrete(message);
        }
        return message;
    }

    private static InputStream parseJsonBody(Object body) throws ParseException {
        JSONParser parser = new JSONParser();
        org.json.simple.JSONObject actualJsonObject = null;
        org.json.simple.JSONArray actualJsonArray = null;
        if (body.getClass().getName().toLowerCase().contains("restassured")) {
            try {
                // if it's a string response body
                String bodyString = ((ResponseBody<?>) body).asString();
                if (!bodyString.isEmpty()) {
                    actualJsonObject = (org.json.simple.JSONObject) parser.parse(bodyString);
                }
            } catch (ClassCastException e) {
                // java.lang.ClassCastException: org.json.simple.JSONArray cannot be cast to
                // org.json.simple.JSONObject
                String bodyString = ((ResponseBody<?>) body).asString();
                if (!bodyString.isEmpty()) {
                    actualJsonArray = (org.json.simple.JSONArray) parser.parse(bodyString);
                }
            }
        } else if (body instanceof org.json.simple.JSONObject) {
            actualJsonObject = (org.json.simple.JSONObject) body;
        } else if (body instanceof org.json.simple.JSONArray) {
            actualJsonArray = (org.json.simple.JSONArray) body;
        } else if (body.getClass().getName().toLowerCase().contains("jsonobject")) {
            actualJsonObject = (org.json.simple.JSONObject) parser
                    .parse(body.toString().replace("\\n", "").replace("\\t", "").replace(" ", ""));
        } else if (body instanceof Map<?, ?> bodyMap) {
            actualJsonObject = new org.json.simple.JSONObject(bodyMap);
        } else {
            actualJsonObject = (org.json.simple.JSONObject) parser.parse(body.toString());
        }
        if (actualJsonObject != null) {
            return new ByteArrayInputStream((new GsonBuilder().setPrettyPrinting().create()
                    .toJson(JsonParser.parseString(actualJsonObject.toJSONString()))).getBytes());
        } else if (actualJsonArray != null) {
            return new ByteArrayInputStream((new GsonBuilder().setPrettyPrinting().create()
                    .toJson(JsonParser.parseString(actualJsonArray.toJSONString()))).getBytes());
        } else {
            // in case of an empty body
            return new ByteArrayInputStream(("").getBytes());
        }
    }

    private static boolean compareJSONEquals(org.json.simple.JSONObject expectedJsonObject,
                                             org.json.simple.JSONArray expectedJsonArray, org.json.simple.JSONObject actualJsonObject,
                                             org.json.simple.JSONArray actualJsonArray) {
        if (expectedJsonObject != null && actualJsonObject != null) {
            // if expected is an object and actual is also an object
            return actualJsonObject.toString().equals(expectedJsonObject.toString());
        } else {
            // if expected is an array and actual response is also an array
            return actualJsonArray.toString().equals(expectedJsonArray.toString());
        }
    }

    private static boolean compareJSONEqualsIgnoringOrder(org.json.simple.JSONObject expectedJsonObject,
                                                          org.json.simple.JSONArray expectedJsonArray, org.json.simple.JSONObject actualJsonObject,
                                                          org.json.simple.JSONArray actualJsonArray) {
        if (expectedJsonObject != null && actualJsonObject != null) {
            // if expected is an object and actual is also an object
            try {
                JSONAssert.assertEquals(expectedJsonObject.toString(), actualJsonObject.toString(), JSONCompareMode.NON_EXTENSIBLE);
                return true;
            } catch (JSONException e) {
                return false;
            }
        } else {
            // if expected is an array and actual response is also an array
            try {
                JSONAssert.assertEquals(expectedJsonArray.toString(), actualJsonArray.toString(), JSONCompareMode.NON_EXTENSIBLE);
                return true;
            } catch (JSONException e) {
                return false;
            }
        }
    }

    @SuppressWarnings("unchecked")
    private static boolean compareJSONContains(Response response, org.json.simple.JSONObject expectedJsonObject,
                                               org.json.simple.JSONArray expectedJsonArray, org.json.simple.JSONObject actualJsonObject,
                                               String jsonPathToTargetArray)
            throws JSONException, ParseException {
        JSONParser parser = new JSONParser();
        if (!jsonPathToTargetArray.isEmpty() && (expectedJsonArray != null)) {
            // if expected is an array and the user provided the path to extract it from the
            // response
            org.json.simple.JSONArray actualJsonArray = (org.json.simple.JSONArray) parser
                    .parse((new Gson()).toJsonTree(getResponseJSONValueAsList(response, jsonPathToTargetArray))
                            .getAsJsonArray().toString());
            return actualJsonArray.containsAll(expectedJsonArray);
        } else if (jsonPathToTargetArray.isEmpty() && (expectedJsonArray != null)) {
            // if expected is an array and the user did not provide the path to extract it
            // from the response
            String actual = (new Gson()).toJson(actualJsonObject);
            String expected = (new Gson()).toJson(expectedJsonArray);
            return actual.contains(expected.substring(1));
        } else if (expectedJsonObject != null) {
            // if expected is an object and actual is also an object
            boolean initialComparison = JSONCompare.compareJSON(expectedJsonObject.toJSONString(),
                    actualJsonObject.toJSONString(), JSONCompareMode.LENIENT).passed();
            if (Boolean.FALSE.equals(initialComparison)) {
                // secondary comparison using java contains
                // not tested
                return actualJsonObject.toString().contains(expectedJsonObject.toString());
            } else {
                return initialComparison;
            }
        } else {
            return false;
        }
    }

    private static String prettyFormatXML(String input) {
        Source xmlInput = new StreamSource(new StringReader(input));
        StringWriter stringWriter = new StringWriter();
        try {
            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            transformerFactory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
            Transformer transformer = transformerFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty(OutputKeys.DOCTYPE_PUBLIC, "yes");
            transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
            transformer.transform(xmlInput, new StreamResult(stringWriter));
            return stringWriter.toString().trim();
        } catch (Exception e) {
            return input;
        }
    }

    private static void initializeSystemProperties() {
        if (SHAFT.Properties.timeouts == null)
            DriverFactory.reloadProperties();
        HTTP_SOCKET_TIMEOUT = SHAFT.Properties.timeouts.apiSocketTimeout();
        // timeout between two consecutive data packets in seconds
        HTTP_CONNECTION_TIMEOUT = SHAFT.Properties.timeouts.apiConnectionTimeout();
        // timeout until a connection is established in seconds
        HTTP_CONNECTION_MANAGER_TIMEOUT = SHAFT.Properties.timeouts.apiConnectionManagerTimeout();
        AUTOMATICALLY_ASSERT_RESPONSE_STATUS_CODE = SHAFT.Properties.flags.automaticallyAssertResponseStatusCode();
    }

    /**
     * private helper method for sendGraphqlRequest() method - WITHOUT TOKEN.
     *
     * @param base_URI_forHelperMethod    The Base URI without "graphql". example:: "<a href="https://api.example.com/">https://api.example.com/</a>"
     * @param requestBody_forHelperMethod the request body.
     * @return Response object
     */
    private static Response graphQlRequestHelper(String base_URI_forHelperMethod, org.json.simple.JSONObject requestBody_forHelperMethod) {
        ReportManager.logDiscrete("GraphQl Request is being Performed with the Following Parameters [Service URL: " + base_URI_forHelperMethod + "graphql | Request Body: " + requestBody_forHelperMethod + "\"");
        return buildNewRequest(base_URI_forHelperMethod, GRAPHQL_END_POINT, RequestType.POST).setRequestBody(requestBody_forHelperMethod)
                .setContentType(ContentType.JSON).performRequest();
    }

    /**
     * Perform Graphql Request using Query - WITHOUT Header.
     *
     * @param base_URI The Base URI without "graphql". example:: "<a href="https://api.example.com/">https://api.example.com/</a>"
     * @param query    graphql query or mutation.
     * @return Graphql Response
     */
    @SuppressWarnings("unchecked")
    public static Response sendGraphQlRequest(String base_URI, String query) {

        org.json.simple.JSONObject requestBody = new org.json.simple.JSONObject();
        requestBody.put("query", query);
        return graphQlRequestHelper(base_URI, requestBody);
    }

    /**
     * Perform Graphql Request using Query and Variables - WITHOUT Header.
     *
     * @param base_URI  The Base URI without "graphql". example:: "<a href="https://api.example.com/">https://api.example.com/</a>"
     * @param query     graphql query or mutation.
     * @param variables graphql variables; dynamic values of the query. please refer to this url for examples:: <a href="https://graphql.org/learn/queries/#variables">https://graphql.org/learn/queries/#variables</a>
     * @return Graphql Response
     */
    @SuppressWarnings("unchecked")
    public static Response sendGraphQlRequest(String base_URI, String query, String variables) {

        org.json.simple.JSONObject requestBody = new org.json.simple.JSONObject();
        requestBody.put("query", query);
        requestBody.put("variables", variables);
        return graphQlRequestHelper(base_URI, requestBody);
    }

    /**
     * Perform Graphql Request using Query, Variables, and Fragments - WITHOUT Header.
     *
     * @param base_URI  The Base URI without "graphql". example:: "<a href="https://api.example.com/">https://api.example.com/</a>"
     * @param query     graphql query or mutation.
     * @param variables graphql variables; dynamic values of the query. please refer to this url for examples:: <a href="https://graphql.org/learn/queries/#variables">https://graphql.org/learn/queries/#variables</a>
     * @param fragment  graphql fragment; reusable units let you construct sets of fields, and then include them in queries where you need to. please refer to this url for examples:: <a href="https://graphql.org/learn/queries/#fragments">https://graphql.org/learn/queries/#fragments</a>
     * @return Graphql Response
     */
    @SuppressWarnings("unchecked")
    public static Response sendGraphQlRequest(String base_URI, String query, String variables, String fragment) {

        org.json.simple.JSONObject requestBody = new org.json.simple.JSONObject();
        requestBody.put("query", query);
        requestBody.put("variables", variables);
        requestBody.put("fragment", fragment);
        return graphQlRequestHelper(base_URI, requestBody);
    }

    /**
     * private helper method for sendGraphqlRequest method WITH Header.
     *
     * @param base_URI_forHelperMethod    The Base URI without "graphql". example:: "<a href="https://api.example.com/">https://api.example.com/</a>"
     * @param requestBody_forHelperMethod the request body.
     * @param headerKey_forHelperMethod   the name of the header that you want to add.
     * @param headerValue_forHelperMethod the value that will be put inside the key.
     * @return Response object
     */
    private static Response graphQlRequestHelperWithHeader(String base_URI_forHelperMethod, org.json.simple.JSONObject requestBody_forHelperMethod, String headerKey_forHelperMethod, String headerValue_forHelperMethod) {
        ReportManager.logDiscrete("GraphQl Request is being Performed with the Following Parameters [Service URL: " + base_URI_forHelperMethod + "graphql | Request Body: " + requestBody_forHelperMethod + " | Header: \"" + headerKey_forHelperMethod + "\":\"" + headerValue_forHelperMethod + "\"\"");
        return buildNewRequest(base_URI_forHelperMethod, GRAPHQL_END_POINT, RequestType.POST).setRequestBody(requestBody_forHelperMethod)
                .setContentType(ContentType.JSON).addHeader(headerKey_forHelperMethod, headerValue_forHelperMethod).performRequest();
    }

    /**
     * Perform Graphql Request using Query - WITH Header.
     *
     * @param base_URI     The Base URI without "graphql". example:: "<a href="https://api.example.com/">https://api.example.com/</a>"
     * @param query        graphql query or mutation.
     * @param header_key   the name of the header that you want to add. example:: "Authorization"
     * @param header_value the value that will be put inside the key. example:: "bearer ${token}"
     * @return Graphql Response
     */
    @SuppressWarnings("unchecked")
    public static Response sendGraphQlRequestWithHeader(String base_URI, String query, String header_key, String header_value) {

        org.json.simple.JSONObject requestBody = new org.json.simple.JSONObject();
        requestBody.put("query", query);
        return graphQlRequestHelperWithHeader(base_URI, requestBody, header_key, header_value);
    }

    /**
     * Perform Graphql Request using Query and Variables - WITH Header.
     *
     * @param base_URI     The Base URI without "graphql". example:: "<a href="https://api.example.com/">https://api.example.com/</a>"
     * @param query        graphql query or mutation.
     * @param variables    graphql variables; dynamic values of the query. please refer to this url for examples:: <a href="https://graphql.org/learn/queries/#variables">https://graphql.org/learn/queries/#variables</a>
     * @param header_key   the name of the header that you want to add. example:: "Authorization"
     * @param header_value the value that will be put inside the key. example:: "bearer ${token}"
     * @return Graphql Response
     */
    @SuppressWarnings("unchecked")
    public static Response sendGraphQlRequestWithHeader(String base_URI, String query, String variables, String header_key, String header_value) {

        org.json.simple.JSONObject requestBody = new org.json.simple.JSONObject();
        requestBody.put("query", query);
        requestBody.put("variables", variables);
        return graphQlRequestHelperWithHeader(base_URI, requestBody, header_key, header_value);
    }

    /**
     * Perform Graphql Request using Query, Variables, and Fragments - WITH Header.
     *
     * @param base_URI     The Base URI without "graphql". example:: "<a href="https://api.example.com/">https://api.example.com/</a>"
     * @param query        graphql query or mutation.
     * @param variables    graphql variables; dynamic values of the query. please refer to this url for examples:: <a href="https://graphql.org/learn/queries/#variables">https://graphql.org/learn/queries/#variables</a>
     * @param fragment     graphql fragment; reusable units let you construct sets of fields, and then include them in queries where you need to. please refer to this url for examples:: <a href="https://graphql.org/learn/queries/#fragments">https://graphql.org/learn/queries/#fragments</a>
     * @param header_key   the name of the header that you want to add. example:: "Authorization"
     * @param header_value the value that will be put inside the key. example:: "bearer ${token}"
     * @return Graphql Response
     */
    @SuppressWarnings("unchecked")
    public static Response sendGraphQlRequestWithHeader(String base_URI, String query, String variables, String fragment, String header_key, String header_value) {

        org.json.simple.JSONObject requestBody = new org.json.simple.JSONObject();
        requestBody.put("query", query);
        requestBody.put("variables", variables);
        requestBody.put("fragment", fragment);
        return graphQlRequestHelperWithHeader(base_URI, requestBody, header_key, header_value);
    }

    public RequestBuilder buildNewRequest(String serviceName, RequestType requestType) {
        return new RequestBuilder(this, serviceName, requestType);
    }

    protected String getServiceURI() {
        return serviceURI;
    }

    protected Map<String, String> getSessionHeaders() {
        return sessionHeaders;
    }

    protected Map<String, Object> getSessionCookies() {
        return sessionCookies;
    }

    protected RestAssuredConfig getSessionConfig() {
        return sessionConfig;
    }

    private RequestSpecBuilder setConfigs(RequestSpecBuilder builder, List<RestAssuredConfig> configs) {
        for (RestAssuredConfig config : configs) {
            builder.setConfig(config);
        }
        return builder;
    }

    private RequestSpecBuilder initializeBuilder(Map<String, Object> sessionCookies, Map<String, String> sessionHeaders, RestAssuredConfig sessionConfig, boolean appendDefaultContentCharsetToContentTypeIfUndefined) {
        RequestSpecBuilder builder = new RequestSpecBuilder();

        builder.addCookies(sessionCookies);
        builder.addHeaders(sessionHeaders);
        //Add configs
        RestAssuredConfig userConfigs = sessionConfig.and().encoderConfig((new EncoderConfig()).defaultContentCharset("UTF-8")
                        .appendDefaultContentCharsetToContentTypeIfUndefined(appendDefaultContentCharsetToContentTypeIfUndefined)).and()
                .httpClient(HttpClientConfig.httpClientConfig()
                        .setParam("http.connection.timeout", HTTP_CONNECTION_TIMEOUT * 1000)
                        .setParam("http.socket.timeout", HTTP_SOCKET_TIMEOUT * 1000)
                        .setParam("http.connection-manager.timeout", HTTP_CONNECTION_MANAGER_TIMEOUT * 1000));
        builder.setConfig(userConfigs);
        // timeouts documentation
        /*
         * CoreConnectionPNames.SO_TIMEOUT='http.socket.timeout': defines the socket
         * timeout (SO_TIMEOUT) in milliseconds (which is the timeout for waiting for
         * data or, put differently, a maximum period inactivity between two consecutive
         * data packets). A timeout value of zero is interpreted as an infinite timeout.
         * This parameter expects a value of type java.lang.Integer. If this parameter
         * is not set, read operations will not time out (infinite timeout).
         *
         * CoreConnectionPNames.CONNECTION_TIMEOUT='http.connection.timeout': determines
         * the timeout in milliseconds until a connection is established. A timeout
         * value of zero is interpreted as an infinite timeout. This parameter expects a
         * value of type java.lang.Integer. If this parameter is not set, connect
         * operations will not time out (infinite timeout).
         *
         * the Connection Manager Timeout (http.connection-manager.timeout) – the time
         * to wait for a connection from the connection manager/pool
         */
        return builder;
    }

    /**
     * Append a header to the current session to be used in all the
     * following requests. Note: This feature is commonly used for authentication
     * tokens.
     *
     * @param key   the name of the header that you want to add
     * @param value the value that will be put inside the key
     * @return self-reference to be used for chaining actions
     */
    public RestActions addHeaderVariable(String key, String value) {
        sessionHeaders.put(key, value);
        return this;
    }

    public RestActions addCookieVariable(String key, String value) {
        sessionCookies.put(key, value);
        return this;
    }


    protected String prepareRequestURL(String serviceURI, String urlArguments, String serviceName) {
        if (urlArguments != null && !urlArguments.isEmpty()) {
            return serviceURI + serviceName + ARGUMENT_SEPARATOR + urlArguments;
        } else {
            return serviceURI + serviceName;
        }
    }

    protected RequestSpecification prepareRequestSpecs(List<List<Object>> parameters, ParametersType parametersType,
                                                       Object body, ContentType contentType, Map<String, Object> sessionCookies, Map<String, String> sessionHeaders, RestAssuredConfig sessionConfig, boolean appendDefaultContentCharsetToContentTypeIfUndefined, boolean urlEncodingEnabled) {
        RequestSpecBuilder builder = initializeBuilder(sessionCookies, sessionHeaders, sessionConfig, appendDefaultContentCharsetToContentTypeIfUndefined);

        // Check if contentType is still ANY and use the Content-Type header value directly
        if (contentType == ContentType.ANY) {
            String contentTypeHeader = sessionHeaders.get("Content-Type");
            if (contentTypeHeader != null) {
                // Set the content type to the exact header value
                builder.setContentType(contentTypeHeader);
            }
        } else {
            // If contentType is explicitly set, use it
            builder.setContentType(contentType);
        }

        builder.setUrlEncodingEnabled(urlEncodingEnabled);

        if (body != null && contentType != null && !body.toString().isEmpty()) {
            prepareRequestBody(builder, body, contentType);
        } else if (parameters != null && !parameters.isEmpty() && !parameters.getFirst().getFirst().equals("")) {
            prepareRequestBody(builder, parameters, parametersType);
        }
        return builder.build();
    }

    private void prepareRequestBody(RequestSpecBuilder builder, Object body, ContentType contentType) {
        if (body instanceof String bodyString && bodyString.contains("\n")) {
            builder.setBody(bodyString);
        } else if (body instanceof JSONObject || body instanceof JSONArray) {
            builder.setBody(body.toString());
        } else {
            try {
                switch (contentType) {
                    case JSON ->
                        // "application/json", "application/javascript", "text/javascript", "text/json" ->
                            builder.setBody(body, ObjectMapperType.GSON);
                    case XML ->
                        //   "application/xml", "text/xml", "application/xhtml+xml" ->
                            builder.setBody(body, ObjectMapperType.JAXB);
                    default -> builder.setBody(body);
                }
            } catch (Exception rootCauseException) {
                failAction("Issue with parsing body content", rootCauseException);
            }
        }
    }

    private void prepareRequestBody(RequestSpecBuilder builder, List<List<Object>> parameters,
                                    ParametersType parametersType) {
        parameters.forEach(param -> {
            if (param.get(1).getClass().equals(File.class)) {
                MultiPartSpecBuilder multiPartSpecBuilder = new MultiPartSpecBuilder(param.get(1));
                multiPartSpecBuilder.controlName(param.get(0).toString());
                String fileName = ((File) param.get(1)).getName();
                multiPartSpecBuilder.fileName(fileName);
                String mimeType;
                mimeType = URLConnection.guessContentTypeFromName(((File) param.get(1)).getName());
                if (mimeType == null) {
                    mimeType = MimeUtil2.getMostSpecificMimeType(MimeUtil.getMimeTypes(fileName)).toString();
                }
                multiPartSpecBuilder.mimeType(mimeType);
                builder.addMultiPart(multiPartSpecBuilder.build());
                // override the default content type as part of the specs
                builder.setContentType("multipart/form-data");
            } else {
                if (parametersType.equals(ParametersType.FORM)) {
                    builder.addFormParam(param.get(0).toString(), param.get(1));
                } else {
                    builder.addQueryParam(param.get(0).toString(), param.get(1));
                }
            }
        });
    }

    Response sendRequest(RequestType requestType, String request, RequestSpecification specs) {
        switch (requestType) {
            case POST -> {
                return given().filter(allureFilter).spec(specs).when().post(request).andReturn();
            }
            case PATCH -> {
                return given().filter(allureFilter).spec(specs).when().patch(request).andReturn();
            }
            case PUT -> {
                return given().filter(allureFilter).spec(specs).when().put(request).andReturn();
            }
            case GET -> {
                return given().filter(allureFilter).spec(specs).when().get(request).andReturn();
            }
            case DELETE -> {
                return given().filter(allureFilter).spec(specs).when().delete(request).andReturn();
            }
            default -> {
            }
        }
        return null;
    }

    private void extractCookiesFromResponse(Response response) {
        if (response.getDetailedCookies().size() > 0) {
            for (Cookie cookie : response.getDetailedCookies()) {
                sessionCookies.put(cookie.getName(), cookie.getValue());
                if (cookie.getName().equals("XSRF-TOKEN")) {
                    sessionHeaders.put("X-XSRF-TOKEN", cookie.getValue());
                }
            }
        }
    }

    private void extractHeadersFromResponse(Response response) {
        if (response.getHeaders().size() > 0) {
            for (Header header : response.getHeaders()) {
                if (header.getName().equals("X-XSRF-TOKEN") || header.getName().equals("Set-Cookie")) {
                    sessionHeaders.put(header.getName(), header.getValue());
                }
            }
        }

        try {
            if (response.jsonPath().getString("type").equalsIgnoreCase("bearer")) {
                headerAuthorization = "Bearer " + getResponseJSONValue(response, "token");
                sessionHeaders.put("Authorization", headerAuthorization);
                sessionHeaders.put("Content-Type", "application/json");

            }
        } catch (JsonPathException | NullPointerException e) {
            // do nothing if the "type" variable was not found
            // or if response was not json

            // JsonPathException | NullPointerException
        }
    }

    protected boolean evaluateResponseStatusCode(Response response, int targetStatusCode) {
        try {
            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            var statusCode = response.getStatusCode();
            ReportManager.logDiscrete("Response status code: \"" + statusCode + "\", status line: \"" + response.getStatusLine() + "\"");
            if (AUTOMATICALLY_ASSERT_RESPONSE_STATUS_CODE) {
                if (targetStatusCode != 0) {
                    if (targetStatusCode == statusCode) {
                        ReportManager.log("Actual response status code \"" + statusCode + "\" matches the expected one \"" + targetStatusCode + "\".");
                    } else {
                        failAction("Actual response status code \"" + statusCode + "\" does not match the expected one \"" + targetStatusCode + "\".");
                    }
                } else {
                    if (statusCode >= 200 && statusCode < 300) {
                        ReportManager.log("Actual response status code \"" + statusCode + "\" is successful (Between 200 and 299).");
                    } else {
                        failAction("Actual response status code \"" + statusCode + "\" is a failure (Not between 200 and 299).");
                    }
                }
            }
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
            return true;
        } catch (AssertionError rootCauseException) {
            return false;
        }
    }

    String prepareReportMessage(Response response, int targetStatusCode, RequestType requestType,
                                String serviceName, ContentType contentType, String urlArguments) {
        if (response != null) {
            extractCookiesFromResponse(response);
            extractHeadersFromResponse(response);
            StringBuilder reportMessage = new StringBuilder();
            reportMessage.append(requestType);
            if (0 != targetStatusCode)
                reportMessage.append(" | Target Status Code: ").append(targetStatusCode);
            reportMessage.append(" | Response Time: ").append(response.timeIn(TimeUnit.MILLISECONDS)).append("ms");
            if (urlArguments != null) {
                reportMessage.append(" | URL Arguments: ").append(urlArguments);
            }
            return reportMessage.toString().trim();
        }
        return "";
    }


    public enum ComparisonType {
        EQUALS, CONTAINS, EQUALS_IGNORING_ORDER
    }

    public enum ParametersType {
        FORM, QUERY
    }

    public enum RequestType {
        POST, GET, PATCH, DELETE, PUT
    }
}
