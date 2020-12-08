package com.shaft.api;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.shaft.tools.io.PropertyFileManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Assertions;
import eu.medsea.mimeutil.MimeUtil;
import eu.medsea.mimeutil.MimeUtil2;
import io.restassured.builder.MultiPartSpecBuilder;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.config.EncoderConfig;
import io.restassured.config.HttpClientConfig;
import io.restassured.config.RestAssuredConfig;
import io.restassured.http.ContentType;
import io.restassured.http.Cookie;
import io.restassured.http.Header;
import io.restassured.mapper.ObjectMapperType;
import io.restassured.path.json.JsonPath;
import io.restassured.path.json.exception.JsonPathException;
import io.restassured.path.xml.element.Node;
import io.restassured.path.xml.element.NodeChildren;
import io.restassured.response.Response;
import io.restassured.specification.RequestSpecification;
import org.apache.commons.io.IOUtils;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.skyscreamer.jsonassert.JSONCompare;
import org.skyscreamer.jsonassert.JSONCompareMode;
import org.testng.Assert;

import javax.xml.XMLConstants;
import javax.xml.transform.*;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import java.io.*;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.TimeUnit;

import static io.restassured.RestAssured.given;

@SuppressWarnings("unused")
public class RestActions {
    private static final String ARGUMENTSEPARATOR = "?";
    private static final String ERROR_NOT_FOUND = "Either actual value is \"null\" or couldn't find anything that matches with the desired ";
    private static final String ERROR_INCORRECT_JSONPATH = "Incorrect jsonPath ";
    private static final String ERROR_INCORRECT_XMLPATH = "Incorrect xmlPath ";
    private static final String ERROR_FAILED_TO_PARSE_JSON = "Failed to parse the JSON document";
    private static int HTTP_SOCKET_TIMEOUT;
    private static int HTTP_CONNECTION_TIMEOUT;
    private static int HTTP_CONNECTION_MANAGER_TIMEOUT;
    private final String serviceURI;
    private final Map<String, String> sessionHeaders;
    private final Map<String, Object> sessionCookies;
    private String headerAuthorization;

    public RestActions(String serviceURI) {
        initializeSystemProperties(System.getProperty("apiConnectionTimeout") == null);
        headerAuthorization = "";
        this.serviceURI = serviceURI;
        sessionCookies = new HashMap<>();
        sessionHeaders = new HashMap<>();
    }

    public static RequestBuilder buildNewRequest(String serviceURI, String serviceName, RequestType requestType) {
        return new RequestBuilder(new RestActions(serviceURI), serviceName, requestType);
    }

    protected static void passAction(String actionName, String testData, Object requestBody, Response response,
                                     Boolean isDiscrete, List<Object> expectedFileBodyAttachment) {
        reportActionResult(actionName, testData, requestBody, response, isDiscrete, expectedFileBodyAttachment, true);
    }

    protected static void passAction(String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(actionName, testData, null, null, true, null);
    }

    protected static void passAction(String testData, List<Object> expectedFileBodyAttachment) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(actionName, testData, null, null, true, expectedFileBodyAttachment);
    }

    static void passAction(String testData, Object requestBody, Response response) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(actionName, testData, requestBody, response, false, null);
    }

    public static InputStream parseBodyToJson(Response response) {
        return parseBodyToJson(response.getBody());
    }

    public static InputStream parseBodyToJson(Object body) {
        try {
            return parseJsonBody(body);
        } catch (Exception e) {
            // response is not parsable to JSON
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            ObjectOutputStream oos;
            try {
                oos = new ObjectOutputStream(baos);
                oos.writeObject(body);
                oos.flush();
                oos.close();
                return new ByteArrayInputStream(baos.toByteArray());
            } catch (IOException ioe) {
                if (body.getClass().getName().toLowerCase().contains("restassured")) {
                    // if it's a string response body
                    return ((io.restassured.response.ResponseBody<?>) body).asInputStream();
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
     *                 https://support.smartbear.com/alertsite/docs/monitors/api/endpoint/jsonpath.html
     *                 http://jsonpath.com/
     * @return a string value that contains the extracted object
     */
    public static String getResponseJSONValue(Response response, String jsonPath) {
        String searchPool = "";
        try {
            searchPool = response.jsonPath().getString(jsonPath);
        } catch (ClassCastException rootCauseException) {
            ReportManager.log(ERROR_INCORRECT_JSONPATH + "[" + jsonPath + "]");
            failAction(jsonPath, rootCauseException);
        } catch (JsonPathException | IllegalArgumentException rootCauseException) {
            ReportManager.log(ERROR_FAILED_TO_PARSE_JSON);
            failAction(jsonPath, rootCauseException);
        }
        if (searchPool != null) {
            passAction(jsonPath);
            return searchPool;
        } else {
            ReportManager.logDiscrete(ERROR_NOT_FOUND + "jsonPath [" + jsonPath + "]");
            passAction(jsonPath);
            return null;
        }
    }

    public static String getResponseJSONValue(Object response, String jsonPath) {
        @SuppressWarnings("unchecked")
        JSONObject obj = new JSONObject((HashMap<String, String>) response);

        String searchPool = "";
        try {
            searchPool = JsonPath.from(obj.toString()).getString(jsonPath);
        } catch (ClassCastException rootCauseException) {
            ReportManager.log(ERROR_INCORRECT_JSONPATH + "[" + jsonPath + "]");
            failAction(jsonPath, rootCauseException);
        } catch (JsonPathException | IllegalArgumentException rootCauseException) {
            ReportManager.log(ERROR_FAILED_TO_PARSE_JSON);
            failAction(jsonPath, rootCauseException);
        }
        if (searchPool != null) {
            passAction(jsonPath);
            return searchPool;
        } else {
            ReportManager.logDiscrete(ERROR_NOT_FOUND + "jsonPath [" + jsonPath + "]");
            passAction(jsonPath);
            return null;
        }
    }

    public static List<Object> getResponseJSONValueAsList(Response response, String jsonPath) {
        List<Object> searchPool = null;
        try {
            searchPool = response.jsonPath().getList(jsonPath);
        } catch (ClassCastException rootCauseException) {
            ReportManager.log(ERROR_INCORRECT_JSONPATH + "[" + jsonPath + "]");
            failAction(jsonPath, rootCauseException);
        } catch (JsonPathException | IllegalArgumentException rootCauseException) {
            ReportManager.log(ERROR_FAILED_TO_PARSE_JSON);
            failAction(jsonPath, rootCauseException);
        }

        if (searchPool != null) {
            passAction(jsonPath);
            return searchPool;
        } else {
            ReportManager.logDiscrete(ERROR_NOT_FOUND + "jsonPath [" + jsonPath + "]");
            passAction(jsonPath);
            return null;
        }
    }

    public static String getResponseXMLValue(Response response, String xmlPath) {
        String searchPool = "";
        try {
            searchPool = response.xmlPath().getString(xmlPath);
        } catch (ClassCastException rootCauseException) {
            ReportManager.log(ERROR_INCORRECT_XMLPATH + "[" + xmlPath + "]");
            failAction(xmlPath, rootCauseException);

        }
        if (searchPool != null) {
            passAction(xmlPath);
            return searchPool;
        } else {
            ReportManager.logDiscrete(ERROR_NOT_FOUND + "xmlPath [" + xmlPath + "]");
            passAction(xmlPath);
            return null;
        }
    }

    public static String getResponseXMLValue(Object response, String xmlPath) {
        String output = "";
        try {
            output = ((Node) response).getAttribute(xmlPath);
        } catch (ClassCastException rootCauseException) {
            ReportManager.log(ERROR_INCORRECT_XMLPATH + "[" + xmlPath + "]");
            failAction(xmlPath, rootCauseException);

        }
        if (output != null) {
            passAction(xmlPath);
            return output;
        } else {
            ReportManager.logDiscrete(ERROR_NOT_FOUND + "xmlPath [" + xmlPath + "]");
            passAction(xmlPath);
            return null;
        }
    }

    public static List<Object> getResponseXMLValueAsList(Response response, String xmlPath) {
        NodeChildren output = null;
        try {
            output = response.xmlPath().get(xmlPath);
        } catch (ClassCastException rootCauseException) {
            ReportManager.log(ERROR_INCORRECT_XMLPATH + "[" + xmlPath + "]");
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
            ReportManager.logDiscrete(ERROR_NOT_FOUND + "xmlPath [" + xmlPath + "]");
            passAction(xmlPath);
            return null;
        }
    }

    public static int getResponseStatusCode(Response response) {
        int statusCode = response.getStatusCode();
        passAction(String.valueOf(statusCode));
        return statusCode;
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
        if (jsonPathToTargetArray.equals("")) {
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
            };
        } catch (IOException rootCauseException) {
            ReportManager.log(rootCauseException);
            failAction("Couldn't find the desired file. [" + referenceJsonFilePath + "].", rootCauseException);
            comparisonResult = false;
        } catch (ParseException | JSONException rootCauseException) {
            ReportManager.log(rootCauseException);
            failAction("Couldn't parse the desired file. [" + referenceJsonFilePath + "].", rootCauseException);
            comparisonResult = false;
        }
        passAction(referenceJsonFilePath, expectedJSONAttachment);
        return comparisonResult;
    }

    public static String formatXML(String input) {
        return prettyFormatXML(input);
    }

    protected static void failAction(String actionName, String testData, Object requestBody, Response response,
                                     Throwable... rootCauseException) {
        String message = reportActionResult(actionName, testData, requestBody, response, false, null, false);
        if (rootCauseException != null && rootCauseException.length >= 1) {
            Assert.fail(message, rootCauseException[0]);
        } else {
            Assert.fail(message);
        }
    }

    protected static void failAction(String testData, Object requestBody, Response response,
                                     Throwable... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(actionName, testData, requestBody, response, rootCauseException);
    }

    protected static void failAction(String testData, Throwable... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(actionName, testData, null, null, rootCauseException);
    }

    private static String reportActionResult(String actionName, String testData, Object requestBody, Response response,
                                             Boolean isDiscrete, List<Object> expectedFileBodyAttachment, Boolean passFailStatus) {

        String message;
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = "API Action [" + actionName + "] successfully performed.";
        } else {
            message = "API Action [" + actionName + "] failed.";
        }

        List<List<Object>> attachments = new ArrayList<>();
        if (testData != null && !testData.isEmpty() && testData.length() >= 500) {
            List<Object> actualValueAttachment = Arrays.asList("API Action Test Data - " + actionName, "Actual Value",
                    testData);
            attachments.add(actualValueAttachment);
        } else if (testData != null && !testData.isEmpty()) {
            message = message + " With the following test data [" + testData + "].";
        }

        Boolean initialLoggingState = ReportManager.isDiscreteLogging();
        if (Boolean.TRUE.equals(isDiscrete)) {
            if (requestBody != null && !requestBody.equals(new JsonObject())) {
                reportRequestBody(requestBody);
            }
            reportResponseBody(response, true);
            ReportManager.logDiscrete(message);
        } else {
            if (requestBody != null && !requestBody.equals(new JsonObject())) {
                attachments.add(reportRequestBody(requestBody));
            }
            attachments.add(expectedFileBodyAttachment);
            attachments.add(reportResponseBody(response, initialLoggingState));

            if (Boolean.FALSE.equals(initialLoggingState)) {
                ReportManager.log(message, attachments);
            } else {
                ReportManager.logDiscrete(message);
            }

        }
        return message;
    }

    private static List<Object> reportRequestBody(Object requestBody) {
        List<Object> requestBodyAttachment = new ArrayList<>();
        if (requestBody.toString() != null && !requestBody.toString().equals("")) {
            if (ReportManager.isDiscreteLogging()) {
                try {
                    ReportManager.logDiscrete("API Request - REST Body:\n"
                            + IOUtils.toString(parseBodyToJson(requestBody), StandardCharsets.UTF_8));
                } catch (IOException e) {
                    ReportManager.logDiscrete("API Request - REST Body:\n" + requestBody);
                }
            } else {
                requestBodyAttachment.add("API Request");
                switch (identifyBodyObjectType(requestBody)) {
                    case 1:
                        // json
                        requestBodyAttachment.add("JSON Body");
                        break;
                    case 2:
                        // xml
                        requestBodyAttachment.add("XML Body");
                        break;
                    case 3:
                    case 4:
                        // I don't remember... may be binary
                        // binary... probably
                        requestBodyAttachment.add("Body");
                        break;
                    default:
                        // unreachable code
                        break;
                }
                requestBodyAttachment.add(parseBodyToJson(requestBody));
                return requestBodyAttachment;
            }
        }
        return null;
    }

    private static List<Object> reportResponseBody(Response responseBody, Boolean isDiscrete) {
        List<Object> responseBodyAttachment = new ArrayList<>();
        if (responseBody != null) {
            if (Boolean.TRUE.equals(isDiscrete)) {
                try {
                    ReportManager.logDiscrete("API Response - REST Body:\n"
                            + IOUtils.toString(parseBodyToJson(responseBody), StandardCharsets.UTF_8));
                } catch (IOException e) {
                    ReportManager.logDiscrete("API Response - REST Body:\n" + responseBody.asString());
                }
            } else {
                responseBodyAttachment.add("API Response");
                switch (identifyBodyObjectType(responseBody)) {
                    case 1:
                        // json
                        responseBodyAttachment.add("JSON Body");
                        break;
                    case 2:
                        // xml
                        responseBodyAttachment.add("XML Body");
                        break;
                    case 3:
                    case 4:
                        // I don't remember... may be binary
                        // binary... probably
                        responseBodyAttachment.add("Body");
                        break;
                    default:
                        // unreachable code
                        break;
                }
                responseBodyAttachment.add(parseBodyToJson(responseBody));
                return responseBodyAttachment;
            }
        }
        return null;
    }

    @SuppressWarnings("UnusedAssignment")
    private static int identifyBodyObjectType(Object body) {
        JSONParser parser = new JSONParser();
        try {
            org.json.simple.JSONObject actualJsonObject = null;
            org.json.simple.JSONArray actualJsonArray = null;
            if (body.getClass().getName().toLowerCase().contains("restassured")) {
                // if it's a string (OR ARRAY) response body
                try {
                    String bodyString = ((io.restassured.response.ResponseBody<?>) body).asString();
                    if (!bodyString.isEmpty()) {
                        actualJsonObject = (org.json.simple.JSONObject) parser.parse(bodyString);
                    }
                } catch (ClassCastException e) {
                    String bodyString = ((io.restassured.response.ResponseBody<?>) body).asString();
                    if (!bodyString.isEmpty()) {
                        actualJsonArray = (org.json.simple.JSONArray) parser.parse(bodyString);
                    }
                } catch (ParseException e) {
                    // happens in case of ZIP file.......
                    return 3;
                }
            } else if (body instanceof org.json.simple.JSONObject) {
                actualJsonObject = (org.json.simple.JSONObject) body;
            } else if (body instanceof org.json.simple.JSONArray) {
                actualJsonArray = (org.json.simple.JSONArray) body;
            } else if (body.getClass().getName().toLowerCase().contains("jsonobject")) {
                actualJsonObject = (org.json.simple.JSONObject) parser
                        .parse(body.toString().replace("\\n", "").replace("\\t", "").replace(" ", ""));
            } else {
                actualJsonObject = (org.json.simple.JSONObject) parser.parse(body.toString());
            }
            return 1; // json
        } catch (Exception e) {
            // response is not parsable to JSON
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            ObjectOutputStream oos;
            try {
                oos = new ObjectOutputStream(baos);
                oos.writeObject(body);
                oos.flush();
                oos.close();
                return 4; // I don't remember... may be binary
            } catch (IOException ioe) {
                if (body.getClass().getName().toLowerCase().contains("restassured")) {
                    // if it's a string response body
                    return 2; // xml
                } else {
                    return 3; // binary
                }
            }
        }
    }

    private static InputStream parseJsonBody(Object body) throws ParseException {
        JSONParser parser = new JSONParser();
        org.json.simple.JSONObject actualJsonObject = null;
        org.json.simple.JSONArray actualJsonArray = null;
        if (body.getClass().getName().toLowerCase().contains("restassured")) {
            try {
                // if it's a string response body
                String bodyString = ((io.restassured.response.ResponseBody<?>) body).asString();
                if (!bodyString.isEmpty()) {
                    actualJsonObject = (org.json.simple.JSONObject) parser.parse(bodyString);
                }
            } catch (ClassCastException e) {
                // java.lang.ClassCastException: org.json.simple.JSONArray cannot be cast to
                // org.json.simple.JSONObject
                String bodyString = ((io.restassured.response.ResponseBody<?>) body).asString();
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

    @SuppressWarnings("unchecked")
    private static boolean compareJSONContains(Response response, org.json.simple.JSONObject expectedJsonObject,
                                               org.json.simple.JSONArray expectedJsonArray, org.json.simple.JSONObject actualJsonObject,
                                               String jsonPathToTargetArray)
            throws JSONException, ParseException {
        JSONParser parser = new JSONParser();
        if (!jsonPathToTargetArray.equals("") && (expectedJsonArray != null)) {
            // if expected is an array and the user provided the path to extract it from the
            // response
            org.json.simple.JSONArray actualJsonArray = (org.json.simple.JSONArray) parser
                    .parse((new Gson()).toJsonTree(getResponseJSONValueAsList(response, jsonPathToTargetArray))
                            .getAsJsonArray().toString());
            return actualJsonArray.containsAll(expectedJsonArray);
        } else if (jsonPathToTargetArray.equals("") && (expectedJsonArray != null)) {
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
        } catch (TransformerException e) {
            ReportManager.log(e);
            return input;
        }
    }

    private static void initializeSystemProperties(boolean readPropertyFilesBeforeInitializing) {
        if (readPropertyFilesBeforeInitializing) {
            PropertyFileManager.readPropertyFiles();
        }
        HTTP_SOCKET_TIMEOUT = Integer.parseInt(System.getProperty("apiSocketTimeout"));
        // timeout between two consecutive data packets in seconds
        HTTP_CONNECTION_TIMEOUT = Integer.parseInt(System.getProperty("apiConnectionTimeout"));
        // timeout until a connection is established in seconds
        HTTP_CONNECTION_MANAGER_TIMEOUT = Integer
                .parseInt(System.getProperty("apiConnectionManagerTimeout"));

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

    public RequestBuilder buildNewRequest(String serviceName, RequestType requestType) {
        return new RequestBuilder(this, serviceName, requestType);
    }

    private RequestSpecBuilder initializeBuilder(Map<String, Object> sessionCookies, Map<String, String> sessionHeaders) {
        RequestSpecBuilder builder = new RequestSpecBuilder();

        builder.addCookies(sessionCookies);
        builder.addHeaders(sessionHeaders);

        // fixing issue with non-unicode content being encoded with a non UTF-8 charset
        // adding timeouts
        builder.setConfig(
                (new RestAssuredConfig()).encoderConfig((new EncoderConfig()).defaultContentCharset("UTF-8")).and()
                        .httpClient(HttpClientConfig.httpClientConfig()
                                .setParam("http.connection.timeout", HTTP_CONNECTION_TIMEOUT * 1000)
                                .setParam("http.socket.timeout", HTTP_SOCKET_TIMEOUT * 1000)
                                .setParam("http.connection-manager.timeout", HTTP_CONNECTION_MANAGER_TIMEOUT * 1000)));

        // timeouts documentation
        /*
         * CoreConnectionPNames.SO_TIMEOUT='http.socket.timeout': defines the socket
         * timeout (SO_TIMEOUT) in milliseconds, which is the timeout for waiting for
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

    /**
     * Attempts to perform a request to a REST API, then checks the response status code, if it matches the target code the step is passed and the response is returned. Otherwise the action fails.
     *
     * @param requestType      POST, GET, PATCH, DELETE, PUT
     * @param targetStatusCode default success code is 200
     * @param serviceName      /servicePATH/serviceNAME
     * @return Response; returns the full response object for further manipulation
     */
    public Response performRequest(RequestType requestType, int targetStatusCode, String serviceName) {
        return performRequest(
                new Object[]{requestType, targetStatusCode, serviceName, null, null, null, null, ContentType.ANY});
    }

    /**
     * Attempts to perform a request to a REST API, then checks the response status code, if it matches the target code the step is passed and the response is returned. Otherwise the action fails.
     *
     * @param requestType      POST, GET, PATCH, DELETE, PUT
     * @param targetStatusCode default success code is 200
     * @param serviceName      /servicePATH/serviceNAME
     * @param urlArguments     '&amp;' separated arguments without a preceding '?',
     *                         is nullable, Example:
     *                         "username=test&amp;password=test"
     * @return Response; returns the full response object for further manipulation
     */
    public Response performRequest(RequestType requestType, int targetStatusCode, String serviceName,
                                   String urlArguments) {
        return performRequest(new Object[]{requestType, targetStatusCode, serviceName, urlArguments, null, null, null,
                ContentType.ANY});
    }

    /**
     * Attempts to perform a request to a REST API, then checks the response status code, if it matches the target code the step is passed and the response is returned. Otherwise the action fails.
     *
     * @param requestType      POST, GET, PATCH, DELETE, PUT
     * @param targetStatusCode default success code is 200
     * @param serviceName      /servicePATH/serviceNAME
     * @param contentType      Enumeration of common IANA content-types. This may be
     *                         used to specify a request or response content-type
     *                         more easily than specifying the full string each
     *                         time. Example: ContentType.ANY
     * @return Response; returns the full response object for further manipulation
     */
    public Response performRequest(RequestType requestType, int targetStatusCode, String serviceName,
                                   ContentType contentType) {
        return performRequest(
                new Object[]{requestType, targetStatusCode, serviceName, null, null, null, null, contentType});
    }

    /**
     * Attempts to perform a request to a REST API, then checks the response status code, if it matches the target code the step is passed and the response is returned. Otherwise the action fails.
     *
     * @param requestType      POST, GET, PATCH, DELETE, PUT
     * @param targetStatusCode default success code is 200
     * @param serviceName      /servicePATH/serviceNAME
     * @param contentType      Enumeration of common IANA content-types. This may be
     *                         used to specify a request or response content-type
     *                         more easily than specifying the full string each
     *                         time. Example: ContentType.ANY
     * @param urlArguments     '&amp;' separated arguments without a preceding '?',
     *                         is nullable, Example:
     *                         "username=test&amp;password=test"
     * @return Response; returns the full response object for further manipulation
     */
    public Response performRequest(RequestType requestType, int targetStatusCode, String serviceName,
                                   ContentType contentType, String urlArguments) {
        return performRequest(new Object[]{requestType, targetStatusCode, serviceName, urlArguments, null, null, null,
                contentType});
    }

    /**
     * Attempts to perform a request to a REST API, then checks the response status code, if it matches the target code the step is passed and the response is returned. Otherwise the action fails.
     *
     * @param requestType      POST, GET, PATCH, DELETE, PUT
     * @param targetStatusCode default success code is 200
     * @param serviceName      /servicePATH/serviceNAME
     * @param parameters       a list of key/value pairs that will be sent as
     *                         parameters with this API call, is nullable, Example:
     *                         Arrays.asList(Arrays.asList("itemId", "123"),
     *                         Arrays.asList("contents", XMLcontents));
     * @param parametersType   FORM, QUERY
     * @param contentType      Enumeration of common IANA content-types. This may be
     *                         used to specify a request or response content-type
     *                         more easily than specifying the full string each
     *                         time. Example: ContentType.ANY
     * @return Response; returns the full response object for further manipulation
     */
    public Response performRequest(RequestType requestType, int targetStatusCode, String serviceName,
                                   List<List<Object>> parameters, ParametersType parametersType, ContentType contentType) {
        return performRequest(new Object[]{requestType, targetStatusCode, serviceName, null, parameters,
                parametersType, null, contentType});
    }

    /**
     * Attempts to perform a request to a REST API, then checks the response status code, if it matches the target code the step is passed and the response is returned. Otherwise the action fails.
     *
     * @param requestType      POST, GET, PATCH, DELETE, PUT
     * @param targetStatusCode default success code is 200
     * @param serviceName      /servicePATH/serviceNAME
     * @param requestBody      Specify an Object request content that will
     *                         automatically be serialized to JSON or XML and sent
     *                         with the request. If the object is a primitive or
     *                         Number the object will be converted to a String and
     *                         put in the request body. This works for the POST, PUT
     *                         and PATCH methods only. Trying to do this for the
     *                         other http methods will cause an exception to be
     *                         thrown, is nullable in case there is no body for that
     *                         request
     * @param contentType      Enumeration of common IANA content-types. This may be
     *                         used to specify a request or response content-type
     *                         more easily than specifying the full string each
     *                         time. Example: ContentType.ANY
     * @return Response; returns the full response object for further manipulation
     */
    public Response performRequest(RequestType requestType, int targetStatusCode, String serviceName,
                                   Object requestBody, ContentType contentType) {
        return performRequest(
                new Object[]{requestType, targetStatusCode, serviceName, null, null, null, requestBody, contentType});
    }

    protected String prepareRequestURL(String serviceURI, String urlArguments, String serviceName) {
        if (urlArguments != null && !urlArguments.equals("")) {
            return serviceURI + serviceName + ARGUMENTSEPARATOR + urlArguments;
        } else {
            return serviceURI + serviceName;
        }
    }

    protected RequestSpecification prepareRequestSpecs(List<List<Object>> parameters, ParametersType parametersType,
                                                       Object body, ContentType contentType, Map<String, Object> sessionCookies, Map<String, String> sessionHeaders) {
        RequestSpecBuilder builder = initializeBuilder(sessionCookies, sessionHeaders);

        // set the default content type as part of the specs
        builder.setContentType(contentType);

        if (body != null && contentType != null && !body.toString().equals("")) {
            prepareRequestBody(builder, body, contentType);
        } else if (parameters != null && !parameters.isEmpty() && !parameters.get(0).get(0).equals("")) {
            prepareRequestBody(builder, parameters, parametersType);
        }
        return builder.build();
    }

    private void prepareRequestBody(RequestSpecBuilder builder, Object body, ContentType contentType) {
        try {
            switch (contentType) {
                case JSON -> builder.setBody(body, ObjectMapperType.GSON);
                case XML -> builder.setBody(body, ObjectMapperType.JAXB);
                default -> builder.setBody(body);
            }
        } catch (Exception rootCauseException) {
            ReportManager.log(rootCauseException);
            failAction("Issue with parsing body content", rootCauseException);

        }
    }

    private void prepareRequestBody(RequestSpecBuilder builder, List<List<Object>> parameters,
                                    ParametersType parametersType) {
        parameters.forEach(param -> {
            if (param.get(1).getClass().equals(File.class)) {
                MultiPartSpecBuilder multispec = new MultiPartSpecBuilder(param.get(1));
                multispec.controlName(param.get(0).toString());
                String fileName = ((File) param.get(1)).getName();
                multispec.fileName(fileName);
                String mimeType;
                mimeType = URLConnection.guessContentTypeFromName(((File) param.get(1)).getName());
                if (mimeType == null) {
                    mimeType = MimeUtil2.getMostSpecificMimeType(MimeUtil.getMimeTypes(fileName)).toString();
                }
                multispec.mimeType(mimeType);
                builder.addMultiPart(multispec.build());
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
            case POST:
                return given().spec(specs).when().post(request).andReturn();
            case PATCH:
                return given().spec(specs).when().patch(request).andReturn();
            case PUT:
                return given().spec(specs).when().put(request).andReturn();
            case GET:
                return given().spec(specs).when().get(request).andReturn();
            case DELETE:
                return given().spec(specs).when().delete(request).andReturn();
            default:
                break;
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
            boolean discreetLoggingState = ReportManager.isDiscreteLogging();
            ReportManager.setDiscreteLogging(true);
            ReportManager.log("Response status code: [" + response.getStatusCode() + "], status line: [" + response.getStatusLine() + "]");
            Assertions.assertEquals(targetStatusCode, response.getStatusCode(),
                    "Evaluating the actual response status code against the expected one...");
            ReportManager.setDiscreteLogging(discreetLoggingState);
            return true;
        } catch (AssertionError rootCauseException) {
            return false;
        }
    }

    /**
     * Attempts to perform POST/PATCH/GET/DELETE request to a REST API, then checks
     * the response status code, if it matches the target code the step is passed
     * and the response is returned. Otherwise the action fails and NULL is
     * returned.
     *
     * @param params 0.requestType      POST/PATCH/GET/DELETE
     *               1.targetStatusCode default success code is 200
     *               2.serviceName      /servicePATH/serviceNAME
     *               3.urlArguments     '&amp;' separated arguments without a preceding '?',
     *               is nullable, Example:
     *               "username=test&amp;password=test"
     *               4.parameters   a list of key/value pairs that will be sent as
     *               parameters with this API call, is nullable, Example:
     *               Arrays.asList(Arrays.asList("itemId", "123"),
     *               Arrays.asList("contents", XMLcontents));
     *               5.parametersType
     *               6.requestBody      Specify an Object request content that will
     *               automatically be serialized to JSON or XML and sent
     *               with the request. If the object is a primitive or
     *               Number the object will be converted to a String and
     *               put in the request body. This works for the POST, PUT
     *               and PATCH methods only. Trying to do this for the
     *               other http methods will cause an exception to be
     *               thrown, is nullable in case there is no body for that
     *               request
     *               7.contentType      Enumeration of common IANA content-types. This may be
     *               used to specify a request or response content-type
     *               more easily than specifying the full string each
     *               time. Example: ContentType.ANY
     * @return Response; returns the full response object for further manipulation
     */
    protected Response performRequest(Object[] params) {
        RequestType requestType = (RequestType) params[0];
        int targetStatusCode = (int) params[1];
        String serviceName = (String) params[2];
        String urlArguments = (String) params[3];
        @SuppressWarnings("unchecked")
        List<List<Object>> parameters = (List<List<Object>>) params[4];
        ParametersType parametersType = (ParametersType) params[5];
        Object requestBody = params[6];
        ContentType contentType = (ContentType) params[7];

        String request = prepareRequestURL(serviceURI, urlArguments, serviceName);

        RequestSpecification specs = prepareRequestSpecs(parameters, parametersType, requestBody, contentType, sessionCookies, sessionHeaders);

        Response response = null;
        try {
            if (requestType.equals(RequestType.POST) || requestType.equals(RequestType.PATCH)
                    || requestType.equals(RequestType.PUT) || requestType.equals(RequestType.GET)
                    || requestType.equals(RequestType.DELETE)) {
                response = sendRequest(requestType, request, specs);
            } else {
                failAction(request);
            }

            boolean responseStatus = evaluateResponseStatusCode(Objects.requireNonNull(response), targetStatusCode);
            String reportMessage = prepareReportMessage(response, targetStatusCode, requestType, serviceName,
                    contentType, urlArguments);
            if (!"".equals(reportMessage) && Boolean.TRUE.equals(responseStatus)) {
                passAction(reportMessage, requestBody, response);
            } else {
                failAction(reportMessage, requestBody, response);
            }
        } catch (Exception rootCauseException) {
            ReportManager.log(rootCauseException);
            if (response != null) {
                failAction(request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms", requestBody,
                        response, rootCauseException);
            } else {
                failAction(request, rootCauseException);
            }
        }
        return response;
    }

    String prepareReportMessage(Response response, int targetStatusCode, RequestType requestType,
                                String serviceName, ContentType contentType, String urlArguments) {
        if (response != null) {
            extractCookiesFromResponse(response);
            extractHeadersFromResponse(response);

            StringBuilder reportMessage = new StringBuilder();
            reportMessage.append("Request Type: \"").append(requestType).append("\"");
            reportMessage.append(" | Target Status Code: \"").append(targetStatusCode).append("\"");
            reportMessage.append(" | Service URL: \"").append(serviceURI).append(serviceName).append("\"");
            reportMessage.append(" | Content Type: \"").append(contentType).append("\"");
            reportMessage.append(" | Response Time: \"").append(response.timeIn(TimeUnit.MILLISECONDS)).append("ms\"");

            if (urlArguments != null) {
                reportMessage.append(" | URL Arguments: \"").append(urlArguments).append("\"");
            }

            return reportMessage.toString().trim();
        }
        return "";
    }

    public enum ComparisonType {
        EQUALS, CONTAINS
    }

    public enum ParametersType {
        FORM, QUERY
    }

    public enum RequestType {
        POST, GET, PATCH, DELETE, PUT
    }
}
