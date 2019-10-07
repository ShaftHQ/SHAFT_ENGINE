package com.shaft.api;

import static io.restassured.RestAssured.given;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import javax.xml.XMLConstants;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.apache.commons.io.IOUtils;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.skyscreamer.jsonassert.JSONCompare;
import org.skyscreamer.jsonassert.JSONCompareMode;
import org.testng.Assert;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.support.JavaActions;
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

public class RestActions {
    private static final String ARGUMENTSEPARATOR = "?";

    private String headerAuthorization;
    private Map<String, String> sessionCookies;
    private Map<String, String> sessionHeaders;
    private String serviceURI;
    private static final String ERROR_NOT_FOUND = "Either actual value is \"null\" or couldn't find anything that matches with the desired ";
    private static final String ERROR_INCORRECT_JSONPATH = "Incorrect jsonPath ";
    private static final String ERROR_INCORRECT_XMLPATH = "Incorrect xmlPath ";

    private static final int HTTP_SOCKET_TIMEOUT = Integer.parseInt(System.getProperty("apiSocketTimeout"));
    // timeout between two consecutive data packets in seconds
    private static final int HTTP_CONNECTION_TIMEOUT = Integer.parseInt(System.getProperty("apiConnectionTimeout"));
    // timeout until a connection is established in seconds
    private static final int HTTP_CONNECTION_MANAGER_TIMEOUT = Integer
	    .parseInt(System.getProperty("apiConnectionManagerTimeout"));
    // timeout to wait for an available connection from the connection manager/pool

    public enum ComparisonType {
	EQUALS, CONTAINS;
    }

    public RestActions(String serviceURI) {
	headerAuthorization = "";
	sessionCookies = new HashMap<>();
	sessionHeaders = new HashMap<>();
	this.serviceURI = serviceURI;
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [private] Reporting Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private static void passAction(String actionName, String testData, Object requestBody, Response response,
	    Boolean isDiscrete, List<List<Object>> attachments) {
	String message = "Successfully performed action [" + actionName + "].";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}
	Boolean initialLoggingState = ReportManager.isDiscreteLogging();

	if (isDiscrete) {
	    // attach body
	    if (requestBody != null && !requestBody.equals(new JsonObject())) {
		reportRequestBody(requestBody);
	    }
	    reportResponseBody(response, isDiscrete);
	    ReportManager.logDiscrete(message);
	} else {
	    if (attachments != null && requestBody == null && response == null) {
		// compareJSON
		ReportManager.log(message, attachments);
	    } else {
		// performRequest
		// attach body
		List<Object> requestBodyAttachment = null;
		if (requestBody != null && !requestBody.equals(new JsonObject())) {
		    requestBodyAttachment = reportRequestBody(requestBody);
		}

		List<Object> responseBodyAttachment = reportResponseBody(response, initialLoggingState);
		if (!initialLoggingState) {
		    ReportManager.log(message, Arrays.asList(requestBodyAttachment, responseBodyAttachment));
		} else {
		    ReportManager.logDiscrete(message);
		}
	    }
	}
    }

    private static void passAction(String actionName, String testData, Boolean isDiscrete) {
	passAction(actionName, testData, null, null, isDiscrete, null);
    }

    private static void passAction(String actionName, String testData, Boolean isDiscrete,
	    List<List<Object>> attachments) {
	passAction(actionName, testData, null, null, isDiscrete, attachments);
    }

    private static void failAction(String actionName, String testData, Object requestBody, Response response) {
	String message = "Failed to perform action [" + actionName + "].";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}

	// attach body
	List<Object> requestBodyAttachment = null;
	if (requestBody != null && !requestBody.equals(new JsonObject())) {
	    requestBodyAttachment = reportRequestBody(requestBody);
	}

	if (response != null) {
	    ReportManager.log(message, Arrays.asList(requestBodyAttachment, reportResponseBody(response, false)));
	} else {
	    ReportManager.log(message, Arrays.asList(requestBodyAttachment));
	}
	Assert.fail(message);
    }

    private static void failAction(String actionName, String testData) {
	failAction(actionName, testData, null, null);
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [private] Preparation and Support Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private String prepareRequestURL(String urlArguments, String serviceName) {
	if (urlArguments != null && !urlArguments.equals("")) {
	    return serviceURI + serviceName + ARGUMENTSEPARATOR + urlArguments;
	} else {
	    return serviceURI + serviceName;
	}
    }

    private void prepareRequestHeaderAuthorization(String[] credentials) {
	if (headerAuthorization.equals("") && credentials.length == 2) {
	    headerAuthorization = "Basic " + JavaActions.convertBase64(credentials[0] + ":" + credentials[1]);

	    sessionHeaders.put("Authorization", headerAuthorization);
	}
    }

    private RequestSpecification prepareRequestSpecs(List<List<Object>> formParameters, Object body,
	    ContentType contentType) {
	RequestSpecBuilder builder = new RequestSpecBuilder();

	// set the default content type as part of the specs
	builder.setContentType(contentType);

	// fixing issue with non-unicode content being encoded with a non UTF-8 charset
	// adding timeouts
	builder.setConfig(
		(new RestAssuredConfig()).encoderConfig((new EncoderConfig()).defaultContentCharset("UTF-8")).and()
			.httpClient(HttpClientConfig.httpClientConfig()
				.setParam("http.connection.timeout", HTTP_CONNECTION_TIMEOUT * 1000)
				.setParam("http.socket.timeout", HTTP_SOCKET_TIMEOUT * 1000)
				.setParam("http.connection-manager.timeout", HTTP_CONNECTION_MANAGER_TIMEOUT * 1000)));

	// timeouts documentation
	/**
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

	if (body != null && contentType != null && !body.toString().equals("")) {
	    try {
		switch (contentType) {
		case JSON:
		    builder.setBody(body, ObjectMapperType.GSON);
		    break;
		case XML:
		    builder.setBody(body, ObjectMapperType.JAXB);
		    break;
		default:
		    builder.setBody(body);
		    break;
		}
//		// attach body
//		if (!body.equals(new JsonObject())) {
//		    reportRequestBody(body);
//		}
	    } catch (Exception e) {
		ReportManager.log(e);
		failAction("performRequest", "Issue with parsing body content");

	    }
	} else if (formParameters != null && !formParameters.isEmpty() && !formParameters.get(0).get(0).equals("")) {
	    formParameters.forEach(param -> {
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
		    builder.addFormParam(param.get(0).toString(), param.get(1));
		}
	    });
	}
	return builder.build();
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
		    // binary... probably
		    requestBodyAttachment.add("Body");
		    break;
		case 4:
		    // I don't remember... may be binary
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
	    if (isDiscrete) {
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
		    // binary... probably
		    responseBodyAttachment.add("Body");
		    break;
		case 4:
		    // I don't remember... may be binary
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

    private static int identifyBodyObjectType(Object body) {
	// TODO: refactor to reduce complexity
	JSONParser parser = new JSONParser();
	try {
	    org.json.simple.JSONObject actualJsonObject = null;
	    org.json.simple.JSONArray actualJsonArray = null;
	    if (body.getClass().getName().toLowerCase().contains("restassured")) {
		// if it's a string (OR ARRAY) response body
		try {
		    actualJsonObject = (org.json.simple.JSONObject) parser
			    .parse(((io.restassured.response.ResponseBody<?>) body).asString());
		} catch (ClassCastException e) {
		    actualJsonArray = (org.json.simple.JSONArray) parser
			    .parse(((io.restassured.response.ResponseBody<?>) body).asString());
		} catch (ParseException e) {
		    // happens in case of ZIP file.......
		    return 3;
		}
	    } else if (body.getClass().getName().toLowerCase().contains("jsonobject")) {
		actualJsonObject = (org.json.simple.JSONObject) parser
			.parse(((JsonObject) body).toString().replace("\\n", "").replace("\\t", "").replace(" ", ""));
	    } else if (body instanceof org.json.simple.JSONArray) {
		actualJsonArray = (org.json.simple.JSONArray) body;
	    } else {
		actualJsonObject = (org.json.simple.JSONObject) parser.parse(body.toString());
	    }
	    if (actualJsonObject != null) {
		actualJsonObject.toString(); // useless
	    } else if (actualJsonArray != null) {
		actualJsonArray.toString(); // useless
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

    private static InputStream parseBodyToJson(Response response) {
	return parseBodyToJson(response.getBody());
    }

    private static InputStream parseBodyToJson(Object body) {
	JSONParser parser = new JSONParser();
	try {
	    org.json.simple.JSONObject actualJsonObject = null;
	    org.json.simple.JSONArray actualJsonArray = null;
	    if (body.getClass().getName().toLowerCase().contains("restassured")) {
		try {
		    // if it's a string response body
		    actualJsonObject = (org.json.simple.JSONObject) parser
			    .parse(((io.restassured.response.ResponseBody<?>) body).asString());
		} catch (java.lang.ClassCastException e) {
		    // java.lang.ClassCastException: org.json.simple.JSONArray cannot be cast to
		    // org.json.simple.JSONObject
		    actualJsonArray = (org.json.simple.JSONArray) parser
			    .parse(((io.restassured.response.ResponseBody<?>) body).asString());
		}
	    } else if (body.getClass().getName().toLowerCase().contains("jsonobject")) {
		actualJsonObject = (org.json.simple.JSONObject) parser
			.parse(((JsonObject) body).toString().replace("\\n", "").replace("\\t", "").replace(" ", ""));
	    } else if (body instanceof org.json.simple.JSONArray) {
		actualJsonArray = (org.json.simple.JSONArray) body;
	    } else {
		actualJsonObject = (org.json.simple.JSONObject) parser.parse(body.toString());
	    }
	    if (actualJsonObject != null) {
		return new ByteArrayInputStream((new GsonBuilder().setPrettyPrinting().create()
			.toJson(new JsonParser().parse(actualJsonObject.toJSONString()))).getBytes());
	    } else {
		return new ByteArrayInputStream((new GsonBuilder().setPrettyPrinting().create()
			.toJson(new JsonParser().parse(actualJsonArray.toJSONString()))).getBytes());
	    }
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

    private Response sendRequest(String requestType, String request, RequestSpecification specs) {
	if (sessionCookies.size() == 0 && sessionHeaders.size() > 0) {
	    switch (requestType.toLowerCase()) {
	    case "post":
		return given().headers(sessionHeaders).spec(specs).when().post(request).andReturn();
	    case "patch":
		return given().headers(sessionHeaders).spec(specs).when().patch(request).andReturn();
	    case "get":
		return given().headers(sessionHeaders).spec(specs).when().get(request).andReturn();
	    case "delete":
		return given().headers(sessionHeaders).spec(specs).when().delete(request).andReturn();
	    default:
		break;
	    }
	} else if (sessionCookies.size() == 0 && sessionHeaders.size() == 0) {
	    switch (requestType.toLowerCase()) {
	    case "post":
		return given().spec(specs).when().post(request).andReturn();
	    case "patch":
		return given().spec(specs).when().patch(request).andReturn();
	    case "get":
		return given().spec(specs).when().get(request).andReturn();
	    case "delete":
		return given().spec(specs).when().delete(request).andReturn();
	    default:
		break;
	    }
	} else {
	    switch (requestType.toLowerCase()) {
	    case "post":
		return given().headers(sessionHeaders).cookies(sessionCookies).spec(specs).when().post(request)
			.andReturn();
	    case "patch":
		return given().headers(sessionHeaders).cookies(sessionCookies).spec(specs).when().patch(request)
			.andReturn();
	    case "get":
		return given().headers(sessionHeaders).cookies(sessionCookies).spec(specs).when().get(request)
			.andReturn();
	    case "delete":
		return given().headers(sessionHeaders).cookies(sessionCookies).spec(specs).when().delete(request)
			.andReturn();
	    default:
		break;
	    }
	}
	return null;
    }

    private void extractCookiesFromResponse(Response response) {
	if (response.getDetailedCookies().size() > 0) {
	    if (sessionCookies == null) {
		sessionCookies = response.getCookies();
	    } else {
		for (Cookie cookie : response.getDetailedCookies()) {
		    sessionCookies.put(cookie.getName(), cookie.getValue());

		    if (cookie.getName().equals("XSRF-TOKEN")) {
			sessionHeaders.put("X-XSRF-TOKEN", cookie.getValue());
		    }
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

    private void assertResponseStatusCode(String request, Object requestBody, Response response,
	    String targetStatusCode) {
	try {
	    Boolean discreetLoggingState = ReportManager.isDiscreteLogging();
	    ReportManager.setDiscreteLogging(true);
	    Assertions.assertEquals(targetStatusCode, String.valueOf(response.getStatusCode()), 1, true);
	    ReportManager.setDiscreteLogging(discreetLoggingState);
	    passAction("performRequest", request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms",
		    requestBody, response, false, null);
	} catch (AssertionError e) {
	    failAction("performRequest", request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms",
		    requestBody, response);
	}
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [Public] Core REST Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * Attempts to perform POST/PATCH/GET/DELETE request to a REST API, then checks
     * the response status code, if it matches the target code the step is passed
     * and the response is returned. Otherwise the action fails and NULL is
     * returned.
     * 
     * @param requestType      POST/PATCH/GET/DELETE
     * @param targetStatusCode default success code is 200
     * @param serviceName      /servicePATH/serviceNAME
     * @param urlArguments     '&amp;' separated arguments without a preceding '?',
     *                         is nullable, Example:
     *                         "username=test&amp;password=test"
     * @param formParameters   a list of key/value pairs that will be sent as
     *                         parameters with this API call, is nullable, Example:
     *                         Arrays.asList(Arrays.asList("itemId", "123"),
     *                         Arrays.asList("contents", XMLcontents));
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
     * @param credentials      an optional array of strings that holds the username,
     *                         password that will be used for the
     *                         headerAuthorization of this request
     * @return Response; returns the full response object for further manipulation
     */
    public Response performRequest(String requestType, String targetStatusCode, String serviceName, String urlArguments,
	    List<List<Object>> formParameters, Object requestBody, ContentType contentType, String... credentials) {

	String request = prepareRequestURL(urlArguments, serviceName);
	prepareRequestHeaderAuthorization(credentials);

	RequestSpecification specs = prepareRequestSpecs(formParameters, requestBody, contentType);

	Response response = null;
	try {
	    if (requestType.equalsIgnoreCase("post") || requestType.equalsIgnoreCase("patch")
		    || requestType.equalsIgnoreCase("get") || requestType.equalsIgnoreCase("delete")) {
		response = sendRequest(requestType, request, specs);
	    } else {
		failAction("performRequest", request);
	    }

	    if (response != null) {
		extractCookiesFromResponse(response);
		extractHeadersFromResponse(response);
		assertResponseStatusCode(request, requestBody, response, targetStatusCode);
	    }
	} catch (Exception e) {
	    ReportManager.log(e);
	    if (response != null) {
		failAction("performRequest",
			request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms", requestBody,
			response);
	    } else {
		failAction("performRequest", request);
	    }
	}
	return response;
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
	} catch (ClassCastException e) {
	    ReportManager.log(ERROR_INCORRECT_JSONPATH + "[" + jsonPath + "]");
	    failAction("getResponseJSONValue", jsonPath);
	} catch (JsonPathException e) {
	    ReportManager.log("Failed to parse the JSON document");
	    failAction("getResponseJSONValue", jsonPath);
	}
	if (searchPool != null) {
	    passAction("getResponseJSONValue", jsonPath, true);
	    return searchPool;
	} else {
	    ReportManager.logDiscrete(ERROR_NOT_FOUND + "jsonPath [" + jsonPath + "]");
	    passAction("getResponseJSONValue", jsonPath, true);
	    return searchPool;
	}
    }

    public static String getResponseJSONValue(Object response, String jsonPath) {
	@SuppressWarnings("unchecked")
	JSONObject obj = new JSONObject((java.util.HashMap<String, String>) response);

	String searchPool = "";
	try {
	    searchPool = JsonPath.from(obj.toString()).getString(jsonPath);
	} catch (ClassCastException e) {
	    ReportManager.log(ERROR_INCORRECT_JSONPATH + "[" + jsonPath + "]");
	    failAction("getResponseJSONValue", jsonPath);
	} catch (JsonPathException e) {
	    ReportManager.log("Failed to parse the JSON document");
	    failAction("getResponseJSONValue", jsonPath);
	}
	if (searchPool != null) {
	    passAction("getResponseJSONValue", jsonPath, true);
	    return searchPool;
	} else {
	    ReportManager.logDiscrete(ERROR_NOT_FOUND + "jsonPath [" + jsonPath + "]");
	    passAction("getResponseJSONValue", jsonPath, true);
	    return searchPool;
	}
    }

    public static List<Object> getResponseJSONValueAsList(Response response, String jsonPath) {
	List<Object> searchPool = null;
	try {
	    searchPool = response.jsonPath().getList(jsonPath);
	} catch (ClassCastException e) {
	    ReportManager.log(ERROR_INCORRECT_JSONPATH + "[" + jsonPath + "]");
	    failAction("getResponseJSONValueAsList", jsonPath);
	} catch (JsonPathException e) {
	    ReportManager.log("Failed to parse the JSON document");
	    failAction("getResponseJSONValueAsList", jsonPath);
	}
	
	if (searchPool != null) {
	    passAction("getResponseJSONValueAsList", jsonPath, true);
	    return searchPool;
	} else {
	    ReportManager.logDiscrete(ERROR_NOT_FOUND + "jsonPath [" + jsonPath + "]");
	    passAction("getResponseJSONValueAsList", jsonPath, true);
	    return searchPool;
	}
    }

    public static String getResponseXMLValue(Response response, String xmlPath) {
	String searchPool = "";
	try {
	    searchPool = response.xmlPath().getString(xmlPath);
	} catch (ClassCastException e) {
	    ReportManager.log(ERROR_INCORRECT_XMLPATH + "[" + xmlPath + "]");
	    failAction("getResponseXMLValue", xmlPath);

	}
	if (searchPool != null) {
	    passAction("getResponseXMLValue", xmlPath, true);
	    return searchPool;
	} else {
	    ReportManager.logDiscrete(ERROR_NOT_FOUND + "xmlPath [" + xmlPath + "]");
	    passAction("getResponseXMLValue", xmlPath, true);
	    return searchPool;
	}
    }

    public static String getResponseXMLValue(Object response, String xmlPath) {
	String output = "";
	try {
	    output = ((Node) response).getAttribute(xmlPath);
	} catch (ClassCastException e) {
	    ReportManager.log(ERROR_INCORRECT_XMLPATH + "[" + xmlPath + "]");
	    failAction("getResponseXMLValue", xmlPath);

	}
	if (output != null) {
	    passAction("getResponseXMLValue", xmlPath, true);
	    return output;
	} else {
	    ReportManager.logDiscrete(ERROR_NOT_FOUND + "xmlPath [" + xmlPath + "]");
	    passAction("getResponseXMLValue", xmlPath, true);
	    return output;
	}
    }

    public static List<Object> getResponseXMLValueAsList(Response response, String xmlPath) {
	NodeChildren output = null;
	try {
	    output = response.xmlPath().get(xmlPath);
	} catch (ClassCastException e) {
	    ReportManager.log(ERROR_INCORRECT_XMLPATH + "[" + xmlPath + "]");
	    failAction("getResponseXMLValueAsList", xmlPath);

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
	    passAction("getResponseXMLValueAsList", xmlPath, true);
	    return searchPool;
	} else {
	    ReportManager.logDiscrete(ERROR_NOT_FOUND + "xmlPath [" + xmlPath + "]");
	    passAction("getResponseXMLValueAsList", xmlPath, true);
	    return searchPool;
	}
    }

    public static int getResponseStatusCode(Response response) {
	int statusCode = response.getStatusCode();
	passAction("getResponseStatusCode", String.valueOf(statusCode), true);
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
     *         in case it failed
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
     *         in case it failed
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
	Boolean comparisonResult;
	JSONParser parser = new JSONParser();
	List<Object> expectedJSONAttachment = null;

	try {
	    org.json.simple.JSONObject actualJsonObject = (org.json.simple.JSONObject) parser
		    .parse(response.asString());
	    org.json.simple.JSONObject expectedJsonObject = null;
	    org.json.simple.JSONArray expectedJsonArray = null;
	    try {
		expectedJsonObject = (org.json.simple.JSONObject) parser.parse(new FileReader(referenceJsonFilePath));
		expectedJSONAttachment = Arrays.asList("File Content", "Expected JSON",
			new GsonBuilder().setPrettyPrinting().create()
				.toJson(new JsonParser().parse(expectedJsonObject.toJSONString())));

	    } catch (ClassCastException e) {
		// org.json.simple.JSONArray cannot be cast to org.json.simple.JSONObject
		expectedJsonArray = (org.json.simple.JSONArray) parser.parse(new FileReader(referenceJsonFilePath));
		expectedJSONAttachment = Arrays.asList("File Content", "Expected JSON", new GsonBuilder()
			.setPrettyPrinting().create().toJson(new JsonParser().parse(expectedJsonArray.toJSONString())));
	    }
	    switch (comparisonType) {
	    case EQUALS:
		// TODO: handle jsonPathToTargetArray and attempt to parse the actual result
		comparisonResult = compareJSONEquals(expectedJsonObject, expectedJsonArray, actualJsonObject);
		break;
	    case CONTAINS:
		comparisonResult = compareJSONContains(response, expectedJsonObject, expectedJsonArray,
			actualJsonObject, jsonPathToTargetArray);
		break;
	    default:
		comparisonResult = false;
		break;
	    }
	} catch (IOException e) {
	    ReportManager.log(e);
	    failAction("compareJSON", "Couldn't find the desired file. [" + referenceJsonFilePath + "].");
	    comparisonResult = false;
	} catch (ParseException | JSONException e) {
	    ReportManager.log(e);
	    failAction("compareJSON", "Couldn't parse the desired file. [" + referenceJsonFilePath + "].");
	    comparisonResult = false;
	}
	passAction("compareJSON", referenceJsonFilePath, true,
		Arrays.asList(expectedJSONAttachment, reportResponseBody(response, false)));
	return comparisonResult;
    }

    private static boolean compareJSONEquals(org.json.simple.JSONObject expectedJsonObject,
	    org.json.simple.JSONArray expectedJsonArray, org.json.simple.JSONObject actualJsonObject) {
	if (expectedJsonObject != null) {
	    // if expected is an object and actual is also an object
	    return actualJsonObject.toString().equals(expectedJsonObject.toString());
	} else {
	    // if expected is an array and actual response is also an array
	    // not tested
	    return actualJsonObject.toString().equals(expectedJsonArray.toString());
	}
    }

    @SuppressWarnings("unchecked")
    private static boolean compareJSONContains(Response response, org.json.simple.JSONObject expectedJsonObject,
	    org.json.simple.JSONArray expectedJsonArray, org.json.simple.JSONObject actualJsonObject,
	    String jsonPathToTargetArray) throws JSONException, ParseException {
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
	    return actual.contains(expected.substring(1, expected.length()));
	} else if (expectedJsonObject != null) {
	    // if expected is an object and actual is also an object
	    Boolean initialComparison = JSONCompare.compareJSON(expectedJsonObject.toJSONString(),
		    actualJsonObject.toJSONString(), JSONCompareMode.LENIENT).passed();
	    if (!initialComparison) {
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

    public static String formatXML(String input) {
	return prettyFormatXML(input, "2");
    }

    private static String prettyFormatXML(String input, String indent) {
	Source xmlInput = new StreamSource(new StringReader(input));
	StringWriter stringWriter = new StringWriter();
	try {
	    TransformerFactory transformerFactory = TransformerFactory.newInstance();
	    transformerFactory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
	    Transformer transformer = transformerFactory.newTransformer();
	    transformer.setOutputProperty(OutputKeys.INDENT, "yes");
	    transformer.setOutputProperty(OutputKeys.DOCTYPE_PUBLIC, "yes");
	    transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", indent);
	    transformer.transform(xmlInput, new StreamResult(stringWriter));
	    return stringWriter.toString().trim();
	} catch (TransformerException e) {
	    ReportManager.log(e);
	    return input;
	}
    }
}
