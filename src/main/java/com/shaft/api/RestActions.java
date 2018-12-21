package com.shaft.api;

import static io.restassured.RestAssured.given;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.testng.Assert;

import com.google.gson.JsonObject;
import com.shaft.io.ReportManager;
import com.shaft.support.JavaActions;
import com.shaft.validation.Assertions;

import io.restassured.http.Cookie;
import io.restassured.http.Header;
import io.restassured.path.json.exception.JsonPathException;
import io.restassured.response.Response;

public class RestActions {
    private static final String ARGUMENTSEPARATOR = "?";

    private String headerAuthorization;
    private Map<String, String> sessionCookies;
    private Map<String, String> sessionHeaders;
    private String serviceURI;

    public RestActions(String serviceURI) {
	headerAuthorization = "";
	sessionCookies = new HashMap<>();
	sessionHeaders = new HashMap<>();
	this.serviceURI = serviceURI;
    }

    private void passAction(String actionName, String testData, Response response) {
	String message = "Successfully performed action [" + actionName + "].";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}
	ReportManager.log(message);
	if (response != null) {
	    ReportManager.attachAsStep("API Response", "REST Body", response.getBody().asString());
	}
    }

    private void passAction(String actionName, String testData) {
	passAction(actionName, testData, null);
    }

    private void failAction(String actionName, String testData, Response response) {
	String message = "Failed to perform action [" + actionName + "].";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}
	ReportManager.log(message);
	if (response != null) {
	    ReportManager.attachAsStep("API Response", "REST Body", response.getBody().asString());
	}
	Assert.fail(message);
    }

    private void failAction(String actionName, String testData) {
	failAction(actionName, testData, null);
    }

    /**
     * Attempts to perform POST/GET/DELETE request to a REST API, then checks the
     * response status code, if it matches the target code the step is passed and
     * the response is returned. Otherwise the action fails and NULL is returned.
     * 
     * @param requestType
     *            POST/GET/DELETE
     * @param targetStatusCode
     *            200
     * @param serviceName
     *            /servicePATH/serviceNAME
     * @param argument
     *            arguments without a preceding ?
     * @param credentials
     * @return Response; returns the full response object for further manipulation
     */
    public Response performRequest(String requestType, String targetStatusCode, String serviceName, String argument, String... credentials) {

	String request = prepareRequest(argument, serviceName);
	prepareHeaders(credentials);

	Response response = null;
	try {
	    if (requestType.equalsIgnoreCase("post") || requestType.equalsIgnoreCase("get") || requestType.equalsIgnoreCase("delete")) {
		response = sendRequest(requestType, request);
	    } else {
		failAction("performRequest", request);
	    }

	    if (response != null) {
		extractCookiesFromResponse(response);
		extractHeadersFromResponse(response);

		assertResponseStatusCode(request, response, targetStatusCode);
	    }
	} catch (Exception e) {
	    ReportManager.log(e);
	    if (response != null) {
		failAction("performRequest", request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms", response);
	    } else {
		failAction("performRequest", request);
	    }
	}
	return response;
    }

    /**
     * Attempts to perform POST/PATCH request with Json body to a REST API, then
     * checks the response status code, if it matches the target code the step is
     * passed and the response is returned. Otherwise the action fails and NULL is
     * returned.
     * 
     * @param requestType
     *            POST/Patch
     * @param targetStatusCode
     *            200
     * @param serviceName
     *            /servicePATH/serviceNAME
     * @param argument
     *            arguments without a preceding ?
     * @param body
     *            Json Object for body data
     * @param credentials
     * @return Response; returns the full response object for further manipulation
     */
    public Response performRequest(String requestType, String targetStatusCode, String serviceName, String argument, JsonObject body, String... credentials) {

	String request = prepareRequest(argument, serviceName);
	prepareHeaders(credentials);

	Response response = null;
	try {
	    if (requestType.equalsIgnoreCase("post") || requestType.equalsIgnoreCase("patch")) {
		response = sendRequest(requestType, request, body);
	    } else {
		failAction("performRequest", request);
	    }

	    if (response != null) {
		extractCookiesFromResponse(response);
		extractHeadersFromResponse(response);

		assertResponseStatusCode(request, response, targetStatusCode);
	    }
	} catch (Exception e) {
	    ReportManager.log(e);
	    if (response != null) {
		failAction("performRequest", request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms", response);
	    } else {
		failAction("performRequest", request);
	    }
	}
	return response;
    }

    private String prepareRequest(String argument, String serviceName) {
	if (!argument.equals("")) {
	    return serviceURI + serviceName + ARGUMENTSEPARATOR + argument;
	} else {
	    return serviceURI + serviceName;
	}
    }

    private void prepareHeaders(String[] credentials) {
	if (headerAuthorization.equals("") && credentials.length == 2) {
	    headerAuthorization = "Basic " + JavaActions.convertBase64(credentials[0] + ":" + credentials[1]);

	    sessionHeaders.put("Authorization", headerAuthorization);
	}
    }

    private Response sendRequest(String requestType, String request) {
	if (sessionCookies.size() == 0 && sessionHeaders.size() > 0) {
	    switch (requestType.toLowerCase()) {
	    case "post":
		return given().headers(sessionHeaders).when().post(request).andReturn();
	    case "get":
		return given().headers(sessionHeaders).when().get(request).andReturn();
	    case "delete":
		return given().headers(sessionHeaders).when().delete(request).andReturn();
	    default:
		break;
	    }
	} else if (sessionCookies.size() == 0 && sessionHeaders.size() == 0) {
	    switch (requestType.toLowerCase()) {
	    case "post":
		return given().when().post(request).andReturn();
	    case "get":
		return given().when().get(request).andReturn();
	    case "delete":
		return given().when().delete(request).andReturn();
	    default:
		break;
	    }
	} else {
	    switch (requestType.toLowerCase()) {
	    case "post":
		return given().headers(sessionHeaders).cookies(sessionCookies).when().post(request).andReturn();
	    case "get":
		return given().headers(sessionHeaders).cookies(sessionCookies).when().get(request).andReturn();
	    case "delete":
		return given().headers(sessionHeaders).cookies(sessionCookies).when().delete(request).andReturn();
	    default:
		break;
	    }
	}
	return null;
    }

    private Response sendRequest(String requestType, String request, JsonObject body) {
	if (sessionCookies.size() == 0 && sessionHeaders.size() > 0) {
	    switch (requestType.toLowerCase()) {
	    case "post":
		return given().headers(sessionHeaders).body(body.toString()).when().post(request).andReturn();
	    case "patch":
		return given().headers(sessionHeaders).body(body.toString()).when().patch(request).andReturn();
	    default:
		break;
	    }
	} else if (sessionCookies.size() == 0 && sessionHeaders.size() == 0) {
	    switch (requestType.toLowerCase()) {
	    case "post":
		return given().when().body(body.toString()).post(request).andReturn();
	    case "patch":
		return given().when().body(body.toString()).patch(request).andReturn();
	    default:
		break;
	    }
	} else {
	    switch (requestType.toLowerCase()) {
	    case "post":
		return given().headers(sessionHeaders).cookies(sessionCookies).when().body(body.toString()).post(request).andReturn();
	    case "patch":
		return given().headers(sessionHeaders).cookies(sessionCookies).when().body(body.toString()).patch(request).andReturn();
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
	    if (response.jsonPath().getString("type").equals("Bearer")) {
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

    public String getResponseJSONValue(Response response, String jsonPath) {
	String searchPool = response.jsonPath().getString(jsonPath);
	if (searchPool != null) {
	    passAction("getResponseJSONValue", jsonPath);
	    return searchPool;
	} else {
	    ReportManager.log("Couldn't find anything that matches with the desired jsonPath [" + jsonPath + "]");
	    failAction("getResponseJSONValue", jsonPath);
	    return "";
	}
    }

    public String getResponseXMLValue(Response response, String xmlPath) {
	String searchPool = response.xmlPath().getString(xmlPath);
	if (searchPool != null) {
	    passAction("getResponseXMLValue", xmlPath);
	    return searchPool;
	} else {
	    ReportManager.log("Couldn't find anything that matches with the desired xmlPath [" + xmlPath + "]");
	    failAction("getResponseXMLValue", xmlPath);
	    return "";
	}
    }

    private void assertResponseStatusCode(String request, Response response, String targetStatusCode) {
	try {
	    Assertions.assertEquals(targetStatusCode, String.valueOf(response.getStatusCode()), 1, true);
	    passAction("performRequest", request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms", response);
	} catch (AssertionError e) {
	    failAction("performRequest", request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms", response);
	}
    }

    public int getResponseStatusCode(Response response) {
	return response.getStatusCode();
    }

}
