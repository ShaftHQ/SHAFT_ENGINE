package com.shaftEngine.restAssuredActionLibrary;

import static io.restassured.RestAssured.given;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.testng.Assert;

import com.shaftEngine.ioActionLibrary.ReportManager;
import com.shaftEngine.supportActionLibrary.JavaActions;

import io.restassured.http.Cookie;
import io.restassured.http.Header;
import io.restassured.path.json.exception.JsonPathException;
import io.restassured.response.Response;

public class RestActions {
//	private static String cookieJSessionID = "";
//	private static String headerXsrfToken = "";
	private static String headerAuthorization = "";

	private static Map<String, String> sessionCookies = new HashMap<>();
	private static Map<String, String> sessionHeaders = new HashMap<>();

	private static final String ARGUMENTSEPARATOR = "?";

	private static void passAction(String actionName, String testData, Response response) {
		String message = "Successfully performed action [" + actionName + "].";
		if (testData != null) {
			message = message + " With the following test data [" + testData + "].";
		}
		ReportManager.log(message);
		if (response != null) {
			ReportManager.attach("REST Response Body", response.getBody().asString());
		}
	}

	private static void passAction(String actionName, String testData) {
		passAction(actionName, testData, null);
	}

	private static void failAction(String actionName, String testData, Response response) {
		String message = "Failed to perform action [" + actionName + "].";
		if (testData != null) {
			message = message + " With the following test data [" + testData + "].";
		}
		ReportManager.log(message);
		if (response != null) {
			ReportManager.attach("REST Response Body", response.getBody().asString());
		}
		Assert.fail(message);
	}

	private static void failAction(String actionName, String testData) {
		failAction(actionName, testData, null);
	}

	/**
	 * Attempts to perform POST/GET request to a REST API, then checks the response
	 * status code, if it matches the target code the step is passed and the
	 * response is returned. Otherwise the action fails and NULL is returned.
	 * 
	 * @param requestType; POST/GET
	 * @param targetStatusCode; 200
	 * @param serviceURI; http://serviceURL.com:PORT/serviceROOT
	 * @param serviceName; /servicePATH/serviceNAME
	 * @param argument; arguments without a preceding ?
	 * @return Response; returns the full response object for further manipulation
	 */
	public static Response performRequest(String requestType, String targetStatusCode, String serviceURI,
			String serviceName, String argument, String... credentials) {
		String request;
		Response response = null;

		if (!argument.equals("")) {
			request = serviceURI + serviceName + ARGUMENTSEPARATOR + argument;
		} else {
			request = serviceURI + serviceName;
		}

		if (headerAuthorization.equals("") && credentials.length == 2) {
			headerAuthorization = "Basic " + JavaActions.convertBase64(credentials[0] + ":" + credentials[1]);

			sessionHeaders.put("Authorization", headerAuthorization);
		}

		try {

			if (sessionCookies.size() == 0 && sessionHeaders.size() > 0) {
				switch (requestType.toLowerCase()) {
				case "post":
					response = given().headers(sessionHeaders).when().post(request).andReturn();
					break;
				case "get":
					response = given().headers(sessionHeaders).when().get(request).andReturn();
					break;
				default:
					failAction("performRequest", request);
					break;
				}
			} else if (sessionCookies.size() == 0 && sessionHeaders.size() == 0) {
				switch (requestType.toLowerCase()) {
				case "post":
					response = given().when().post(request).andReturn();
					break;
				case "get":
					response = given().when().get(request).andReturn();
					break;
				default:
					failAction("performRequest", request);
					break;
				}
			} else {
				switch (requestType.toLowerCase()) {
				case "post":
					response = given().headers(sessionHeaders).cookies(sessionCookies).when().post(request).andReturn();
					break;
				case "get":
					response = given().headers(sessionHeaders).cookies(sessionCookies).when().get(request).andReturn();
					break;
				default:
					failAction("performRequest", request);
					break;
				}
			}

			if (response != null) {
				if (response.getDetailedCookies().size() > 0) {
					if (sessionCookies == null) {
						sessionCookies = response.getCookies();
					} else {
						for (Cookie cookie : response.getDetailedCookies()) {
							sessionCookies.put(cookie.getName(), cookie.getValue());
							///////////////////////

							if (cookie.getName().equals("XSRF-TOKEN")) {
								sessionHeaders.put("X-XSRF-TOKEN", cookie.getValue());
							}
						}
					}
				}

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
					}
				} catch (JsonPathException | NullPointerException e) {
					// do nothing if the "type" variable was not found
					// or if response was not json

					// JsonPathException | NullPointerException
				}

				if (assertResponseStatusCode(response, targetStatusCode)) {
					passAction("performRequest",
							request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms", response);
					return response;
				} else {
					failAction("performRequest",
							request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms", response);
				}
			}
		} catch (Exception e) {
			ReportManager.log(e);
			if (response != null) {
				failAction("performRequest",
						request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms", response);
			} else {
				failAction("performRequest", request);
			}
		}
		return null;
	}

	public static boolean assertResponseJSONContainsValue(Response response, String jsonPath, String expectedValue) {
		String searchPool = response.jsonPath().getString(jsonPath);
		if (searchPool.contains(expectedValue)) {
			passAction("assertResponseJSONContainsValue", jsonPath + ", " + expectedValue);
			return true;
		} else {
			failAction("assertResponseJSONContainsValue", jsonPath + ", " + expectedValue);
			return false;
		}
	}

	public static String getResponseJSONValue(Response response, String jsonPath) {
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

	public static boolean assertResponseXMLContainsValue(Response response, String xmlPath, String expectedValue) {
		String searchPool = response.xmlPath().getString(xmlPath);
		if (searchPool.contains(expectedValue)) {
			passAction("assertResponseXMLContainsValue", xmlPath + ", " + expectedValue);
			return true;
		} else {
			failAction("assertResponseXMLContainsValue", xmlPath + ", " + expectedValue);
			return false;
		}
	}

	public static String getResponseXMLValue(Response response, String xmlPath) {
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

	private static boolean assertResponseStatusCode(Response response, String targetStatusCode) {
		return String.valueOf(response.getStatusCode()).equals(targetStatusCode);
	}

	public static int getResponseStatusCode(Response response) {
		return response.getStatusCode();
	}

}
