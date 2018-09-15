package com.shaftEngine.restAssuredActionLibrary;

import static io.restassured.RestAssured.given;

import java.util.List;
import java.util.concurrent.TimeUnit;

import org.testng.Assert;

import com.shaftEngine.ioActionLibrary.ReportManager;

import io.restassured.response.Response;

public class RestActions {
	private static String cookieJSessionID = "";
	private static String cookieXsrfToken = "";

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
	 * @param requestType;
	 *            POST/GET
	 * @param targetStatusCode;
	 *            200
	 * @param serviceURI;
	 *            http://serviceURL.com:PORT/serviceROOT
	 * @param serviceName;
	 *            /servicePATH/serviceNAME
	 * @param argument;
	 *            arguments without a preceding ?
	 * @return Response; returns the full response object for further manipulation
	 */
	public static Response performRequest(String requestType, String targetStatusCode, String serviceURI,
			String serviceName, String argument) {
		String request;
		Response response = null;
		if (!argument.equals("")) {
			request = serviceURI + serviceName + ARGUMENTSEPARATOR + argument;
		} else {
			request = serviceURI + serviceName;
		}

		try {
			switch (requestType.toLowerCase()) {

			case "post":
				response = given().header("X-XSRF-TOKEN", cookieXsrfToken)
						.cookies("JSESSIONID", cookieJSessionID, "XSRF-TOKEN", cookieXsrfToken).when().post(request)
						.andReturn();

				if (response.getCookie("JSESSIONID") != null) {
					cookieJSessionID = response.getCookie("JSESSIONID");
				}
				if (response.getCookie("XSRF-TOKEN") != null) {
					cookieXsrfToken = response.getCookie("XSRF-TOKEN");
				}
				break;

			case "get":
				response = given().header("X-XSRF-TOKEN", cookieXsrfToken)
						.cookies("JSESSIONID", cookieJSessionID, "XSRF-TOKEN", cookieXsrfToken).when().get(request)
						.andReturn();
				if (response.getCookie("JSESSIONID") != null) {
					cookieJSessionID = response.getCookie("JSESSIONID");
				}
				if (response.getCookie("XSRF-TOKEN") != null) {
					cookieXsrfToken = response.getCookie("XSRF-TOKEN");
				}
				break;

			default:
				failAction("performRequest", request);
				break;
			}

			if (assertResponseStatusCode(response, targetStatusCode)) {
				passAction("performRequest",
						request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms", response);
				return response;
			} else {
				failAction("performRequest",
						request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms", response);
			}
		} catch (NullPointerException e) {
			ReportManager.log(e);
			failAction("performRequest", request);
		} catch (Exception e) {
			ReportManager.log(e);
			failAction("performRequest", request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms",
					response);
		}

		return null;
	}

	public static boolean assertResponseJSONContainsValue(Response response, String jsonPath, String expectedValue) {
		List<String> searchPool = response.jsonPath().get(jsonPath);
		if (searchPool.contains(expectedValue)) {
			passAction("assertResponseJSONContainsValue", jsonPath + ", " + expectedValue);
			return true;
		} else {
			failAction("assertResponseJSONContainsValue", jsonPath + ", " + expectedValue);
			return false;
		}
	}

	public static boolean assertResponseXMLContainsValue(Response response, String xmlPath, String expectedValue) {
		String searchPool = response.xmlPath().get(xmlPath);
		if (searchPool.contains(expectedValue)) {
			passAction("assertResponseXMLContainsValue", xmlPath + ", " + expectedValue);
			return true;
		} else {
			failAction("assertResponseXMLContainsValue", xmlPath + ", " + expectedValue);
			return false;
		}
	}

	private static boolean assertResponseStatusCode(Response response, String targetStatusCode) {
		if (String.valueOf(response.getStatusCode()).equals(targetStatusCode)) {
			return true;
		} else {
			return false;
		}
	}

}
