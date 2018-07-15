package com.shaftEngine.restAssuredActionLibrary;

import static io.restassured.RestAssured.given;

import java.util.List;
import java.util.concurrent.TimeUnit;

import org.testng.Assert;

import com.shaftEngine.ioActionLibrary.ReportManager;

import io.restassured.response.Response;

public class RestActions {
	private static String cookie_JSESSIONID = "";
	private static String cookie_XSRF_TOKEN = "";

	private static final String argumentSeparator = "?";

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
			request = serviceURI + serviceName + argumentSeparator + argument;
		} else {
			request = serviceURI + serviceName;
		}

		try {
			switch (requestType.toLowerCase()) {

			case "post":
				response = given().header("X-XSRF-TOKEN", cookie_XSRF_TOKEN).cookies("JSESSIONID", cookie_JSESSIONID , "XSRF-TOKEN", cookie_XSRF_TOKEN)
						.when().post(request).andReturn();
				
				if (response.getCookie("JSESSIONID") !=null) {
					cookie_JSESSIONID = response.getCookie("JSESSIONID");
				}
				if (response.getCookie("XSRF-TOKEN") != null) {
					cookie_XSRF_TOKEN = response.getCookie("XSRF-TOKEN");
				}
				break;

			case "get":
				response = given().header("X-XSRF-TOKEN", cookie_XSRF_TOKEN).cookies("JSESSIONID", cookie_JSESSIONID , "XSRF-TOKEN", cookie_XSRF_TOKEN)
						.when().get(request).andReturn();
				if (response.getCookie("JSESSIONID") !=null) {
					cookie_JSESSIONID = response.getCookie("JSESSIONID");
				}
				if (response.getCookie("XSRF-TOKEN") != null) {
					cookie_XSRF_TOKEN = response.getCookie("XSRF-TOKEN");
				}
				break;

			default:
				failAction("performRequest", request);
				break;
			}

			if (assertResponse_StatusCode(response, targetStatusCode)) {
				passAction("performRequest",
						request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms", response);
				return response;
			} else {
				failAction("performRequest",
						request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms", response);
			}
		} catch (NullPointerException t) {
			ReportManager.log(t.getMessage());
			failAction("performRequest", request);
		} catch (Throwable t) {
			ReportManager.log(t.getMessage());
			failAction("performRequest", request + ", Response Time: " + response.timeIn(TimeUnit.MILLISECONDS) + "ms",
					response);
		}

		return null;
	}

	public static boolean assertResponse_JSON_ContainsValue(Response response, String jsonPath, String expectedValue) {
		List<String> searchPool = response.jsonPath().get(jsonPath);
		if (searchPool.contains(expectedValue)) {
			passAction("assertResponse_JSON_ContainsValue", jsonPath + ", " + expectedValue);
			return true;
		} else {
			failAction("assertResponse_JSON_ContainsValue", jsonPath + ", " + expectedValue);
			return false;
		}
	}

	public static boolean assertResponse_StatusCode(Response response, String targetStatusCode) {
		if (String.valueOf(response.getStatusCode()).equals(targetStatusCode)) {
			passAction("assertResponse_StatusCode", String.valueOf(response.getStatusCode()));
			return true;
		} else {
			failAction("assertResponse_StatusCode", String.valueOf(response.getStatusCode()));
			return false;
		}
	}

}
