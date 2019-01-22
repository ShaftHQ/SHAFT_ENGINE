package com.shaft.io;

import java.io.FileReader;
import java.io.IOException;

import org.json.JSONException;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.skyscreamer.jsonassert.JSONCompare;
import org.skyscreamer.jsonassert.JSONCompareMode;
import org.skyscreamer.jsonassert.JSONCompareResult;
import org.testng.Assert;
import io.restassured.response.Response;

public class JsonFileManager {

	/**
	 * Typically comparison between actual jsonObject of response and expected one
	 * initialized in the constructor Return True if files are typically (order,
	 * size, keys and values) equal and false otherwise
	 * 
	 * @param actualJsonObject
	 *            JSONObject
	 * @return boolean value
	 */

	public static boolean compareTypically(Response response, String jsFilePath) {

		JSONParser parser = new JSONParser();
		JSONObject expectedJsonObject;
		JSONObject actualJsonObject;
		try {
			expectedJsonObject = (JSONObject) parser.parse(new FileReader(jsFilePath));
			actualJsonObject = (JSONObject) parser.parse(response.asString());
			return expectedJsonObject.equals(actualJsonObject);
		} catch (IOException e) {
			ReportManager.log(e);
			ReportManager.log("Couldn't find the desired file. [" + jsFilePath + "].");
			Assert.fail("Couldn't find the desired file. [" + jsFilePath + "].");
			return false;
		} catch (ParseException e) {
			ReportManager.log(e);
			return false;
		}
	}

	/**
	 * Strictly comparison between actual jsonObject of response and expected one
	 * initialized in the constructor. Return comparison result as boolean value,
	 * true if two objects are strictly matching (strict array ordering), otherwise
	 * return false
	 * 
	 * @param actualJsonObject
	 * @param jsFilePath
	 * @return boolean value
	 */

	public static boolean compareStrictly(Response response, String jsFilePath) {
		JSONParser parser = new JSONParser();
		JSONObject expectedJsonObject;
		JSONObject actualJsonObject;
		JSONCompareResult result = null;

		try {
			expectedJsonObject = (JSONObject) parser.parse(new FileReader(jsFilePath));
			actualJsonObject = (JSONObject) parser.parse(response.asString());
			result = JSONCompare.compareJSON(actualJsonObject.toJSONString(), expectedJsonObject.toJSONString(),
					JSONCompareMode.STRICT);
		} catch (JSONException e) {
			ReportManager.log(e);
		} catch (IOException e) {
			ReportManager.log(e);
			ReportManager.log("Couldn't find the desired file. [" + jsFilePath + "].");
			Assert.fail("Couldn't find the desired file. [" + jsFilePath + "].");
		} catch (ParseException e) {
			ReportManager.log(e);
		}

		if (result != null) {
			return result.passed();
		} else {
			return false;
		}
	}

	/**
	 * Non Strictly comparison between actual jsonObject of response and expected
	 * one initialized in the constructor. Return comparison result as boolean
	 * value, true if two objects are non-strictly matching (non-strict array
	 * ordering), otherwise return false
	 * 
	 * @param actualJsonObject
	 * @param jsFilePath
	 * @return boolean value
	 */

	public static boolean compareNonStrictly(Response response, String jsFilePath) {
		JSONParser parser = new JSONParser();
		JSONObject expectedJsonObject;
		JSONObject actualJsonObject;
		JSONCompareResult result = null;

		try {
			expectedJsonObject = (JSONObject) parser.parse(new FileReader(jsFilePath));
			actualJsonObject = (JSONObject) parser.parse(response.asString());
			result = JSONCompare.compareJSON(actualJsonObject.toJSONString(), expectedJsonObject.toJSONString(),
					JSONCompareMode.NON_EXTENSIBLE);
		} catch (IOException e) {
			ReportManager.log(e);
			ReportManager.log("Couldn't find the desired file. [" + jsFilePath + "].");
			Assert.fail("Couldn't find the desired file. [" + jsFilePath + "].");
		} catch (ParseException e) {
			ReportManager.log(e);
		} catch (JSONException e) {
			ReportManager.log(e);
		}

		if (result != null) {
			return result.passed();
		} else {
			return false;
		}
	}

	/**
	 * Comparison between actual jsonObject of response and expected one initialized
	 * in the constructor. Return comparison result as boolean value, true if
	 * expected object contains all elements in actual object, otherwise return
	 * false (if element is array, it should be as same as expected)
	 * 
	 * @param actualJsonObject
	 * @param jsFilePath
	 * @return boolean value
	 */

	public static boolean containElements(Response response, String jsFilePath) {
		JSONParser parser = new JSONParser();
		JSONObject expectedJsonObject;
		JSONObject actualJsonObject;
		JSONCompareResult result = null;
		try {
			expectedJsonObject = (JSONObject) parser.parse(new FileReader(jsFilePath));
			actualJsonObject = (JSONObject) parser.parse(response.asString());
			result = JSONCompare.compareJSON(expectedJsonObject.toJSONString(), actualJsonObject.toJSONString(),
					JSONCompareMode.LENIENT);
		} catch (IOException e) {
			ReportManager.log(e);
			ReportManager.log("Couldn't find the desired file. [" + jsFilePath + "].");
			Assert.fail("Couldn't find the desired file. [" + jsFilePath + "].");
		} catch (ParseException e) {
			ReportManager.log(e);
		} catch (JSONException e) {
			ReportManager.log(e);
		}

		if (result != null) {
			return result.passed();
		} else {
			return false;
		}
	}
}
