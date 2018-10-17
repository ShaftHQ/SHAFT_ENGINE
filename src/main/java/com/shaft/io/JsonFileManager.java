package com.shaft.io;

import java.io.FileNotFoundException;
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

public class JsonFileManager {

	JSONObject expactedJsonObject;

	/**
	 * Creates a new instance of the json file reader using the expected json file
	 * path
	 * 
	 * @param jsFilePath
	 *            the expected path for the target json file
	 */
	public JsonFileManager(String jsFilePath) {

		JSONParser parser = new JSONParser();
		try {
			expactedJsonObject = (JSONObject) parser.parse(new FileReader(jsFilePath));

		} catch (FileNotFoundException e) {
			ReportManager.log(e);
		} catch (IOException e) {
			ReportManager.log(e);
		    ReportManager.log("Couldn't find the desired file. [" + jsFilePath + "].");
		    Assert.fail("Couldn't find the desired file. [" + jsFilePath + "].");
		} catch (ParseException e) {
			 ReportManager.log(e);
		}
	}

	/**
	 * Receive actual jsonObject of response and compare it with expected one
	 * initialized in the constructor then return comparison result as boolean value
	 * (true if two objects are strictly equal, otherwise return false)
	 * 
	 * @param actualJsonObject
	 *            JSONObject
	 * @return boolean value
	 */

	public boolean compareEqual_Strict_Order(JSONObject actualJsonObject) {
		JSONCompareResult result = null;
		try {
			result = JSONCompare.compareJSON(actualJsonObject.toJSONString(), expactedJsonObject.toJSONString(),
					JSONCompareMode.STRICT);
		} catch (JSONException e) {
			 ReportManager.log(e);
		}
		return result.passed();
	}

	/**
	 * Receive actual jsonObject of response and compare it with expected one
	 * initialized in the constructor then return comparison result as boolean value
	 * (true if two objects are equal, otherwise return false)
	 * 
	 * It is non-strict comparison (ignore the elements' order)
	 * 
	 * @param actualJsonObject
	 *            JSONObject
	 * @return boolean value
	 */

	public boolean compareEqual_NonStrict_Order(JSONObject actualJsonObject) {
		JSONCompareResult result = null;
		try {
			result = JSONCompare.compareJSON(actualJsonObject.toJSONString(), expactedJsonObject.toJSONString(),
					JSONCompareMode.NON_EXTENSIBLE);
		} catch (JSONException e) {
			 ReportManager.log(e);
		}
		return result.passed();
	}
}
