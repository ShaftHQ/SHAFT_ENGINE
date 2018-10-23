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

public class JsonFileManager {

    JSONObject expectedJsonObject;

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
	    expectedJsonObject = (JSONObject) parser.parse(new FileReader(jsFilePath));

	} catch (IOException e) {
	    ReportManager.log(e);
	    ReportManager.log("Couldn't find the desired file. [" + jsFilePath + "].");
	    Assert.fail("Couldn't find the desired file. [" + jsFilePath + "].");
	} catch (ParseException e) {
	    ReportManager.log(e);
	}
    }

    /**
     * Typically comparison between actual jsonObject of response and expected one
     * initialized in the constructor Return True if files are typically (order,
     * size, keys and values) equal and false otherwise
     * 
     * @param actualJsonObject
     *            JSONObject
     * @return boolean value
     */

    public boolean compareTypically(JSONObject actualJsonObject) {
	return expectedJsonObject.equals(actualJsonObject);
    }

    /**
     * Strictly comparison between actual jsonObject of response and expected one
     * initialized in the constructor. Return comparison result as boolean value,
     * true if two objects are strictly matching (strict array ordering), otherwise
     * return false
     * 
     * @param actualJsonObject
     *            JSONObject
     * @return boolean value
     */

    public boolean compareStrictly(JSONObject actualJsonObject) {
	JSONCompareResult result = null;
	try {
	    result = JSONCompare.compareJSON(actualJsonObject.toJSONString(), expectedJsonObject.toJSONString(), JSONCompareMode.STRICT);
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
     * Non Strictly comparison between actual jsonObject of response and expected
     * one initialized in the constructor. Return comparison result as boolean
     * value, true if two objects are non-strictly matching (non-strict array
     * ordering), otherwise return false
     * 
     * @param actualJsonObject
     *            JSONObject
     * @return boolean value
     */

    public boolean compareNonStrictly(JSONObject actualJsonObject) {
	JSONCompareResult result = null;
	try {
	    result = JSONCompare.compareJSON(actualJsonObject.toJSONString(), expectedJsonObject.toJSONString(), JSONCompareMode.NON_EXTENSIBLE);
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
     *            JSONObject
     * @return boolean value
     */

    public boolean containElements(JSONObject actualJsonObject) {
	JSONCompareResult result = null;
	try {
	    result = JSONCompare.compareJSON(actualJsonObject.toJSONString(), expectedJsonObject.toJSONString(), JSONCompareMode.LENIENT);
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
