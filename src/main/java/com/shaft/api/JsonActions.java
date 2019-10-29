package com.shaft.api;

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

import com.shaft.api.RestActions.ComparisonType;
import com.shaft.tools.io.ReportManager;

import io.restassured.response.Response;

public class JsonActions {

    private JsonActions() {
	throw new IllegalStateException("Utility class");
    }

    /**
     * Typically comparison between actual jsonObject of response and expected one
     * initialized in the constructor Return True if files are typically (order,
     * size, keys and values) equal and false otherwise
     * 
     * <p>
     * This method has been @deprecated. Use
     * {@link RestActions#compareJSON(Response response, String jsFilePath, ComparisonType comparisonType)}
     * instead.
     * 
     * @param response   JSON file returned from a RestAction
     * @param jsFilePath the JSON file that's holding the expected result
     * @return boolean value; true if the comparison passes, and false if the
     *         comparison fails
     */
    @Deprecated
    public static boolean compareTypically(Response response, String jsFilePath) {

	JSONParser parser = new JSONParser();
	JSONObject expectedJsonObject;
	JSONObject actualJsonObject;
	try {
	    expectedJsonObject = (JSONObject) parser.parse(new FileReader(jsFilePath));
	    actualJsonObject = (JSONObject) parser.parse(response.asString());
	    return actualJsonObject.equals(expectedJsonObject);
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
     * Comparison between actual jsonObject of response and expected one initialized
     * in the constructor. Return comparison result as boolean value, true if
     * expected object contains all elements in actual object, otherwise return
     * false (if element is array, it should be as same as expected)
     * 
     * <p>
     * This method has been @deprecated. Use
     * {@link RestActions#compareJSON(Response response, String jsFilePath, ComparisonType comparisonType)}
     * instead.
     * 
     * @param response   JSON file returned from a RestAction
     * @param jsFilePath the JSON file that's holding the expected result
     * @return boolean value; true if the comparison passes, and false if the
     *         comparison fails
     */
    @Deprecated
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
	} catch (ParseException | JSONException e) {
	    ReportManager.log(e);
	}

	if (result != null) {
	    return result.passed();
	} else {
	    return false;
	}
    }

}
