package testPackage01;

import org.json.JSONException;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.skyscreamer.jsonassert.JSONCompare;
import org.skyscreamer.jsonassert.JSONCompareMode;
import org.skyscreamer.jsonassert.JSONCompareResult;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Test_JsonActions {
    @Test
    public void f() throws IOException, ParseException, JSONException {
        String expectedResponse = new String(
                Files.readAllBytes(Paths.get(System.getProperty("testDataFolderPath") + "JsonFileTest2.json")));
        String actualResponse = new String(
                Files.readAllBytes(Paths.get(System.getProperty("testDataFolderPath") + "JsonFileTest.json")));

        JSONObject expectedJsonObject = (JSONObject) (new JSONParser()).parse(expectedResponse);
        JSONObject actualJsonObject = (JSONObject) (new JSONParser()).parse(actualResponse);

        String expectedJSONString = expectedJsonObject.toJSONString();
        String actualJSONString = actualJsonObject.toJSONString();

        JSONCompareResult result = JSONCompare.compareJSON(expectedJSONString, actualJSONString,
                JSONCompareMode.LENIENT);
        boolean finalResult = result.passed();
        Assert.assertTrue(finalResult);
    }

    @Test
    public void f2() throws IOException, ParseException, JSONException {
        String expectedResponse = new String(
                Files.readAllBytes(Paths.get(System.getProperty("testDataFolderPath") +"JsonFileTest2.json")));
        String actualResponse = new String(
                Files.readAllBytes(Paths.get(System.getProperty("testDataFolderPath") + "JsonFileTest.json")));

        JSONObject expectedJsonObject = (JSONObject) (new JSONParser()).parse(expectedResponse);
        JSONObject actualJsonObject = (JSONObject) (new JSONParser()).parse(actualResponse);

        String expectedJSONString = expectedJsonObject.toJSONString();
        String actualJSONString = actualJsonObject.toJSONString();

        JSONCompareResult result = JSONCompare.compareJSON(expectedJSONString, actualJSONString,
                JSONCompareMode.LENIENT);
        boolean finalResult = result.passed();
        Assert.assertTrue(finalResult);
    }
}
