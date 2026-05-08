package testPackage.legacy;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.driver.SHAFT;
import org.json.JSONException;
import org.skyscreamer.jsonassert.JSONCompare;
import org.skyscreamer.jsonassert.JSONCompareMode;
import org.skyscreamer.jsonassert.JSONCompareResult;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class JsonActionsTests {
    @Test
    public void f() throws IOException, JSONException {
        String expectedResponse = new String(
                Files.readAllBytes(Paths.get(SHAFT.Properties.paths.testData() + "JsonFileTest2.json")));
        String actualResponse = new String(
                Files.readAllBytes(Paths.get(SHAFT.Properties.paths.testData() + "JsonFileTest.json")));

        ObjectMapper mapper = new ObjectMapper();
        ObjectNode expectedJsonObject = (ObjectNode) mapper.readTree(expectedResponse);
        ObjectNode actualJsonObject = (ObjectNode) mapper.readTree(actualResponse);

        JSONCompareResult result = JSONCompare.compareJSON(expectedJsonObject.toString(), actualJsonObject.toString(),
                JSONCompareMode.LENIENT);
        boolean finalResult = result.passed();
        Assert.assertTrue(finalResult);
    }

    @Test
    public void f2() throws IOException, JSONException {
        String expectedResponse = new String(
                Files.readAllBytes(Paths.get(SHAFT.Properties.paths.testData() + "JsonFileTest2.json")));
        String actualResponse = new String(
                Files.readAllBytes(Paths.get(SHAFT.Properties.paths.testData() + "JsonFileTest.json")));

        ObjectMapper mapper = new ObjectMapper();
        ObjectNode expectedJsonObject = (ObjectNode) mapper.readTree(expectedResponse);
        ObjectNode actualJsonObject = (ObjectNode) mapper.readTree(actualResponse);

        JSONCompareResult result = JSONCompare.compareJSON(expectedJsonObject.toString(), actualJsonObject.toString(),
                JSONCompareMode.LENIENT);
        boolean finalResult = result.passed();
        Assert.assertTrue(finalResult);
    }
}
