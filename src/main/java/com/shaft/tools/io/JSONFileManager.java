package com.shaft.tools.io;

import io.restassured.path.json.JsonPath;
import io.restassured.path.json.exception.JsonPathException;
import org.testng.Assert;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class JSONFileManager {
    private final String jsonFilePath;
    private FileReader reader = null;

    /**
     * Creates a new instance of the test data json reader using the target json
     * file path
     *
     * @param jsonFilePath target test data json file path
     */
    public JSONFileManager(String jsonFilePath) {
        this.jsonFilePath = jsonFilePath;
        try {
            this.reader = new FileReader(jsonFilePath);
        } catch (FileNotFoundException rootCauseException) {
            ReportManager.log(rootCauseException);
            ReportManager.log("Couldn't find the desired file. [" + jsonFilePath + "].");
            Assert.fail("Couldn't find the desired file. [" + jsonFilePath + "].");
        }
        List<List<Object>> attachments = new ArrayList<>();
        List<Object> testDataFileAttachment = null;
        try {
            testDataFileAttachment = Arrays.asList("Test Data", "JSON",
                    new FileInputStream(jsonFilePath));
        } catch (FileNotFoundException e) {
            //unreachable code because if the file was not found then the reader would have failed at a previous step
        }
        attachments.add(testDataFileAttachment);
        ReportManager.log("Successfully loaded the following test data file [" + jsonFilePath + "].", attachments);
    }

    /**
     * Used internally to remove x. from the beginning of any jsonpath. This matches the generated jsonpaths by online helper tools such as https://jsonpathfinder.com/
     *
     * @param jsonPath the generated jsonpath
     * @return a clean jsonpath without the x. at the beginning of the string
     */
    private String cleanJsonPath(String jsonPath) {
        if (jsonPath.startsWith("x.")) {
            return jsonPath.replace("x.", "");
        } else {
            return jsonPath;
        }

    }

    /**
     * Reads the string value at the desired jsonpath within the target test data file
     *
     * @param jsonPath the desired jsonpath that points to the needed test data, it can be written manually or generated using helper tools such as https://jsonpathfinder.com/
     * @return the string value of the desired test data
     */
    public String getTestData(String jsonPath) {
        Object testData = getTestData(cleanJsonPath(jsonPath), DataType.STRING);
        if (testData != null) {
            return String.valueOf(testData);
        } else {
            return null;
        }
    }

    /**
     * Reads the list value at the desired jsonpath within the target test data file
     *
     * @param jsonPath the desired jsonpath that points to the needed test data, it can be written manually or generated using helper tools such as https://jsonpathfinder.com/
     * @return the list value of the desired test data
     */
    public List getTestDataAsList(String jsonPath) {
        Object testData = getTestData(cleanJsonPath(jsonPath), DataType.LIST);
        if (testData != null) {
            return (List) testData;
        } else {
            return null;
        }
    }

    /**
     * Reads the map value at the desired jsonpath within the target test data file
     *
     * @param jsonPath the desired jsonpath that points to the needed test data, it can be written manually or generated using helper tools such as https://jsonpathfinder.com/
     * @return the map value of the desired test data
     */
    public Map getTestDataAsMap(String jsonPath) {
        Object testData = getTestData(cleanJsonPath(jsonPath), DataType.MAP);
        if (testData != null) {
            return (Map) testData;
        } else {
            return null;
        }
    }

    /**
     * Handles internal parsing and data retrieval
     *
     * @param jsonPath target jsonPath within the target test data JSON file
     * @param dataType desired data type for the returned object
     * @return a generic object that holds the desired test data value
     */
    private Object getTestData(String jsonPath, DataType dataType) {
        Object testData = null;
        try {
            switch (dataType) {
                case STRING -> testData = JsonPath.from(this.reader).getString(jsonPath);
                case LIST -> testData = JsonPath.from(this.reader).getList(jsonPath);
                case MAP -> testData = JsonPath.from(this.reader).getMap(jsonPath);
            }
        } catch (ClassCastException rootCauseException) {
            ReportManager.log(rootCauseException);
            ReportManager.log("Incorrect jsonPath. [" + jsonPath + "].");
            Assert.fail("Incorrect jsonPath. [" + jsonPath + "].");
        } catch (JsonPathException | IllegalArgumentException rootCauseException) {
            ReportManager.log(rootCauseException);
            ReportManager.log("Couldn't read the desired file. [" + this.jsonFilePath + "].");
            Assert.fail("Couldn't read the desired file. [" + this.jsonFilePath + "].");
        }
        return testData;
    }

    public enum DataType {
        STRING, LIST, MAP
    }
}
