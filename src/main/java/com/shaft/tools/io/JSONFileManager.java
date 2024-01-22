package com.shaft.tools.io;

import com.shaft.cli.FileActions;
import com.shaft.driver.DriverFactory;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.restassured.path.json.JsonPath;
import io.restassured.path.json.exception.JsonPathException;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

@SuppressWarnings("unused")
public class JSONFileManager {
    private static final ThreadLocal<FileReader> reader = new ThreadLocal<>();
    private final String jsonFilePath;

    /**
     * Creates a new instance of the test data json reader using the target json
     * file path
     *
     * @param jsonFilePath target test data json file path
     */
    public JSONFileManager(String jsonFilePath) {
        DriverFactory.reloadProperties();
        jsonFilePath = JavaHelper.appendTestDataToRelativePath(jsonFilePath);
        this.jsonFilePath = jsonFilePath;
        initializeReader();
        List<List<Object>> attachments = new ArrayList<>();
        List<Object> testDataFileAttachment = null;
        try {
            testDataFileAttachment = Arrays.asList("Test Data", "JSON",
                    new FileInputStream(jsonFilePath));
        } catch (FileNotFoundException e) {
            //unreachable code because if the file was not found then the reader would have failed at a previous step
        }
        attachments.add(testDataFileAttachment);
        ReportManagerHelper.log("Loaded Test Data: \"" + jsonFilePath + "\".", attachments);
    }

    /**
     * Used internally to remove x. from the beginning of any jsonpath. This matches the generated Json Paths by online helper tools such as <a href="https://jsonpathfinder.com/">https://jsonpathfinder.com/</a>
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
     * @param jsonPath the desired jsonpath that points to the needed test data, it can be written manually or generated using helper tools such as <a href="https://jsonpathfinder.com/">https://jsonpathfinder.com/</a>
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
     * Reads the json object value at the desired jsonpath within the target test data file to map it to java object
     *
     * @param jsonPath the desired jsonpath that points to the needed test data, it can be written manually or generated using helper tools such as <a href="https://jsonpathfinder.com/">https://jsonpathfinder.com/</a>
     * @return the json value of the desired test data as Object
     */
    public Object getTestDataAsJson(String jsonPath) {
        return getTestData(cleanJsonPath(jsonPath), DataType.JSON);
    }

    /**
     * Reads the list value at the desired jsonpath within the target test data file
     *
     * @param jsonPath the desired jsonpath that points to the needed test data, it can be written manually or generated using helper tools such as <a href="https://jsonpathfinder.com/">https://jsonpathfinder.com/</a>
     * @return the list value of the desired test data
     */
    public List<?> getTestDataAsList(String jsonPath) {
        Object testData = getTestData(cleanJsonPath(jsonPath), DataType.LIST);
        if (testData != null) {
            return (List<?>) testData;
        } else {
            return null;
        }
    }

    /**
     * Reads the map value at the desired jsonpath within the target test data file
     *
     * @param jsonPath the desired jsonpath that points to the needed test data, it can be written manually or generated using helper tools such as <a href="https://jsonpathfinder.com/">https://jsonpathfinder.com/</a>
     * @return the map value of the desired test data
     */
    public Map<?, ?> getTestDataAsMap(String jsonPath) {
        Object testData = getTestData(cleanJsonPath(jsonPath), DataType.MAP);
        if (testData != null) {
            return (Map<?, ?>) testData;
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
        initializeReader();
        try {
            switch (dataType) {
                case STRING -> testData = JsonPath.from(reader.get()).getString(jsonPath);
                case LIST -> testData = JsonPath.from(reader.get()).getList(jsonPath);
                case MAP -> testData = JsonPath.from(reader.get()).getMap(jsonPath);
                case JSON -> testData = JsonPath.from(reader.get()).getJsonObject(jsonPath);
            }
        } catch (ClassCastException rootCauseException) {
            FailureReporter.fail(this.getClass(), "Incorrect jsonPath. [" + jsonPath + "].", rootCauseException);
        } catch (JsonPathException | IllegalArgumentException rootCauseException) {
            FailureReporter.fail(this.getClass(), "Couldn't read the desired file. [" + this.jsonFilePath + "].", rootCauseException);
        }
        return testData;
    }

    /**
     * initializes the json reader using the target json file path
     */
    private void initializeReader() {
        try {
            reader.set(new FileReader(FileActions.getInstance(true).getAbsolutePath(jsonFilePath), StandardCharsets.UTF_8));
        } catch (FileNotFoundException rootCauseException) {
            FailureReporter.fail(this.getClass(), "Couldn't read the desired file. [" + this.jsonFilePath + "].", rootCauseException);
        } catch (IOException formatException) {
            FailureReporter.fail(this.getClass(), "file didn't match the specified format. [" + this.jsonFilePath + "].", formatException);
        }
    }

    public enum DataType {
        STRING, LIST, MAP, JSON
    }
}
