package com.shaft.tools.io;

import com.shaft.cli.FileActions;
import com.shaft.driver.DriverFactory;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.restassured.path.json.JsonPath;
import io.restassured.path.json.exception.JsonPathException;

import java.io.ByteArrayInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * Reads and parses JSON test-data files using JSONPath expressions.
 *
 * <p>Test data files are resolved relative to the project's
 * {@code src/test/resources/testDataFiles/} directory. Values can be
 * retrieved as strings or lists using JSONPath syntax.
 *
 * <p><b>Usage example:</b>
 * <pre>{@code
 * SHAFT.TestData.JSON testData = new SHAFT.TestData.JSON("users.json");
 * String name = testData.getTestData("$.users[0].name");
 * }</pre>
 *
 * @see com.shaft.driver.SHAFT.TestData.JSON
 */
@SuppressWarnings("unused")
public class JSONFileManager {
    private static final ThreadLocal<FileReader> reader = new ThreadLocal<>();
    private final String jsonFilePath;
    private final Path resolvedJsonFilePath;
    private String cachedJsonContent;
    private long cachedFileSize = -1L;
    private FileTime cachedLastModifiedTime;

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
        this.resolvedJsonFilePath = Paths.get(FileActions.getInstance(true).getAbsolutePath(jsonFilePath));
        List<List<Object>> attachments = new ArrayList<>();
        byte[] raw = readJsonBytes();
        List<Object> testDataFileAttachment = Arrays.asList("Test Data", "JSON", new ByteArrayInputStream(raw));
        attachments.add(testDataFileAttachment);
        ReportManagerHelper.log("Loaded JSON test data: \"" + jsonFilePath + "\".", attachments);
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
     * Alias to getTestData
     *
     * @param jsonPath the desired jsonpath that points to the needed test data, it can be written manually or generated using helper tools such as <a href="https://jsonpathfinder.com/">https://jsonpathfinder.com/</a>
     * @return the string value of the desired test data
     */
    public String get(String jsonPath) {
        return getTestData(jsonPath);
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
        try {
            String jsonContent = getJsonContent();
            switch (dataType) {
                case STRING -> testData = JsonPath.from(jsonContent).getString(jsonPath);
                case LIST -> testData = JsonPath.from(jsonContent).getList(jsonPath);
                case MAP -> testData = JsonPath.from(jsonContent).getMap(jsonPath);
                case JSON -> testData = JsonPath.from(jsonContent).getJsonObject(jsonPath);
            }
        } catch (ClassCastException rootCauseException) {
            FailureReporter.fail(this.getClass(), "Incorrect jsonPath. [" + jsonPath + "].", rootCauseException);
        } catch (JsonPathException | IllegalArgumentException rootCauseException) {
            FailureReporter.fail(this.getClass(), "Couldn't read the desired file. [" + this.jsonFilePath + "].", rootCauseException);
        }
        return testData;
    }

    /**
     * Returns cached JSON content, refreshing it when the underlying file changes.
     *
     * @return JSON content as UTF-8 text
     */
    private synchronized String getJsonContent() {
        if (isCacheStale()) {
            readJsonBytes();
        }
        return cachedJsonContent;
    }

    private synchronized byte[] readJsonBytes() {
        try {
            BasicFileAttributes attributes = Files.readAttributes(resolvedJsonFilePath, BasicFileAttributes.class);
            byte[] raw = Files.readAllBytes(resolvedJsonFilePath);
            cachedJsonContent = new String(raw, StandardCharsets.UTF_8);
            cachedFileSize = attributes.size();
            cachedLastModifiedTime = attributes.lastModifiedTime();
            reader.remove();
            return raw;
        } catch (NoSuchFileException rootCauseException) {
            FailureReporter.fail(this.getClass(), "Couldn't read the desired file. [" + this.jsonFilePath + "].", rootCauseException);
        } catch (IOException formatException) {
            FailureReporter.fail(this.getClass(), "file didn't match the specified format. [" + this.jsonFilePath + "].", formatException);
        }
        return new byte[0];
    }

    private boolean isCacheStale() {
        if (cachedJsonContent == null || cachedLastModifiedTime == null) {
            return true;
        }
        try {
            BasicFileAttributes attributes = Files.readAttributes(resolvedJsonFilePath, BasicFileAttributes.class);
            return cachedFileSize != attributes.size()
                    || !cachedLastModifiedTime.equals(attributes.lastModifiedTime());
        } catch (IOException formatException) {
            FailureReporter.fail(this.getClass(), "file didn't match the specified format. [" + this.jsonFilePath + "].", formatException);
        }
        return true;
    }

    public enum DataType {
        STRING, LIST, MAP, JSON
    }
}
