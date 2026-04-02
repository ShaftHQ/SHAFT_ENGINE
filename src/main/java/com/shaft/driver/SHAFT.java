package com.shaft.driver;

import com.shaft.api.RequestBuilder;
import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.db.DatabaseActions;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.WizardHelpers;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.AsyncElementActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.element.internal.Actions;
import com.shaft.listeners.internal.WebDriverListener;
import com.shaft.tools.io.*;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.internal.RestValidationsBuilder;
import io.appium.java_client.AppiumDriver;
import io.restassured.response.Response;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.support.events.EventFiringDecorator;

import java.io.InputStream;
import java.sql.ResultSet;
import java.util.List;

/**
 * The main entry point for the SHAFT test automation framework.
 *
 * <p>SHAFT provides a unified, fluent API for automating Web, Mobile, API, CLI,
 * and Database interactions. All subsystems are accessed through nested static
 * classes:
 * <ul>
 *   <li>{@link GUI} &ndash; Browser, element, touch, and alert actions via Selenium/Appium.</li>
 *   <li>{@link API} &ndash; RESTful API interactions via REST Assured.</li>
 *   <li>{@link CLI} &ndash; Terminal and file-system operations.</li>
 *   <li>{@link DB}  &ndash; Database queries and result handling.</li>
 *   <li>{@link Validations} &ndash; Standalone hard and soft assertions.</li>
 *   <li>{@link TestData} &ndash; JSON, Excel, CSV, and YAML test-data readers.</li>
 *   <li>{@link Properties} &ndash; Runtime configuration management.</li>
 *   <li>{@link Report} &ndash; Custom logging and attachments for Allure reports.</li>
 * </ul>
 *
 * <p><b>Quick-start example:</b>
 * <pre>{@code
 * SHAFT.GUI.WebDriver driver = new SHAFT.GUI.WebDriver();
 * driver.browser().navigateToURL("https://example.com")
 *       .and().assertThat().browser().title().contains("Example");
 * driver.quit();
 * }</pre>
 *
 * @see <a href="https://shafthq.github.io/">SHAFT User Guide</a>
 * @see GUI.WebDriver
 * @see API
 */
@SuppressWarnings("unused")
public class SHAFT {

    /**
     * Contains GUI-related subsystems for web and mobile test automation.
     *
     * @see WebDriver
     * @see Locator
     */
    public static class GUI {

        /**
         * Manages a Selenium/Appium WebDriver session and exposes fluent actions
         * for browser navigation, element interaction, touch gestures, alert
         * handling, and assertions.
         *
         * <p>Create one instance per test method (typically in a {@code @BeforeMethod}),
         * perform actions via the fluent API, and call {@link #quit()} in
         * {@code @AfterMethod} to release resources.
         *
         * <p><b>Usage example:</b>
         * <pre>{@code
         * SHAFT.GUI.WebDriver driver = new SHAFT.GUI.WebDriver();
         * driver.browser().navigateToURL("https://example.com");
         * driver.element().click(By.id("submit"));
         * driver.assertThat().browser().title().contains("Success");
         * driver.quit();
         * }</pre>
         *
         * @see <a href="https://shafthq.github.io/">SHAFT User Guide</a>
         */
        public static class WebDriver {
            DriverFactoryHelper helper;
            DriverFactory factory = new DriverFactory();

            /**
             * Creates a new WebDriver session using the browser type configured in
             * the execution properties.
             */
            public WebDriver() {
                helper = factory.getHelper();
            }

            /**
             * Creates a new WebDriver session with the specified driver type.
             *
             * @param driverType the type of driver to create (e.g.,
             *                   {@link DriverFactory.DriverType#CHROME},
             *                   {@link DriverFactory.DriverType#FIREFOX})
             */
            public WebDriver(DriverFactory.DriverType driverType) {
                helper = factory.getHelper(driverType);
            }

            /**
             * Creates a new WebDriver session with the specified driver type and
             * custom capabilities.
             *
             * @param driverType          the type of driver to create
             * @param mutableCapabilities custom capabilities to merge into the session
             */
            public WebDriver(DriverFactory.DriverType driverType, MutableCapabilities mutableCapabilities) {
                helper = factory.getHelper(driverType, mutableCapabilities);
            }

            /**
             * Wraps an existing Selenium WebDriver instance for use with SHAFT's
             * fluent API.
             *
             * @param driver an already-initialized Selenium WebDriver instance
             */
            public WebDriver(org.openqa.selenium.WebDriver driver) {
                helper = factory.getHelper(driver);
            }

            /**
             * Terminates the current WebDriver session and releases all associated
             * resources. Should be called in {@code @AfterMethod} (TestNG) or
             * {@code @AfterEach} (JUnit 5).
             */
            public void quit() {
                if (helper != null)
                    helper.closeDriver();
                if (factory != null)
                    factory.setHelper(null);
                helper = null;
                factory = null;
            }

            /**
             * Returns an element actions object for interacting with web elements
             * (click, type, select, etc.).
             *
             * @return an {@link Actions} instance scoped to this driver session
             */
            public Actions element() {
                return new Actions(helper);
            }

            /**
             * Returns a touch actions object for mobile gesture interactions
             * (swipe, tap, pinch, etc.).
             *
             * @return a {@link TouchActions} instance scoped to this driver session
             */
            public TouchActions touch() {
                return new TouchActions(helper);
            }

            /**
             * Returns a browser actions object for navigation, window management,
             * cookies, and screenshots.
             *
             * @return a {@link BrowserActions} instance scoped to this driver session
             */
            public BrowserActions browser() {
                return new BrowserActions(helper);
            }

            /**
             * Returns an alert actions object for interacting with JavaScript alert,
             * confirm, and prompt dialogs.
             *
             * @return an {@link AlertActions} instance scoped to this driver session
             */
            public AlertActions alert() {
                return new AlertActions(helper);
            }

            /**
             * Starts building a hard assertion (the test fails immediately on mismatch).
             *
             * @return a {@link WizardHelpers.WebDriverAssertions} builder for browser,
             *         element, and other assertions
             */
            public WizardHelpers.WebDriverAssertions assertThat() {
                return new WizardHelpers.WebDriverAssertions(helper);
            }

            /**
             * Starts building a soft verification (failures are collected and reported
             * at the end of the test).
             *
             * @return a {@link WizardHelpers.WebDriverVerifications} builder
             */
            public WizardHelpers.WebDriverVerifications verifyThat() {
                return new WizardHelpers.WebDriverVerifications(helper);
            }

            /**
             * Returns the current Selenium WebDriver instance for custom manipulation.
             *
             * <p>Note: Event-firing decoration is only applied for non-remote, non-Appium
             * drivers when {@code enableTrueNativeMode} is disabled. Because most local
             * Selenium drivers extend {@link RemoteWebDriver}, they will typically be
             * returned undecorated.</p>
             *
             * @return the underlying {@link org.openqa.selenium.WebDriver} instance for
             *         this session
             */
            public org.openqa.selenium.WebDriver getDriver() {
                /*
                 * Decorator is not working for appium drivers as per the following issues/articles
                 * https://github.com/appium/java-client/issues/1694
                 * https://github.com/appium/java-client/blob/master/docs/The-event_firing.md#createproxy-api-since-java-client-830
                 * https://github.com/SeleniumHQ/selenium/blob/316f9738a8e2079265a0691954ca8847e68c598d/java/test/org/openqa/selenium/support/events/EventFiringDecoratorTest.java#L422
                 */
                if (helper.getDriver() instanceof AppiumDriver | helper.getDriver() instanceof RemoteWebDriver) {
                    // remote execution
                    return helper.getDriver();
                } else {
                    // local execution
                    if (!SHAFT.Properties.flags.enableTrueNativeMode()) {
                        return new EventFiringDecorator<>(org.openqa.selenium.WebDriver.class, new WebDriverListener()).decorate(helper.getDriver());
                    } else {
                        return helper.getDriver();
                    }
                }
            }

            /**
             * Returns an async-actions entry point for performing element interactions
             * asynchronously.
             *
             * @return an {@link Async} instance scoped to this driver session
             */
            public Async async() {
                return new Async();
            }

            /**
             * Provides asynchronous element actions that run without blocking the
             * calling thread.
             */
            public class Async {
                /**
                 * Returns an asynchronous element actions object.
                 *
                 * @return an {@link AsyncElementActions} instance
                 */
                public AsyncElementActions element() {
                    return new AsyncElementActions(helper);
                }

            }
        }

        /**
         * Entry point for building element locators using SHAFT's fluent
         * {@link com.shaft.gui.internal.locator.Locator Locator} API.
         *
         * @see <a href="https://shafthq.github.io/">SHAFT User Guide &ndash; Locators</a>
         */
        public static class Locator extends com.shaft.gui.internal.locator.Locator {
        }
    }

    /**
     * Manages a REST API session and exposes a fluent interface for building
     * and executing HTTP requests (GET, POST, PUT, PATCH, DELETE).
     *
     * <p><b>Usage example:</b>
     * <pre>{@code
     * SHAFT.API api = new SHAFT.API("https://jsonplaceholder.typicode.com");
     * api.get("/posts/1").setTargetStatusCode(200).performRequest();
     * api.assertThatResponse().extractedJsonValue("$.title").isNotNull().perform();
     * }</pre>
     *
     * @see RequestBuilder
     * @see <a href="https://shafthq.github.io/">SHAFT User Guide &ndash; API Testing</a>
     */
    public static class API {
        private final RestActions session;
        private String serviceURI;

        /**
         * Creates a new API session pointing at the given base service URI.
         *
         * @param serviceURI the base URI of the target web service
         *                   (e.g., {@code "https://api.example.com"})
         */
        public API(String serviceURI) {
            session = new RestActions(serviceURI, this);
        }

        /**
         * Wraps an existing {@link RestActions} session for continued use.
         *
         * @param existingSession an already-initialised REST session
         */
        public API(RestActions existingSession) {
            session = existingSession;
        }

        /**
         * Factory method equivalent to {@code new API(serviceURI)}.
         *
         * @param serviceURI the base URI of the target web service
         * @return a new {@link API} instance
         */
        public static API getInstance(String serviceURI) {
            return new API(serviceURI);
        }

        /**
         * Builds a GET request for the specified service endpoint.
         *
         * @param serviceName the endpoint path (appended to the base URI)
         * @return a {@link RequestBuilder} for further request configuration
         */
        public RequestBuilder get(String serviceName) {
            return session.buildNewRequest(serviceName, RestActions.RequestType.GET);
        }

        /**
         * Builds a POST request for the specified service endpoint.
         *
         * @param serviceName the endpoint path (appended to the base URI)
         * @return a {@link RequestBuilder} for further request configuration
         */
        public RequestBuilder post(String serviceName) {
            return session.buildNewRequest(serviceName, RestActions.RequestType.POST);
        }

        /**
         * Builds a PATCH request for the specified service endpoint.
         *
         * @param serviceName the endpoint path (appended to the base URI)
         * @return a {@link RequestBuilder} for further request configuration
         */
        public RequestBuilder patch(String serviceName) {
            return session.buildNewRequest(serviceName, RestActions.RequestType.PATCH);
        }

        /**
         * Builds a DELETE request for the specified service endpoint.
         *
         * @param serviceName the endpoint path (appended to the base URI)
         * @return a {@link RequestBuilder} for further request configuration
         */
        public RequestBuilder delete(String serviceName) {
            return session.buildNewRequest(serviceName, RestActions.RequestType.DELETE);
        }

        /**
         * Builds a PUT request for the specified service endpoint.
         *
         * @param serviceName the endpoint path (appended to the base URI)
         * @return a {@link RequestBuilder} for further request configuration
         */
        public RequestBuilder put(String serviceName) {
            return session.buildNewRequest(serviceName, RestActions.RequestType.PUT);
        }

        /**
         * Adds a persistent header that will be sent with every subsequent request
         * in this session.
         *
         * @param key   the header name
         * @param value the header value
         */
        public void addHeader(String key, String value) {
            session.addHeaderVariable(key, value);
        }

        /**
         * Adds a persistent cookie that will be sent with every subsequent request
         * in this session.
         *
         * @param key   the cookie name
         * @param value the cookie value
         */
        public void addCookie(String key, String value) {
            session.addCookieVariable(key, value);
        }

        /**
         * Starts building a hard assertion against the last API response.
         *
         * @return a {@link RestValidationsBuilder} for response assertions
         */
        public RestValidationsBuilder assertThatResponse() {
            return com.shaft.validation.Validations.assertThat().response(session.getResponse());
        }

        /**
         * Starts building a soft verification against the last API response.
         *
         * @return a {@link RestValidationsBuilder} for response verifications
         */
        public RestValidationsBuilder verifyThatResponse() {
            return com.shaft.validation.Validations.verifyThat().response(session.getResponse());
        }

        /**
         * Returns the raw REST Assured {@link Response} from the most recent request.
         *
         * @return the last {@link Response}
         */
        public Response getResponse() {
            return session.getResponse();
        }

        /**
         * Returns the response body of the most recent request as a string.
         *
         * @return the response body text
         */
        public String getResponseBody() {
            return RestActions.getResponseBody(session.getResponse());
        }

        /**
         * Returns the HTTP status code of the most recent response.
         *
         * @return the status code (e.g., 200, 404)
         */
        public int getResponseStatusCode() {
            return RestActions.getResponseStatusCode(session.getResponse());
        }

        /**
         * Returns the response time in milliseconds for the most recent request.
         *
         * @return the response time in milliseconds
         */
        public long getResponseTime() {
            return RestActions.getResponseTime(session.getResponse());
        }

        /**
         * Extracts a value from the most recent JSON response using a JSONPath expression.
         *
         * @param jsonPath the JSONPath expression (e.g., {@code "$.data.id"})
         * @return the extracted value as a string
         */
        public String getResponseJSONValue(String jsonPath) {
            return RestActions.getResponseJSONValue(session.getResponse(), jsonPath);
        }

        /**
         * Extracts a list of values from the most recent JSON response using a JSONPath expression.
         *
         * @param jsonPath the JSONPath expression
         * @return the extracted values as a list of objects
         */
        public List<Object> getResponseJSONValueAsList(String jsonPath) {
            return RestActions.getResponseJSONValueAsList(session.getResponse(), jsonPath);
        }

        /**
         * Extracts a value from the most recent XML response using an XPath expression.
         *
         * @param xmlPath the XPath expression
         * @return the extracted value as a string
         */
        public String getResponseXMLValue(String xmlPath) {
            return RestActions.getResponseXMLValue(session.getResponse(), xmlPath);
        }

        /**
         * Extracts a list of values from the most recent XML response using an XPath expression.
         *
         * @param xmlPath the XPath expression
         * @return the extracted values as a list of objects
         */
        public List<Object> getResponseXMLValueAsList(String xmlPath) {
            return RestActions.getResponseXMLValueAsList(session.getResponse(), xmlPath);
        }
    }

    /**
     * Provides access to command-line and file-system operations.
     *
     * <p><b>Usage example:</b>
     * <pre>{@code
     * SHAFT.CLI.terminal().performTerminalCommand("echo hello");
     * SHAFT.CLI.file().readFile("path/to/file.txt");
     * }</pre>
     */
    public static class CLI {
        private CLI() {
            throw new IllegalStateException("Utility class");
        }

        /**
         * Creates a new terminal actions instance for executing shell commands.
         *
         * @return a {@link TerminalActions} instance
         */
        public static TerminalActions terminal() {
            return new TerminalActions();
        }

        /**
         * Creates a new file actions instance for file-system operations.
         *
         * @return a {@link FileActions} instance
         */
        public static FileActions file() {
            return new FileActions();
        }
    }

    /**
     * Provides database connectivity and query execution.
     *
     * <p><b>Usage example:</b>
     * <pre>{@code
     * SHAFT.DB db = new SHAFT.DB(DatabaseActions.DatabaseType.MY_SQL,
     *         "localhost", "3306", "mydb", "user", "pass");
     * ResultSet rs = db.executeSelectQuery("SELECT * FROM users");
     * }</pre>
     *
     * @see DatabaseActions
     */
    public static class DB extends DatabaseActions {

        /**
         * Creates a new database connection using a custom JDBC connection string.
         *
         * @param customConnectionString a full JDBC connection URL
         */
        public DB(String customConnectionString) {
            super(customConnectionString);
        }

        /**
         * Creates a new database connection using individual connection parameters.
         *
         * @param databaseType the target database type
         * @param ip           the host address of the database server
         * @param port         the port number
         * @param name         the database name
         * @param username     the authentication username
         * @param password     the authentication password
         */
        public DB(DatabaseActions.DatabaseType databaseType, String ip, String port, String name, String username, String password) {
            super(databaseType, ip, port, name, username, password);
        }

        /**
         * Factory method for creating a database connection.
         *
         * @param databaseType the target database type
         * @param ip           the host address of the database server
         * @param port         the port number
         * @param name         the database name
         * @param username     the authentication username
         * @param password     the authentication password
         * @return a new {@link DB} instance
         */
        public static DB getInstance(DatabaseActions.DatabaseType databaseType, String ip, String port, String name, String username, String password) {
            return new DB(databaseType, ip, port, name, username, password);
        }

        /**
         * Factory method for creating a database connection with a custom JDBC URL.
         *
         * @param customConnectionString a full JDBC connection URL
         * @return a new {@link DB} instance
         */
        public static DB getInstance(String customConnectionString) {
            return new DB(customConnectionString);
        }

        /**
         * Converts the entire {@link ResultSet} into a string representation.
         *
         * @param resultSet the query result set
         * @return all result data as a string
         */
        public static String getResult(ResultSet resultSet) {
            return DatabaseActions.getResult(resultSet);
        }

        /**
         * Extracts the values of a specific column from the result set.
         *
         * @param resultSet  the query result set
         * @param columnName the name of the column to extract
         * @return the column values as a string
         */
        public static String getColumn(ResultSet resultSet, String columnName) {
            return DatabaseActions.getColumn(resultSet, columnName);
        }

        /**
         * Retrieves a row matching the specified column value.
         *
         * @param resultSet      the query result set
         * @param columnName     the column to search in
         * @param knownCellValue the value to match
         * @return the matching row data as a string
         */
        public static String getRow(ResultSet resultSet, String columnName, String knownCellValue) {
            return DatabaseActions.getRow(resultSet, columnName, knownCellValue);
        }

        /**
         * Returns the total number of rows in the result set.
         *
         * @param resultSet the query result set
         * @return the row count
         */
        public static int getRowCount(ResultSet resultSet) {
            return DatabaseActions.getRowCount(resultSet);
        }

    }

    /**
     * Provides standalone assertion and verification builders for use outside
     * of a WebDriver context (e.g., API response validation, object comparison).
     *
     * <p><b>Usage example:</b>
     * <pre>{@code
     * SHAFT.Validations.assertThat().object(actualValue).isEqualTo(expectedValue).perform();
     * SHAFT.Validations.verifyThat().number(count).isGreaterThan(0).perform();
     * }</pre>
     */
    public static class Validations {
        private Validations() {
            throw new IllegalStateException("Utility class");
        }

        /**
         * Starts building a hard assertion. The test fails immediately if the
         * assertion does not pass.
         *
         * @return a {@link WizardHelpers.StandaloneAssertions} builder
         */
        public static WizardHelpers.StandaloneAssertions assertThat() {
            return new WizardHelpers.StandaloneAssertions();
        }

        /**
         * Starts building a soft verification. Failures are collected and reported
         * at the end of the test without stopping execution.
         *
         * @return a {@link WizardHelpers.StandaloneVerifications} builder
         */
        public static WizardHelpers.StandaloneVerifications verifyThat() {
            return new WizardHelpers.StandaloneVerifications();
        }
    }

    /**
     * Provides test data readers for various file formats.
     *
     * <p>Test data files are typically stored under
     * {@code src/test/resources/testDataFiles/}.
     */
    public static class TestData {
        /** Reads test data from JSON files. */
        public static class JSON extends JSONFileManager {
            /**
             * Creates a new instance of the test data json reader using the target json
             * file path
             *
             * @param jsonFilePath target test data json file path
             */
            public JSON(String jsonFilePath) {
                super(jsonFilePath);
            }

            /**
             * Factory method for creating a JSON test-data reader.
             *
             * @param jsonFilePath target test data JSON file path
             * @return a new {@link JSONFileManager} instance
             */
            public static JSONFileManager getInstance(String jsonFilePath) {
                return new JSONFileManager(jsonFilePath);
            }
        }

        /** Reads test data from Microsoft Excel files. */
        public static class EXCEL extends ExcelFileManager {
            /**
             * Creates a new instance of the test data Excel reader using the target Excel
             * file path
             *
             * @param excelFilePath target test data Excel file path
             */
            public EXCEL(String excelFilePath) {
                super(excelFilePath);
            }

            /**
             * Factory method for creating an Excel test-data reader.
             *
             * @param excelFilePath target test data Excel file path
             * @return a new {@link ExcelFileManager} instance
             */
            public static ExcelFileManager getInstance(String excelFilePath) {
                return new ExcelFileManager(excelFilePath);
            }
        }

        /** Reads test data from CSV files. */
        public static class CSV extends CSVFileManager {
            /**
             * Creates a new instance of the test data CSV reader using the target CSV
             * file path
             *
             * @param csvFilePath target test data CSV file path
             */
            public CSV(String csvFilePath) {
                super(csvFilePath);
            }

            /**
             * Factory method for creating a CSV test-data reader.
             *
             * @param csvFilePath target test data CSV file path
             * @return a new {@link CSVFileManager} instance
             */
            public static CSVFileManager getInstance(String csvFilePath) {
                return new CSVFileManager(csvFilePath);
            }
        }

        /** Reads test data from YAML files. */
        public static class YAML extends YAMLFileManager {
            /**
             * Creates a new instance of the test data Excel reader using the target Excel
             * file path
             *
             * @param yamlFilePath target test data yaml file path
             */
            public YAML(String yamlFilePath) {
                super(yamlFilePath);
            }

            /**
             * Factory method for creating a YAML test-data reader.
             *
             * @param yamlFilePath target test data YAML file path
             * @return a new {@link YAMLFileManager} instance
             */
            public static YAMLFileManager getInstance(String yamlFilePath) {
                return new YAMLFileManager(yamlFilePath);
            }
        }
    }

    /**
     * Exposes SHAFT's runtime configuration properties.
     *
     * @see com.shaft.properties.internal.Properties
     */
    public static class Properties extends com.shaft.properties.internal.Properties {
    }

    /**
     * Utility class for emitting log messages and attaching artifacts to
     * the Allure execution report.
     *
     * <p><b>Usage example:</b>
     * <pre>{@code
     * SHAFT.Report.report("Step completed successfully");
     * SHAFT.Report.attach("text/plain", "response.json", responseBody);
     * }</pre>
     */
    public static class Report {
        private Report() {
            throw new IllegalStateException("Utility class");
        }

        /**
         * Logs a message discretely (visible in the execution log file but not
         * as a report step).
         *
         * @param message the message to log
         */
        public static void log(String message) {
            ReportManager.logDiscrete(message);
        }

        /**
         * Logs a message as a visible step in the Allure execution report.
         *
         * @param message the message to report
         */
        public static void report(String message) {
            ReportManager.log(message);
        }

        /**
         * Attaches a string artifact to the Allure report.
         *
         * @param attachmentType    the MIME type of the attachment (e.g., {@code "text/plain"})
         * @param attachmentName    a human-readable name for the attachment
         * @param attachmentContent the content to attach
         */
        public static void attach(String attachmentType, String attachmentName, String attachmentContent) {
            ReportManagerHelper.attach(attachmentType, attachmentName, attachmentContent);
        }

        /**
         * Attaches a binary artifact (e.g., an image) to the Allure report.
         *
         * @param attachmentType    the MIME type of the attachment (e.g., {@code "image/png"})
         * @param attachmentName    a human-readable name for the attachment
         * @param attachmentContent an {@link InputStream} providing the binary content
         */
        public static void attach(String attachmentType, String attachmentName, InputStream attachmentContent) {
            ReportManagerHelper.attach(attachmentType, attachmentName, attachmentContent);
        }
    }
}
