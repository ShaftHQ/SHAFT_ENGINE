package com.shaft.driver;

import com.shaft.api.RequestBuilder;
import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.db.DatabaseActions;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.WebDriverBrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.SikuliActions;
import com.shaft.tools.io.*;
import com.shaft.validation.RestValidationsBuilder;
import io.restassured.config.RestAssuredConfig;
import io.restassured.response.Response;
import org.openqa.selenium.MutableCapabilities;
import org.sikuli.script.App;

import java.io.InputStream;
import java.util.List;

@SuppressWarnings("unused")
public class SHAFT {
    public static class GUI {
        public static class WebDriver {
            private final ThreadLocal<org.openqa.selenium.WebDriver> driverThreadLocal = new ThreadLocal<>();

            public WebDriver() {
                driverThreadLocal.set(DriverFactory.getDriver());
            }

            public WebDriver(DriverFactory.DriverType driverType) {
                driverThreadLocal.set(DriverFactory.getDriver(driverType));
            }

            public WebDriver(DriverFactory.DriverType driverType, MutableCapabilities mutableCapabilities) {
                driverThreadLocal.set(DriverFactory.getDriver(driverType, mutableCapabilities));
            }

            public void quit() {
                DriverFactory.closeAllDrivers();
            }

            public ElementActions element() {
                return new ElementActions(driverThreadLocal.get());
            }

            public WebDriverBrowserActions browser() {
                return BrowserActions.performBrowserAction(driverThreadLocal.get());
            }

            public WizardHelpers.WebDriverAssertions assertThat() {
                return new WizardHelpers.WebDriverAssertions(driverThreadLocal);
            }

            public WizardHelpers.WebDriverVerifications verifyThat() {
                return new WizardHelpers.WebDriverVerifications(driverThreadLocal);
            }

            /**
             * Returns the current Selenium WebDriver instance for custom manipulation
             *
             * @return the current Selenium WebDriver instance for custom manipulation
             */
            public org.openqa.selenium.WebDriver getDriver() {
                return driverThreadLocal.get();
            }
        }

        public static class SikuliDriver {

            private final App sikuliApp;

            public SikuliDriver(String applicationName) {
                sikuliApp = DriverFactory.getSikuliApp(applicationName);
            }

            public void quit() {
                DriverFactory.closeSikuliApp(sikuliApp);
            }

            public SikuliActions element() {
                return new SikuliActions(sikuliApp);
            }

            public App getDriver(String applicationName) {
                return sikuliApp;
            }
        }
    }
    public static class API {
        private final RestActions session;
        private String serviceURI;

        public API(String serviceURI) {
            session = new RestActions(serviceURI);
        }

        public RequestBuilder get(String serviceName) {
            return session.buildNewRequest(serviceName, RestActions.RequestType.GET);
        }

        public RequestBuilder post(String serviceName) {
            return session.buildNewRequest(serviceName, RestActions.RequestType.POST);
        }

        public RequestBuilder patch(String serviceName) {
            return session.buildNewRequest(serviceName, RestActions.RequestType.PATCH);
        }

        public RequestBuilder delete(String serviceName) {
            return session.buildNewRequest(serviceName, RestActions.RequestType.DELETE);
        }

        public RequestBuilder put(String serviceName) {
            return session.buildNewRequest(serviceName, RestActions.RequestType.PUT);
        }

        public void addConfig(RestAssuredConfig restAssuredConfig) {
            session.addConfigVariable(restAssuredConfig);
        }

        public void addHeader(String key, String value) {
            session.addHeaderVariable(key, value);
        }

        public void addCookie(String key, String value) {
            session.addCookieVariable(key, value);
        }

        public RestValidationsBuilder assertThatResponse() {
            return com.shaft.validation.Validations.assertThat().response(RestActions.getLastResponse());
        }

        public RestValidationsBuilder verifyThatResponse() {
            return com.shaft.validation.Validations.verifyThat().response(RestActions.getLastResponse());
        }

        public Response getResponse() {
            return RestActions.getLastResponse();
        }

        public String getResponseBody() {
            return RestActions.getResponseBody(RestActions.getLastResponse());
        }

        public int getResponseStatusCode() {
            return RestActions.getResponseStatusCode(RestActions.getLastResponse());
        }

        public long getResponseTime() {
            return RestActions.getResponseTime(RestActions.getLastResponse());
        }

        public String getResponseJSONValue(String jsonPath) {
            return RestActions.getResponseJSONValue(RestActions.getLastResponse(), jsonPath);
        }

        public List<Object> getResponseJSONValueAsList(String jsonPath) {
            return RestActions.getResponseJSONValueAsList(RestActions.getLastResponse(), jsonPath);
        }

        public String getResponseXMLValue(String xmlPath) {
            return RestActions.getResponseXMLValue(RestActions.getLastResponse(), xmlPath);
        }

        public List<Object> getResponseXMLValueAsList(String xmlPath) {
            return RestActions.getResponseXMLValueAsList(RestActions.getLastResponse(), xmlPath);
        }
    }

    public static class CLI {
        public TerminalActions terminal() {
            return new TerminalActions();
        }

        public FileActions file() {
            return new FileActions();
        }
    }

    public static class DB {
        public DatabaseActions performDatabaseActions(DatabaseActions.DatabaseType databaseType, String ip, String port, String name, String username,
                                                      String password) {
            return new DatabaseActions(databaseType, ip, port, name, username, password);
        }

        public DatabaseActions performDatabaseActions(String customConnectionString) {
            return new DatabaseActions(customConnectionString);
        }
    }

    public static class Validations {
        public static WizardHelpers.StandaloneAssertions assertThat() {
            return new WizardHelpers.StandaloneAssertions();
        }

        public static WizardHelpers.StandaloneVerifications verifyThat() {
            return new WizardHelpers.StandaloneVerifications();
        }
    }

    public static class TestData {
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
        }
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
        }

        public static class YAML extends YAMLFileManager {
            /**
             * Creates a new instance of the test data Excel reader using the target Excel
             *  file path
             * @param yamlFilePath target test data yaml file path
             */
            public YAML(String yamlFilePath) {
                super(yamlFilePath);
            }
        }
    }
    public static class Report {
        public static void log(String message) {
            ReportManager.logDiscrete(message);
        }

        public static void report(String message) {
            ReportManager.log(message);
        }

        public static void attach(String attachmentType, String attachmentName, String attachmentContent) {
            ReportManagerHelper.attach(attachmentType, attachmentName, attachmentContent);
        }

        public static void attach(String attachmentType, String attachmentName, InputStream attachmentContent) {
            ReportManagerHelper.attach(attachmentType, attachmentName, attachmentContent);
        }
    }
}
