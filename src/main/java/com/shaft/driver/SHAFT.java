package com.shaft.driver;

import com.shaft.api.RequestBuilder;
import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.db.DatabaseActions;
import com.shaft.driver.internal.DriverFactoryHelper;
import com.shaft.driver.internal.WizardHelpers;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.SikuliActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.listeners.internal.WebDriverListener;
import com.shaft.tools.io.ExcelFileManager;
import com.shaft.tools.io.JSONFileManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.YAMLFileManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.internal.RestValidationsBuilder;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import io.restassured.response.Response;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.support.events.EventFiringDecorator;
import org.sikuli.script.App;

import java.io.InputStream;
import java.sql.ResultSet;
import java.util.List;

@SuppressWarnings("unused")
public class SHAFT {
    public static class GUI {
        public static class WebDriver {
            public static WebDriver getInstance() {
                return new WebDriver();
            }

            public static WebDriver getInstance(DriverFactory.DriverType driverType) {
                return new WebDriver(driverType);
            }

            public static WebDriver getInstance(DriverFactory.DriverType driverType, MutableCapabilities mutableCapabilities) {
                return new WebDriver(driverType, mutableCapabilities);
            }

            public WebDriver() {
                DriverFactory.getDriver();
            }

            public WebDriver(DriverFactory.DriverType driverType) {
                DriverFactory.getDriver(driverType);
            }

            public WebDriver(DriverFactory.DriverType driverType, MutableCapabilities mutableCapabilities) {
                DriverFactory.getDriver(driverType, mutableCapabilities);
            }

            public void quit() {
                DriverFactoryHelper.closeDriver();
            }

            public ElementActions element() {
                return ElementActions.getInstance();
            }

            public TouchActions touch() {
                return new TouchActions();
            }

            public BrowserActions browser() {
                return BrowserActions.getInstance();
            }

            public AlertActions alert() {
                return new AlertActions();
            }

            public WizardHelpers.WebDriverAssertions assertThat() {
                return new WizardHelpers.WebDriverAssertions();
            }

            public WizardHelpers.WebDriverVerifications verifyThat() {
                return new WizardHelpers.WebDriverVerifications();
            }

            /**
             * Returns the current Selenium WebDriver instance for custom manipulation
             *
             * @return the current Selenium WebDriver instance for custom manipulation
             */
            @SuppressWarnings("CommentedOutCode")
            public org.openqa.selenium.WebDriver getDriver() {
                /*
                 * Decorator is not working for appium drivers as per the following issues/articles
                 * https://github.com/appium/java-client/issues/1694
                 * https://github.com/appium/java-client/blob/master/docs/The-event_firing.md#createproxy-api-since-java-client-830
                 * https://github.com/SeleniumHQ/selenium/blob/316f9738a8e2079265a0691954ca8847e68c598d/java/test/org/openqa/selenium/support/events/EventFiringDecoratorTest.java#L422
                 */

                if (DriverFactoryHelper.getDriver() instanceof AndroidDriver androidDriver) {
//                    AndroidDriver decoratedDriver = createProxy(
//                            AndroidDriver.class,
//                            new Object[] {androidDriver},
//                            new Class[] {AndroidDriver.class},
//                            webDriverListener
//                    );
//                    return decoratedDriver;
//                    return new EventFiringDecorator<>(AndroidDriver.class, listener).decorate(androidDriver);
                    return DriverFactoryHelper.getDriver();
                } else if (DriverFactoryHelper.getDriver() instanceof IOSDriver iosDriver) {
//                    IOSDriver decoratedDriver = createProxy(
//                            IOSDriver.class,
//                            new Object[] {iosDriver},
//                            new Class[] {IOSDriver.class},
//                            webDriverListener
//                    );
//                    return decoratedDriver;
//                    return new EventFiringDecorator<>(IOSDriver.class, listener).decorate(iosDriver);
                    return DriverFactoryHelper.getDriver();
//                } else if (driverThreadLocal.get() instanceof RemoteWebDriver remoteWebDriver) {
//                    driverThreadLocal.set(new EventFiringDecorator<>(RemoteWebDriver.class, new WebDriverListener()).decorate(remoteWebDriver));
                } else {
                    if(!SHAFT.Properties.flags.enableTrueNativeMode()){
                        return new EventFiringDecorator<>(org.openqa.selenium.WebDriver.class, new WebDriverListener()).decorate(DriverFactoryHelper.getDriver());
                    }
                    else{
                        return DriverFactoryHelper.getDriver();
                    }
                    }
            }
        }

        public static class Locator extends com.shaft.gui.internal.locator.Locator {
        }

        public static class SikuliDriver {
            public static SikuliDriver getInstance(String applicationName) {
                return new SikuliDriver(applicationName);
            }

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

        public static API getInstance(String serviceURI) {
            return new API(serviceURI);
        }

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
        private CLI() {
            throw new IllegalStateException("Utility class");
        }

        public static TerminalActions terminal() {
            return new TerminalActions();
        }

        public static FileActions file() {
            return new FileActions();
        }
    }

    public static class DB extends DatabaseActions {
        public DB(String customConnectionString) {
            super(customConnectionString);
        }

        public DB(DatabaseActions.DatabaseType databaseType, String ip, String port, String name, String username,
                  String password) {
            super(databaseType, ip, port, name, username, password);
        }

        public static DB getInstance(DatabaseActions.DatabaseType databaseType, String ip, String port, String name, String username,
                                     String password) {
            return new DB(databaseType, ip, port, name, username, password);
        }

        public static DB getInstance(String customConnectionString) {
            return new DB(customConnectionString);
        }

        public static String getResult(ResultSet resultSet) {
            return DatabaseActions.getResult(resultSet);
        }

        public static String getColumn(ResultSet resultSet, String columnName) {
            return DatabaseActions.getColumn(resultSet, columnName);
        }

        public static String getRow(ResultSet resultSet, String columnName, String knownCellValue) {
            return DatabaseActions.getRow(resultSet, columnName, knownCellValue);
        }

        public static int getRowCount(ResultSet resultSet) {
            return DatabaseActions.getRowCount(resultSet);
        }

    }

    public static class Validations {
        private Validations() {
            throw new IllegalStateException("Utility class");
        }

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

            public static JSONFileManager getInstance(String jsonFilePath) {
                return new JSONFileManager(jsonFilePath);
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

            public static ExcelFileManager getInstance(String excelFilePath) {
                return new ExcelFileManager(excelFilePath);
            }
        }

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

            public static YAMLFileManager getInstance(String yamlFilePath) {
                return new YAMLFileManager(yamlFilePath);
            }
        }
    }

    public static class Properties extends com.shaft.properties.internal.Properties {
    }

    public static class Report {
        private Report() {
            throw new IllegalStateException("Utility class");
        }

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
