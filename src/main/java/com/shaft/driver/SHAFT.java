package com.shaft.driver;

import com.google.common.annotations.Beta;
import com.shaft.api.RequestBuilder;
import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.db.DatabaseActions;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.WebDriverBrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.tools.io.ExcelFileManager;
import com.shaft.tools.io.JSONFileManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import com.shaft.validation.RestValidationsBuilder;
import io.restassured.config.RestAssuredConfig;
import io.restassured.response.Response;
import org.openqa.selenium.MutableCapabilities;

import java.io.InputStream;

@SuppressWarnings("unused")
@Beta
public class SHAFT {
    @Beta
    public static class GUI {
        @Beta
        public static class WebDriver {
            private ThreadLocal<org.openqa.selenium.WebDriver> driverThreadLocal = new ThreadLocal<>();

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

//        public static class Playwright {
//            public static Page getDriver() {
//                return DriverFactory.getPlaywrightDriver();
//            }
//
//            public static Page getDriver(DriverType driverType) {
//                return DriverFactory.getPlaywrightDriver(driverType);
//            }
//
//            public static Page getDriver(DriverType driverType, BrowserType.LaunchOptions launchOptions) {
//                return DriverFactory.getPlaywrightDriver(driverType, launchOptions);
//            }
//
//            public static void closeDriver() {
//                DriverFactory.closePlayWrightDriver();
//            }
//
//            public static void closeDriver(Page page) {
//                PlayWrightBrowserActions.closeCurrentWindow(page);
//            }
//        }
//
//        public static class Sikuli {
//            public static App getDriver(String applicationName) {
//                return DriverFactory.getSikuliApp(applicationName);
//            }
//
//            public static void closeDriver(App application) {
//                DriverFactory.closeSikuliApp(application);
//            }
//        }
    }
    @Beta
    public static class API {
        private RestActions session;
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

        public RestValidationsBuilder assertThatResponse() {
            return com.shaft.validation.Validations.assertThat().response(RestActions.getLastResponse());
        }

        public RestValidationsBuilder verifyThatResponse() {
            return com.shaft.validation.Validations.verifyThat().response(RestActions.getLastResponse());
        }

        public Response getResponse() {
            return RestActions.getLastResponse();
        }
    }

    @Beta
    public static class CLI {
        public TerminalActions terminal() {
            return new TerminalActions();
        }

        public FileActions file() {
            return new FileActions();
        }
    }

    @Beta
    public static class DB {
        public DatabaseActions performDatabaseActions(DatabaseActions.DatabaseType databaseType, String ip, String port, String name, String username,
                                                      String password) {
            return new DatabaseActions(databaseType, ip, port, name, username, password);
        }

        public DatabaseActions performDatabaseActions(String customConnectionString) {
            return new DatabaseActions(customConnectionString);
        }
    }

    @Beta
    public static class Validations {
        public static WizardHelpers.StandaloneAssertions assertThat() {
            return new WizardHelpers.StandaloneAssertions();
        }

        public static WizardHelpers.StandaloneVerifications verifyThat() {
            return new WizardHelpers.StandaloneVerifications();
        }
    }

    @Beta
    public static class TestData {
        @Beta
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
        @Beta
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
    }
    @Beta
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
