package com.shaft.driver;

import com.microsoft.playwright.BrowserType;
import com.microsoft.playwright.Page;
import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.db.DatabaseActions;
import com.shaft.driver.DriverFactory.DriverType;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.PlayWrightBrowserActions;
import com.shaft.gui.browser.WebDriverBrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.tools.io.*;
import com.shaft.validation.ValidationsBuilder;
import org.openqa.selenium.MutableCapabilities;
import org.sikuli.script.App;

import java.io.InputStream;

@SuppressWarnings("unused")
public class SHAFT_Backup {

    public static class GUI {
        public static class WebDriver {
            public static org.openqa.selenium.WebDriver getDriver() {
                return DriverFactory.getDriver();
            }

            public static org.openqa.selenium.WebDriver getDriver(DriverType driverType) {
                return DriverFactory.getDriver(driverType);
            }

            public static org.openqa.selenium.WebDriver getDriver(DriverType driverType, MutableCapabilities mutableCapabilities) {
                return DriverFactory.getDriver(driverType, mutableCapabilities);
            }

            public static void closeDriver() {
                DriverFactory.closeAllDrivers();
            }

            public static void closeDriver(org.openqa.selenium.WebDriver driver) {
                BrowserActions.closeCurrentWindow(driver);
            }

            public static ElementActions performElementAction(org.openqa.selenium.WebDriver driver) {
                return new ElementActions(driver);
            }

            public static WebDriverBrowserActions performBrowserAction(org.openqa.selenium.WebDriver driver) {
                return BrowserActions.performBrowserAction(driver);
            }
        }

        public static class Playwright {
            public static Page getDriver() {
                return DriverFactory.getPlaywrightDriver();
            }

            public static Page getDriver(DriverType driverType) {
                return DriverFactory.getPlaywrightDriver(driverType);
            }

            public static Page getDriver(DriverType driverType, BrowserType.LaunchOptions launchOptions) {
                return DriverFactory.getPlaywrightDriver(driverType, launchOptions);
            }

            public static void closeDriver() {
                DriverFactory.closePlayWrightDriver();
            }

            public static void closeDriver(Page page) {
                PlayWrightBrowserActions.closeCurrentWindow(page);
            }
        }

        public static class Sikuli {
            public static App getDriver(String applicationName) {
                return DriverFactory.getSikuliApp(applicationName);
            }

            public static void closeDriver(App application) {
                DriverFactory.closeSikuliApp(application);
            }
        }
    }

    public static class API {
        public static RestActions getDriver(String serviceURI) {
            return new RestActions(serviceURI);
        }
    }

    public static class CLI {
        public static TerminalActions performCLIAction() {
            return new TerminalActions();
        }

        public static FileActions performFileAction() {
            return new FileActions();
        }
    }

    public static class DB {
        public static DatabaseActions performDatabaseActions(DatabaseActions.DatabaseType databaseType, String ip, String port, String name, String username,
                                                             String password) {
            return new DatabaseActions(databaseType, ip, port, name, username, password);
        }

        public static DatabaseActions performDatabaseActions(String customConnectionString) {
            return new DatabaseActions(customConnectionString);
        }
    }

    public static class Validations {
        public static ValidationsBuilder assertThat() {
            return com.shaft.validation.Validations.assertThat();
        }

        public static ValidationsBuilder verifyThat() {
            return com.shaft.validation.Validations.verifyThat();
        }
    }

    public static class TestData {
        public static JSONFileManager json(String jsonFilePath) {
            return new JSONFileManager(jsonFilePath);
        }

        public static ExcelFileManager excel(String excelFilePath) {
            return new ExcelFileManager(excelFilePath);
        }

        public static PdfFileManager pdf(String pdfFilePath) {
            return new PdfFileManager(pdfFilePath);
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
