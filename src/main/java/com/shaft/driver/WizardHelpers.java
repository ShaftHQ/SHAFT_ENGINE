package com.shaft.driver;

import com.shaft.validation.*;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class WizardHelpers {
    public static class WebDriverAssertions {
        private ThreadLocal<org.openqa.selenium.WebDriver> driverThreadLocal;

        public WebDriverAssertions(ThreadLocal<WebDriver> driverThreadLocal) {
            this.driverThreadLocal = driverThreadLocal;
        }

        public WebDriverBrowserValidationsBuilder browser() {
            return com.shaft.validation.Validations.assertThat().browser(driverThreadLocal.get());
        }

        public WebDriverElementValidationsBuilder element(By locator) {
            return com.shaft.validation.Validations.assertThat().element(driverThreadLocal.get(), locator);
        }
    }

    public static class WebDriverVerifications {
        private ThreadLocal<org.openqa.selenium.WebDriver> driverThreadLocal;

        public WebDriverVerifications(ThreadLocal<WebDriver> driverThreadLocal) {
            this.driverThreadLocal = driverThreadLocal;
        }

        public WebDriverBrowserValidationsBuilder browser() {
            return com.shaft.validation.Validations.verifyThat().browser(driverThreadLocal.get());
        }

        public WebDriverElementValidationsBuilder element(By locator) {
            return com.shaft.validation.Validations.verifyThat().element(driverThreadLocal.get(), locator);
        }
    }

    public static class StandaloneAssertions {
        public StandaloneAssertions() {

        }

        public NativeValidationsBuilder object(Object actual) {
            return com.shaft.validation.Validations.assertThat().object(actual);
        }

        public NumberValidationsBuilder number(Number actual) {
            return com.shaft.validation.Validations.assertThat().number(actual);
        }

        public FileValidationsBuilder file(String folderRelativePath, String fileName) {
            return com.shaft.validation.Validations.assertThat().file(folderRelativePath, fileName);
        }

        public ValidationsExecutor forceFail() {
            return com.shaft.validation.Validations.assertThat().forceFail();
        }
    }

    public static class StandaloneVerifications {
        public StandaloneVerifications() {

        }

        public NativeValidationsBuilder object(Object actual) {
            return com.shaft.validation.Validations.verifyThat().object(actual);
        }

        public NumberValidationsBuilder number(Number actual) {
            return com.shaft.validation.Validations.verifyThat().number(actual);
        }

        public FileValidationsBuilder file(String folderRelativePath, String fileName) {
            return com.shaft.validation.Validations.verifyThat().file(folderRelativePath, fileName);
        }

        public ValidationsExecutor forceFail() {
            return com.shaft.validation.Validations.verifyThat().forceFail();
        }
    }


}
