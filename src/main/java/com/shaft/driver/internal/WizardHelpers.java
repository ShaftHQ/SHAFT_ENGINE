package com.shaft.driver.internal;

import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.validation.internal.*;
import org.openqa.selenium.By;

public class WizardHelpers {
    static DriverFactoryHelper helper;

    public static class WebDriverAssertions {
        public WebDriverAssertions(DriverFactoryHelper helper) {
            WizardHelpers.helper = helper;
        }

        public WebDriverBrowserValidationsBuilder browser() {
            return com.shaft.validation.Validations.assertThat().browser(helper.getDriver());
        }

        public WebDriverElementValidationsBuilder element(By locator) {
            return com.shaft.validation.Validations.assertThat().element(helper.getDriver(), locator);
        }
    }

    public static class WebDriverVerifications {

        public WebDriverVerifications(DriverFactoryHelper helper) {
            WizardHelpers.helper = helper;
        }

        public WebDriverBrowserValidationsBuilder browser() {
            return com.shaft.validation.Validations.verifyThat().browser(helper.getDriver());
        }

        public WebDriverElementValidationsBuilder element(By locator) {
            return com.shaft.validation.Validations.verifyThat().element(helper.getDriver(), locator);
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

    @SuppressWarnings("unused")
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
