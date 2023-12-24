package com.shaft.driver.internal;

import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.validation.internal.builder.Number;
import com.shaft.validation.internal.builder.*;
import com.shaft.validation.internal.executor.GenericExecutor;
import org.openqa.selenium.By;

public class WizardHelpers {
    static DriverFactoryHelper helper;
    public static class WebDriverAssertions {
        public WebDriverAssertions(DriverFactoryHelper helper) {
            WizardHelpers.helper = helper;
        }

        public WebBrowser browser() {
            return com.shaft.validation.Validations.assertThat().browser(helper.getDriver());
        }

        public WebElement element(By locator) {
            return com.shaft.validation.Validations.assertThat().element(helper.getDriver(), locator);
        }
    }

    public static class WebDriverVerifications {

        public WebDriverVerifications(DriverFactoryHelper helper) {
            WizardHelpers.helper = helper;
        }

        public WebBrowser browser() {
            return com.shaft.validation.Validations.verifyThat().browser(helper.getDriver());
        }

        public WebElement element(By locator) {
            return com.shaft.validation.Validations.verifyThat().element(helper.getDriver(), locator);
        }
    }

    public static class StandaloneAssertions {
        public StandaloneAssertions() {

        }

        public Native object(Object actual) {
            return com.shaft.validation.Validations.assertThat().object(actual);
        }

        public Number number(java.lang.Number actual) {
            return com.shaft.validation.Validations.assertThat().number(actual);
        }

        public File file(String folderRelativePath, String fileName) {
            return com.shaft.validation.Validations.assertThat().file(folderRelativePath, fileName);
        }

        public GenericExecutor forceFail() {
            return com.shaft.validation.Validations.assertThat().forceFail();
        }
    }

    @SuppressWarnings("unused")
    public static class StandaloneVerifications {
        public StandaloneVerifications() {

        }

        public Native object(Object actual) {
            return com.shaft.validation.Validations.verifyThat().object(actual);
        }

        public Number number(java.lang.Number actual) {
            return com.shaft.validation.Validations.verifyThat().number(actual);
        }

        public File file(String folderRelativePath, String fileName) {
            return com.shaft.validation.Validations.verifyThat().file(folderRelativePath, fileName);
        }

        public GenericExecutor forceFail() {
            return com.shaft.validation.Validations.verifyThat().forceFail();
        }
    }


}
