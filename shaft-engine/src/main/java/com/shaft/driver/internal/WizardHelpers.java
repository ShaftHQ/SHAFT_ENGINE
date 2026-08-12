package com.shaft.driver.internal;

import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.*;
import org.openqa.selenium.By;

public class WizardHelpers {
    static DriverFactoryHelper helper;

    public static class WebDriverAssertions implements com.shaft.gui.driver.DriverAssertions {
        private final DriverFactoryHelper instanceHelper;

        public WebDriverAssertions(DriverFactoryHelper helper) {
            WizardHelpers.helper = helper;
            this.instanceHelper = helper;
        }

        @Override
        public WebDriverBrowserValidationsBuilder browser() {
            return com.shaft.validation.Validations.assertThat().browser(instanceHelper.getDriver());
        }

        @Override
        public WebDriverElementValidationsBuilder element(By locator) {
            return com.shaft.validation.Validations.assertThat().element(helper.getDriver(), locator);
        }

        @Override
        public NativeValidationsBuilder object(Object object) {
            return com.shaft.validation.Validations.assertThat().object(object);
        }

        @Override
        public WebDriverMobileValidationsBuilder mobileValues() {
            return new WebDriverMobileValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT,
                    instanceHelper.getDriver(), new StringBuilder("the mobile session "));
        }
    }

    public static class WebDriverVerifications implements com.shaft.gui.driver.DriverVerifications {
        private final DriverFactoryHelper instanceHelper;

        public WebDriverVerifications(DriverFactoryHelper helper) {
            WizardHelpers.helper = helper;
            this.instanceHelper = helper;
        }

        @Override
        public WebDriverBrowserValidationsBuilder browser() {
            return com.shaft.validation.Validations.verifyThat().browser(instanceHelper.getDriver());
        }

        @Override
        public WebDriverElementValidationsBuilder element(By locator) {
            return com.shaft.validation.Validations.verifyThat().element(helper.getDriver(), locator);
        }

        @Override
        public NativeValidationsBuilder object(Object accessible) {
            return com.shaft.validation.Validations.verifyThat().object(accessible);
        }

        @Override
        public WebDriverMobileValidationsBuilder mobileValues() {
            return new WebDriverMobileValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT,
                    instanceHelper.getDriver(), new StringBuilder("the mobile session "));
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

        public ValidationsExecutor forceFail(String message) {
            return com.shaft.validation.Validations.assertThat().forceFail(message);
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
