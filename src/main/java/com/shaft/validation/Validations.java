package com.shaft.validation;

import org.openqa.selenium.WebDriver;

public class Validations {
    public static ValidationsBuilder assertThat() {
        return new ValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT);
    }

    public static ValidationsBuilder verifyThat() {
        return new ValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT);
    }

    public static WebDriverValidationsBuilder assertThat(WebDriver driver) {
        return new WebDriverValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT, driver);
    }

    public static WebDriverValidationsBuilder verifyThat(WebDriver driver) {
        return new WebDriverValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver);
    }
}
