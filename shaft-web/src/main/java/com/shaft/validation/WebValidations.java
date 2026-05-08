package com.shaft.validation;

import com.shaft.validation.internal.WebValidationsBuilder;

public class WebValidations {
    private WebValidations() {
        throw new IllegalStateException("Utility class");
    }

    public static WebValidationsBuilder assertThat() {
        return new WebValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT);
    }

    public static WebValidationsBuilder verifyThat() {
        return new WebValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT);
    }
}
