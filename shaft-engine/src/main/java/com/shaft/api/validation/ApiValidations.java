package com.shaft.api.validation;

import com.shaft.api.validation.internal.RestValidationsBuilder;
import com.shaft.validation.ValidationEnums;

public class ApiValidations {

    private ApiValidations() {}

    /**
     * Soft assertion — records failures and reports them at the end of the test.
     */
    public static RestValidationsBuilder verifyThat(Object response) {
        return new RestValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT, response);
    }

    /**
     * Hard assertion — throws immediately on first failure.
     */
    public static RestValidationsBuilder assertThat(Object response) {
        return new RestValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT, response);
    }
}
