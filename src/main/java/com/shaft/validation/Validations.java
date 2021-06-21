package com.shaft.validation;

public class Validations {
    public static ValidationsBuilder assertThat() {
        return new ValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT);
    }

    public static ValidationsBuilder verifyThat() {
        return new ValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT);
    }
}
