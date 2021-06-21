package com.shaft.validation;

public class ValidationsBuilder {
    ValidationEnums.ValidationCategory validationCategory;
    String validationMethod;
    ValidationEnums.ValidationType validationType;
    boolean condition;
    Object actualValue;

    public ValidationsBuilder(ValidationEnums.ValidationCategory validationCategory) {
        this.validationCategory = validationCategory;
    }

    public ValidationsExecutor forceFail() {
        this.validationMethod = "forceFail";
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor conditionIsTrue(boolean condition) {
        this.validationMethod = "conditionIsTrue";
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        this.condition = condition;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor conditionIsFalse(boolean condition) {
        this.validationMethod = "conditionIsTrue";
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        this.condition = condition;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor objectIsNull(Object actualValue) {
        this.validationMethod = "objectIsNull";
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        this.actualValue = actualValue;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor objectIsNotNull(Object actualValue) {
        this.validationMethod = "objectIsNull";
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        this.actualValue = actualValue;
        return new ValidationsExecutor(this);
    }

    public ValidationsComparisonTypeManager object(Object actualValue) {
        this.validationMethod = "objectsAreEqual";
        this.actualValue = actualValue;
        return new ValidationsComparisonTypeManager(this);
    }
}
