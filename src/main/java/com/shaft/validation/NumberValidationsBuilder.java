package com.shaft.validation;

public class NumberValidationsBuilder {
    ValidationEnums.ValidationCategory validationCategory;
    ValidationEnums.ValidationType validationType;
    String validationMethod;

    Number expectedValue;
    Object actualValue;
    ValidationEnums.NumbersComparativeRelation numbersComparativeRelation;

    public NumberValidationsBuilder(ValidationsBuilder validationsBuilder) {
        this.validationCategory = validationsBuilder.validationCategory;
        this.validationMethod = validationsBuilder.validationMethod;
        this.actualValue = validationsBuilder.actualValue;
    }

    public ValidationsExecutor isEqualTo(Number expectedValue) {
        this.expectedValue = expectedValue;
        this.numbersComparativeRelation = ValidationEnums.NumbersComparativeRelation.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor doesNotEqual(Number expectedValue) {
        this.expectedValue = expectedValue;
        this.numbersComparativeRelation = ValidationEnums.NumbersComparativeRelation.EQUALS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor isGreaterThanOrEquals(Number expectedValue) {
        this.expectedValue = expectedValue;
        this.numbersComparativeRelation = ValidationEnums.NumbersComparativeRelation.GREATER_THAN_OR_EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor isGreaterThan(Number expectedValue) {
        this.expectedValue = expectedValue;
        this.numbersComparativeRelation = ValidationEnums.NumbersComparativeRelation.GREATER_THAN;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor isLessThanOrEquals(Number expectedValue) {
        this.expectedValue = expectedValue;
        this.numbersComparativeRelation = ValidationEnums.NumbersComparativeRelation.LESS_THAN_OR_EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    public ValidationsExecutor isLessThan(Number expectedValue) {
        this.expectedValue = expectedValue;
        this.numbersComparativeRelation = ValidationEnums.NumbersComparativeRelation.LESS_THAN;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }
}
