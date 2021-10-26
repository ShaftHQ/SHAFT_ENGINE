package com.shaft.validation;

public class NumberValidationsBuilder {
    protected ValidationEnums.ValidationCategory validationCategory;
    protected ValidationEnums.ValidationType validationType;
    protected String validationMethod;

    protected Number expectedValue;
    protected Object actualValue;
    protected ValidationEnums.NumbersComparativeRelation numbersComparativeRelation;

    public NumberValidationsBuilder(ValidationsBuilder validationsBuilder) {
        this.validationCategory = validationsBuilder.validationCategory;
        this.validationMethod = validationsBuilder.validationMethod;
        this.actualValue = validationsBuilder.actualValue;
    }

    /**
     * Use this to check that the actual number is equal to the expected value
     * @param expectedValue the test data / expected value for the number under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isEqualTo(Number expectedValue) {
        this.expectedValue = expectedValue;
        this.numbersComparativeRelation = ValidationEnums.NumbersComparativeRelation.EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the actual number does not equal the expected value
     * @param expectedValue the test data / expected value for the number under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotEqual(Number expectedValue) {
        this.expectedValue = expectedValue;
        this.numbersComparativeRelation = ValidationEnums.NumbersComparativeRelation.EQUALS;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the actual number is greater than or equal to the expected value
     * @param expectedValue the test data / expected value for the number under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isGreaterThanOrEquals(Number expectedValue) {
        this.expectedValue = expectedValue;
        this.numbersComparativeRelation = ValidationEnums.NumbersComparativeRelation.GREATER_THAN_OR_EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the actual number is greater than the expected value
     * @param expectedValue the test data / expected value for the number under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isGreaterThan(Number expectedValue) {
        this.expectedValue = expectedValue;
        this.numbersComparativeRelation = ValidationEnums.NumbersComparativeRelation.GREATER_THAN;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the actual number is less than or equal to the expected value
     * @param expectedValue the test data / expected value for the number under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isLessThanOrEquals(Number expectedValue) {
        this.expectedValue = expectedValue;
        this.numbersComparativeRelation = ValidationEnums.NumbersComparativeRelation.LESS_THAN_OR_EQUALS;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }

    /**
     * Use this to check that the actual number is less than the expected value
     * @param expectedValue the test data / expected value for the number under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor isLessThan(Number expectedValue) {
        this.expectedValue = expectedValue;
        this.numbersComparativeRelation = ValidationEnums.NumbersComparativeRelation.LESS_THAN;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        return new ValidationsExecutor(this);
    }
}
