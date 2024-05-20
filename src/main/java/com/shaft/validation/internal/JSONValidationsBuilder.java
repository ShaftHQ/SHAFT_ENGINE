package com.shaft.validation.internal;

import com.shaft.validation.ValidationEnums;

@SuppressWarnings("unused")
public class JSONValidationsBuilder extends NativeValidationsBuilder {
    //TODO: implement all the methods
    public JSONValidationsBuilder(RestValidationsBuilder restValidationsBuilder) {
        super(restValidationsBuilder);
    }

    /**
     * Use this to check that the actual json response is equal to the expected json value (ignoring ordering)
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor equalsIgnoringOrder(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.MATCHES;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("equals \"").append(expectedValue).append("\", ignoring ordering.");
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }

    /**
     * Use this to check that the actual json response is not equal to the expected json value (ignoring ordering)
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public ValidationsExecutor doesNotEqualIgnoringOrder(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.MATCHES;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("does not equal \"").append(expectedValue).append("\", ignoring ordering.");
        var executor = new ValidationsExecutor(this);
        executor.internalPerform();
        return executor;
    }
}
