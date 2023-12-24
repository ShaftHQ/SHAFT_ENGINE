package com.shaft.validation.internal.builder;

import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.executor.GenericExecutor;

@SuppressWarnings("unused")
public class JSON extends Native {
    //TODO: implement all the methods
    public JSON(API restValidationsBuilder) {
        super(restValidationsBuilder);
    }

    /**
     * Use this to check that the actual json response is equal to the expected json value (ignoring ordering)
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public GenericExecutor equalsIgnoringOrder(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.MATCHES;
        this.validationType = ValidationEnums.ValidationType.POSITIVE;
        reportMessageBuilder.append("equals \"").append(expectedValue).append("\", ignoring ordering.");
        return new GenericExecutor(this);
    }

    /**
     * Use this to check that the actual json response is not equal to the expected json value (ignoring ordering)
     *
     * @param expectedValue the test data / expected value for the object under test
     * @return a ValidationsExecutor object to set your custom validation message (if needed) and then perform() your validation
     */
    public GenericExecutor doesNotEqualIgnoringOrder(Object expectedValue) {
        this.expectedValue = expectedValue;
        this.validationComparisonType = ValidationEnums.ValidationComparisonType.MATCHES;
        this.validationType = ValidationEnums.ValidationType.NEGATIVE;
        reportMessageBuilder.append("does not equal \"").append(expectedValue).append("\", ignoring ordering.");
        return new GenericExecutor(this);
    }
}
