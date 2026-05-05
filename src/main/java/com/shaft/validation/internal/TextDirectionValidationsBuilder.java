package com.shaft.validation.internal;

import com.shaft.validation.ValidationEnums;

/**
 * Fluent builder for validating text direction metadata (LTR/RTL).
 */
public class TextDirectionValidationsBuilder {
    private final NativeValidationsBuilder nativeValidationsBuilder;

    /**
     * Package-private constructor to enforce creation through parent fluent builders.
     *
     * @param nativeValidationsBuilder parent native builder carrying assertion context
     */
    TextDirectionValidationsBuilder(NativeValidationsBuilder nativeValidationsBuilder) {
        this.nativeValidationsBuilder = nativeValidationsBuilder;
    }

    /**
     * Validates that text direction metadata equals the target direction.
     *
     * @param direction expected direction (LTR/RTL)
     * @return validations executor for optional custom message and perform
     */
    public ValidationsExecutor is(ValidationEnums.TextDirection direction) {
        return nativeValidationsBuilder.isEqualTo(direction.getValue());
    }

    /**
     * Validates that text direction metadata is not equal to the target direction.
     *
     * @param direction unexpected direction (LTR/RTL)
     * @return validations executor for optional custom message and perform
     */
    public ValidationsExecutor isNot(ValidationEnums.TextDirection direction) {
        return nativeValidationsBuilder.doesNotEqual(direction.getValue());
    }

    /**
     * Convenience alias for asserting left-to-right direction.
     *
     * @return validations executor for optional custom message and perform
     */
    public ValidationsExecutor isLeftToRight() {
        return is(ValidationEnums.TextDirection.LTR);
    }

    /**
     * Convenience alias for asserting right-to-left direction.
     *
     * @return validations executor for optional custom message and perform
     */
    public ValidationsExecutor isRightToLeft() {
        return is(ValidationEnums.TextDirection.RTL);
    }
}
