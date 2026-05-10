package com.shaft.validation.internal;

import com.shaft.validation.ValidationEnums;
import org.testng.Assert;
import org.testng.annotations.Test;

class ValidationsExecutorIsolationTest {

    @Test
    void hardAssertCategoryShouldBeAssert() {
        ValidationsExecutor executor = new ValidationsExecutor(
            new ValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT));
        Assert.assertEquals(executor.validationCategoryString, "Assert");
    }

    @Test
    void softAssertCategoryShouldBeVerify() {
        ValidationsExecutor executor = new ValidationsExecutor(
            new ValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT));
        Assert.assertEquals(executor.validationCategoryString, "Verify");
    }

    @Test
    void performShouldNotThrowWhenShaftWebAbsent() {
        ValidationsExecutor executor = new ValidationsExecutor(
            new ValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT));
        executor.perform();
    }

    @Test
    void invokeHelperShouldWrapClassNotFoundExceptionAsUnsupportedOperation() {
        ValidationsExecutor executor = new ValidationsExecutor(
            ValidationEnums.ValidationCategory.HARD_ASSERT,
            ValidationEnums.ValidationType.POSITIVE,
            "conditionIsTrue",
            new StringBuilder("test condition"));

        try {
            executor.invokeHelper("validateTrue");
            Assert.fail("Expected UnsupportedOperationException");
        } catch (UnsupportedOperationException e) {
            Assert.assertTrue(e.getCause() instanceof ClassNotFoundException,
                "invokeHelper must wrap ClassNotFoundException as UnsupportedOperationException, root cause was: " + e.getCause());
        }
    }
}
