package com.shaft.validation.internal;

import com.shaft.validation.ValidationEnums;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Field;

class ValidationsExecutorIsolationTest {

    private static String readValidationCategoryString(ValidationsExecutor executor) throws Exception {
        Field f = ValidationsExecutor.class.getDeclaredField("validationCategoryString");
        f.setAccessible(true);
        return (String) f.get(executor);
    }

    @Test
    void hardAssertCategoryShouldBeAssert() throws Exception {
        ValidationsExecutor executor = new ValidationsExecutor(
            new ValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT));
        Assert.assertEquals(readValidationCategoryString(executor), "Assert");
    }

    @Test
    void softAssertCategoryShouldBeVerify() throws Exception {
        ValidationsExecutor executor = new ValidationsExecutor(
            new ValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT));
        Assert.assertEquals(readValidationCategoryString(executor), "Verify");
    }

    @Test
    void performShouldNotThrowWhenShaftWebAbsent() {
        ValidationsExecutor executor = new ValidationsExecutor(
            new ValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT));
        executor.perform();
    }

    @Test
    void invokeHelperShouldWrapClassNotFoundExceptionAsUnsupportedOperation() throws Exception {
        ValidationsExecutor executor = new ValidationsExecutor(
            ValidationEnums.ValidationCategory.HARD_ASSERT,
            ValidationEnums.ValidationType.POSITIVE,
            "conditionIsTrue",
            new StringBuilder("test condition"));

        java.lang.reflect.Method m = ValidationsExecutor.class.getDeclaredMethod(
            "invokeHelper", String.class, Object[].class);
        m.setAccessible(true);

        try {
            m.invoke(executor, "validateTrue",
                new Object[]{new Object[]{ValidationEnums.ValidationCategory.HARD_ASSERT, true,
                    ValidationEnums.ValidationType.POSITIVE, "test"}});
            Assert.fail("Expected UnsupportedOperationException");
        } catch (java.lang.reflect.InvocationTargetException e) {
            Assert.assertTrue(e.getCause() instanceof UnsupportedOperationException,
                "invokeHelper must wrap ClassNotFoundException as UnsupportedOperationException, was: " + e.getCause());
            Assert.assertTrue(e.getCause().getCause() instanceof ClassNotFoundException,
                "Root cause must be ClassNotFoundException, was: " + e.getCause().getCause());
        }
    }
}
