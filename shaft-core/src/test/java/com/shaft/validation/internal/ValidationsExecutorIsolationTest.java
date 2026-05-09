package com.shaft.validation.internal;

import com.shaft.validation.ValidationEnums;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.*;

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
        assertEquals("Assert", readValidationCategoryString(executor));
    }

    @Test
    void softAssertCategoryShouldBeVerify() throws Exception {
        ValidationsExecutor executor = new ValidationsExecutor(
            new ValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT));
        assertEquals("Verify", readValidationCategoryString(executor));
    }

    @Test
    void performShouldNotThrowWhenShaftWebAbsent() {
        ValidationsExecutor executor = new ValidationsExecutor(
            new ValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT));
        assertDoesNotThrow(executor::perform,
            "perform() must not throw — it only logs the custom message");
    }

    @Test
    void invokeHelperShouldWrapClassNotFoundExceptionAsUnsupportedOperation() throws Exception {
        // Call the private invokeHelper directly to test the ClassNotFoundException wrapping
        // without triggering the full internalPerform() stack (which needs Properties bootstrapped).
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
            fail("Expected UnsupportedOperationException");
        } catch (java.lang.reflect.InvocationTargetException e) {
            assertTrue(e.getCause() instanceof UnsupportedOperationException,
                "invokeHelper must wrap ClassNotFoundException as UnsupportedOperationException, was: " + e.getCause());
            assertTrue(e.getCause().getCause() instanceof ClassNotFoundException,
                "Root cause must be ClassNotFoundException, was: " + e.getCause().getCause());
        }
    }
}
