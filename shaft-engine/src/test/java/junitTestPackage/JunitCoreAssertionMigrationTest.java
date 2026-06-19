package junitTestPackage;

import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.tools.internal.support.JavaHelper;
import org.junit.jupiter.api.Test;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class JunitCoreAssertionMigrationTest {
    @Test
    void compareTwoObjectsLiteralFailureShouldKeepExpectedActualOrder() throws Throwable {
        MethodHandle compareTwoObjectsPositively = MethodHandles.privateLookupIn(JavaHelper.class, MethodHandles.lookup())
                .findStatic(JavaHelper.class, "compareTwoObjectsPositively",
                        MethodType.methodType(int.class, Object.class, Object.class, int.class));

        AssertionError error = assertThrows(AssertionError.class,
                () -> compareTwoObjectsPositively.invoke("expected-value", "actual-value", 1));

        assertTrue(error.getMessage().contains("expected-value"),
                "Failure message should include the expected value.");
        assertTrue(error.getMessage().contains("actual-value"),
                "Failure message should include the actual value.");
        assertTrue(error.getMessage().indexOf("expected-value") < error.getMessage().indexOf("actual-value"),
                "Expected value should be reported before actual value.");
    }

    @Test
    void failActionShouldPreserveMessageAndRootCause() {
        RuntimeException rootCause = new RuntimeException("root cause message");
        ElementActionsHelper helper = new ElementActionsHelper(true);

        AssertionError error = assertThrows(AssertionError.class,
                () -> helper.failAction(null, "click", "submit", null, null, rootCause));

        assertTrue(error.getMessage().contains("Failed to Click \"submit\""),
                "Failure message should include the action message.");
        assertTrue(error.getMessage().contains("root cause message"),
                "Failure message should include the root cause message.");
        assertSame(rootCause, error.getCause(),
                "JUnit Assertions.fail(message, cause) should preserve the original cause.");
    }
}
