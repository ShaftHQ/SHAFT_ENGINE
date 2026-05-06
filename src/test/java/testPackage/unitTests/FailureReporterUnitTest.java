package testPackage.unitTests;

import com.shaft.tools.io.internal.FailureReporter;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Unit tests for {@link FailureReporter}.
 * Verifies the static utility methods {@code getRootCause()} and {@code fail()}.
 */
public class FailureReporterUnitTest {

    // ─── getRootCause ─────────────────────────────────────────────────────────

    @Test(description = "getRootCause: simple exception returns non-null, non-empty message")
    public void getRootCauseSimpleExceptionReturnsMessage() {
        Throwable cause = new IllegalArgumentException("bad input");
        String result = FailureReporter.getRootCause(cause);
        Assert.assertNotNull(result, "getRootCause must not return null");
        Assert.assertFalse(result.isBlank(), "getRootCause must not return blank string");
    }

    @Test(description = "getRootCause: result contains the exception class name")
    public void getRootCauseResultContainsClassName() {
        Throwable cause = new IllegalArgumentException("bad input");
        String result = FailureReporter.getRootCause(cause);
        Assert.assertTrue(result.contains("IllegalArgumentException"),
                "getRootCause result should contain the exception class name");
    }

    @Test(description = "getRootCause: result contains the exception message")
    public void getRootCauseResultContainsMessage() {
        Throwable cause = new RuntimeException("root message here");
        String result = FailureReporter.getRootCause(cause);
        Assert.assertTrue(result.contains("root message here"),
                "getRootCause result should contain the exception message");
    }

    @Test(description = "getRootCause: nested exception returns the root-cause message")
    public void getRootCauseNestedExceptionReturnsRootCauseMessage() {
        Throwable root = new IllegalStateException("the real cause");
        Throwable wrapper = new RuntimeException("wrapper", root);
        Throwable outer = new RuntimeException("outer", wrapper);
        String result = FailureReporter.getRootCause(outer);
        Assert.assertTrue(result.contains("the real cause"),
                "getRootCause should drill down to the innermost exception message");
        Assert.assertTrue(result.contains("IllegalStateException"),
                "getRootCause should include the root exception class name");
    }

    @Test(description = "getRootCause: exception with null message returns result with class name only")
    public void getRootCauseNullMessageExceptionReturnsClassNameOnly() {
        Throwable cause = new NullPointerException(); // message is null
        String result = FailureReporter.getRootCause(cause);
        Assert.assertNotNull(result, "getRootCause must not return null even when message is null");
        Assert.assertTrue(result.contains("NullPointerException"),
                "getRootCause with null message must still include the class name");
    }

    @Test(description = "getRootCause: result starts with ' Root cause: '")
    public void getRootCauseResultStartsWithExpectedPrefix() {
        Throwable cause = new Exception("test");
        String result = FailureReporter.getRootCause(cause);
        Assert.assertTrue(result.startsWith(" Root cause: "),
                "getRootCause result should start with ' Root cause: '");
    }

    // ─── fail(String) ─────────────────────────────────────────────────────────

    @Test(description = "fail(message): throws RuntimeException for regular message")
    public void failWithRegularMessageThrowsRuntimeException() {
        try {
            FailureReporter.fail("something went wrong");
            Assert.fail("Expected RuntimeException to be thrown");
        } catch (RuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("something went wrong"),
                    "RuntimeException message should contain the original message");
        }
    }

    @Test(description = "fail(message): message containing 'assert' throws AssertionError")
    public void failWithAssertMessageThrowsAssertionError() {
        try {
            FailureReporter.fail("assertion failed for element");
            Assert.fail("Expected AssertionError to be thrown");
        } catch (AssertionError e) {
            Assert.assertTrue(e.getMessage().contains("assertion failed"),
                    "AssertionError message should contain the original message");
        }
    }

    @Test(description = "fail(message): message with 'Assert' (upper case) throws AssertionError")
    public void failWithUpperCaseAssertMessageThrowsAssertionError() {
        try {
            FailureReporter.fail("Assert that value is not null");
            Assert.fail("Expected AssertionError");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        }
    }
}
