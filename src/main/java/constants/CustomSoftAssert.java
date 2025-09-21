package constants;

import org.testng.asserts.IAssert;
import org.testng.asserts.SoftAssert;

import java.util.ArrayList;
import java.util.List;

public class CustomSoftAssert extends SoftAssert {
    private final List<String> failureMessages = new ArrayList<>();

    @Override
    public void onAssertFailure(IAssert<?> assertCommand, AssertionError ex) {
        StackTraceElement[] stackTrace = ex.getStackTrace();
        for (StackTraceElement element : stackTrace) {
            if (element.getClassName().contains("testPackage")) { // adjust root package
                String className = element.getClassName();
                String simpleClassName = className.substring(className.lastIndexOf(".") + 1);
                String methodName = element.getMethodName();
                int lineNumber = element.getLineNumber();

                String failureMessage =
                        "\n====================================" +
                        "\n❌ Assertion Failed!" +
                        "\n🔍 Details: " + ex.getMessage() +
                        "\n📍 Location:" +
                        "\n    at " + className + "." + methodName +
                        "(" + simpleClassName + ".java:" + lineNumber + ")" +
                        "\n====================================";

                failureMessages.add(failureMessage);
                break;
            }
        }
        super.onAssertFailure(assertCommand, ex);
    }

    @Override
    public void assertAll() {
        if (!failureMessages.isEmpty()) {
            System.out.println("\n=== Assertion Failures Summary ===");
            for (String message : failureMessages) {
                System.out.println(message);
            }
            System.out.println("====================================");
        }
        super.assertAll();
    }

    public void clearFailures() {
        failureMessages.clear();
    }
}
