package external;

import com.shaft.cli.FileActions;

import java.lang.reflect.Method;

public final class FileActionsReflectionInvoker {
    private FileActionsReflectionInvoker() {
        throw new IllegalStateException("Utility class");
    }

    public static String invokePassReport(FileActions actions) throws Exception {
        Method reportActionResult = FileActions.class.getDeclaredMethod(
                "reportActionResult", String.class, String.class, String.class, Boolean.class, Exception[].class);
        reportActionResult.setAccessible(true);
        return (String) reportActionResult.invoke(actions, "externalAction", "", "", true, new Exception[0]);
    }
}
