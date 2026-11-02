package testPackage.LambdaTest;

import com.shaft.driver.SHAFT;
import org.testng.SkipException;

final class LambdaTestCredentials {
    private LambdaTestCredentials() {
        // utility class
    }

    static void apply() {
        String username = firstNonBlank(
                System.getProperty("LambdaTest.username"),
                System.getenv("LAMBDATEST_USERNAME"),
                System.getenv("LT_USERNAME"));
        String accessKey = firstNonBlank(
                System.getProperty("LambdaTest.accessKey"),
                System.getenv("LAMBDATEST_ACCESS_KEY"),
                System.getenv("LT_ACCESS_KEY"));

        if (username == null || accessKey == null) {
            throw new SkipException("LambdaTest credentials are not configured for this run.");
        }

        SHAFT.Properties.lambdaTest.set().username(username);
        SHAFT.Properties.lambdaTest.set().accessKey(accessKey);
    }

    private static String firstNonBlank(String... candidates) {
        for (String candidate : candidates) {
            if (candidate != null && !candidate.isBlank()) {
                return candidate;
            }
        }
        return null;
    }

}
