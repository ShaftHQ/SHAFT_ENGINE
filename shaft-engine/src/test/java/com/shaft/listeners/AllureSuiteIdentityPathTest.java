package com.shaft.listeners;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.AllureSuiteIdentity;
import io.qameta.allure.model.Parameter;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.TestResult;
import org.testng.annotations.Test;

import java.util.List;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotEquals;

/**
 * historyId and testCaseId are set in {@link AllureListener#beforeTestStop} from the
 * shipped identity helper, before Allure builds the report.
 */
public class AllureSuiteIdentityPathTest {
    @Test
    public void platformOrLanguageChangesBothIdsAndARepeatKeepsThem() {
        String originalPlatform = SHAFT.Properties.platform.targetPlatform();
        String originalLanguage = SHAFT.Properties.mobile.language();
        String originalLocale = SHAFT.Properties.mobile.locale();
        AllureListener listener = new AllureListener();
        try {
            SHAFT.Properties.platform.set().targetPlatform("Android");
            SHAFT.Properties.mobile.set().language("ar").locale("EG");
            TestResult android = result();
            listener.beforeTestStop(android);

            SHAFT.Properties.platform.set().targetPlatform("iOS");
            TestResult ios = result();
            listener.beforeTestStop(ios);

            assertNotEquals(android.getHistoryId(), ios.getHistoryId());
            assertNotEquals(android.getTestCaseId(), ios.getTestCaseId());
            assertEquals(android.getStatus(), Status.PASSED);
            assertEquals(ios.getStatus(), Status.PASSED);

            TestResult iosAgain = result();
            listener.beforeTestStop(iosAgain);
            assertEquals(ios.getHistoryId(), iosAgain.getHistoryId());
            assertEquals(ios.getTestCaseId(), iosAgain.getTestCaseId());

            SHAFT.Properties.mobile.set().language("en").locale("US");
            TestResult iosEnglish = result();
            listener.beforeTestStop(iosEnglish);
            assertNotEquals(ios.getHistoryId(), iosEnglish.getHistoryId());
            assertNotEquals(ios.getTestCaseId(), iosEnglish.getTestCaseId());

            SHAFT.Properties.platform.set().targetPlatform("Android");
            SHAFT.Properties.mobile.set().language("").locale("");
            TestResult parameterized = result();
            parameterized.setParameters(List.of(new Parameter().setName("platform").setValue("IOS")));
            listener.beforeTestStop(parameterized);
            TestResult direct = result();
            AllureSuiteIdentity.apply(direct, "IOS", "");
            assertEquals(parameterized.getHistoryId(), direct.getHistoryId());
            assertEquals(parameterized.getTestCaseId(), direct.getTestCaseId());
        } finally {
            SHAFT.Properties.platform.set().targetPlatform(originalPlatform);
            SHAFT.Properties.mobile.set().language(originalLanguage).locale(originalLocale);
        }
    }

    private static TestResult result() {
        return new TestResult()
                .setStatus(Status.PASSED)
                .setName("pay")
                .setFullName("Checkout.pay");
    }
}
