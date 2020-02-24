package unitTests;

import com.shaft.validation.Verifications;
import org.testng.annotations.Test;

public class tests_validations_verifications {
    @Test
    public void verifyEquals_expectedToPass() {
        Verifications.verifyEquals(1, 1);
    }

    @Test
    public void verifyEquals_expectedToFail() {
        Verifications.verifyEquals(1, 1);
        Verifications.verifyEquals(11, 1);
        Verifications.verifyEquals(1, 1);
        Verifications.verifyEquals(12, 1);
    }
}
