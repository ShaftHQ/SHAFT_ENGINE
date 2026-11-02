package testPackage.SHAFTWizard;

import com.shaft.driver.SHAFT;

public class DBWizardTests {

//    @Test
    public void test() {
        SHAFT.DB.getInstance("").executeSelectQuery("");
    }
}
