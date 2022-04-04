package testPackage01.SHAFTWizard;

import com.shaft.driver.SHAFT;

public class Test_Wizard_DB {
    SHAFT.DB driver;

//    @Test
    public void test() {
        driver = new SHAFT.DB();
        driver.performDatabaseActions("").executeSelectQuery("");
    }
}
