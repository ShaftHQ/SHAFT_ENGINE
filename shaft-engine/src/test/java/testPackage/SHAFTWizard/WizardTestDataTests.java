package testPackage.SHAFTWizard;

import com.google.gson.Gson;
import com.shaft.driver.SHAFT;
import objectclass.BodyObject;
import org.testng.annotations.Test;

public class WizardTestDataTests {
    private SHAFT.TestData.JSON testData;

    @Test
    public void test_mapping_test_data_to_java_object() {
        testData = new SHAFT.TestData.JSON("jsonToJavaObjectTest.json");
        Object obj = testData.getTestDataAsJson("data.jsonData");
        Gson gson = new Gson();
        BodyObject bodyObject = gson.fromJson(obj.toString(), BodyObject.class);
        SHAFT.Validations.assertThat().object(bodyObject.getKey1()).isEqualTo("SHAFT");
    }
}
