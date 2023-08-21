package testPackage.SHAFTWizard;

import com.google.gson.Gson;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.JSONFileManager;
import org.testng.annotations.Test;
import objectclass.BodyObject;

public class Test_Wizard_Test_Data {
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
