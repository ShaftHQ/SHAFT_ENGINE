package testPackage;

import com.shaft.driver.SHAFT;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class TestClass {
    SHAFT.API driver;
    SHAFT.TestData.JSON testData;
    String serviceURI = "https://geocoding-api.open-meteo.com/v1/";

    @Test
    public void getCountryInfoUsingCapitalName() {
        driver.get("search")
                .setUrlArguments("name=Cairo&count=1")
                .perform()
                .assertThatResponse().extractedJsonValue("results[0].country").isEqualTo(testData.get("expectedCountryName")).perform();
    }

    @BeforeClass
    public void beforeClass() {
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }

    @BeforeMethod
    public void beforeMethod() {
        driver = new SHAFT.API(serviceURI);
    }

    @AfterMethod
    public void afterMethod() {
        driver = null;
    }
}
