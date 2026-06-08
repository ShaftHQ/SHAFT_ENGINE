package testPackage;

import com.shaft.driver.SHAFT;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class TestClass {
    SHAFT.API driver;
    SHAFT.TestData.JSON testData;
    String serviceURI = "https://restcountries.com/v3.1/";

    @Test
    public void getCountryInfoUsingCapitalName() {
        driver.get("capital/{capital}".replace("{capital}", "Cairo"))
                .perform()
                .assertThatResponse().extractedJsonValue("[0].name.common").isEqualTo(testData.get("expectedCountryName")).perform();
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
