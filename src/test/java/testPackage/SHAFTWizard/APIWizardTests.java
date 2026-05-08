package testPackage.SHAFTWizard;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

@SuppressWarnings("unchecked")
public class APIWizardTests {
    private ThreadLocal<SHAFT.API> driver = new ThreadLocal<>();

    @Test
    public void test_get_matchesSchema() {
        driver.set(new SHAFT.API("http://api.zippopotam.us/"));
        driver.get().get("us/90210").perform();
        driver.get().assertThatResponse().matchesSchema("schema.json").perform();
    }

    @Test
    public void test_assertResponseEqualsIgnoringOrder() {
        driver.set(new SHAFT.API("http://api.zippopotam.us/"));
        driver.get().get("us/90210").perform();
        driver.get().assertThatResponse().isEqualToFileContentIgnoringOrder("test_assertResponseEqualsIgnoringOrder.json").perform();
    }

    @Test
    public void test_assertResponseDoesNotEqualFileContent() {
        driver.set(new SHAFT.API("http://api.zippopotam.us/"));
        driver.get().get("us/90210").perform();
        driver.get().assertThatResponse().doesNotEqualFileContent("test_assertResponseEqualsIgnoringOrder.json").perform();
    }
}
