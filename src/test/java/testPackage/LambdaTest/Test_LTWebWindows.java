package testPackage.LambdaTest;

import com.shaft.api.RequestBuilder;
import com.shaft.api.RestActions;
import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.util.Arrays;
import java.util.List;

public class Test_LTWebWindows {
    SHAFT.GUI.WebDriver driver;

    //locators of test_ClickUsingJavaScript
    By emailField = By.xpath("//input[@name='user-name']");
    By passwordField = By.xpath("//input[@name='password']");
    By loginButton = By.xpath("//input[@name='login-button']");
    By product1 = By.xpath("//button[@name='add-to-cart-sauce-labs-backpack']");
    By shoppingCartButton = By.xpath("//a[@class='shopping_cart_link']");
    By productName = By.xpath("//div[@class='inventory_item_name']");
    @Test
    public void test_ClickUsingJavaScript() {
        driver.browser().navigateToURL("https://www.saucedemo.com")
                .element().type(emailField, "standard_user")
                .type(passwordField, "secret_sauce")
                .clickUsingJavascript(loginButton)
                .clickUsingJavascript(product1)
                .clickUsingJavascript(shoppingCartButton)
                .verifyThat(productName).text().isEqualTo("Sauce Labs Backpack").perform();
    }

//    @Test
//    public void test3() {
//        List<List<Object>> parameters = Arrays.asList(Arrays.asList("appFile",new File("/Users/magdy/Documents/GitHub/SHAFT_ENGINE/src/test/resources/testDataFiles/apps/ApiDemos-debug.apk") ), Arrays.asList("name","appname"));
//        SHAFT.API api = new SHAFT.API("https://manual-api.lambdatest.com");
//        api.post("/app/upload/realDevice").setAuthentication("magdy.heibavodafone", "pA1PmVOfkQ5gKfbjk4Heh7Jo4Ly7SUslr2JCcUCCXPYKrZRBB8", RequestBuilder.AuthenticationType.BASIC).setContentType("multipart/form-data").setParameters(parameters, RestActions.ParametersType.FORM).perform();
//        String body = api.getResponseBody();
//        System.out.println(body);
//        String value = api.getResponseJSONValue("app_url");
//        System.out.println(value);
//    }

    @BeforeMethod
    public void beforeMethod() {
        SHAFT.Properties.lambdaTest.set().browserVersion("114.0");
        SHAFT.Properties.platform.set().targetPlatform("windows");
        SHAFT.Properties.platform.set().executionAddress("lambdatest");
        SHAFT.Properties.web.set().targetBrowserName("chrome");
        SHAFT.Properties.lambdaTest.set().osVersion("11");
        SHAFT.Properties.lambdaTest.set().isRealMobile(false);
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.quit();
    }
}