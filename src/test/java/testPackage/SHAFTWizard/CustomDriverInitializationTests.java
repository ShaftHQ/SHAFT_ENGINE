package testPackage.SHAFTWizard;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.chrome.ChromeDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class CustomDriverInitializationTests {
    SHAFT.GUI.WebDriver driver;
    By upgradeNowButton = SHAFT.GUI.Locator.hasTagName("a")
            .containsClass("button--secondary")
            .containsText("Upgrade Now")
            .hasIndex(1).build();

    @BeforeMethod
    public void beforeMethod(){
        driver = new SHAFT.GUI.WebDriver(new ChromeDriver());
    }

    @Test(enabled = false)
    public void upgradeNowButton(){
        driver.browser().navigateToURL("https://shafthq.github.io/")
                .and().element().assertThat(upgradeNowButton).exists();
    }

    @AfterMethod
    public void afterMethod(){
        driver.quit();
    }
}
