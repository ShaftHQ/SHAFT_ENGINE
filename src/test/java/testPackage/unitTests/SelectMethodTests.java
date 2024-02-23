package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;

public class SelectMethodTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    private final By userNameField = By.name("username");
    private final By passwordField = By.name("password");
    private final By loginBtn = By.tagName("button");
    private final By dropDownList = By.xpath("//span[@class='oxd-userdropdown-tab']");

    //    @BeforeMethod
    protected void setUp() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("https://opensource-demo.orangehrmlive.com/web/index.php/auth/login");

    }

    //    @Test
    public void testValidSelect(){
        login("Admin","admin123")
                .clickDropDownList("Support");
    }

    //    @Test(expectedExceptions = {AssertionError.class})
public void testInvalidSelect(){
        login("Admin","admin123")
                .clickDropDownList("sds");
    }

    //    @AfterMethod
    protected void tearDown(){ driver.get().quit();}

    private SelectMethodTests login(String userName, String password) {
        fillLoginInfo(userName, password).clickLoginBtn();
        return this;
    }

    private SelectMethodTests fillLoginInfo(String userName, String password) {
        driver.get().element().type(userNameField, userName);
        driver.get().element().type(passwordField, password);
        return this;
    }

    private SelectMethodTests clickLoginBtn() {
        driver.get().element().click(loginBtn);
        return this;
    }

    private SelectMethodTests clickDropDownList(String text) {
        driver.get().element().select(dropDownList, text);
        return this;
    }
}
