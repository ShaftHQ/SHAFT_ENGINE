package testPackage.legacy;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import testPackage.Tests;

public class WaitActionsTests extends Tests {
    double defaultTimeout;

    @BeforeClass
    public void reduceTimeout() {
        defaultTimeout = SHAFT.Properties.timeouts.defaultElementIdentificationTimeout();
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(2);
    }

    @AfterClass
    public void returnTimeout() {
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(defaultTimeout);
    }

    @Test(expectedExceptions = AssertionError.class)
    public void waitUntilNumberOfElementsToBe() {
        driverThreadLocal.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilNumberOfElementsToBe(By.className("buttons_AeoN"), 2);
    }

    @Test(expectedExceptions = AssertionError.class)
    public void waitUntilNumberOfElementsToBeLessThan() {
        driverThreadLocal.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilNumberOfElementsToBeLessThan(By.className("buttons_AeoN"), 0);
    }

    @Test(expectedExceptions = AssertionError.class)
    public void waitUntilNumberOfElementsToBeMoreThan() {
        driverThreadLocal.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilNumberOfElementsToBeMoreThan(By.className("buttons_AeoN"), 1);
    }

    @Test(expectedExceptions = AssertionError.class)
    public void waitUntilAttributeContains() {
        driverThreadLocal.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilAttributeContains(By.className("buttons_AeoN"), "disabled", "true");
    }

    @Test(expectedExceptions = AssertionError.class)
    public void waitUntilElementTextToBe() {
        driverThreadLocal.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilElementTextToBe(By.className("buttons_AeoN"), "disabled");
    }

    @Test(expectedExceptions = AssertionError.class)
    public void waitUntilElementToBeSelected() {
        driverThreadLocal.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilElementToBeSelected(By.className("buttons_AeoN"));
    }

    @Test(expectedExceptions = AssertionError.class)
    public void waitUntilPresenceOfAllElementsLocatedBy() {
        driverThreadLocal.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilPresenceOfAllElementsLocatedBy(By.className("buttons_AeoN_doesntEXIST"));
    }

    @Test
    public void waitUntilNumberOfElementsToBe1() {
        driverThreadLocal.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilNumberOfElementsToBe(By.className("buttons_AeoN"), 1);
    }

    @Test
    public void waitUntilNumberOfElementsToBeLessThan1() {
        driverThreadLocal.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilNumberOfElementsToBeLessThan(By.className("buttons_AeoN"), 2);
    }

    @Test
    public void waitUntilNumberOfElementsToBeMoreThan1() {
        driverThreadLocal.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilNumberOfElementsToBeMoreThan(By.className("buttons_AeoN"), 0);
    }

    @Test
    public void waitUntilAttributeContains1() {
        driverThreadLocal.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilAttributeContains(By.className("buttons_AeoN"), "class", "buttons_AeoN");
    }

    @Test
    public void waitUntilElementTextToBe1() {
        driverThreadLocal.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilElementTextToBe(By.className("buttons_AeoN"), "⚡ Upgrade Now ⚡");
    }

    //    @Test
    public void waitUntilElementToBeSelected1() {
        driverThreadLocal.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilElementToBeSelected(By.className("buttons_AeoN"));
    }

    @Test
    public void waitUntilPresenceOfAllElementsLocatedBy1() {
        driverThreadLocal.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilPresenceOfAllElementsLocatedBy(By.className("buttons_AeoN"));
    }
}
