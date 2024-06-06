package testPackage.legacy;

import org.openqa.selenium.By;
import org.testng.annotations.Test;
import testPackage.Tests;

public class WaitActionsTests extends Tests {

    @Test(expectedExceptions = AssertionError.class)
    public void waitUntilNumberOfElementsToBe() {
        driver.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilNumberOfElementsToBe(By.className("buttons_AeoN"), 2);
    }

    @Test(expectedExceptions = AssertionError.class)
    public void waitUntilNumberOfElementsToBeLessThan() {
        driver.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilNumberOfElementsToBeLessThan(By.className("buttons_AeoN"), 0);
    }

    @Test(expectedExceptions = AssertionError.class)
    public void waitUntilNumberOfElementsToBeMoreThan() {
        driver.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilNumberOfElementsToBeMoreThan(By.className("buttons_AeoN"), 5);
    }

    @Test(expectedExceptions = AssertionError.class)
    public void waitUntilAttributeContains() {
        driver.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilAttributeContains(By.className("buttons_AeoN"), "disabled", "true");
    }

    @Test(expectedExceptions = AssertionError.class)
    public void waitUntilElementTextToBe() {
        driver.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilElementTextToBe(By.className("buttons_AeoN"), "disabled");
    }

    @Test(expectedExceptions = AssertionError.class)
    public void waitUntilElementToBeSelected() {
        driver.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilElementToBeSelected(By.className("buttons_AeoN"));
    }

    @Test(expectedExceptions = AssertionError.class)
    public void waitUntilPresenceOfAllElementsLocatedBy() {
        driver.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilPresenceOfAllElementsLocatedBy(By.className("buttons_AeoN_doesntEXIST"));
    }

    @Test
    public void waitUntilNumberOfElementsToBe1() {
        driver.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilNumberOfElementsToBe(By.className("buttons_AeoN"), 4);
    }

    @Test
    public void waitUntilNumberOfElementsToBeLessThan1() {
        driver.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilNumberOfElementsToBeLessThan(By.className("buttons_AeoN"), 5);
    }

    @Test
    public void waitUntilNumberOfElementsToBeMoreThan1() {
        driver.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilNumberOfElementsToBeMoreThan(By.className("buttons_AeoN"), 0);
    }

    @Test
    public void waitUntilAttributeContains1() {
        driver.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilAttributeContains(By.className("buttons_AeoN"), "class", "buttons_AeoN");
    }

    @Test
    public void waitUntilElementTextToBe1() {
        driver.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilElementTextToBe(By.className("buttons_AeoN"), "⚡ Upgrade Now ⚡");
    }

    //    @Test
    public void waitUntilElementToBeSelected1() {
        driver.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilElementToBeSelected(By.className("buttons_AeoN"));
    }

    @Test
    public void waitUntilPresenceOfAllElementsLocatedBy1() {
        driver.get().browser().navigateToURL("https://shafthq.github.io/")
                .and().element().waitUntilPresenceOfAllElementsLocatedBy(By.className("buttons_AeoN"));
    }
}
