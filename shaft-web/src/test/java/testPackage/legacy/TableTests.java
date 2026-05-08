package testPackage.legacy;

import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.Test;
import testPackage.Tests;

@Test public class TableTests extends Tests {

    public void checkContactNameForErnstHandelInAustria(){
        var company = "Ernst Handel";
        var country = "Austria";
        var contactName = "Roland Mendel";

        navigateToW3SchoolsTables();
        String contactNameText = getContactNameUsingCompanyAndCountry(company, country);
        Assert.assertEquals(contactNameText, contactName);
    }

    public void checkContactNameForAlfredsFutterkisteInGermany(){
        var company = "Alfreds Futterkiste";
        var country = "Germany";
        var contactName = "Maria Anders";

        navigateToW3SchoolsTables();
        String contactNameText = getContactNameUsingCompanyAndCountry(company, country);
        Assert.assertEquals(contactNameText, contactName);
    }

    private void navigateToW3SchoolsTables(){
        driver.get().browser().navigateToURL("https://www.w3schools.com/html/html_tables.asp");
    }

    private String getContactNameUsingCompanyAndCountry(String company, String country){
        By contactName = By.xpath("//tr[contains(.,'"+company+"')][contains(.,'"+country+"')]/td[2]");
        return driver.get().element().get().text(contactName);
    }
}