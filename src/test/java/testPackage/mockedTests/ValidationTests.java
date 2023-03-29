package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class ValidationTests {
    private final By locator = SHAFT.GUI.Locator.hasTagName("input").build();
    private SHAFT.GUI.WebDriver driver;
    private String defaultElementIdentificationTimeout;

    @Test(description = "Assert that assertEquals works as expected when the two values are equal.")
    public void assertEquals_true_expectedToPass() {
        Validations.verifyThat().number(1).isEqualTo(1).perform();
        Validations.assertThat().number(1).isEqualTo(1);
    }

    @Test
    public void assertEquals_true_expectedToPass__() {
        Validations.assertThat().number(1).isEqualTo(1).perform();
    }

    @Test
    public void assertEquals_longCharacters() {
        Validations.verifyThat().object("jLvZpYHnzi7J2LioTBKIRg22r9Fz2qKoPbhnPk8vVo2IKvtdbZWVzWBvQ9fPhDO9Mg290JmiviQBiBTd9IEqoqvZFSCrL2J8wOIDhJLt0lg8hYGGDgkaPWhwMtjJN0jgxLikgXK8i5TAhlrCU8sVFTbGYtCdcA104FEZBR8UtaFUYyp1bjNOYBlH9BOk5k3CKPplDYMLCxsDNqbTCSSzEGAA88DbImhBiKRlC6amHQj3GS6BsYmBRpMj2YUmtJhstEH8flwdX7VWU4QT9Ym1IU4HTslNjsnmoagkqit9D2mtaWuPhZPaslBT7P0liynOCOxe9FFcGKydmAz3UV1PhcKOA7TZy8F5xKd2R3mcyYzeFT5pec7lDfMBrwXTeSoGs8FZtWMmQJYlKLqFRhJxzaJhsTWIj0HENyyrdBPDqy4scf0pp4HrWtn5GVTWxjmEzxuGEx9g9H9ImIphLXpCBIrpT5Jm8jhhxjm9GUtaFGPrVALKAKJk1")
                .isEqualTo("jLvZpYHnzi7J2LioTBKIRg22r9Fz2qKoPbhnPk8vVo2IKvtdbZWVzWBvQ9fPhDO9Mg290JmiviQBiBTd9IEqoqvZFSCrL2J8wOIDhJLt0lg8hYGGDgkaPWhwMtjJN0jgxLikgXK8i5TAhlrCU8sVFTbGYtCdcA104FEZBR8UtaFUYyp1bjNOYBlH9BOk5k3CKPplDYMLCxsDNqbTCSSzEGAA88DbImhBiKRlC6amHQj3GS6BsYmBRpMj2YUmtJhstEH8flwdX7VWU4QT9Ym1IU4HTslNjsnmoagkqit9D2mtaWuPhZPaslBT7P0liynOCOxe9FFcGKydmAz3UV1PhcKOA7TZy8F5xKd2R3mcyYzeFT5pec7lDfMBrwXTeSoGs8FZtWMmQJYlKLqFRhJxzaJhsTWIj0HENyyrdBPDqy4scf0pp4HrWtn5GVTWxjmEzxuGEx9g9H9ImIphLXpCBIrpT5Jm8jhhxjm9GUtaFGPrVALKAKJk1")
                .perform();
        Validations.assertThat().object("jLvZpYHnzi7J2LioTBKIRg22r9Fz2qKoPbhnPk8vVo2IKvtdbZWVzWBvQ9fPhDO9Mg290JmiviQBiBTd9IEqoqvZFSCrL2J8wOIDhJLt0lg8hYGGDgkaPWhwMtjJN0jgxLikgXK8i5TAhlrCU8sVFTbGYtCdcA104FEZBR8UtaFUYyp1bjNOYBlH9BOk5k3CKPplDYMLCxsDNqbTCSSzEGAA88DbImhBiKRlC6amHQj3GS6BsYmBRpMj2YUmtJhstEH8flwdX7VWU4QT9Ym1IU4HTslNjsnmoagkqit9D2mtaWuPhZPaslBT7P0liynOCOxe9FFcGKydmAz3UV1PhcKOA7TZy8F5xKd2R3mcyYzeFT5pec7lDfMBrwXTeSoGs8FZtWMmQJYlKLqFRhJxzaJhsTWIj0HENyyrdBPDqy4scf0pp4HrWtn5GVTWxjmEzxuGEx9g9H9ImIphLXpCBIrpT5Jm8jhhxjm9GUtaFGPrVALKAKJk1")
                .isEqualTo("jLvZpYHnzi7J2LioTBKIRg22r9Fz2qKoPbhnPk8vVo2IKvtdbZWVzWBvQ9fPhDO9Mg290JmiviQBiBTd9IEqoqvZFSCrL2J8wOIDhJLt0lg8hYGGDgkaPWhwMtjJN0jgxLikgXK8i5TAhlrCU8sVFTbGYtCdcA104FEZBR8UtaFUYyp1bjNOYBlH9BOk5k3CKPplDYMLCxsDNqbTCSSzEGAA88DbImhBiKRlC6amHQj3GS6BsYmBRpMj2YUmtJhstEH8flwdX7VWU4QT9Ym1IU4HTslNjsnmoagkqit9D2mtaWuPhZPaslBT7P0liynOCOxe9FFcGKydmAz3UV1PhcKOA7TZy8F5xKd2R3mcyYzeFT5pec7lDfMBrwXTeSoGs8FZtWMmQJYlKLqFRhJxzaJhsTWIj0HENyyrdBPDqy4scf0pp4HrWtn5GVTWxjmEzxuGEx9g9H9ImIphLXpCBIrpT5Jm8jhhxjm9GUtaFGPrVALKAKJk1")
                .perform();
    }

    @Test
    public void assertEquals_longCharacters_Discreet() {
        boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
        ReportManagerHelper.setDiscreteLogging(true);
        Validations.assertThat().object("1LH5pROcDBjjQk0t0mCr0lXuzQu9lcGj28kY8R1H81X67eVAQJgWQhILiRWVPUgZ6uCtK5cnBkF55Jr9vYngjGO1Iyf0Mktv6lruDlL9T8MAUPcUZJGHZtji6nIeadujDSNmWMj5d6C8zlFqw0CRqBU0hO5adIasHgBSNoLotAjce3NGXoDwAlp3rYreeV16VIyZXROQY")
                .isEqualTo("1LH5pROcDBjjQk0t0mCr0lXuzQu9lcGj28kY8R1H81X67eVAQJgWQhILiRWVPUgZ6uCtK5cnBkF55Jr9vYngjGO1Iyf0Mktv6lruDlL9T8MAUPcUZJGHZtji6nIeadujDSNmWMj5d6C8zlFqw0CRqBU0hO5adIasHgBSNoLotAjce3NGXoDwAlp3rYreeV16VIyZXROQY")
                .perform();
        ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void assertEquals_true_expectedToFail() {
        Validations.assertThat().number(1).isEqualTo(2);
    }

    @Test
    public void assertEquals_false_expectedToPass() {
        Validations.assertThat().number(1).doesNotEqual(2);
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void assertEquals_false_expectedToFail() {
        Validations.assertThat().number(1).doesNotEqual(1);
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_true_greaterThan_expectedToPass() {
        Validations.assertThat().number(10).isGreaterThanOrEquals(2).perform();
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_true_equals_expectedToPass() {
        Validations.assertThat().number(1).isGreaterThanOrEquals(1).perform();
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void assertComparativeRelation_greaterThanOrEquals_true_greaterThan_expectedToFail() {
        Validations.assertThat().number(2).isGreaterThanOrEquals(1).perform();
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void assertComparativeRelation_greaterThanOrEquals_true_equals_expectedToFail() {
        Validations.assertThat().number(1).isGreaterThanOrEquals(0).perform();
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_false_greaterThan_expectedToPass() {
        Validations.assertThat().number(1).isLessThan(10).perform();
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void assertComparativeRelation_greaterThanOrEquals_false_greaterThan_expectedToFail() {
        Validations.assertThat().number(1).isLessThan(2).perform();
    }

    @Test(groups = {"WebBased"})
    public void assertElementExists_true_expectedToPass() {
        driver.element().assertThat(locator).exists().perform();
    }

    @Test(groups = {"WebBased"}, expectedExceptions = {AssertionError.class})
    public void assertElementExists_true_expectedToFail() {
        driver.element().assertThat(By.id("fakeElement")).exists().perform();
    }

    @Test(groups = {"WebBased"})
    public void assertElementExists_false_expectedToPass() {
        driver.element().assertThat(By.id("fakeElement")).doesNotExist().perform();
    }

    @Test(groups = {"WebBased"}, expectedExceptions = {AssertionError.class})
    public void assertElementExists_false_expectedToFail() {
        driver.element().assertThat(locator).doesNotExist().perform();
    }

    @Test(groups = {"WebBased"}, expectedExceptions = {AssertionError.class})
    public void assertElementExists_true_multipleElementsFound_expectedToFail() {
        driver.element().assertThat(By.xpath("//div")).exists().perform();
    }

    @Test(groups = {"WebBased"}, expectedExceptions = {AssertionError.class})
    public void assertElementExists_false_multipleElementsFound_expectedToFail() {
        driver.element().assertThat(By.xpath("//div")).doesNotExist().perform();
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_true_literalComparison_expectedToPass() {
        driver.element().type(locator, "Automation")
                .assertThat(locator).text().isEqualTo("Automation").perform();
    }

    @Test(groups = {"WebBased"}, expectedExceptions = {AssertionError.class})
    public void assertElementAttribute_true_literalComparison_expectedToFail() {
        driver.element().type(locator, "Automation");
        driver.element().assertThat(locator).text().isEqualTo("Automation123").perform();
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_true_regexComparison_expectedToPass() {
        driver.element().type(locator, "Automation123")
                .assertThat(locator).text().matchesRegex("Automation.*").perform();
    }

    @Test(groups = {"WebBased"}, expectedExceptions = {AssertionError.class})
    public void assertElementAttribute_true_regexComparison_expectedToFail() {
        driver.element().type(locator, "Automation123");
        driver.element().assertThat(locator).text().matchesRegex("Automation").perform();
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_true_containsComparison_expectedToPass() {
        driver.element().type(locator, "Automation123")
                .assertThat(locator).text().contains("Automation").perform();
    }

    @Test(groups = {"WebBased"}, expectedExceptions = {AssertionError.class})
    public void assertElementAttribute_true_containsComparison_expectedToFail() {
        driver.element().type(locator, "Automation123");
        driver.element().assertThat(locator).text().contains("Automation1234").perform();
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_true_caseInsensitiveComparison_expectedToPass() {
        driver.element().type(locator, "AUTOMATION")
                .assertThat(locator).text().equalsIgnoringCaseSensitivity("AutomaTion").perform();
    }

    @Test(groups = {"WebBased"}, expectedExceptions = {AssertionError.class})
    public void assertElementAttribute_true_caseInsensitiveComparison_expectedToFail() {
        driver.element().type(locator, "AUTOMATION");
        driver.element().assertThat(locator).text().equalsIgnoringCaseSensitivity("AutomaTion123").perform();
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_false_literalComparison_expectedToPass() {
        driver.element().type(locator, "Automation123")
                .assertThat(locator).text().doesNotEqual("Automation").perform();
    }

    @Test(groups = {"WebBased"}, expectedExceptions = {AssertionError.class})
    public void assertElementAttribute_false_literalComparison_expectedToFail() {
        driver.element().type(locator, "Automation");
        driver.element().assertThat(locator).text().doesNotEqual("Automation123").perform();
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_false_regexComparison_expectedToPass() {
        driver.element().type(locator, "Automation123")
                .assertThat(locator).text().doesNotMatchRegex("Automation").perform();
    }

    @Test(groups = {"WebBased"}, expectedExceptions = {AssertionError.class})
    public void assertElementAttribute_false_regexComparison_expectedToFail() {
        driver.element().type(locator, "Automation123");
        driver.element().assertThat(locator).text().doesNotMatchRegex("Automation.*").perform();
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_false_containsComparison_expectedToPass() {
        driver.element().type(locator, "Automation123")
                .assertThat(locator).text().doesNotContain("Automation1234").perform();
    }

    @Test(groups = {"WebBased"}, expectedExceptions = {AssertionError.class})
    public void assertElementAttribute_false_containsComparison_expectedToFail() {
        driver.element().type(locator, "Automation123");
        driver.element().assertThat(locator).text().doesNotContain("Automation").perform();
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_false_caseInsensitiveComparison_expectedToPass() {
        driver.element().type(locator, "AUTOMATION")
                .assertThat(locator).text().doesNotContain("AutomaTion123").perform();
    }

    @Test(groups = {"WebBased"}, expectedExceptions = {AssertionError.class})
    public void assertElementAttribute_false_caseInsensitiveComparison_expectedToFail() {
        driver.element().type(locator, "AUTOMATION");
        driver.element().assertThat(locator).text().doesNotEqualIgnoringCaseSensitivity("AutomaTion").perform();
    }

    @Test
    public void assertFileExists_true_expectedToPass() {
        Validations.assertThat().file("src/main/java/com/shaft/gui/element/", "ElementActions.java").exists().perform();
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void assertFileExists_true_expectedToFail() {
        Validations.assertThat().file("src/main/java/com/shaft/gui/element/", "ElementActions.java_fail").exists().perform();
    }

    @Test
    public void assertFileExists_false_expectedToPass() {
        Validations.assertThat().file("src/main/java/com/shaft/gui/element/", "ElementActions.java_fail").doesNotExist().perform();
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void assertFileExists_false_expectedToFail() {
        Validations.assertThat().file("src/main/java/com/shaft/gui/element/", "ElementActions.java").doesNotExist().perform();
    }

    @AfterMethod(onlyForGroups = {"WebBased"}, alwaysRun = true)
    public void afterMethod() {
        System.setProperty("defaultElementIdentificationTimeout", defaultElementIdentificationTimeout);
        driver.quit();
    }

    @BeforeMethod(onlyForGroups = {"WebBased"})
    public void beforeMethod() {
        defaultElementIdentificationTimeout = System.getProperty("defaultElementIdentificationTimeout");
        System.setProperty("defaultElementIdentificationTimeout", "2");
        driver = SHAFT.GUI.WebDriver.getInstance();
        String testElement = "data:text/html,<input type=\"text\"><br><br>";
        driver.browser().navigateToURL(testElement);
    }
}