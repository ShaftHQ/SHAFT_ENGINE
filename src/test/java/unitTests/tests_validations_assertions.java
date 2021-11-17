package unitTests;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.tools.io.ReportManagerHelper;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionComparisonType;
import com.shaft.validation.Assertions.AssertionType;
import com.shaft.validation.Assertions.ComparativeRelationType;
import com.shaft.validation.Verifications;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import poms.GoogleSearch;

public class tests_validations_assertions {
    private WebDriver driver;

    @Test(description = "Assert that assertEquals works as expected when the two values are equal.")
    public void assertEquals_true_expectedToPass() {
        Verifications.verifyEquals(1, 1);
        Assertions.assertEquals(1, 1);
    }

    @Test
    public void assertEquals_true_expectedToPass__() {
        Assertions.assertEquals(1, 1, AssertionComparisonType.EQUALS, AssertionType.POSITIVE);
    }

    @Test
    public void assertEquals_longCharacters() {
        Verifications.verifyEquals(
                "jLvZpYHnzi7J2LioTBKIRg22r9Fz2qKoPbhnPk8vVo2IKvtdbZWVzWBvQ9fPhDO9Mg290JmiviQBiBTd9IEqoqvZFSCrL2J8wOIDhJLt0lg8hYGGDgkaPWhwMtjJN0jgxLikgXK8i5TAhlrCU8sVFTbGYtCdcA104FEZBR8UtaFUYyp1bjNOYBlH9BOk5k3CKPplDYMLCxsDNqbTCSSzEGAA88DbImhBiKRlC6amHQj3GS6BsYmBRpMj2YUmtJhstEH8flwdX7VWU4QT9Ym1IU4HTslNjsnmoagkqit9D2mtaWuPhZPaslBT7P0liynOCOxe9FFcGKydmAz3UV1PhcKOA7TZy8F5xKd2R3mcyYzeFT5pec7lDfMBrwXTeSoGs8FZtWMmQJYlKLqFRhJxzaJhsTWIj0HENyyrdBPDqy4scf0pp4HrWtn5GVTWxjmEzxuGEx9g9H9ImIphLXpCBIrpT5Jm8jhhxjm9GUtaFGPrVALKAKJk1",
                "jLvZpYHnzi7J2LioTBKIRg22r9Fz2qKoPbhnPk8vVo2IKvtdbZWVzWBvQ9fPhDO9Mg290JmiviQBiBTd9IEqoqvZFSCrL2J8wOIDhJLt0lg8hYGGDgkaPWhwMtjJN0jgxLikgXK8i5TAhlrCU8sVFTbGYtCdcA104FEZBR8UtaFUYyp1bjNOYBlH9BOk5k3CKPplDYMLCxsDNqbTCSSzEGAA88DbImhBiKRlC6amHQj3GS6BsYmBRpMj2YUmtJhstEH8flwdX7VWU4QT9Ym1IU4HTslNjsnmoagkqit9D2mtaWuPhZPaslBT7P0liynOCOxe9FFcGKydmAz3UV1PhcKOA7TZy8F5xKd2R3mcyYzeFT5pec7lDfMBrwXTeSoGs8FZtWMmQJYlKLqFRhJxzaJhsTWIj0HENyyrdBPDqy4scf0pp4HrWtn5GVTWxjmEzxuGEx9g9H9ImIphLXpCBIrpT5Jm8jhhxjm9GUtaFGPrVALKAKJk1",
                Verifications.VerificationComparisonType.EQUALS, Verifications.VerificationType.POSITIVE);
        Assertions.assertEquals(
                "jLvZpYHnzi7J2LioTBKIRg22r9Fz2qKoPbhnPk8vVo2IKvtdbZWVzWBvQ9fPhDO9Mg290JmiviQBiBTd9IEqoqvZFSCrL2J8wOIDhJLt0lg8hYGGDgkaPWhwMtjJN0jgxLikgXK8i5TAhlrCU8sVFTbGYtCdcA104FEZBR8UtaFUYyp1bjNOYBlH9BOk5k3CKPplDYMLCxsDNqbTCSSzEGAA88DbImhBiKRlC6amHQj3GS6BsYmBRpMj2YUmtJhstEH8flwdX7VWU4QT9Ym1IU4HTslNjsnmoagkqit9D2mtaWuPhZPaslBT7P0liynOCOxe9FFcGKydmAz3UV1PhcKOA7TZy8F5xKd2R3mcyYzeFT5pec7lDfMBrwXTeSoGs8FZtWMmQJYlKLqFRhJxzaJhsTWIj0HENyyrdBPDqy4scf0pp4HrWtn5GVTWxjmEzxuGEx9g9H9ImIphLXpCBIrpT5Jm8jhhxjm9GUtaFGPrVALKAKJk1",
                "jLvZpYHnzi7J2LioTBKIRg22r9Fz2qKoPbhnPk8vVo2IKvtdbZWVzWBvQ9fPhDO9Mg290JmiviQBiBTd9IEqoqvZFSCrL2J8wOIDhJLt0lg8hYGGDgkaPWhwMtjJN0jgxLikgXK8i5TAhlrCU8sVFTbGYtCdcA104FEZBR8UtaFUYyp1bjNOYBlH9BOk5k3CKPplDYMLCxsDNqbTCSSzEGAA88DbImhBiKRlC6amHQj3GS6BsYmBRpMj2YUmtJhstEH8flwdX7VWU4QT9Ym1IU4HTslNjsnmoagkqit9D2mtaWuPhZPaslBT7P0liynOCOxe9FFcGKydmAz3UV1PhcKOA7TZy8F5xKd2R3mcyYzeFT5pec7lDfMBrwXTeSoGs8FZtWMmQJYlKLqFRhJxzaJhsTWIj0HENyyrdBPDqy4scf0pp4HrWtn5GVTWxjmEzxuGEx9g9H9ImIphLXpCBIrpT5Jm8jhhxjm9GUtaFGPrVALKAKJk1");
    }

    @Test
    public void assertEquals_longCharacters_Discreet() {
        boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
        ReportManagerHelper.setDiscreteLogging(true);
        Assertions.assertEquals(
                "1LH5pROcDBjjQk0t0mCr0lXuzQu9lcGj28kY8R1H81X67eVAQJgWQhILiRWVPUgZ6uCtK5cnBkF55Jr9vYngjGO1Iyf0Mktv6lruDlL9T8MAUPcUZJGHZtji6nIeadujDSNmWMj5d6C8zlFqw0CRqBU0hO5adIasHgBSNoLotAjce3NGXoDwAlp3rYreeV16VIyZXROQY",
                "1LH5pROcDBjjQk0t0mCr0lXuzQu9lcGj28kY8R1H81X67eVAQJgWQhILiRWVPUgZ6uCtK5cnBkF55Jr9vYngjGO1Iyf0Mktv6lruDlL9T8MAUPcUZJGHZtji6nIeadujDSNmWMj5d6C8zlFqw0CRqBU0hO5adIasHgBSNoLotAjce3NGXoDwAlp3rYreeV16VIyZXROQY");
        ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
    }

    @Test
    public void assertEquals_true_expectedToFail() {
        try {
            Assertions.assertEquals(1, 2);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test
    public void assertEquals_false_expectedToPass() {
        Assertions.assertEquals(1, 2, AssertionComparisonType.EQUALS, AssertionType.NEGATIVE);
    }

    @Test
    public void assertEquals_false_expectedToFail() {
        try {
            Assertions.assertEquals(1, 1, AssertionComparisonType.EQUALS, AssertionType.NEGATIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test
    public void assertNull_true_expectedToPass() {
        Assertions.assertNull(null, AssertionType.POSITIVE);
    }

    @Test
    public void assertNull_true_expectedToFail() {
        try {
            Assertions.assertNull(1, AssertionType.POSITIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test
    public void assertNull_false_expectedToPass() {
        Assertions.assertNull(1, AssertionType.NEGATIVE);
    }

    @Test
    public void assertNull_false_expectedToFail() {
        try {
            Assertions.assertNull(null, AssertionType.NEGATIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_true_greaterThan_expectedToPass() {
        Assertions.assertComparativeRelation(1, 2, ComparativeRelationType.GREATER_THAN_OR_EQUALS,
                AssertionType.POSITIVE);
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_true_equals_expectedToPass() {
        Assertions.assertComparativeRelation(1, 1, ComparativeRelationType.GREATER_THAN_OR_EQUALS,
                AssertionType.POSITIVE);
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_true_greaterThan_expectedToFail() {
        try {
            Assertions.assertComparativeRelation(2, 1, ComparativeRelationType.GREATER_THAN_OR_EQUALS,
                    AssertionType.POSITIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_true_equals_expectedToFail() {
        try {
            Assertions.assertComparativeRelation(1, 0, ComparativeRelationType.GREATER_THAN_OR_EQUALS,
                    AssertionType.POSITIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_false_greaterThan_expectedToPass() {
        Assertions.assertComparativeRelation(2, 1, ComparativeRelationType.GREATER_THAN_OR_EQUALS,
                AssertionType.NEGATIVE);
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_false_equals_expectedToPass() {
        Assertions.assertComparativeRelation(2, 1, ComparativeRelationType.GREATER_THAN_OR_EQUALS,
                AssertionType.NEGATIVE);
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_false_greaterThan_expectedToFail() {
        try {
            Assertions.assertComparativeRelation(1, 2, ComparativeRelationType.GREATER_THAN_OR_EQUALS,
                    AssertionType.NEGATIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_false_equals_expectedToFail() {
        try {
            Assertions.assertComparativeRelation(1, 1, ComparativeRelationType.GREATER_THAN_OR_EQUALS,
                    AssertionType.NEGATIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test(groups = {"WebBased"})
    public void assertElementExists_true_expectedToPass() {
        Assertions.assertElementExists(driver, GoogleSearch.googleLogo_image, AssertionType.POSITIVE);
    }

    @Test(groups = {"WebBased"})
    public void assertElementExists_true_expectedToFail() {
        try {
            Assertions.assertElementExists(driver, By.id("fakeElement"), AssertionType.POSITIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test(groups = {"WebBased"})
    public void assertElementExists_false_expectedToPass() {
        Assertions.assertElementExists(driver, By.id("fakeElement"), AssertionType.NEGATIVE);
    }

    @Test(groups = {"WebBased"})
    public void assertElementExists_false_expectedToFail() {
        try {
            Assertions.assertElementExists(driver, By.id("hplogo"), AssertionType.NEGATIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test(groups = {"WebBased"})
    public void assertElementExists_true_multipleElementsFound_expectedToFail() {
        try {
            Assertions.assertElementExists(driver, By.xpath("//div"), AssertionType.POSITIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test(groups = {"WebBased"})
    public void assertElementExists_false_multipleElementsFound_expectedToFail() {
        try {
            Assertions.assertElementExists(driver, By.xpath("//div"), AssertionType.NEGATIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_type_true_literalComparison_expectedToPass() {
        ElementActions.type(driver, By.name("q"), "Automation!@#$%^&*()_+{}[]\\';/.,");
        Assertions.assertElementAttribute(driver, By.name("q"), Assertions.ElementAttributeType.TEXT, "Automation!@#$%^&*()_+{}[]\\';/.,",
                AssertionComparisonType.EQUALS, AssertionType.POSITIVE);
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_true_literalComparison_expectedToPass() {
        ElementActions.type(driver, By.name("q"), "Automation!@#$%^&*()_+{}[]\\';/.,");
        Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation!@#$%^&*()_+{}[]\\';/.,",
                AssertionComparisonType.EQUALS, AssertionType.POSITIVE);
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_true_literalComparison_expectedToFail() {
        ElementActions.type(driver, By.name("q"), "Automation!@#$%^&*()_+{}[]\\';/.,");
        try {
            Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation",
                    AssertionComparisonType.EQUALS, AssertionType.POSITIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_true_regexComparison_expectedToPass() {
        ElementActions.type(driver, By.name("q"), "Automation123");
        Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation.*", AssertionComparisonType.MATCHES,
                AssertionType.POSITIVE);
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_true_regexComparison_expectedToFail() {
        ElementActions.type(driver, By.name("q"), "Automation123");
        try {
            Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation",
                    AssertionComparisonType.MATCHES, AssertionType.POSITIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_true_containsComparison_expectedToPass() {
        ElementActions.type(driver, By.name("q"), "Automation123");
        Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation", AssertionComparisonType.CONTAINS,
                AssertionType.POSITIVE);
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_true_containsComparison_expectedToFail() {
        ElementActions.type(driver, By.name("q"), "Automation123");
        try {
            Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation1234",
                    AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_true_caseInsensitiveComparison_expectedToPass() {
        ElementActions.type(driver, By.name("q"), "AUTOMATION");
        Assertions.assertElementAttribute(driver, By.name("q"), "text", "AutomaTion",
                AssertionComparisonType.CASE_INSENSITIVE, AssertionType.POSITIVE);
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_true_caseInsensitiveComparison_expectedToFail() {
        ElementActions.type(driver, By.name("q"), "AUTOMATION");
        try {
            Assertions.assertElementAttribute(driver, By.name("q"), "text", "AutomaTion123",
                    AssertionComparisonType.CASE_INSENSITIVE, AssertionType.POSITIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_false_literalComparison_expectedToPass() {
        ElementActions.type(driver, By.name("q"), "Automation!@#$%^&*()_+{}[]\\';/.,");
        Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation", AssertionComparisonType.EQUALS,
                AssertionType.NEGATIVE);
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_false_literalComparison_expectedToFail() {
        ElementActions.type(driver, By.name("q"), "Automation!@#$%^&*()_+{}[]\\';/.,");
        try {
            Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation!@#$%^&*()_+{}[]\\\\';/.,",
                    AssertionComparisonType.EQUALS, AssertionType.NEGATIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_false_regexComparison_expectedToPass() {
        ElementActions.type(driver, By.name("q"), "Automation123");
        Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation", AssertionComparisonType.MATCHES,
                AssertionType.NEGATIVE);
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_false_regexComparison_expectedToFail() {
        ElementActions.type(driver, By.name("q"), "Automation123");
        try {
            Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation.*",
                    AssertionComparisonType.MATCHES, AssertionType.NEGATIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_false_containsComparison_expectedToPass() {
        ElementActions.type(driver, By.name("q"), "Automation123");
        Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation1234",
                AssertionComparisonType.CONTAINS, AssertionType.NEGATIVE);
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_false_containsComparison_expectedToFail() {
        ElementActions.type(driver, By.name("q"), "Automation123");
        try {
            Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation",
                    AssertionComparisonType.CONTAINS, AssertionType.NEGATIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_false_caseInsensitiveComparison_expectedToPass() {
        ElementActions.type(driver, By.name("q"), "AUTOMATION");
        Assertions.assertElementAttribute(driver, By.name("q"), "text", "AutomaTion123",
                AssertionComparisonType.CASE_INSENSITIVE, AssertionType.NEGATIVE);
    }

    @Test(groups = {"WebBased"})
    public void assertElementAttribute_false_caseInsensitiveComparison_expectedToFail() {
        ElementActions.type(driver, By.name("q"), "AUTOMATION");
        try {
            Assertions.assertElementAttribute(driver, By.name("q"), "text", "AutomaTion",
                    AssertionComparisonType.CASE_INSENSITIVE, AssertionType.NEGATIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test
    public void assertFileExists_true_expectedToPass() {
        Assertions.assertFileExists("src/main/java/com/shaft/gui/element/", "ElementActions.java", 1,
                AssertionType.POSITIVE);
    }

    @Test
    public void assertFileExists_true_expectedToFail() {
        try {
            Assertions.assertFileExists("src/main/java/com/shaft/gui/element/", "ElementActions.java_fail", 1,
                    AssertionType.POSITIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test
    public void assertFileExists_false_expectedToPass() {
        Assertions.assertFileExists("src/main/java/com/shaft/gui/element/", "ElementActions.java_fail", 1,
                AssertionType.NEGATIVE);
    }

    @Test
    public void assertFileExists_false_expectedToFail() {
        try {
            Assertions.assertFileExists("src/main/java/com/shaft/gui/element/", "ElementActions.java", 1,
                    AssertionType.NEGATIVE);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @AfterMethod(onlyForGroups = {"WebBased"})
    public void afterMethod() {
        BrowserActions.closeCurrentWindow(driver);
    }

    @BeforeMethod(onlyForGroups = {"WebBased"})
    public void beforeMethod() {
        driver = BrowserFactory.getBrowser();
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");
    }
}