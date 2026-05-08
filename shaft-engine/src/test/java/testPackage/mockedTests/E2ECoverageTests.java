package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

/**
 * End-to-end browser coverage tests.
 *
 * <p>Covers the following previously-uncovered production classes using local HTML pages
 * loaded via {@code SHAFT.Properties.paths.testData()} URLs:
 * <ul>
 *   <li>{@link com.shaft.validation.internal.TextDirectionValidationsBuilder}</li>
 *   <li>{@link com.shaft.validation.internal.TextLanguageValidationsBuilder}</li>
 *   <li>{@link com.shaft.validation.internal.WebDriverElementValidationsBuilder}</li>
 *   <li>{@link com.shaft.validation.internal.WebDriverBrowserValidationsBuilder}</li>
 *   <li>{@link com.shaft.validation.internal.ValidationsHelper2} (element assertion paths)</li>
 * </ul>
 *
 * <p>Tests load the bundled {@code coverageTestPage.html} file so no internet
 * access is required.  The tests are designed to run with a headless Chrome browser
 * (the same setup used by the project CI in {@code e2eLocalTests.yml}).
 */
public class E2ECoverageTests {

    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    // Locators for the coverage test page
    private static final By VISIBLE_EL = By.id("visibleEl");
    private static final By HIDDEN_EL = By.id("hiddenEl");
    private static final By ENABLED_INPUT = By.id("enabledInput");
    private static final By DISABLED_INPUT = By.id("disabledInput");
    private static final By CHECKBOX_CHECKED = By.id("checkboxChecked");
    private static final By CHECKBOX_UNCHECKED = By.id("checkboxUnchecked");
    private static final By LTR_TEXT = By.id("ltrText");
    private static final By RTL_TEXT = By.id("rtlText");
    private static final By ENGLISH_TEXT = By.id("englishText");

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL(
                SHAFT.Properties.paths.testData() + "coverageTestPage.html");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        if (driver.get() != null) {
            driver.get().quit();
            driver.remove();
        }
    }

    // ─── WebDriverElementValidationsBuilder ──────────────────────────────────

    @Test(description = "exists: visible element should exist")
    public void elementExistsShouldPass() {
        driver.get().assertThat().element(VISIBLE_EL).exists().perform();
    }

    @Test(description = "doesNotExist: non-existent element should not exist")
    public void elementDoesNotExistShouldPass() {
        driver.get().assertThat().element(By.id("nonExistentElement")).doesNotExist().perform();
    }

    @Test(description = "isVisible: visible element should be visible")
    public void elementIsVisibleShouldPass() {
        driver.get().assertThat().element(VISIBLE_EL).isVisible().perform();
    }

    @Test(description = "isHidden: hidden element (display:none) should be hidden")
    public void elementIsHiddenShouldPass() {
        driver.get().assertThat().element(HIDDEN_EL).isHidden().perform();
    }

    @Test(description = "isEnabled: enabled input should be enabled")
    public void elementIsEnabledShouldPass() {
        driver.get().assertThat().element(ENABLED_INPUT).isEnabled().perform();
    }

    @Test(description = "isDisabled: disabled input should be disabled")
    public void elementIsDisabledShouldPass() {
        driver.get().assertThat().element(DISABLED_INPUT).isDisabled().perform();
    }

    @Test(description = "isChecked: checked checkbox should be checked")
    public void elementIsCheckedShouldPass() {
        driver.get().assertThat().element(CHECKBOX_CHECKED).isChecked().perform();
    }

    @Test(description = "isNotChecked: unchecked checkbox should not be checked")
    public void elementIsNotCheckedShouldPass() {
        driver.get().assertThat().element(CHECKBOX_UNCHECKED).isNotChecked().perform();
    }

    @Test(description = "isSelected: selected option in select element")
    public void elementIsSelectedShouldPass() {
        driver.get().assertThat()
                .element(By.cssSelector("#selectEl option[selected]")).isSelected().perform();
    }

    @Test(description = "isNotSelected: unselected option should not be selected")
    public void elementIsNotSelectedShouldPass() {
        driver.get().assertThat()
                .element(By.cssSelector("#selectEl option:not([selected])")).isNotSelected().perform();
    }

    @Test(description = "text isEqualTo: element text equals expected value")
    public void elementTextIsEqualToShouldPass() {
        driver.get().assertThat().element(VISIBLE_EL).text().isEqualTo("I am visible").perform();
    }

    @Test(description = "text contains: element text contains partial string")
    public void elementTextContainsShouldPass() {
        driver.get().assertThat().element(VISIBLE_EL).text().contains("visible").perform();
    }

    @Test(description = "textTrimmed: trimmed text comparison")
    public void elementTextTrimmedContainsShouldPass() {
        driver.get().assertThat().element(VISIBLE_EL).textTrimmed().contains("visible").perform();
    }

    @Test(description = "attribute: element attribute value matches")
    public void elementAttributeShouldPass() {
        driver.get().assertThat().element(ENABLED_INPUT).attribute("type").isEqualTo("text").perform();
    }

    @Test(description = "domProperty: element domProperty value matches")
    public void elementDomPropertyShouldPass() {
        driver.get().assertThat().element(ENABLED_INPUT).domProperty("value").isEqualTo("hello").perform();
    }

    @Test(description = "domAttribute: element domAttribute value matches")
    public void elementDomAttributeShouldPass() {
        driver.get().assertThat().element(ENABLED_INPUT).domAttribute("type").isEqualTo("text").perform();
    }

    @Test(description = "cssProperty: element CSS color property is accessible")
    public void elementCssPropertyShouldPass() {
        driver.get().assertThat().element(VISIBLE_EL).cssProperty("color").contains("").perform();
    }

    // ─── WebDriverBrowserValidationsBuilder ──────────────────────────────────

    @Test(description = "browser title: page title contains expected text")
    public void browserTitleContainsShouldPass() {
        driver.get().assertThat().browser().title().contains("Coverage Test Page").perform();
    }

    @Test(description = "browser url: URL contains test page file name")
    public void browserUrlContainsShouldPass() {
        driver.get().assertThat().browser().url().contains("coverageTestPage").perform();
    }

    @Test(description = "browser attribute url: URL attribute contains expected text")
    public void browserAttributeUrlShouldPass() {
        driver.get().assertThat().browser().attribute("url").contains("coverageTestPage").perform();
    }

    @Test(description = "browser attribute title: title attribute contains expected text")
    public void browserAttributeTitleShouldPass() {
        driver.get().assertThat().browser().attribute("title").contains("Coverage Test Page").perform();
    }

    // ─── TextDirectionValidationsBuilder ─────────────────────────────────────

    @Test(description = "direction isLeftToRight: element with dir=ltr")
    public void elementTextDirectionIsLeftToRightShouldPass() {
        driver.get().element().assertThat(LTR_TEXT).text().direction().isLeftToRight().perform();
    }

    @Test(description = "direction isRightToLeft: element with dir=rtl")
    public void elementTextDirectionIsRightToLeftShouldPass() {
        driver.get().element().assertThat(RTL_TEXT).text().direction().isRightToLeft().perform();
    }

    @Test(description = "direction is(LTR enum): element with dir=ltr")
    public void elementTextDirectionIsLtrEnumShouldPass() {
        driver.get().element().assertThat(LTR_TEXT).text()
                .direction().is(ValidationEnums.TextDirection.LTR).perform();
    }

    @Test(description = "direction is(RTL enum): element with dir=rtl")
    public void elementTextDirectionIsRtlEnumShouldPass() {
        driver.get().element().assertThat(RTL_TEXT).text()
                .direction().is(ValidationEnums.TextDirection.RTL).perform();
    }

    @Test(description = "direction isNot(RTL): LTR element should not be RTL")
    public void elementTextDirectionIsNotRtlShouldPass() {
        driver.get().element().assertThat(LTR_TEXT).text()
                .direction().isNot(ValidationEnums.TextDirection.RTL).perform();
    }

    @Test(description = "alignment isLeftToRight: element with dir=ltr")
    public void elementTextAlignmentIsLeftToRightShouldPass() {
        driver.get().element().assertThat(LTR_TEXT).text().alignment().isLeftToRight().perform();
    }

    @Test(description = "isArabic: RTL element with Arabic text")
    public void elementTextIsArabicShouldPass() {
        driver.get().element().assertThat(RTL_TEXT).text().isArabic().perform();
    }

    // ─── TextLanguageValidationsBuilder ──────────────────────────────────────

    @Test(description = "language is ENGLISH: element with English text")
    public void elementTextLanguageIsEnglishShouldPass() {
        driver.get().element().assertThat(ENGLISH_TEXT).text()
                .language().is(ValidationEnums.TextLanguage.ENGLISH).perform();
    }

    @Test(description = "language is ARABIC: element with Arabic text")
    public void elementTextLanguageIsArabicShouldPass() {
        driver.get().element().assertThat(RTL_TEXT).text()
                .language().is(ValidationEnums.TextLanguage.ARABIC).perform();
    }

    @Test(description = "language is 'en': English text element")
    public void elementTextLanguageIsEnCodeShouldPass() {
        driver.get().element().assertThat(ENGLISH_TEXT).text().language().is("en").perform();
    }

    @Test(description = "language is 'ar': Arabic text element")
    public void elementTextLanguageIsArCodeShouldPass() {
        driver.get().element().assertThat(RTL_TEXT).text().language().is("ar").perform();
    }

    @Test(description = "language isNot ARABIC: English text element")
    public void elementTextLanguageIsNotArabicShouldPass() {
        driver.get().element().assertThat(ENGLISH_TEXT).text()
                .language().isNot(ValidationEnums.TextLanguage.ARABIC).perform();
    }

    @Test(description = "language isNot 'ar': English text element")
    public void elementTextLanguageIsNotArCodeShouldPass() {
        driver.get().element().assertThat(ENGLISH_TEXT).text().language().isNot("ar").perform();
    }

    // ─── Browser text language and direction ─────────────────────────────────

    @Test(description = "browser text direction: LTR page should be LTR")
    public void browserTextDirectionIsLeftToRightShouldPass() {
        driver.get().browser().assertThat().text().direction().isLeftToRight().perform();
    }

    // ─── ValidationsHelper2 via Validations facade ───────────────────────────

    @Test(description = "verifyThat element exists via Validations facade")
    public void verifyThatElementExistsShouldPass() {
        Validations.verifyThat().element(driver.get().getDriver(), VISIBLE_EL).exists().perform();
    }

    @Test(description = "verifyThat element isVisible via Validations facade")
    public void verifyThatElementIsVisibleShouldPass() {
        Validations.verifyThat().element(driver.get().getDriver(), VISIBLE_EL).isVisible().perform();
    }

    @Test(description = "assertThat element with custom report message")
    public void assertThatElementWithCustomMessageShouldPass() {
        Validations.assertThat().element(driver.get().getDriver(), VISIBLE_EL).isVisible()
                .withCustomReportMessage("Visible element must be visible").perform();
    }

    @Test(description = "assertThat browser title via Validations facade")
    public void assertThatBrowserTitleContainsShouldPass() {
        Validations.assertThat().browser(driver.get().getDriver()).title()
                .contains("Coverage Test Page").perform();
    }

    @Test(description = "assertThat element text via Validations facade")
    public void assertThatElementTextShouldPass() {
        Validations.assertThat().element(driver.get().getDriver(), VISIBLE_EL)
                .text().isEqualTo("I am visible").perform();
    }
}
