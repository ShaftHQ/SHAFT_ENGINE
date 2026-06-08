package testPackage.unitTests;

import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.ValidationsHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

/**
 * Unit tests for {@link ValidationEnums.TextDirection} enum.
 * Covers enum values, {@code getValue()}, and ensures completeness of the enum definition.
 *
 * <p>Note: {@link com.shaft.validation.internal.TextDirectionValidationsBuilder} requires an
 * element/browser assertion context (obtained via {@code .text()}) and is therefore covered by
 * the mocked-browser {@code ValidationTests} suite rather than pure unit tests.
 */
public class TextDirectionValidationsBuilderUnitTest {

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    // ─── TextDirection enum values ────────────────────────────────────────────

    @Test(description = "TextDirection.LTR getValue returns 'ltr'")
    public void textDirectionLtrGetValueReturnsLtr() {
        Assert.assertEquals(ValidationEnums.TextDirection.LTR.getValue(), "ltr",
                "LTR enum value should be 'ltr'");
    }

    @Test(description = "TextDirection.RTL getValue returns 'rtl'")
    public void textDirectionRtlGetValueReturnsRtl() {
        Assert.assertEquals(ValidationEnums.TextDirection.RTL.getValue(), "rtl",
                "RTL enum value should be 'rtl'");
    }

    @Test(description = "TextDirection enum has exactly 2 constants")
    public void textDirectionEnumHasTwoConstants() {
        Assert.assertEquals(ValidationEnums.TextDirection.values().length, 2,
                "TextDirection must have exactly LTR and RTL");
    }

    @Test(description = "TextDirection.LTR and RTL are distinct")
    public void textDirectionLtrAndRtlAreDistinct() {
        Assert.assertNotEquals(ValidationEnums.TextDirection.LTR.getValue(),
                ValidationEnums.TextDirection.RTL.getValue(),
                "LTR and RTL must have different string values");
    }

    @Test(description = "TextDirection.LTR can be retrieved by name via valueOf")
    public void textDirectionLtrValueOfReturnsCorrectConstant() {
        ValidationEnums.TextDirection direction = ValidationEnums.TextDirection.valueOf("LTR");
        Assert.assertEquals(direction, ValidationEnums.TextDirection.LTR);
    }

    @Test(description = "TextDirection.RTL can be retrieved by name via valueOf")
    public void textDirectionRtlValueOfReturnsCorrectConstant() {
        ValidationEnums.TextDirection direction = ValidationEnums.TextDirection.valueOf("RTL");
        Assert.assertEquals(direction, ValidationEnums.TextDirection.RTL);
    }

    // ─── TextLanguage enum values ─────────────────────────────────────────────

    @Test(description = "TextLanguage.ARABIC language code is 'ar'")
    public void textLanguageArabicLanguageCodeIsAr() {
        Assert.assertEquals(ValidationEnums.TextLanguage.ARABIC.getLanguageCode(), "ar");
    }

    @Test(description = "TextLanguage.ENGLISH language code is 'en'")
    public void textLanguageEnglishLanguageCodeIsEn() {
        Assert.assertEquals(ValidationEnums.TextLanguage.ENGLISH.getLanguageCode(), "en");
    }

    @Test(description = "TextLanguage.SPANISH language code is 'es'")
    public void textLanguageSpanishLanguageCodeIsEs() {
        Assert.assertEquals(ValidationEnums.TextLanguage.SPANISH.getLanguageCode(), "es");
    }

    @Test(description = "TextLanguage.FRENCH language code is 'fr'")
    public void textLanguageFrenchLanguageCodeIsFr() {
        Assert.assertEquals(ValidationEnums.TextLanguage.FRENCH.getLanguageCode(), "fr");
    }

    @Test(description = "TextLanguage.GERMAN language code is 'de'")
    public void textLanguageGermanLanguageCodeIsDe() {
        Assert.assertEquals(ValidationEnums.TextLanguage.GERMAN.getLanguageCode(), "de");
    }

    @Test(description = "TextLanguage.ARABIC detection regex matches Arabic text")
    public void textLanguageArabicDetectionRegexMatchesArabicText() {
        String arabicText = "\u0645\u0631\u062d\u0628\u0627"; // "مرحبا"
        Assert.assertTrue(arabicText.matches(ValidationEnums.TextLanguage.ARABIC.getDetectionRegex()),
                "ARABIC regex should match Arabic text");
    }

    @Test(description = "TextLanguage.ARABIC detection regex does not match English-only text")
    public void textLanguageArabicDetectionRegexDoesNotMatchEnglishText() {
        Assert.assertFalse("Hello".matches(ValidationEnums.TextLanguage.ARABIC.getDetectionRegex()),
                "ARABIC regex should not match English-only text");
    }

    @Test(description = "TextLanguage.ENGLISH detection regex matches English text")
    public void textLanguageEnglishDetectionRegexMatchesEnglishText() {
        Assert.assertTrue("Hello".matches(ValidationEnums.TextLanguage.ENGLISH.getDetectionRegex()),
                "ENGLISH regex should match English text");
    }

    @Test(description = "TextLanguage enum has exactly 5 constants")
    public void textLanguageEnumHasFiveConstants() {
        Assert.assertEquals(ValidationEnums.TextLanguage.values().length, 5,
                "TextLanguage must have ARABIC, ENGLISH, SPANISH, FRENCH, GERMAN");
    }

    @Test(description = "Every TextLanguage constant has a non-blank detection regex")
    public void everyTextLanguageConstantHasNonBlankDetectionRegex() {
        for (ValidationEnums.TextLanguage lang : ValidationEnums.TextLanguage.values()) {
            Assert.assertFalse(lang.getDetectionRegex().isBlank(),
                    lang.name() + " detection regex must not be blank");
        }
    }

    @Test(description = "Every TextLanguage constant has a non-blank language code")
    public void everyTextLanguageConstantHasNonBlankLanguageCode() {
        for (ValidationEnums.TextLanguage lang : ValidationEnums.TextLanguage.values()) {
            Assert.assertFalse(lang.getLanguageCode().isBlank(),
                    lang.name() + " language code must not be blank");
        }
    }
}
