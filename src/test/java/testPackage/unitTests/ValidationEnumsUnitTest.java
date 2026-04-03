package testPackage.unitTests;

import com.shaft.validation.ValidationEnums;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Unit tests for {@link ValidationEnums} enumerations.
 * Verifies that each enum constant exists and its associated value getter
 * returns the correct result.
 */
public class ValidationEnumsUnitTest {

    // ─── ValidationType ────────────────────────────────────────────────────────

    @Test(description = "ValidationType.POSITIVE should have value true")
    public void validationTypePositiveShouldHaveValueTrue() {
        Assert.assertTrue(ValidationEnums.ValidationType.POSITIVE.getValue(),
                "POSITIVE ValidationType should return true");
    }

    @Test(description = "ValidationType.NEGATIVE should have value false")
    public void validationTypeNegativeShouldHaveValueFalse() {
        Assert.assertFalse(ValidationEnums.ValidationType.NEGATIVE.getValue(),
                "NEGATIVE ValidationType should return false");
    }

    @Test(description = "ValidationType should have exactly 2 constants")
    public void validationTypeShouldHaveExactlyTwoConstants() {
        Assert.assertEquals(ValidationEnums.ValidationType.values().length, 2,
                "ValidationType should declare POSITIVE and NEGATIVE only");
    }

    // ─── ValidationComparisonType ──────────────────────────────────────────────

    @Test(description = "ValidationComparisonType.EQUALS should have value 1")
    public void validationComparisonTypeEqualsShouldBeOne() {
        Assert.assertEquals(ValidationEnums.ValidationComparisonType.EQUALS.getValue(), 1,
                "EQUALS comparison type should map to integer value 1");
    }

    @Test(description = "ValidationComparisonType.CONTAINS should have value 3")
    public void validationComparisonTypeContainsShouldBeThree() {
        Assert.assertEquals(ValidationEnums.ValidationComparisonType.CONTAINS.getValue(), 3,
                "CONTAINS comparison type should map to integer value 3");
    }

    @Test(description = "ValidationComparisonType.MATCHES should have value 2")
    public void validationComparisonTypeMatchesShouldBeTwo() {
        Assert.assertEquals(ValidationEnums.ValidationComparisonType.MATCHES.getValue(), 2,
                "MATCHES comparison type should map to integer value 2");
    }

    @Test(description = "ValidationComparisonType.CASE_INSENSITIVE should have value 4")
    public void validationComparisonTypeCaseInsensitiveShouldBeFour() {
        Assert.assertEquals(ValidationEnums.ValidationComparisonType.CASE_INSENSITIVE.getValue(), 4,
                "CASE_INSENSITIVE comparison type should map to integer value 4");
    }

    @Test(description = "ValidationComparisonType should have exactly 4 constants")
    public void validationComparisonTypeShouldHaveFourConstants() {
        Assert.assertEquals(ValidationEnums.ValidationComparisonType.values().length, 4,
                "ValidationComparisonType should declare EQUALS, CONTAINS, MATCHES, CASE_INSENSITIVE");
    }

    // ─── NumbersComparativeRelation ────────────────────────────────────────────

    @Test(description = "NumbersComparativeRelation.GREATER_THAN should have value '>'")
    public void numbersComparativeRelationGreaterThanShouldBeGT() {
        Assert.assertEquals(ValidationEnums.NumbersComparativeRelation.GREATER_THAN.getValue(), ">",
                "GREATER_THAN should map to '>'");
    }

    @Test(description = "NumbersComparativeRelation.GREATER_THAN_OR_EQUALS should have value '>='")
    public void numbersComparativeRelationGTEShouldBeGTE() {
        Assert.assertEquals(ValidationEnums.NumbersComparativeRelation.GREATER_THAN_OR_EQUALS.getValue(), ">=",
                "GREATER_THAN_OR_EQUALS should map to '>='");
    }

    @Test(description = "NumbersComparativeRelation.LESS_THAN should have value '<'")
    public void numbersComparativeRelationLessThanShouldBeLT() {
        Assert.assertEquals(ValidationEnums.NumbersComparativeRelation.LESS_THAN.getValue(), "<",
                "LESS_THAN should map to '<'");
    }

    @Test(description = "NumbersComparativeRelation.LESS_THAN_OR_EQUALS should have value '<='")
    public void numbersComparativeRelationLTEShouldBeLTE() {
        Assert.assertEquals(ValidationEnums.NumbersComparativeRelation.LESS_THAN_OR_EQUALS.getValue(), "<=",
                "LESS_THAN_OR_EQUALS should map to '<='");
    }

    @Test(description = "NumbersComparativeRelation.EQUALS should have value '=='")
    public void numbersComparativeRelationEqualsShouldBeDoubleEquals() {
        Assert.assertEquals(ValidationEnums.NumbersComparativeRelation.EQUALS.getValue(), "==",
                "EQUALS should map to '=='");
    }

    @Test(description = "NumbersComparativeRelation should have exactly 5 constants")
    public void numbersComparativeRelationShouldHaveFiveConstants() {
        Assert.assertEquals(ValidationEnums.NumbersComparativeRelation.values().length, 5,
                "NumbersComparativeRelation should declare GT, GTE, LT, LTE, EQUALS");
    }

    // ─── ValidationCategory ────────────────────────────────────────────────────

    @Test(description = "ValidationCategory.HARD_ASSERT should exist and be a distinct constant")
    public void validationCategoryHardAssertShouldExist() {
        Assert.assertNotNull(ValidationEnums.ValidationCategory.HARD_ASSERT,
                "HARD_ASSERT should be a valid ValidationCategory constant");
    }

    @Test(description = "ValidationCategory.SOFT_ASSERT should exist and be a distinct constant")
    public void validationCategorySoftAssertShouldExist() {
        Assert.assertNotNull(ValidationEnums.ValidationCategory.SOFT_ASSERT,
                "SOFT_ASSERT should be a valid ValidationCategory constant");
    }

    @Test(description = "ValidationCategory.HARD_ASSERT and SOFT_ASSERT should be different")
    public void validationCategoryHardAndSoftShouldBeDifferent() {
        Assert.assertNotEquals(ValidationEnums.ValidationCategory.HARD_ASSERT,
                ValidationEnums.ValidationCategory.SOFT_ASSERT,
                "HARD_ASSERT and SOFT_ASSERT must be distinct enum constants");
    }

    @Test(description = "ValidationCategory should have exactly 2 constants")
    public void validationCategoryShouldHaveTwoConstants() {
        Assert.assertEquals(ValidationEnums.ValidationCategory.values().length, 2,
                "ValidationCategory should declare HARD_ASSERT and SOFT_ASSERT only");
    }

    // ─── ValidationState ───────────────────────────────────────────────────────

    @Test(description = "ValidationState.PASSED should have value true")
    public void validationStatePassedShouldHaveValueTrue() {
        Assert.assertTrue(ValidationEnums.ValidationState.PASSED.getValue(),
                "PASSED ValidationState should return true");
    }

    @Test(description = "ValidationState.FAILED should have value false")
    public void validationStateFailedShouldHaveValueFalse() {
        Assert.assertFalse(ValidationEnums.ValidationState.FAILED.getValue(),
                "FAILED ValidationState should return false");
    }

    @Test(description = "ValidationState should have exactly 2 constants")
    public void validationStateShouldHaveTwoConstants() {
        Assert.assertEquals(ValidationEnums.ValidationState.values().length, 2,
                "ValidationState should declare PASSED and FAILED only");
    }

    // ─── VisualValidationEngine ────────────────────────────────────────────────

    @Test(description = "VisualValidationEngine should include EXACT_SHUTTERBUG")
    public void visualValidationEngineShouldIncludeExactShutterbug() {
        Assert.assertNotNull(ValidationEnums.VisualValidationEngine.EXACT_SHUTTERBUG,
                "EXACT_SHUTTERBUG constant should exist");
    }

    @Test(description = "VisualValidationEngine should include EXACT_OPENCV")
    public void visualValidationEngineShouldIncludeExactOpenCV() {
        Assert.assertNotNull(ValidationEnums.VisualValidationEngine.EXACT_OPENCV,
                "EXACT_OPENCV constant should exist");
    }

    @Test(description = "VisualValidationEngine should include EXACT_EYES")
    public void visualValidationEngineShouldIncludeExactEyes() {
        Assert.assertNotNull(ValidationEnums.VisualValidationEngine.EXACT_EYES,
                "EXACT_EYES constant should exist");
    }

    @Test(description = "VisualValidationEngine should include STRICT_EYES")
    public void visualValidationEngineShouldIncludeStrictEyes() {
        Assert.assertNotNull(ValidationEnums.VisualValidationEngine.STRICT_EYES,
                "STRICT_EYES constant should exist");
    }

    @Test(description = "VisualValidationEngine should include CONTENT_EYES")
    public void visualValidationEngineShouldIncludeContentEyes() {
        Assert.assertNotNull(ValidationEnums.VisualValidationEngine.CONTENT_EYES,
                "CONTENT_EYES constant should exist");
    }

    @Test(description = "VisualValidationEngine should include LAYOUT_EYES")
    public void visualValidationEngineShouldIncludeLayoutEyes() {
        Assert.assertNotNull(ValidationEnums.VisualValidationEngine.LAYOUT_EYES,
                "LAYOUT_EYES constant should exist");
    }

    @Test(description = "VisualValidationEngine should have exactly 6 constants")
    public void visualValidationEngineShouldHaveSixConstants() {
        Assert.assertEquals(ValidationEnums.VisualValidationEngine.values().length, 6,
                "VisualValidationEngine should declare exactly 6 engine types");
    }

    // ─── valueOf / name round-trips ────────────────────────────────────────────

    @Test(description = "ValidationType.valueOf should resolve POSITIVE by name")
    public void validationTypeValueOfPositiveShouldResolve() {
        Assert.assertEquals(ValidationEnums.ValidationType.valueOf("POSITIVE"),
                ValidationEnums.ValidationType.POSITIVE,
                "valueOf('POSITIVE') should return the POSITIVE constant");
    }

    @Test(description = "NumbersComparativeRelation.valueOf should resolve GREATER_THAN by name")
    public void numbersComparativeRelationValueOfGreaterThanShouldResolve() {
        Assert.assertEquals(ValidationEnums.NumbersComparativeRelation.valueOf("GREATER_THAN"),
                ValidationEnums.NumbersComparativeRelation.GREATER_THAN,
                "valueOf('GREATER_THAN') should return the GREATER_THAN constant");
    }

    @Test(description = "ValidationState.name() should return the enum constant name")
    public void validationStateNameShouldMatchConstantName() {
        Assert.assertEquals(ValidationEnums.ValidationState.PASSED.name(), "PASSED",
                "name() should return 'PASSED'");
        Assert.assertEquals(ValidationEnums.ValidationState.FAILED.name(), "FAILED",
                "name() should return 'FAILED'");
    }
}
