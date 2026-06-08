package testPackage.unitTests;

import com.shaft.tools.internal.support.HTMLHelper;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Unit tests for {@link HTMLHelper} enum.
 * Validates that HTML template constants exist and contain expected placeholders.
 */
public class HTMLHelperUnitTest {

    @Test(description = "HTMLHelper should have exactly 4 enum constants")
    public void htmlHelperShouldHaveFourConstants() {
        Assert.assertEquals(HTMLHelper.values().length, 4,
                "HTMLHelper should define exactly 4 constants");
    }

    @Test(description = "CHECKPOINT_COUNTER should have non-null, non-empty value")
    public void checkpointCounterShouldBeNonEmpty() {
        String value = HTMLHelper.CHECKPOINT_COUNTER.getValue();
        Assert.assertNotNull(value, "CHECKPOINT_COUNTER value should not be null");
        Assert.assertFalse(value.isEmpty(), "CHECKPOINT_COUNTER value should not be empty");
    }

    @Test(description = "CHECKPOINT_COUNTER should contain HTML structure markers")
    public void checkpointCounterShouldContainHtmlStructure() {
        String value = HTMLHelper.CHECKPOINT_COUNTER.getValue();
        Assert.assertTrue(value.contains("<!DOCTYPE html>"),
                "CHECKPOINT_COUNTER should contain DOCTYPE declaration");
        Assert.assertTrue(value.contains("<html"),
                "CHECKPOINT_COUNTER should contain html tag");
        Assert.assertTrue(value.contains("</html>"),
                "CHECKPOINT_COUNTER should contain closing html tag");
    }

    @Test(description = "CHECKPOINT_COUNTER should contain expected title")
    public void checkpointCounterShouldContainTitle() {
        String value = HTMLHelper.CHECKPOINT_COUNTER.getValue();
        Assert.assertTrue(value.contains("Checkpoints Report"),
                "CHECKPOINT_COUNTER should contain 'Checkpoints Report' title");
    }

    @Test(description = "CHECKPOINT_DETAILS_FORMAT should have non-null, non-empty value")
    public void checkpointDetailsFormatShouldBeNonEmpty() {
        String value = HTMLHelper.CHECKPOINT_DETAILS_FORMAT.getValue();
        Assert.assertNotNull(value, "CHECKPOINT_DETAILS_FORMAT value should not be null");
        Assert.assertFalse(value.isEmpty(), "CHECKPOINT_DETAILS_FORMAT value should not be empty");
    }

    @Test(description = "CHECKPOINT_DETAILS_FORMAT should contain format placeholders")
    public void checkpointDetailsFormatShouldContainPlaceholders() {
        String value = HTMLHelper.CHECKPOINT_DETAILS_FORMAT.getValue();
        Assert.assertTrue(value.contains("%s"),
                "CHECKPOINT_DETAILS_FORMAT should contain %s placeholder(s)");
    }

    @Test(description = "EXECUTION_SUMMARY should have non-null, non-empty value")
    public void executionSummaryShouldBeNonEmpty() {
        String value = HTMLHelper.EXECUTION_SUMMARY.getValue();
        Assert.assertNotNull(value, "EXECUTION_SUMMARY value should not be null");
        Assert.assertFalse(value.isEmpty(), "EXECUTION_SUMMARY value should not be empty");
    }

    @Test(description = "EXECUTION_SUMMARY should contain HTML structure markers")
    public void executionSummaryShouldContainHtmlStructure() {
        String value = HTMLHelper.EXECUTION_SUMMARY.getValue();
        Assert.assertTrue(value.contains("<!DOCTYPE html>"),
                "EXECUTION_SUMMARY should contain DOCTYPE declaration");
        Assert.assertTrue(value.contains("<html"),
                "EXECUTION_SUMMARY should contain html tag");
    }

    @Test(description = "EXECUTION_SUMMARY_DETAILS_FORMAT should have non-null, non-empty value")
    public void executionSummaryDetailsFormatShouldBeNonEmpty() {
        String value = HTMLHelper.EXECUTION_SUMMARY_DETAILS_FORMAT.getValue();
        Assert.assertNotNull(value, "EXECUTION_SUMMARY_DETAILS_FORMAT value should not be null");
        Assert.assertFalse(value.isEmpty(), "EXECUTION_SUMMARY_DETAILS_FORMAT should not be empty");
    }

    @Test(description = "EXECUTION_SUMMARY_DETAILS_FORMAT should contain format placeholders")
    public void executionSummaryDetailsFormatShouldContainPlaceholders() {
        String value = HTMLHelper.EXECUTION_SUMMARY_DETAILS_FORMAT.getValue();
        Assert.assertTrue(value.contains("%s"),
                "EXECUTION_SUMMARY_DETAILS_FORMAT should contain %s placeholder(s)");
    }

    @Test(description = "All HTMLHelper values should have non-null values via getValue()")
    public void allValuesShouldBeNonNull() {
        for (HTMLHelper helper : HTMLHelper.values()) {
            Assert.assertNotNull(helper.getValue(),
                    helper.name() + " should have a non-null value");
        }
    }

    @Test(description = "valueOf should return correct enum for known name")
    public void valueOfShouldReturnCorrectEnum() {
        Assert.assertEquals(HTMLHelper.valueOf("CHECKPOINT_COUNTER"), HTMLHelper.CHECKPOINT_COUNTER);
        Assert.assertEquals(HTMLHelper.valueOf("EXECUTION_SUMMARY"), HTMLHelper.EXECUTION_SUMMARY);
    }

    @Test(description = "valueOf should throw IllegalArgumentException for unknown name",
            expectedExceptions = IllegalArgumentException.class)
    public void valueOfShouldThrowForUnknownName() {
        HTMLHelper.valueOf("NON_EXISTENT");
    }
}
