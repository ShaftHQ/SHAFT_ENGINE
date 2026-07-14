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
        // Re-titled to the branded suite overview (issue #3504).
        Assert.assertTrue(value.contains("SHAFT Overview"),
                "CHECKPOINT_COUNTER should contain the 'SHAFT Overview' title");
        Assert.assertTrue(value.contains("Traces captured"),
                "CHECKPOINT_COUNTER should surface the captured-traces KPI");
    }

    @Test(description = "CHECKPOINT_COUNTER should expose the per-type breakdown and fail-only filter (#3523)")
    public void checkpointCounterShouldExposeBreakdownAndFilter() {
        String value = HTMLHelper.CHECKPOINT_COUNTER.getValue();
        Assert.assertTrue(value.contains("By type"), "expected a per-type breakdown panel");
        Assert.assertTrue(value.contains("${ASSERTIONS_PASSED}") && value.contains("${VERIFICATIONS_FAILED}"),
                "expected per-type breakdown placeholders");
        Assert.assertTrue(value.contains("Show failures only"), "expected a fail-only filter control");
        Assert.assertTrue(value.contains("fail-only"), "expected the fail-only CSS/class hook");
        // Checkpoint-browser per-test filter (#3534): a test dropdown, its options placeholder,
        // the composable test-hidden CSS hook, and the filter function.
        Assert.assertTrue(value.contains("Filter by test"), "expected a per-test filter control");
        Assert.assertTrue(value.contains("${CHECKPOINTS_TEST_OPTIONS}"), "expected the test-options placeholder");
        Assert.assertTrue(value.contains("test-hidden"), "expected the test-hidden CSS hook");
        Assert.assertTrue(value.contains("shaftFilterCheckpointsByTest"), "expected the test-filter function");
    }

    @Test(description = "CHECKPOINT_DETAILS_FORMAT should carry a filterable data-status (#3523) and per-test data-test (#3534)")
    public void checkpointDetailsFormatShouldCarryDataStatus() {
        String value = HTMLHelper.CHECKPOINT_DETAILS_FORMAT.getValue();
        Assert.assertTrue(value.contains("data-status=\"%s\""), "expected a filterable data-status attribute");
        Assert.assertTrue(value.contains("checkpoint-row"), "expected the checkpoint-row class hook");
        // Per-test attribution for the checkpoint browser: a filterable data-test attribute plus a Test cell.
        Assert.assertTrue(value.contains("data-test=\"%s\""), "expected a filterable data-test attribute");
        Assert.assertTrue(value.contains("checkpoint-test"), "expected the checkpoint-test cell hook");
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

    @Test(description = "Generated report templates should use the SHAFT UI theme without remote UI dependencies")
    public void reportTemplatesShouldUseShaftThemeWithoutRemoteUiDependencies() {
        String checkpoint = HTMLHelper.CHECKPOINT_COUNTER.getValue();
        String summary = HTMLHelper.EXECUTION_SUMMARY.getValue();

        Assert.assertTrue(checkpoint.contains("--shaft-primary"), checkpoint);
        Assert.assertTrue(summary.contains("--shaft-primary"), summary);
        Assert.assertFalse(checkpoint.contains("colorlib.com"), checkpoint);
        Assert.assertFalse(summary.contains("colorlib.com"), summary);
        Assert.assertFalse(summary.contains("ajax.googleapis.com"), summary);
        Assert.assertTrue(summary.contains("function filterCases()"), summary);
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
