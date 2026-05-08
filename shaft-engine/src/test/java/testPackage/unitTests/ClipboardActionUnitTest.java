package testPackage.unitTests;

import com.shaft.enums.internal.ClipboardAction;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Unit tests for {@link ClipboardAction} enum.
 * Validates enum constants, their string values, and valueOf behavior.
 */
public class ClipboardActionUnitTest {

    @Test(description = "ClipboardAction should have exactly 5 enum constants")
    public void clipboardActionShouldHaveFiveConstants() {
        Assert.assertEquals(ClipboardAction.values().length, 5,
                "ClipboardAction should define exactly 5 constants");
    }

    @Test(description = "COPY should have value 'copy'")
    public void copyValueShouldBeCopy() {
        Assert.assertEquals(ClipboardAction.COPY.getValue(), "copy",
                "COPY constant should have value 'copy'");
    }

    @Test(description = "PASTE should have value 'paste'")
    public void pasteValueShouldBePaste() {
        Assert.assertEquals(ClipboardAction.PASTE.getValue(), "paste",
                "PASTE constant should have value 'paste'");
    }

    @Test(description = "CUT should have value 'cut'")
    public void cutValueShouldBeCut() {
        Assert.assertEquals(ClipboardAction.CUT.getValue(), "cut",
                "CUT constant should have value 'cut'");
    }

    @Test(description = "SELECT_ALL should have value 'select all'")
    public void selectAllValueShouldBeSelectAll() {
        Assert.assertEquals(ClipboardAction.SELECT_ALL.getValue(), "select all",
                "SELECT_ALL constant should have value 'select all'");
    }

    @Test(description = "UNSELECT_ALL should have value 'unselect'")
    public void unselectAllValueShouldBeUnselect() {
        Assert.assertEquals(ClipboardAction.UNSELECT_ALL.getValue(), "unselect",
                "UNSELECT_ALL constant should have value 'unselect'");
    }

    @Test(description = "valueOf should return correct enum for known name")
    public void valueOfShouldReturnCorrectEnum() {
        Assert.assertEquals(ClipboardAction.valueOf("COPY"), ClipboardAction.COPY,
                "valueOf('COPY') should return COPY");
        Assert.assertEquals(ClipboardAction.valueOf("PASTE"), ClipboardAction.PASTE,
                "valueOf('PASTE') should return PASTE");
        Assert.assertEquals(ClipboardAction.valueOf("CUT"), ClipboardAction.CUT,
                "valueOf('CUT') should return CUT");
    }

    @Test(description = "valueOf should throw IllegalArgumentException for unknown name",
            expectedExceptions = IllegalArgumentException.class)
    public void valueOfShouldThrowForUnknownName() {
        ClipboardAction.valueOf("UNKNOWN");
    }

    @Test(description = "All ClipboardAction values should have non-null, non-empty string values")
    public void allValuesShouldHaveNonNullNonEmptyValues() {
        for (ClipboardAction action : ClipboardAction.values()) {
            Assert.assertNotNull(action.getValue(),
                    action.name() + " should have a non-null value");
            Assert.assertFalse(action.getValue().isEmpty(),
                    action.name() + " should have a non-empty value");
        }
    }
}
