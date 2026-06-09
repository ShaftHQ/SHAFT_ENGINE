package testPackage.unitTests;

import com.shaft.enums.internal.NavigationAction;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Unit tests for {@link NavigationAction} enum.
 * Validates enum constants, ordinals, and valueOf behavior.
 */
public class NavigationActionUnitTest {

    @Test(description = "NavigationAction should have exactly 3 enum constants")
    public void navigationActionShouldHaveThreeConstants() {
        Assert.assertEquals(NavigationAction.values().length, 3,
                "NavigationAction should define exactly 3 constants");
    }

    @Test(description = "NavigationAction should contain FORWARD constant")
    public void shouldContainForward() {
        Assert.assertNotNull(NavigationAction.valueOf("FORWARD"),
                "NavigationAction should have a FORWARD constant");
    }

    @Test(description = "NavigationAction should contain BACK constant")
    public void shouldContainBack() {
        Assert.assertNotNull(NavigationAction.valueOf("BACK"),
                "NavigationAction should have a BACK constant");
    }

    @Test(description = "NavigationAction should contain REFRESH constant")
    public void shouldContainRefresh() {
        Assert.assertNotNull(NavigationAction.valueOf("REFRESH"),
                "NavigationAction should have a REFRESH constant");
    }

    @Test(description = "valueOf should throw IllegalArgumentException for unknown name",
            expectedExceptions = IllegalArgumentException.class)
    public void valueOfShouldThrowForUnknownName() {
        NavigationAction.valueOf("RELOAD");
    }

    @Test(description = "values() should return constants in declared order: FORWARD, BACK, REFRESH")
    public void valuesShouldReturnConstantsInDeclaredOrder() {
        NavigationAction[] values = NavigationAction.values();
        Assert.assertEquals(values[0], NavigationAction.FORWARD,
                "First constant should be FORWARD");
        Assert.assertEquals(values[1], NavigationAction.BACK,
                "Second constant should be BACK");
        Assert.assertEquals(values[2], NavigationAction.REFRESH,
                "Third constant should be REFRESH");
    }

    @Test(description = "Enum name() should return the constant name as declared")
    public void nameShouldReturnDeclaredName() {
        Assert.assertEquals(NavigationAction.FORWARD.name(), "FORWARD");
        Assert.assertEquals(NavigationAction.BACK.name(), "BACK");
        Assert.assertEquals(NavigationAction.REFRESH.name(), "REFRESH");
    }
}
