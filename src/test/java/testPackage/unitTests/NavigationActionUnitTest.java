package testPackage.unitTests;

import com.shaft.enums.internal.NavigationAction;
import org.testng.Assert;
import org.testng.annotations.Test;

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

    @Test(description = "Enum constants should have correct ordinal values")
    public void ordinalValuesShouldBeSequential() {
        Assert.assertEquals(NavigationAction.FORWARD.ordinal(), 0,
                "FORWARD should have ordinal 0");
        Assert.assertEquals(NavigationAction.BACK.ordinal(), 1,
                "BACK should have ordinal 1");
        Assert.assertEquals(NavigationAction.REFRESH.ordinal(), 2,
                "REFRESH should have ordinal 2");
    }

    @Test(description = "Enum name() should return the constant name as declared")
    public void nameShouldReturnDeclaredName() {
        Assert.assertEquals(NavigationAction.FORWARD.name(), "FORWARD");
        Assert.assertEquals(NavigationAction.BACK.name(), "BACK");
        Assert.assertEquals(NavigationAction.REFRESH.name(), "REFRESH");
    }
}
