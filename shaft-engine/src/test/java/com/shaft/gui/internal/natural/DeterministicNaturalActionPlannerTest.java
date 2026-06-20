package com.shaft.gui.internal.natural;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.pagefactory.ByAll;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

import static org.mockito.Mockito.mock;

public class DeterministicNaturalActionPlannerTest {
    private final DeterministicNaturalActionPlanner planner = new DeterministicNaturalActionPlanner();

    @Test
    public void blankIntentShouldReturnUnsupportedPlan() {
        NaturalActionPlan plan = planner.plan(request("   "));

        Assert.assertEquals(plan.plannerId(), "deterministic");
        Assert.assertTrue(plan.steps().isEmpty());
        Assert.assertEquals(plan.trust(), 0.0);
        Assert.assertTrue(plan.explanation().contains("blank"));
    }

    @Test
    public void browserNavigationShouldUseSuppliedUrlArgument() {
        NaturalActionPlan plan = planner.plan(request("open the application", "https://example.test"));

        Assert.assertEquals(plan.steps().size(), 1);
        Assert.assertEquals(plan.steps().getFirst().kind(), NaturalActionKind.BROWSER_NAVIGATE);
        Assert.assertEquals(plan.steps().getFirst().data(), "https://example.test");
        Assert.assertTrue(plan.trust() > 0.8);
    }

    @Test
    public void loginIntentShouldProduceUsernamePasswordAndSubmitSteps() {
        NaturalActionPlan plan = planner.plan(request("Log in", "user@example.test", "secret"));

        Assert.assertEquals(plan.steps().size(), 3);
        Assert.assertEquals(plan.steps().get(0).kind(), NaturalActionKind.ELEMENT_TYPE);
        Assert.assertEquals(plan.steps().get(1).kind(), NaturalActionKind.ELEMENT_TYPE_SECURELY);
        Assert.assertEquals(plan.steps().get(2).kind(), NaturalActionKind.ELEMENT_CLICK);
        Assert.assertTrue(plan.steps().get(0).locator() instanceof ByAll);
        Assert.assertEquals(plan.steps().get(1).data(), "secret");
    }

    @Test
    public void elementAndTouchIntentsShouldResolveSemanticLocators() {
        Assert.assertEquals(
                planner.plan(request("click Submit")).steps().getFirst().kind(),
                NaturalActionKind.ELEMENT_CLICK);
        Assert.assertEquals(
                planner.plan(request("type into Email", "user@example.test")).steps().getFirst().kind(),
                NaturalActionKind.ELEMENT_TYPE);
        Assert.assertEquals(
                planner.plan(request("enter Password", "secret")).steps().getFirst().kind(),
                NaturalActionKind.ELEMENT_TYPE);
        Assert.assertEquals(
                planner.plan(request("clear Search")).steps().getFirst().kind(),
                NaturalActionKind.ELEMENT_CLEAR);
        Assert.assertEquals(
                planner.plan(request("tap Continue")).steps().getFirst().kind(),
                NaturalActionKind.TOUCH_TAP);
    }

    @Test
    public void missingRequiredArgumentsShouldKeepPlansUnsupported() {
        Assert.assertTrue(planner.plan(request("login")).steps().isEmpty());
        Assert.assertTrue(planner.plan(request("open settings")).steps().isEmpty());
        Assert.assertTrue(planner.plan(request("type into Email")).steps().isEmpty());
        Assert.assertTrue(planner.plan(request("unknown intent")).steps().isEmpty());
    }

    private NaturalActionRequest request(String intent, Object... args) {
        return new NaturalActionRequest(mock(WebDriver.class), intent, List.of(args), false, false);
    }
}
