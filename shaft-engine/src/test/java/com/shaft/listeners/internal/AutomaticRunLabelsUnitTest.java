package com.shaft.listeners.internal;

import com.shaft.driver.SHAFT;
import io.qameta.allure.model.Label;
import io.qameta.allure.model.TestResult;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.List;

public class AutomaticRunLabelsUnitTest {

    @AfterMethod(alwaysRun = true)
    public void reset() {
        SHAFT.Properties.allure.set().automaticRunLabels(true);
        SHAFT.Properties.platform.set().targetPlatform("Linux").executionAddress("local");
    }

    @Test
    public void operatingSystemIsNormalized() {
        Assert.assertEquals(AutomaticRunLabels.operatingSystem("ANDROID"), "Android");
        Assert.assertEquals(AutomaticRunLabels.operatingSystem("ios"), "iOS");
        Assert.assertEquals(AutomaticRunLabels.operatingSystem("Mac"), "macOS");
        Assert.assertEquals(AutomaticRunLabels.operatingSystem("windows"), "Windows");
        Assert.assertEquals(AutomaticRunLabels.operatingSystem("Linux"), "Linux");
        Assert.assertEquals(AutomaticRunLabels.operatingSystem(" "), "Unknown");
        Assert.assertEquals(AutomaticRunLabels.operatingSystem(null), "Unknown");
        Assert.assertEquals(AutomaticRunLabels.operatingSystem("Plan9"), "Plan9");
    }

    @Test
    public void softwareUnderTestIsClassified() {
        Assert.assertEquals(AutomaticRunLabels.softwareUnderTest("Android", true, false), "MobileWeb");
        Assert.assertEquals(AutomaticRunLabels.softwareUnderTest("iOS", false, true), "Native");
        Assert.assertEquals(AutomaticRunLabels.softwareUnderTest("Android", true, true), "Native");
        Assert.assertEquals(AutomaticRunLabels.softwareUnderTest("Linux", false, false), "Web");
    }

    @Test
    public void executionLocationIsClassified() {
        Assert.assertEquals(AutomaticRunLabels.executionLocation("browserstack", "Android"), "BrowserStack");
        Assert.assertEquals(AutomaticRunLabels.executionLocation("https://hub.lambdatest.com/wd/hub", "Linux"), "LambdaTest");
        Assert.assertEquals(AutomaticRunLabels.executionLocation("dockerized", "Linux"), "Docker");
        Assert.assertEquals(AutomaticRunLabels.executionLocation("testcontainers", "Linux"), "Docker");
        Assert.assertEquals(AutomaticRunLabels.executionLocation("local", "iOS"), "LocalSimulator");
        Assert.assertEquals(AutomaticRunLabels.executionLocation("localhost:4723", "Android"), "LocalEmulator");
        Assert.assertEquals(AutomaticRunLabels.executionLocation("local", "Windows"), "Local");
        Assert.assertEquals(AutomaticRunLabels.executionLocation("grid.example:4444", "Linux"), "Remote");
    }

    @Test
    public void appliesLabelsAndTagsFromProperties() {
        SHAFT.Properties.platform.set().targetPlatform("Linux").executionAddress("dockerized");
        TestResult result = new TestResult();
        AutomaticRunLabels.apply(result);
        List<String> labels = result.getLabels().stream().map(l -> l.getName() + "=" + l.getValue()).toList();
        Assert.assertTrue(labels.containsAll(List.of("os=Linux", "tag=os:Linux", "sut=Web", "tag=sut:Web",
                "location=Docker", "tag=location:Docker")), labels.toString());
    }

    @Test
    public void handSetLabelsWinAndNothingIsDuplicated() {
        TestResult result = new TestResult();
        result.getLabels().add(new Label().setName("os").setValue("Custom"));
        AutomaticRunLabels.apply(result, "Linux", "Web", "Local");
        AutomaticRunLabels.apply(result, "Linux", "Web", "Local");
        long osLabels = result.getLabels().stream().filter(l -> "os".equals(l.getName())).count();
        long osTags = result.getLabels().stream().filter(l -> "tag:os:Linux".equals(l.getName() + ":" + l.getValue())).count();
        Assert.assertEquals(osLabels, 1);
        Assert.assertEquals(osTags, 1);
    }

    @Test
    public void canBeDisabled() {
        SHAFT.Properties.allure.set().automaticRunLabels(false);
        TestResult result = new TestResult();
        AutomaticRunLabels.apply(result);
        Assert.assertTrue(result.getLabels().isEmpty());
    }
}
