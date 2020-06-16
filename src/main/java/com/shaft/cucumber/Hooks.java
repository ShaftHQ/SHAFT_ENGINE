package com.shaft.cucumber;

import com.shaft.gui.element.ElementActions;
import com.shaft.tools.io.PropertiesFileManager;
import com.shaft.tools.io.ReportManager;
import io.cucumber.java.After;
import io.cucumber.java.Before;
import io.cucumber.java.Scenario;

public class Hooks {
    private static boolean isFirstScenario = true;

    @Before
    public void before(Scenario scenario) {
        if (Boolean.TRUE.equals(isFirstScenario)) {
            PropertiesFileManager.readPropertyFiles();
            isFirstScenario = false;
        }
        ReportManager.setCucumberScenario(scenario);
    }

    @After
    public void after(Scenario scenario) {
        ElementActions.switchToDefaultContent();
    }

}
