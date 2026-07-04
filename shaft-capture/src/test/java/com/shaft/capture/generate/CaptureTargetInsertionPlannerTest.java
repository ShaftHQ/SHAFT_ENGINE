package com.shaft.capture.generate;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureTargetInsertionPlannerTest {
    @TempDir
    Path temp;

    @Test
    void planReusesExistingLocatorFieldsAndSkipsDuplicateActionLines() throws Exception {
        Path generated = temp.resolve("GeneratedSearchTest.java");
        Files.writeString(generated, """
                package generated;

                import com.shaft.driver.SHAFT;
                import org.openqa.selenium.By;
                import org.testng.annotations.Test;

                public class GeneratedSearchTest {
                    private SHAFT.GUI.WebDriver driver;

                    @Test
                    public void replaySearch() {
                        driver.element().type(SHAFT.GUI.Locator.inputField("Search"), "shaft");
                        driver.element().click(SHAFT.GUI.Locator.button("Search"));
                    }
                }
                """);
        Path target = temp.resolve("SearchPage.java");
        Files.writeString(target, """
                package pages;

                import com.shaft.driver.SHAFT;
                import org.openqa.selenium.By;

                public class SearchPage {
                    private final SHAFT.GUI.WebDriver browser;
                    private final By searchInput = SHAFT.GUI.Locator.inputField("Search");
                    private final By searchButton = SHAFT.GUI.Locator.button("Search");

                    public SearchPage search(String query) {
                        browser.element().type(searchInput, "shaft");
                        return this;
                    }
                }
                """);

        CaptureTargetInsertionPlan plan = new CaptureTargetInsertionPlanner()
                .plan(generated, target, "search", "browser");

        assertAll(
                () -> assertFalse(plan.locatorFields().contains("inputField(\"Search\")"), plan.locatorFields()),
                () -> assertFalse(plan.locatorFields().contains("button(\"Search\")"), plan.locatorFields()),
                () -> assertFalse(plan.actionSnippet().contains("type(searchInput"), plan.actionSnippet()),
                () -> assertTrue(plan.actionSnippet().contains("browser.element().click(searchButton);"),
                        plan.actionSnippet()),
                () -> assertTrue(plan.warnings().stream()
                        .anyMatch(warning -> warning.contains("Reused existing locator field searchInput")),
                        plan.warnings().toString()),
                () -> assertTrue(plan.warnings().stream()
                        .anyMatch(warning -> warning.contains("Skipped duplicate action line")),
                        plan.warnings().toString()));
    }
}
