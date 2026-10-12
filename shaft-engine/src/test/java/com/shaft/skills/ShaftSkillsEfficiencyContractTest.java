package com.shaft.skills;

import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

/**
 * Reads the shipped shaft-skills text. The mobile skill must state the locator tap,
 * forbid coordinate taps, and tell the agent to file silent no-ops upstream.
 * Every #6909 contract row must appear in the skill file it names.
 */
public class ShaftSkillsEfficiencyContractTest {
    @Test
    public void mobileSkillAndEfficiencyContractMatchShippedText() throws Exception {
        Path skills = Path.of("..", "shaft-skills");
        String mobile = Files.readString(skills.resolve("shaft-mobile-actions/SKILL.md"))
                + "\n"
                + Files.readString(skills.resolve("shaft-mobile-actions/references/playbook.md"));
        assertTrue(mobile.contains("driver.touch().tap(locator)"), mobile);
        assertTrue(mobile.contains("Coordinate taps are forbidden."), mobile);
        assertTrue(mobile.contains("File silent no-ops upstream."), mobile);

        List<String> rows = Files.readAllLines(skills.resolve("references/efficiency-contract.md"));
        int shipped = 0;
        for (String row : rows) {
            if (!row.matches("^[EG]\\d{2} \\|\\| .*")) {
                continue;
            }
            String[] parts = row.split(" \\|\\| ", 3);
            assertEquals(parts.length, 3, row);
            String body = Files.readString(skills.resolve(parts[1].trim()));
            assertTrue(body.contains(parts[2]), parts[0] + " missing from " + parts[1] + ": " + parts[2]);
            shipped++;
        }
        assertEquals(shipped, 40);
    }
}
