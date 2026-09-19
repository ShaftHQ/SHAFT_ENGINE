package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RequirementsPlaybookContractTest {
    private static final Pattern PRACTICE = Pattern.compile("(?m)^(\\d+)\\. ");

    @Test
    void livePlaybookStillHasTenNumberedPracticesMappedByRules() throws Exception {
        Path playbook = Path.of("..", "shaft-skills", "shaft-requirements-analysis", "references", "playbook.md");
        String text = Files.readString(playbook);
        TreeSet<Integer> numbered = new TreeSet<>();
        Matcher matcher = PRACTICE.matcher(text);
        while (matcher.find()) {
            numbered.add(Integer.parseInt(matcher.group(1)));
        }
        assertEquals(DesignPlaybookRules.PRACTICES, numbered);
        assertTrue(text.contains("ISTQB-CTFL"));
    }
}
