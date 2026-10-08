package com.shaft.intellij.actions;

import com.shaft.intellij.testindex.LastRunResults;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FixWithShaftAssistantIntentionTest {
    @Test
    void promptCarriesTheTestTheFailingLineAndTheFullMessage() {
        LastRunResults.Result failure = new LastRunResults.Result("demo.CartTest.total", "failed", 1, 1,
                new LastRunResults.Frame("demo.CartTest", "total", 12), "expected 42\nbut found 41");

        String prompt = FixWithShaftAssistantIntention.prompt(failure);

        assertTrue(prompt.startsWith("Fix this code: "), "routes through the Assistant's fix-code intent");
        assertTrue(prompt.contains("demo.CartTest.total failed at demo.CartTest.total line 12"));
        assertTrue(prompt.contains("expected 42\nbut found 41"));
    }

    @Test
    void promptFallsBackToTheStatusWithoutAMessageOrFrame() {
        String prompt = FixWithShaftAssistantIntention.prompt(
                new LastRunResults.Result("demo.A.b", "broken", 1, 1, null, ""));
        assertTrue(prompt.contains("demo.A.b failed with:\nbroken"));
    }

    @Test
    void intentionIsRegisteredWithItsDescription() throws Exception {
        String xml = Files.readString(Path.of("src/main/resources/META-INF/io.github.shafthq.shaft-withJava.xml"));
        assertTrue(xml.contains("<className>com.shaft.intellij.actions.FixWithShaftAssistantIntention</className>"));
        assertTrue(Files.isRegularFile(Path.of(
                "src/main/resources/intentionDescriptions/FixWithShaftAssistantIntention/description.html")));
        assertEquals("Fix with SHAFT Assistant", FixWithShaftAssistantIntention.TEXT);
    }
}
