package com.shaft.intellij.actions;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PageObjectFromCaptureTest {
    private static final String FIXTURE = """
            {"successful":true,"codeBlocks":[
              {"id":"capture-test-method","code":"void t() {}","imports":[]},
              {"id":"capture-page-object-draft","imports":["com.shaft.driver.SHAFT","org.openqa.selenium.By"],
               "code":"public final class LoginPage {\\n    private final SHAFT.GUI.WebDriver driver;\\n    private final By user = By.id(\\"user\\");\\n}\\n"}]}
            """;

    @Test
    void assemblesACompilationUnitFromTheDraftBlock() {
        PageObjectFromCapture.Draft draft = PageObjectFromCapture.draft(FIXTURE, "com.acme.pages");
        assertEquals("LoginPage", draft.className());
        assertEquals("com/acme/pages/LoginPage.java", draft.relativePath());
        assertTrue(draft.source().startsWith("package com.acme.pages;\n\nimport com.shaft.driver.SHAFT;\nimport org.openqa.selenium.By;\n\npublic final class LoginPage {"));
    }

    @Test
    void unwrapsMcpContentEnvelopeAndRejectsSessionsWithoutADraft() {
        String wrapped = "{\"content\":[{\"type\":\"text\",\"text\":" + com.google.gson.JsonParser.parseString(FIXTURE).toString()
                .transform(s -> new com.google.gson.JsonPrimitive(s).toString()) + "}]}";
        assertEquals("LoginPage", PageObjectFromCapture.draft(wrapped, "pages").className());
        assertNull(PageObjectFromCapture.draft("{\"codeBlocks\":[]}", "pages"));
        assertNull(PageObjectFromCapture.draft("not json", "pages"));
    }
}
