package com.shaft.intellij.actions;

import com.shaft.intellij.testindex.LastRunResults;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GoToLastFailureActionTest {
    @Test
    void entryTextNamesClassMethodAndFailingLine() {
        LastRunResults.Result result = new LastRunResults.Result("demo.LoginTest.signIn", "failed", 1, 2,
                new LastRunResults.Frame("demo.LoginTest", "signIn", 42));
        assertEquals("LoginTest#signIn (line 42)", GoToLastFailureAction.entryText(result));
        assertEquals("LoginTest#signIn", GoToLastFailureAction.entryText(
                new LastRunResults.Result("demo.LoginTest.signIn", "broken", 1, 2, null)));
    }
}
