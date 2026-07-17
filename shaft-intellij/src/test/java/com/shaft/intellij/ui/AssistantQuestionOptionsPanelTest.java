package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AssistantQuestionOptionsPanelTest {

    @Test
    void rendersOneButtonPerOption() {
        AssistantQuestionOptionsPanel panel = new AssistantQuestionOptionsPanel(
                List.of("Use the sample page", "I'll give you a URL"), option -> { });

        List<JButton> buttons = panel.optionButtonsForTest();
        List<String> labels = buttons.stream().map(JButton::getText).toList();

        assertAll(
                () -> assertEquals(2, buttons.size()),
                () -> assertTrue(labels.contains("Use the sample page")),
                () -> assertTrue(labels.contains("I'll give you a URL")));
    }

    @Test
    void clickingAChipReportsItsExactTextAndCanBeClickedAgain() {
        AtomicReference<String> chosen = new AtomicReference<>();
        AtomicInteger callCount = new AtomicInteger();
        AssistantQuestionOptionsPanel panel = new AssistantQuestionOptionsPanel(
                List.of("Yes", "No"), option -> {
                    chosen.set(option);
                    callCount.incrementAndGet();
                });
        JButton yes = panel.optionButtonsForTest().stream()
                .filter(button -> "Yes".equals(button.getText())).findFirst().orElseThrow();

        yes.doClick();

        assertAll(
                () -> assertEquals("Yes", chosen.get()),
                () -> assertEquals(1, callCount.get()),
                () -> assertTrue(yes.isEnabled(),
                        "picking a suggested answer is non-destructive; unlike a one-shot approval "
                                + "decision the chip must stay clickable"));

        // A second click (the user changed their mind) must report again, not be swallowed.
        JButton no = panel.optionButtonsForTest().stream()
                .filter(button -> "No".equals(button.getText())).findFirst().orElseThrow();
        no.doClick();

        assertAll(
                () -> assertEquals("No", chosen.get()),
                () -> assertEquals(2, callCount.get()));
    }

    @Test
    void everyButtonHasAnAccessibleName() {
        AssistantQuestionOptionsPanel panel = new AssistantQuestionOptionsPanel(
                List.of("Yes", "No"), option -> { });

        assertAll(panel.optionButtonsForTest().stream()
                .map(button -> (org.junit.jupiter.api.function.Executable) () -> assertFalse(
                        button.getAccessibleContext().getAccessibleName() == null
                                || button.getAccessibleContext().getAccessibleName().isBlank(),
                        "button should have an accessible name")));
    }

    @Test
    void rendersNoButtonsForAnEmptyOptionList() {
        AssistantQuestionOptionsPanel panel = new AssistantQuestionOptionsPanel(List.of(), option -> { });

        assertTrue(panel.optionButtonsForTest().isEmpty());
    }

    @Test
    void toleratesANullOptionList() {
        AssistantQuestionOptionsPanel panel = new AssistantQuestionOptionsPanel(null, option -> { });

        assertTrue(panel.optionButtonsForTest().isEmpty());
    }
}
