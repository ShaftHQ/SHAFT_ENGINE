package com.shaft.heal.internal;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.heal.HealingConfiguration;
import com.shaft.heal.ShaftHeal;
import com.shaft.heal.model.HealingReport;
import com.shaft.tools.io.ReportManager;
import io.qameta.allure.Allure;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

final class HealingReportWriter {
    private static final ObjectMapper JSON = new ObjectMapper();
    private final HealingConfiguration configuration;

    HealingReportWriter(HealingConfiguration configuration) {
        this.configuration = configuration;
    }

    void publish(HealingReport report) {
        ShaftHeal.record(report);
        String content;
        try {
            content = JSON.writerWithDefaultPrettyPrinter().writeValueAsString(report) + "\n";
        } catch (JsonProcessingException exception) {
            ReportManager.logDiscrete("SHAFT Heal report serialization failed.");
            return;
        }
        Allure.addAttachment(
                "SHAFT Heal " + report.attemptId(),
                "application/json",
                new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8)),
                ".json");
        writeLocal(report.attemptId(), content);
        ReportManager.logDiscrete("SHAFT Heal decision: " + report.decision().status()
                + " for " + report.originalLocator() + ".");
    }

    private void writeLocal(String attemptId, String content) {
        try {
            Path parent = configuration.historyPath().getParent();
            if (parent == null) {
                return;
            }
            Path directory = parent.resolve("reports");
            Files.createDirectories(directory);
            Files.writeString(directory.resolve(attemptId + ".json"), content, StandardCharsets.UTF_8);
        } catch (IOException ignored) {
            // Allure remains the primary report path.
        }
    }
}
