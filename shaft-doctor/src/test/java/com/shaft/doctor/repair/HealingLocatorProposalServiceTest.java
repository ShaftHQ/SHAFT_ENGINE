package com.shaft.doctor.repair;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class HealingLocatorProposalServiceTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @BeforeEach
    void enableSourcePatchProposals() {
        SHAFT.Properties.healing.set().sourcePatchEnabled(true);
    }

    @AfterEach
    void cleanup() {
        SHAFT.Properties.clearForCurrentThread();
    }

    @Test
    void verifiedHealingCreatesProposalWithoutEditingSource(@TempDir Path repository) throws Exception {
        Path source = source(repository, "private static final By LOGIN = By.id(\"old-login\");");
        String original = Files.readString(source, StandardCharsets.UTF_8);
        Path report = report(repository, true, "RECOVERED", "PASSED");
        HealingLocatorProposalService service = new HealingLocatorProposalService();

        HealingLocatorProposal proposal = service.propose(new HealingLocatorProposalRequest(
                repository,
                report,
                "src/test/java/example/LoginTest.java",
                true,
                repository.resolve("target/proposals")));

        assertEquals(original, Files.readString(source, StandardCharsets.UTF_8));
        assertEquals("By.id(\"old-login\")", proposal.originalExpression());
        assertEquals("By.id(\"new-login\")", proposal.proposedExpression());
        assertTrue(proposal.patch().content().contains("By.id(\"new-login\")"));
        assertTrue(Files.isRegularFile(Path.of(proposal.manifestPath())));
        assertTrue(service.isSourceCurrent(repository, proposal));

        Files.writeString(source, original + "\n// changed", StandardCharsets.UTF_8);
        assertFalse(service.isSourceCurrent(repository, proposal));
    }

    @Test
    void proposalRequiresExplicitConsentAndUnambiguousSuccessfulRecovery(@TempDir Path repository)
            throws Exception {
        source(repository, "private static final By LOGIN = By.id(\"old-login\");");
        HealingLocatorProposalService service = new HealingLocatorProposalService();
        Path validReport = report(repository, true, "RECOVERED", "PASSED");

        assertThrows(IllegalArgumentException.class, () -> service.propose(
                new HealingLocatorProposalRequest(
                        repository,
                        validReport,
                        "src/test/java/example/LoginTest.java",
                        false,
                        repository.resolve("target/no-consent"))));

        Path ambiguousReport = report(repository, false, "AMBIGUOUS", "FAILED");
        assertThrows(IllegalArgumentException.class, () -> service.propose(
                new HealingLocatorProposalRequest(
                        repository,
                        ambiguousReport,
                        "src/test/java/example/LoginTest.java",
                        true,
                        repository.resolve("target/ambiguous"))));
    }

    @Test
    void disabledSourcePatchConfigurationBlocksProposal(@TempDir Path repository) throws Exception {
        source(repository, "private static final By LOGIN = By.id(\"old-login\");");
        Path validReport = report(repository, true, "RECOVERED", "PASSED");
        SHAFT.Properties.healing.set().sourcePatchEnabled(false);

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> new HealingLocatorProposalService().propose(new HealingLocatorProposalRequest(
                        repository,
                        validReport,
                        "src/test/java/example/LoginTest.java",
                        true,
                        repository.resolve("target/disabled"))));

        assertTrue(failure.getMessage().contains("healing.sourcePatch.enabled"));
    }

    @Test
    void mobileAccessibilityLocatorCreatesReviewableProposal(@TempDir Path repository)
            throws Exception {
        Path source = source(repository,
                "private static final By LOGIN = AppiumBy.accessibilityId(\"old-login\");");
        String original = Files.readString(source, StandardCharsets.UTF_8);
        Path report = report(
                repository,
                true,
                "RECOVERED",
                "PASSED",
                "AppiumBy.accessibilityId: old-login",
                "AppiumBy.accessibilityId: new-login");

        HealingLocatorProposal proposal = new HealingLocatorProposalService().propose(
                new HealingLocatorProposalRequest(
                        repository,
                        report,
                        "src/test/java/example/LoginTest.java",
                        true,
                        repository.resolve("target/mobile")));

        assertEquals(original, Files.readString(source, StandardCharsets.UTF_8));
        assertEquals(
                "AppiumBy.accessibilityId(\"new-login\")",
                proposal.proposedExpression());
    }

    @Test
    void dynamicLocatorConstructionShouldBeRejected(@TempDir Path repository) throws Exception {
        source(repository, "private static final By LOGIN = By.id(BASE + \"-login\");");
        Path validReport = report(repository, true, "RECOVERED", "PASSED");

        assertThrows(IllegalArgumentException.class, () -> new HealingLocatorProposalService().propose(
                new HealingLocatorProposalRequest(
                        repository,
                        validReport,
                        "src/test/java/example/LoginTest.java",
                        true,
                        repository.resolve("target/dynamic"))));
    }

    @Test
    void pathUnsafeAttemptIdentifierShouldBeRejected(@TempDir Path repository) throws Exception {
        source(repository, "private static final By LOGIN = By.id(\"old-login\");");
        Path report = report(repository, true, "RECOVERED", "PASSED");
        ObjectNode payload = (ObjectNode) JSON.readTree(report.toFile());
        payload.put("attemptId", "../escape");
        Files.writeString(report, JSON.writeValueAsString(payload), StandardCharsets.UTF_8);

        assertThrows(IllegalArgumentException.class, () -> new HealingLocatorProposalService().propose(
                new HealingLocatorProposalRequest(
                        repository,
                        report,
                        "src/test/java/example/LoginTest.java",
                        true,
                        repository.resolve("target/unsafe"))));
    }

    private static Path source(Path repository, String locatorLine) throws Exception {
        Path source = repository.resolve("src/test/java/example/LoginTest.java");
        Files.createDirectories(source.getParent());
        Files.writeString(source, """
                package example;

                import org.openqa.selenium.By;

                class LoginTest {
                    %s
                }
                """.formatted(locatorLine), StandardCharsets.UTF_8);
        return source;
    }

    private static Path report(
            Path repository,
            boolean unique,
            String decisionStatus,
            String actionOutcome) throws Exception {
        return report(
                repository,
                unique,
                decisionStatus,
                actionOutcome,
                "By.id: old-login",
                "By.id: new-login");
    }

    private static Path report(
            Path repository,
            boolean unique,
            String decisionStatus,
            String actionOutcome,
            String originalLocator,
            String proposedLocator) throws Exception {
        ObjectNode report = JSON.createObjectNode();
        report.put("schemaVersion", "2.0");
        report.put("attemptId", "attempt-2859");
        report.put("originalLocator", originalLocator);
        ObjectNode candidate = report.putArray("candidates").addObject();
        candidate.put("candidateId", "candidate-1");
        candidate.put("proposedLocator", proposedLocator);
        candidate.put("unique", unique);
        candidate.put("contextMatched", true);
        candidate.putArray("evidence").add("test-id exact match");
        ObjectNode decision = report.putObject("decision");
        decision.put("status", decisionStatus);
        decision.put("selectedCandidateId", "candidate-1");
        decision.put("confidence", 0.96);
        ObjectNode action = report.putObject("action");
        action.put("outcome", actionOutcome);
        action.put("postActionVerification",
                "PASSED".equals(actionOutcome) ? "ELEMENT_INTERACTABLE" : "FAILED");
        Path path = repository.resolve("healing-report-" + decisionStatus.toLowerCase() + ".json");
        Files.writeString(path, JSON.writerWithDefaultPrettyPrinter().writeValueAsString(report),
                StandardCharsets.UTF_8);
        return path;
    }
}
