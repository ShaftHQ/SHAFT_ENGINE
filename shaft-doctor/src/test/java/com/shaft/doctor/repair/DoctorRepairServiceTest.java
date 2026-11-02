package com.shaft.doctor.repair;

import com.shaft.doctor.format.DoctorJsonCodec;
import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.Diagnosis;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DoctorRepairServiceTest {
    @Test
    void proposalUsesIsolatedWorktreeAndRequiresSeparateApproval(@TempDir Path temp) throws Exception {
        RepositoryFixture fixture = repository(temp);
        Files.writeString(fixture.repository().resolve("README.md"), "user dirty change\n");
        String statusBefore = git(fixture.repository(), "status", "--porcelain", "--untracked-files=no");
        RecordingRunner runner = new RecordingRunner(false);
        DoctorRepairService service = service(runner);

        RepairProposal proposal = service.propose(request(
                fixture,
                List.of(new DoctorRepairRequest.FilePatch(
                        "src/Example.java",
                        DoctorRepairRequest.FilePatch.Operation.REPLACE,
                        "final class Example { int value() { return 2; } }\n",
                        "Correct the diagnosed assertion behavior.",
                        List.of()),
                        new DoctorRepairRequest.FilePatch(
                                "src/NewExample.java",
                                DoctorRepairRequest.FilePatch.Operation.CREATE,
                                "final class NewExample {}\n",
                                "Add the reviewed regression fixture.",
                                List.of())),
                List.of(
                        command("mvn", "-pl", "shaft-doctor", "-am", "test",
                                "-Dtest=DoctorRepairServiceTest"),
                        command("mvn", "-pl", "shaft-doctor", "-am", "compile", "-DskipTests")),
                10_000));

        assertEquals("main", git(fixture.repository(), "branch", "--show-current").trim());
        assertEquals(fixture.baseSha(), git(fixture.repository(), "rev-parse", "HEAD").trim());
        assertEquals(statusBefore, git(fixture.repository(), "status", "--porcelain", "--untracked-files=no"));
        assertTrue(Files.readString(fixture.repository().resolve("src/Example.java")).contains("return 1"));
        assertTrue(proposal.unifiedDiff().contains("return 2"));
        assertTrue(proposal.unifiedDiff().contains("src/NewExample.java"));
        assertTrue(proposal.validationPassed());
        assertEquals(1, proposal.validationResults().getFirst().allureResultCount());
        assertTrue(proposal.validationPlan().getFirst().contains("-DheadlessExecution=true"));
        assertTrue(proposal.validationPlan().getFirst().contains("--offline"));
        assertTrue(Files.isRegularFile(Path.of(proposal.manifestPath())));

        assertThrows(IllegalArgumentException.class, () -> service.publishDraft(
                new DoctorRepairPublicationRequest(
                        Path.of(proposal.manifestPath()), false, proposal.approvalToken(),
                        false, "", "")));
        assertThrows(IllegalArgumentException.class, () -> service.publishDraft(
                new DoctorRepairPublicationRequest(
                        Path.of(proposal.manifestPath()), true, "wrong-token",
                        false, "", "")));
        service.cancel(Path.of(proposal.manifestPath()), proposal.approvalToken());
        assertFalse(Files.exists(Path.of(proposal.worktreePath())));
    }

    @Test
    void unsafePathsContentSizesAndCommandsAreRejected(@TempDir Path temp) throws Exception {
        RepositoryFixture fixture = repository(temp);
        DoctorRepairService service = service(new RecordingRunner(false));

        assertThrows(IllegalArgumentException.class, () -> service.propose(request(
                fixture,
                List.of(patch("../outside.java", "class Outside {}")),
                List.of(command("mvn", "compile")),
                10_000)));
        assertThrows(IllegalArgumentException.class, () -> service.propose(request(
                fixture,
                List.of(patch(".github/workflows/release.yml", "name: release")),
                List.of(command("mvn", "compile")),
                10_000)));
        assertThrows(IllegalArgumentException.class, () -> service.propose(request(
                fixture,
                List.of(patch("src/Example.java", "Authorization: Bearer secret-value")),
                List.of(command("mvn", "compile")),
                10_000)));
        assertThrows(IllegalArgumentException.class, () -> service.propose(request(
                fixture,
                List.of(patch("src/Example.java", "oversized")),
                List.of(command("mvn", "compile")),
                2)));
        assertThrows(IllegalArgumentException.class, () -> service.propose(request(
                fixture,
                List.of(patch("src/data.bin", "not really binary")),
                List.of(command("mvn", "compile")),
                10_000)));
        assertThrows(IllegalArgumentException.class, () -> service.propose(request(
                fixture,
                List.of(patch("src/Example.java", "class Example {}")),
                List.of(command("sh", "-c", "mvn test")),
                10_000)));
        assertThrows(IllegalArgumentException.class, () -> service.propose(request(
                fixture,
                List.of(patch("src/Example.java", "class Example {}")),
                List.of(command("mvn", "test", "&&", "git", "clean", "-fdx")),
                10_000)));
    }

    @Test
    void failedValidationRequiresRecordedOverrideAndPublicationIsIdempotent(@TempDir Path temp)
            throws Exception {
        RepositoryFixture fixture = repository(temp);
        RecordingRunner runner = new RecordingRunner(true);
        DoctorRepairService service = service(runner);
        RepairProposal proposal = service.propose(request(
                fixture,
                List.of(patch(
                        "src/Example.java",
                        "final class Example { int value() { return 3; } }\n")),
                List.of(command("mvn", "test", "-Dtest=DoctorRepairServiceTest")),
                10_000));
        assertFalse(proposal.validationPassed());

        Path manifest = Path.of(proposal.manifestPath());
        assertThrows(IllegalArgumentException.class, () -> service.publishDraft(
                new DoctorRepairPublicationRequest(
                        manifest, true, proposal.approvalToken(), false, "", "")));

        RepairPublicationResult published = service.publishDraft(
                new DoctorRepairPublicationRequest(
                        manifest,
                        true,
                        proposal.approvalToken(),
                        true,
                        "The focused fixture intentionally exercises the override contract.",
                        "Draft: reviewed Doctor repair"));

        assertEquals("https://github.com/example/repository/pull/42", published.pullRequestUrl());
        assertFalse(published.reusedExisting());
        assertFalse(Files.exists(Path.of(proposal.worktreePath())));
        assertTrue(git(fixture.repository(), "branch", "--list", published.branchName())
                .contains(published.branchName()));
        assertTrue(runner.commands().stream().anyMatch(arguments ->
                arguments.size() > 3
                        && arguments.getFirst().equals("gh")
                        && arguments.contains("--draft")
                        && arguments.contains("create")));
        assertFalse(runner.commands().stream().anyMatch(arguments ->
                arguments.contains("merge") || arguments.contains("--ready")));

        RepairPublicationResult reused = service.publishDraft(
                new DoctorRepairPublicationRequest(
                        manifest, true, proposal.approvalToken(), true, "same override", ""));
        assertTrue(reused.reusedExisting());
        assertEquals(published.pullRequestUrl(), reused.pullRequestUrl());
    }

    @Test
    void interruptedPublicationReusesReviewedCommitOnRetry(@TempDir Path temp) throws Exception {
        RepositoryFixture fixture = repository(temp);
        RecordingRunner runner = new RecordingRunner(false, true);
        DoctorRepairService service = service(runner);
        RepairProposal proposal = service.propose(request(
                fixture,
                List.of(patch(
                        "src/Example.java",
                        "final class Example { int value() { return 4; } }\n")),
                List.of(command("mvn", "test", "-Dtest=DoctorRepairServiceTest")),
                10_000));
        DoctorRepairPublicationRequest publication = new DoctorRepairPublicationRequest(
                Path.of(proposal.manifestPath()), true, proposal.approvalToken(), false, "", "");

        assertThrows(IllegalStateException.class, () -> service.publishDraft(publication));
        assertFalse(git(Path.of(proposal.worktreePath()), "rev-parse", "HEAD").trim()
                .equals(proposal.baseSha()));

        RepairPublicationResult retried = service.publishDraft(publication);

        assertEquals("https://github.com/example/repository/pull/42", retried.pullRequestUrl());
        assertFalse(Files.exists(Path.of(proposal.worktreePath())));
    }

    @Test
    void detachedConcurrentAndStaleWorktreesRemainIsolated(@TempDir Path temp) throws Exception {
        RepositoryFixture fixture = repository(temp);
        git(fixture.repository(), "checkout", "--detach", fixture.baseSha());
        DoctorRepairService service = service(new RecordingRunner(false));
        RepairProposal first = service.propose(request(
                fixture,
                List.of(patch(
                        "src/Example.java",
                        "final class Example { int value() { return 5; } }\n")),
                List.of(command("mvn", "test", "-Dtest=DoctorRepairServiceTest")),
                10_000));
        RepairProposal concurrent = service.propose(request(
                fixture,
                List.of(patch(
                        "src/Example.java",
                        "final class Example { int value() { return 6; } }\n")),
                List.of(command("mvn", "compile")),
                10_000));

        assertTrue(git(fixture.repository(), "branch", "--show-current").isBlank());
        assertEquals(fixture.baseSha(), git(fixture.repository(), "rev-parse", "HEAD").trim());
        assertTrue(Files.isDirectory(Path.of(first.worktreePath())));
        assertTrue(Files.isDirectory(Path.of(concurrent.worktreePath())));

        service.cancel(Path.of(concurrent.manifestPath()), concurrent.approvalToken());
        Files.setLastModifiedTime(
                Path.of(first.worktreePath()),
                FileTime.from(java.time.Instant.now().minus(Duration.ofDays(2))));
        RepairProposal recovered = service.propose(request(
                fixture,
                List.of(patch(
                        "src/Example.java",
                        "final class Example { int value() { return 7; } }\n")),
                List.of(command("mvn", "compile")),
                10_000));

        assertFalse(Files.exists(Path.of(first.worktreePath())));
        assertTrue(Files.isDirectory(Path.of(recovered.worktreePath())));
        service.cancel(Path.of(recovered.manifestPath()), recovered.approvalToken());
    }

    private static DoctorRepairService service(RecordingRunner runner) {
        return new DoctorRepairService(
                runner, new RepairProposalStore(), new RepairGuard(), new DoctorJsonCodec());
    }

    private static DoctorRepairRequest request(
            RepositoryFixture fixture,
            List<DoctorRepairRequest.FilePatch> patches,
            List<DoctorRepairRequest.ValidationCommand> commands,
            long maxBytes) {
        return new DoctorRepairRequest(
                fixture.repository(),
                fixture.baseSha(),
                fixture.diagnosis(),
                null,
                "2857",
                List.of("src"),
                patches,
                commands,
                false,
                fixture.output(),
                maxBytes,
                Duration.ofSeconds(30));
    }

    private static DoctorRepairRequest.FilePatch patch(String path, String content) {
        return new DoctorRepairRequest.FilePatch(
                path,
                DoctorRepairRequest.FilePatch.Operation.REPLACE,
                content,
                "Apply a reviewed repair.",
                List.of());
    }

    private static DoctorRepairRequest.ValidationCommand command(String... arguments) {
        return new DoctorRepairRequest.ValidationCommand(List.of(arguments));
    }

    private static RepositoryFixture repository(Path temp) throws Exception {
        Path bare = temp.resolve("remote.git");
        git(temp, "init", "--bare", bare.toString());
        Path repository = Files.createDirectories(temp.resolve("repository"));
        git(repository, "init", "-b", "main");
        git(repository, "config", "user.name", "Doctor Test");
        git(repository, "config", "user.email", "doctor@example.invalid");
        Files.createDirectories(repository.resolve("src"));
        Files.createDirectories(repository.resolve(".github/workflows"));
        Files.writeString(repository.resolve("src/Example.java"),
                "final class Example { int value() { return 1; } }\n");
        Files.write(repository.resolve("src/data.bin"), new byte[]{0, 1, 2, 3});
        Files.writeString(repository.resolve("README.md"), "clean\n");
        Files.writeString(repository.resolve(".github/workflows/release.yml"), "name: release\n");
        git(repository, "add", ".");
        git(repository, "commit", "-m", "initial");
        git(repository, "remote", "add", "origin", bare.toString());
        git(repository, "push", "-u", "origin", "main");
        String sha = git(repository, "rev-parse", "HEAD").trim();
        Path diagnosis = temp.resolve("diagnosis.json");
        new DoctorJsonCodec().write(diagnosis, new Diagnosis(
                Diagnosis.CURRENT_SCHEMA_VERSION,
                CauseCategory.TEST,
                List.of(),
                Confidence.HIGH,
                "A deterministic test defect was identified.",
                "The failure evidence points to the test implementation.",
                List.of(),
                List.of(),
                List.of()));
        return new RepositoryFixture(repository, bare, sha, diagnosis, temp.resolve("proposals"));
    }

    private static String git(Path directory, String... arguments) throws Exception {
        List<String> command = new ArrayList<>(List.of("git"));
        command.addAll(List.of(arguments));
        Process process = new ProcessBuilder(command)
                .directory(directory.toFile())
                .redirectErrorStream(true)
                .start();
        String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        if (process.waitFor() != 0) {
            throw new IllegalStateException(output);
        }
        return output;
    }

    private record RepositoryFixture(
            Path repository,
            Path remote,
            String baseSha,
            Path diagnosis,
            Path output) {
    }

    private static final class RecordingRunner extends RepairProcessRunner {
        private final boolean failValidation;
        private boolean failGhListOnce;
        private final List<List<String>> commands = new ArrayList<>();

        private RecordingRunner(boolean failValidation) {
            this(failValidation, false);
        }

        private RecordingRunner(boolean failValidation, boolean failGhListOnce) {
            this.failValidation = failValidation;
            this.failGhListOnce = failGhListOnce;
        }

        @Override
        ProcessResult run(List<String> command, Path directory, Duration timeout) {
            commands.add(List.copyOf(command));
            String executable = Path.of(command.getFirst()).getFileName().toString();
            if (executable.equalsIgnoreCase("mvn") || executable.equalsIgnoreCase("mvn.cmd")
                    || executable.equalsIgnoreCase("mvnw") || executable.equalsIgnoreCase("mvnw.cmd")) {
                if (failValidation) {
                    return new ProcessResult(1, false, "Focused validation failed.");
                }
                if (RepairGuard.expectsAllure(command)) {
                    try {
                        Path allure = Files.createDirectories(
                                directory.resolve("shaft-doctor/target/allure-results"));
                        Files.writeString(allure.resolve("repair-result.json"),
                                "{\"name\":\"repair\",\"status\":\"passed\"}\n");
                    } catch (IOException exception) {
                        throw new IllegalStateException(exception);
                    }
                }
                return new ProcessResult(0, false, "Validation completed.");
            }
            if (command.getFirst().equals("gh") && command.contains("list")) {
                if (failGhListOnce) {
                    failGhListOnce = false;
                    return new ProcessResult(1, false, "Temporary GitHub inspection failure.");
                }
                return new ProcessResult(0, false, "[]");
            }
            if (command.getFirst().equals("gh") && command.contains("create")) {
                return new ProcessResult(
                        0, false, "https://github.com/example/repository/pull/42\n");
            }
            return super.run(command, directory, timeout);
        }

        private List<List<String>> commands() {
            return List.copyOf(commands);
        }
    }
}
