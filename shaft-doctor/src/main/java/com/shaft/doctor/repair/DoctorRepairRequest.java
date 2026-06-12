package com.shaft.doctor.repair;

import java.nio.file.Path;
import java.time.Duration;
import java.util.List;

/**
 * Explicit policy for creating an isolated Doctor repair proposal.
 *
 * @param repositoryRoot approved Git repository root
 * @param baseSha exact commit used to create the isolated worktree
 * @param diagnosisPath Doctor diagnosis or combined report JSON
 * @param evidenceBundlePath optional Doctor evidence bundle JSON
 * @param issueReference issue number, URL, or session identifier
 * @param allowedPaths repository-relative files or directories that may change
 * @param patches structured file patches; shell commands are not accepted
 * @param validationCommands approved Maven validation commands
 * @param networkValidationApproved whether Maven validation may use the network
 * @param outputDirectory proposal manifests and temporary worktrees
 * @param maxPatchBytes maximum aggregate UTF-8 patch content
 * @param commandTimeout maximum duration for each validation command
 */
public record DoctorRepairRequest(
        Path repositoryRoot,
        String baseSha,
        Path diagnosisPath,
        Path evidenceBundlePath,
        String issueReference,
        List<String> allowedPaths,
        List<FilePatch> patches,
        List<ValidationCommand> validationCommands,
        boolean networkValidationApproved,
        Path outputDirectory,
        long maxPatchBytes,
        Duration commandTimeout) {
    /**
     * Conservative default aggregate patch limit.
     */
    public static final long DEFAULT_MAX_PATCH_BYTES = 512 * 1024;

    /**
     * Conservative default validation timeout.
     */
    public static final Duration DEFAULT_COMMAND_TIMEOUT = Duration.ofMinutes(15);

    /**
     * Creates a validated immutable request.
     */
    public DoctorRepairRequest {
        if (repositoryRoot == null || diagnosisPath == null || outputDirectory == null) {
            throw new IllegalArgumentException("Repository, diagnosis, and output paths are required.");
        }
        baseSha = required(baseSha, "Base SHA");
        issueReference = required(issueReference, "Issue reference");
        allowedPaths = allowedPaths == null ? List.of() : List.copyOf(allowedPaths);
        patches = patches == null ? List.of() : List.copyOf(patches);
        validationCommands = validationCommands == null ? List.of() : List.copyOf(validationCommands);
        if (allowedPaths.isEmpty()) {
            throw new IllegalArgumentException("At least one approved repair path is required.");
        }
        if (patches.isEmpty()) {
            throw new IllegalArgumentException("At least one structured repair patch is required.");
        }
        if (validationCommands.isEmpty()) {
            throw new IllegalArgumentException("At least one approved validation command is required.");
        }
        if (maxPatchBytes <= 0) {
            maxPatchBytes = DEFAULT_MAX_PATCH_BYTES;
        }
        if (commandTimeout == null || commandTimeout.isNegative() || commandTimeout.isZero()) {
            commandTimeout = DEFAULT_COMMAND_TIMEOUT;
        }
    }

    /**
     * Structured file replacement applied only inside the isolated worktree.
     *
     * @param path approved repository-relative path
     * @param operation create, replace, or delete operation
     * @param content complete UTF-8 file content; empty for delete
     * @param rationale concise reason for the change
     * @param evidenceIds Doctor evidence references supporting the change
     */
    public record FilePatch(
            String path,
            Operation operation,
            String content,
            String rationale,
            List<String> evidenceIds) {
        /**
         * Supported file operations.
         */
        public enum Operation {
            CREATE,
            REPLACE,
            DELETE
        }

        /**
         * Creates a validated structured patch.
         */
        public FilePatch {
            path = required(path, "Patch path").replace('\\', '/');
            operation = operation == null ? Operation.REPLACE : operation;
            content = content == null ? "" : content;
            rationale = required(rationale, "Patch rationale");
            evidenceIds = evidenceIds == null ? List.of() : List.copyOf(evidenceIds);
            if (operation == Operation.DELETE && !content.isEmpty()) {
                throw new IllegalArgumentException("Delete patches cannot include file content.");
            }
        }
    }

    /**
     * Tokenized Maven command; no shell parsing or interpolation is performed.
     *
     * @param arguments executable followed by Maven arguments
     */
    public record ValidationCommand(List<String> arguments) {
        /**
         * Creates a validated command record.
         */
        public ValidationCommand {
            arguments = arguments == null ? List.of() : List.copyOf(arguments);
            if (arguments.isEmpty()) {
                throw new IllegalArgumentException("Validation command arguments are required.");
            }
        }
    }

    private static String required(String value, String label) {
        String normalized = value == null ? "" : value.trim();
        if (normalized.isEmpty()) {
            throw new IllegalArgumentException(label + " is required.");
        }
        return normalized;
    }
}
