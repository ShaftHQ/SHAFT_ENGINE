package com.shaft.doctor.repair;

import com.shaft.doctor.internal.DoctorHashing;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

final class RepairGuard {
    private static final Set<String> BINARY_EXTENSIONS = Set.of(
            "7z", "avi", "bin", "bmp", "class", "dll", "doc", "docx", "exe", "gif",
            "gz", "ico", "jar", "jpeg", "jpg", "mov", "mp3", "mp4", "pdf", "png",
            "ppt", "pptx", "so", "tar", "war", "webp", "xls", "xlsx", "zip");
    private static final Set<String> SECRET_NAMES = Set.of(
            ".env", ".npmrc", ".pypirc", "credentials", "credentials.json", "id_dsa",
            "id_ed25519", "id_rsa", "keystore.jks", "settings.xml");
    private static final List<String> FORBIDDEN_PATH_PREFIXES = List.of(
            ".git", ".github/workflows", "target", "build", "out", "node_modules");
    private static final List<String> SECRET_MARKERS = List.of(
            "-----BEGIN PRIVATE KEY-----", "-----BEGIN OPENSSH PRIVATE KEY-----",
            "ghp_", "github_pat_", "AKIA", "Authorization: Bearer ", "\"client_secret\":");
    private static final Set<String> ALLOWED_GOALS = Set.of(
            "compile", "test-compile", "test", "verify", "package", "install",
            "javadoc:javadoc", "surefire:test", "failsafe:integration-test", "failsafe:verify");

    List<Path> allowedPaths(Path repository, List<String> values) {
        List<Path> resolved = new ArrayList<>();
        for (String value : values) {
            Path relative = relative(value, "Approved path");
            rejectProtected(relative);
            resolved.add(repository.resolve(relative).normalize());
        }
        return List.copyOf(resolved);
    }

    GuardedPatch guard(
            Path repository,
            List<Path> allowedPaths,
            DoctorRepairRequest.FilePatch patch,
            long remainingBytes,
            Set<String> allowedEvidenceIds) {
        Path relative = relative(patch.path(), "Patch path");
        rejectProtected(relative);
        Path target = repository.resolve(relative).normalize();
        if (!target.startsWith(repository)
                || allowedPaths.stream().noneMatch(allowed ->
                target.equals(allowed) || target.startsWith(allowed))) {
            throw new IllegalArgumentException("Patch path is outside the approved file allowlist.");
        }
        rejectSymlink(repository, target);
        rejectBinary(target);
        boolean exists = Files.exists(target, LinkOption.NOFOLLOW_LINKS);
        if (exists && !Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS)) {
            throw new IllegalArgumentException("Patch targets must be regular files.");
        }
        if (patch.operation() == DoctorRepairRequest.FilePatch.Operation.CREATE && exists) {
            throw new IllegalArgumentException("Create patch targets an existing file.");
        }
        if (patch.operation() != DoctorRepairRequest.FilePatch.Operation.CREATE && !exists) {
            throw new IllegalArgumentException("Replace or delete patch targets a missing file.");
        }
        if (!allowedEvidenceIds.containsAll(patch.evidenceIds())) {
            throw new IllegalArgumentException("Patch references evidence outside the diagnosis or bundle.");
        }
        byte[] content = patch.content().getBytes(StandardCharsets.UTF_8);
        if (content.length > remainingBytes) {
            throw new IllegalArgumentException("Doctor repair patch exceeds the approved size limit.");
        }
        if (containsNul(content)) {
            throw new IllegalArgumentException("Binary patch content is not allowed.");
        }
        rejectSecrets(patch.content());
        return new GuardedPatch(relative, target, content,
                DoctorHashing.sha256(content), content.length);
    }

    List<String> validationCommand(
            DoctorRepairRequest.ValidationCommand requested,
            boolean networkApproved) {
        List<String> command = new ArrayList<>(requested.arguments());
        String requestedExecutable = command.getFirst().replace('\\', '/').toLowerCase(Locale.ROOT);
        String executable = Path.of(command.getFirst()).getFileName().toString().toLowerCase(Locale.ROOT);
        if (!Set.of("mvn", "mvn.cmd", "mvnw", "mvnw.cmd").contains(executable)) {
            throw new IllegalArgumentException("Only tokenized Maven validation commands are allowed.");
        }
        if (requestedExecutable.contains("/")
                && !Set.of("./mvnw", "./mvnw.cmd").contains(requestedExecutable)) {
            throw new IllegalArgumentException(
                    "Maven validation executable paths must use the repository wrapper.");
        }
        boolean hasGoal = false;
        boolean runsTests = false;
        boolean headless = false;
        for (int index = 1; index < command.size(); index++) {
            String argument = command.get(index);
            if (argument == null || argument.isBlank() || argument.length() > 2_000
                    || containsShellSyntax(argument)) {
                throw new IllegalArgumentException("Validation command contains an unsafe argument.");
            }
            String normalized = argument.toLowerCase(Locale.ROOT);
            if (normalized.equals("-f") || normalized.startsWith("--file")
                    || normalized.equals("-s") || normalized.startsWith("--settings")
                    || normalized.equals("-gs") || normalized.startsWith("--global-settings")
                    || normalized.equals("-t") || normalized.startsWith("--toolchains")
                    || normalized.startsWith("-dmaven.ext.class.path")
                    || normalized.startsWith("-dmaven.multimoduleprojectdirectory")) {
                throw new IllegalArgumentException(
                        "Validation commands cannot redirect Maven outside the approved worktree.");
            }
            if (normalized.startsWith("-p") && normalized.contains("release")) {
                throw new IllegalArgumentException("Release Maven profiles are forbidden.");
            }
            if (normalized.startsWith("-dheadlessexecution=")) {
                if (!"-dheadlessexecution=true".equals(normalized)) {
                    throw new IllegalArgumentException("Doctor browser validation must run headlessly.");
                }
                headless = true;
            }
            if (!argument.startsWith("-")) {
                if (normalized.contains("deploy") || normalized.contains("release")
                        || normalized.startsWith("scm:") || normalized.startsWith("versions:")) {
                    throw new IllegalArgumentException("Release and destructive Maven goals are forbidden.");
                }
                if (ALLOWED_GOALS.contains(normalized)) {
                    hasGoal = true;
                    runsTests |= normalized.contains("test") || Set.of("verify", "package", "install")
                            .contains(normalized);
                } else if (!previousOptionTakesValue(command, index)) {
                    throw new IllegalArgumentException("Maven validation goal is not allowlisted: " + argument);
                }
            }
        }
        if (!hasGoal) {
            throw new IllegalArgumentException("Validation command requires an allowlisted Maven goal.");
        }
        if (runsTests && !headless) {
            command.add("-DheadlessExecution=true");
        }
        if (!networkApproved && command.stream().noneMatch(value ->
                value.equals("-o") || value.equals("--offline"))) {
            command.add("--offline");
        }
        return List.copyOf(command);
    }

    static boolean expectsAllure(List<String> command) {
        boolean skipped = command.stream()
                .map(value -> value.toLowerCase(Locale.ROOT))
                .anyMatch(value -> value.equals("-dskiptests")
                        || value.equals("-dskiptests=true")
                        || value.equals("-dmaven.test.skip=true"));
        return !skipped && command.stream()
                .map(value -> value.toLowerCase(Locale.ROOT))
                .anyMatch(value -> value.equals("test") || value.equals("verify")
                        || value.equals("package") || value.equals("install")
                        || value.equals("surefire:test")
                        || value.equals("failsafe:integration-test")
                        || value.equals("failsafe:verify"));
    }

    private static boolean previousOptionTakesValue(List<String> command, int index) {
        if (index <= 1) {
            return false;
        }
        String option = command.get(index - 1).toLowerCase(Locale.ROOT);
        if (Set.of("-pl", "--projects").contains(option)
                && (command.get(index).contains("..")
                || Path.of(command.get(index)).isAbsolute())) {
            throw new IllegalArgumentException("Maven project selection must remain repository-relative.");
        }
        return Set.of("-pl", "--projects", "-p", "--activate-profiles")
                .contains(command.get(index - 1).toLowerCase(Locale.ROOT));
    }

    private static Path relative(String value, String label) {
        String normalized = value == null ? "" : value.trim().replace('\\', '/');
        Path path;
        try {
            path = Path.of(normalized).normalize();
        } catch (RuntimeException exception) {
            throw new IllegalArgumentException(label + " is invalid.", exception);
        }
        if (normalized.isEmpty() || path.isAbsolute() || path.startsWith("..")
                || normalized.equals("..") || normalized.contains("/../")) {
            throw new IllegalArgumentException(label + " must be repository-relative.");
        }
        return path;
    }

    private static void rejectProtected(Path relative) {
        String normalized = relative.toString().replace('\\', '/').toLowerCase(Locale.ROOT);
        for (String prefix : FORBIDDEN_PATH_PREFIXES) {
            if (normalized.equals(prefix) || normalized.startsWith(prefix + "/")) {
                throw new IllegalArgumentException("Protected or generated paths cannot be repaired.");
            }
        }
        String name = relative.getFileName().toString().toLowerCase(Locale.ROOT);
        if (SECRET_NAMES.contains(name) || name.endsWith(".pem") || name.endsWith(".p12")
                || name.endsWith(".pfx") || name.endsWith(".key")) {
            throw new IllegalArgumentException("Credential-bearing paths cannot be repaired.");
        }
    }

    private static void rejectSymlink(Path root, Path target) {
        Path current = root;
        Path relative = root.relativize(target);
        for (Path segment : relative) {
            current = current.resolve(segment);
            if (Files.isSymbolicLink(current)) {
                throw new IllegalArgumentException("Symlink patch paths are not allowed.");
            }
            if (!Files.exists(current, LinkOption.NOFOLLOW_LINKS)) {
                break;
            }
        }
    }

    private static void rejectBinary(Path target) {
        String name = target.getFileName().toString();
        int separator = name.lastIndexOf('.');
        if (separator >= 0 && BINARY_EXTENSIONS.contains(
                name.substring(separator + 1).toLowerCase(Locale.ROOT))) {
            throw new IllegalArgumentException("Binary file patches are not allowed.");
        }
        if (!Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS)) {
            return;
        }
        try {
            byte[] sample;
            try (var input = Files.newInputStream(target)) {
                sample = input.readNBytes(8_192);
            }
            if (containsNul(sample)) {
                throw new IllegalArgumentException("Binary file patches are not allowed.");
            }
        } catch (IOException exception) {
            throw new IllegalArgumentException("Patch target could not be inspected.", exception);
        }
    }

    private static void rejectSecrets(String content) {
        String normalized = content.toLowerCase(Locale.ROOT);
        for (String marker : SECRET_MARKERS) {
            if (normalized.contains(marker.toLowerCase(Locale.ROOT))) {
                throw new IllegalArgumentException("Patch content contains secret-like material.");
            }
        }
    }

    private static boolean containsNul(byte[] content) {
        for (byte value : content) {
            if (value == 0) {
                return true;
            }
        }
        return false;
    }

    private static boolean containsShellSyntax(String value) {
        return value.contains(";") || value.contains("&&") || value.contains("||")
                || value.contains("|") || value.contains(">") || value.contains("<")
                || value.contains("`") || value.contains("$(")
                || value.contains("\r") || value.contains("\n");
    }

    record GuardedPatch(
            Path relativePath,
            Path target,
            byte[] content,
            String contentSha256,
            long sizeBytes) {
    }
}
