package com.shaft.mcp;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.List;

/**
 * Local MCP workspace boundary for user-supplied filesystem paths.
 */
final class McpWorkspacePolicy {
    private final Path root;

    private McpWorkspacePolicy(Path root) {
        this.root = realDirectory(root);
    }

    /**
     * Creates a policy from the effective MCP runtime root.
     *
     * @return effective workspace policy
     */
    static McpWorkspacePolicy current() {
        return new McpWorkspacePolicy(McpRuntimePaths.currentRoot());
    }

    /**
     * Creates a policy for tests or explicitly scoped services.
     *
     * @param root workspace root
     * @return policy for the supplied root
     */
    static McpWorkspacePolicy of(Path root) {
        return new McpWorkspacePolicy(root);
    }

    /**
     * Returns the resolved workspace root.
     *
     * @return real workspace root
     */
    Path root() {
        return root;
    }

    /**
     * Resolves an existing path inside the workspace.
     *
     * @param value user path
     * @param label diagnostic label
     * @return real path under the workspace root
     */
    Path existing(String value, String label) {
        Path candidate = resolveRaw(value, label);
        try {
            Path real = candidate.toRealPath();
            if (!real.startsWith(root)) {
                throw outside(label);
            }
            return real;
        } catch (IOException exception) {
            throw new IllegalArgumentException(label + " cannot be resolved inside the MCP workspace.", exception);
        }
    }

    /**
     * Resolves an output path inside the workspace; the final path may not exist yet.
     *
     * @param value user path
     * @param label diagnostic label
     * @return normalized output path under the workspace root
     */
    Path output(String value, String label) {
        Path candidate = resolveRaw(value, label);
        Path ancestor = candidate;
        while (ancestor != null && !Files.exists(ancestor, LinkOption.NOFOLLOW_LINKS)) {
            ancestor = ancestor.getParent();
        }
        if (ancestor == null) {
            throw new IllegalArgumentException(label + " has no resolvable workspace ancestor.");
        }
        try {
            Path realAncestor = ancestor.toRealPath();
            if (!realAncestor.startsWith(root)) {
                throw outside(label);
            }
            Path resolved = realAncestor.resolve(ancestor.relativize(candidate)).normalize();
            if (!resolved.startsWith(root)) {
                throw outside(label);
            }
            return resolved;
        } catch (IOException exception) {
            throw new IllegalArgumentException(label + " cannot be resolved inside the MCP workspace.", exception);
        }
    }

    /**
     * Resolves optional existing paths inside the workspace.
     *
     * @param values path values
     * @param label diagnostic label
     * @return resolved paths
     */
    List<Path> existingList(List<String> values, String label) {
        if (values == null) {
            return List.of();
        }
        return values.stream()
                .map(value -> existing(value, label))
                .toList();
    }

    /**
     * Resolves repository-relative approved source paths and rejects escapes.
     *
     * @param repository repository root already inside workspace
     * @param values approved source paths
     * @return normalized repository-relative paths
     */
    List<String> sourceAllowlist(Path repository, List<String> values) {
        if (values == null) {
            return List.of();
        }
        Path realRepository = realDirectory(repository);
        if (!realRepository.startsWith(root)) {
            throw outside("Repository root");
        }
        return values.stream()
                .map(value -> sourcePath(realRepository, value))
                .toList();
    }

    private String sourcePath(Path repository, String value) {
        String normalized = value == null ? "" : value.trim().replace('\\', '/');
        if (normalized.isBlank()) {
            throw new IllegalArgumentException("Approved source path is required.");
        }
        Path relative = Path.of(normalized).normalize();
        if (relative.isAbsolute() || relative.startsWith("..")) {
            throw outside("Approved source path");
        }
        Path resolved = repository.resolve(relative).normalize();
        if (!resolved.startsWith(repository) || !resolved.startsWith(root)) {
            throw outside("Approved source path");
        }
        return relative.toString().replace('\\', '/');
    }

    private Path resolveRaw(String value, String label) {
        String normalized = value == null ? "" : value.trim();
        if (normalized.isBlank()) {
            throw new IllegalArgumentException(label + " is required.");
        }
        Path path = Path.of(normalized);
        Path resolved = path.isAbsolute() ? path : root.resolve(path);
        resolved = resolved.toAbsolutePath().normalize();
        if (!resolved.startsWith(root)) {
            throw outside(label);
        }
        return resolved;
    }

    private static Path realDirectory(Path path) {
        try {
            Path real = path.toAbsolutePath().normalize().toRealPath();
            if (!Files.isDirectory(real)) {
                throw new IllegalArgumentException("MCP workspace root must be a directory.");
            }
            return real;
        } catch (IOException exception) {
            throw new IllegalArgumentException("MCP workspace root cannot be resolved.", exception);
        }
    }

    private static IllegalArgumentException outside(String label) {
        return new IllegalArgumentException(label + " is outside the MCP workspace.");
    }
}
