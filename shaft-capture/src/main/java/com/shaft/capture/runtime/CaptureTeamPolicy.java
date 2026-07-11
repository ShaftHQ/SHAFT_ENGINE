package com.shaft.capture.runtime;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;

/**
 * Team recorder policy checked into the repository at {@code .shaft/recorder-policy.json}
 * (issue #3425 C4). When present, every recording started against that workspace — from the
 * IntelliJ plugin, an agent CLI, or a raw MCP call — honors the same team defaults, so recordings
 * land in a consistent place with consistent privacy/browser settings across the whole team.
 *
 * <p>Schema (all fields optional):</p>
 * <pre>{@code
 * {
 *   "headless": false,           // lock recordings to headed/headless
 *   "outputDirectory": "recordings", // workspace-relative directory every recording lands in
 *   "browser": "Chrome"          // default browser when a request does not name one
 * }
 * }</pre>
 */
public record CaptureTeamPolicy(
        Optional<Boolean> headless,
        String outputDirectory,
        String browser) {
    /** Workspace-relative location of the policy file. */
    public static final String POLICY_PATH = ".shaft/recorder-policy.json";
    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final CaptureTeamPolicy EMPTY = new CaptureTeamPolicy(Optional.empty(), "", "");

    /**
     * Creates an immutable policy.
     */
    public CaptureTeamPolicy {
        headless = headless == null ? Optional.empty() : headless;
        outputDirectory = outputDirectory == null ? "" : outputDirectory.trim();
        browser = browser == null ? "" : browser.trim();
    }

    /**
     * Loads the policy for a workspace root; an absent or unreadable file yields the empty policy
     * so recording never breaks because of a malformed policy — it just falls back to defaults.
     *
     * @param workspaceRoot workspace root directory
     * @return the parsed policy, or an empty policy
     */
    public static CaptureTeamPolicy load(Path workspaceRoot) {
        if (workspaceRoot == null) {
            return EMPTY;
        }
        Path policyFile = workspaceRoot.resolve(POLICY_PATH);
        if (!Files.isRegularFile(policyFile)) {
            return EMPTY;
        }
        try {
            JsonNode json = MAPPER.readTree(policyFile.toFile());
            return new CaptureTeamPolicy(
                    json.hasNonNull("headless") && json.get("headless").isBoolean()
                            ? Optional.of(json.get("headless").asBoolean())
                            : Optional.empty(),
                    json.path("outputDirectory").asText(""),
                    json.path("browser").asText(""));
        } catch (Exception malformedPolicy) {
            return EMPTY;
        }
    }

    /**
     * Whether the policy file declared anything at all.
     *
     * @return true when at least one field is set
     */
    public boolean present() {
        return headless.isPresent() || !outputDirectory.isBlank() || !browser.isBlank();
    }

    /**
     * Re-roots a requested output path into the policy's output directory, preserving the
     * requested file name. Blank requests get a caller-supplied default file name.
     *
     * @param requestedOutputPath the caller's output path, possibly blank
     * @param defaultFileName file name used when the request is blank
     * @return workspace-relative output path honoring the policy directory, or the original
     *         request when the policy declares no directory
     */
    public String applyOutputDirectory(String requestedOutputPath, String defaultFileName) {
        String requested = requestedOutputPath == null ? "" : requestedOutputPath.trim();
        if (outputDirectory.isBlank()) {
            return requested;
        }
        String fileName = requested.isBlank()
                ? defaultFileName
                : Path.of(requested).getFileName().toString();
        return outputDirectory + "/" + fileName;
    }
}
