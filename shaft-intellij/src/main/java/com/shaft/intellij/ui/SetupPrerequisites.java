package com.shaft.intellij.ui;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Detects the tools the SHAFT MCP setup flow depends on and produces the per-OS terminal commands
 * that install a missing one, so the setup screen can run fully portable on a fresh machine: every
 * prerequisite is either detected as present or paired with a simple command the user can paste
 * into their terminal. Java is bootstrapped automatically by the installer when absent, so it is
 * advisory rather than blocking; Python is required to run the installer script itself.
 */
final class SetupPrerequisites {
    /**
     * One detected prerequisite: display name, whether it was found on PATH, whether setup can
     * proceed without it, and the terminal command that installs it on this OS.
     */
    record Prerequisite(String name, boolean present, boolean required, String installCommand) {
    }

    private SetupPrerequisites() {
        throw new IllegalStateException("Utility class");
    }

    static List<Prerequisite> detect(String family) {
        return detect(family, AssistantLocalAgentRunner::isCommandAvailable, isWindows(), isMac(),
                userHome(), true);
    }

    /**
     * Package-private overload used by tests to stub PATH detection and the OS. Does not scan the
     * real user home or process environment, so a developer machine that already has {@code grok}
     * installed cannot flip these tests.
     */
    static List<Prerequisite> detect(String family, Predicate<String> commandAvailable, boolean windows, boolean mac) {
        return detect(family, commandAvailable, windows, mac, null, false);
    }

    /**
     * Same as the test overload, plus an explicit home directory for official on-disk install
     * locations ({@code ~/.grok/bin}, {@code ~/.local/bin}).
     */
    static List<Prerequisite> detect(String family, Predicate<String> commandAvailable, boolean windows, boolean mac,
            Path home) {
        return detect(family, commandAvailable, windows, mac, home, false);
    }

    private static List<Prerequisite> detect(String family, Predicate<String> commandAvailable, boolean windows,
            boolean mac, Path home, boolean includeProcessEnvironment) {
        List<Prerequisite> prerequisites = new ArrayList<>();
        boolean python = commandAvailable.test(windows ? "py" : "python3") || commandAvailable.test("python");
        prerequisites.add(new Prerequisite("Python 3", python, true, pythonInstallCommand(windows, mac)));
        prerequisites.add(new Prerequisite("Java (JDK)", commandAvailable.test("java"), false,
                javaInstallCommand(windows, mac)));
        prerequisites.add(new Prerequisite("Maven", commandAvailable.test("mvn"), false,
                mavenInstallCommand(windows, mac)));
        String agentExecutable = agentExecutableFor(family);
        if (agentExecutable != null) {
            String installCommand = agentInstallCommandFor(family, windows, mac);
            boolean agentPresent = commandAvailable.test(agentExecutable)
                    || officialBinaryPresent(agentExecutable, home, windows, includeProcessEnvironment);
            if (!agentPresent && installCommand.startsWith("npm ") && !commandAvailable.test("node")) {
                prerequisites.add(new Prerequisite("Node.js (required to install " + agentDisplayNameFor(family) + ")",
                        false, true, nodeInstallCommand(windows, mac)));
            }
            prerequisites.add(new Prerequisite(agentDisplayNameFor(family), agentPresent, true, installCommand));
        }
        return List.copyOf(prerequisites);
    }

    /**
     * True when the official installer already wrote this CLI somewhere the login shell would find
     * it, even if the IDE process PATH was captured before that directory was added.
     */
    static boolean officialBinaryPresent(String executable, Path home, boolean windows,
            boolean includeProcessEnvironment) {
        if (executable == null || executable.isBlank()) {
            return false;
        }
        for (Path candidate : officialBinaryCandidates(executable, home, windows, includeProcessEnvironment)) {
            if (Files.isRegularFile(candidate) && Files.isExecutable(candidate)) {
                return true;
            }
        }
        return false;
    }

    private static List<Path> officialBinaryCandidates(String executable, Path home, boolean windows,
            boolean includeProcessEnvironment) {
        List<Path> candidates = new ArrayList<>();
        if ("grok".equals(executable) && home != null) {
            Path bin = home.resolve(".grok").resolve("bin");
            candidates.add(bin.resolve(windows ? "grok.exe" : "grok"));
            if (windows) {
                candidates.add(bin.resolve("grok"));
            }
        }
        if ("agy".equals(executable)) {
            if (!windows && home != null) {
                candidates.add(home.resolve(".local").resolve("bin").resolve("agy"));
            }
            if (windows) {
                if (home != null) {
                    candidates.add(home.resolve("AppData").resolve("Local").resolve("agy").resolve("bin")
                            .resolve("agy.exe"));
                }
                if (includeProcessEnvironment) {
                    String localAppData = System.getenv("LOCALAPPDATA");
                    if (localAppData != null && !localAppData.isBlank()) {
                        candidates.add(Path.of(localAppData).resolve("agy").resolve("bin").resolve("agy.exe"));
                    }
                }
            }
        }
        return candidates;
    }

    private static Path userHome() {
        String home = System.getProperty("user.home", "");
        return home.isBlank() ? null : Path.of(home);
    }

    static String agentExecutableFor(String family) {
        return switch (normalize(family)) {
            case "CLAUDE" -> "claude";
            case "COPILOT" -> "copilot";
            case "GROK" -> "grok";
            case "ANTIGRAVITY" -> "agy";
            case "CODEX" -> "codex";
            case "GEMINI", "" -> null;
            default -> null;
        };
    }

    static String agentDisplayNameFor(String family) {
        return switch (normalize(family)) {
            case "CLAUDE" -> "Claude Code CLI";
            case "COPILOT" -> "GitHub Copilot CLI";
            case "GROK" -> com.shaft.intellij.settings.GrokInstallIdentity.detectedLabel();
            case "ANTIGRAVITY" -> "Antigravity";
            case "CODEX" -> "Codex CLI";
            default -> "";
        };
    }

    static String agentInstallCommandFor(String family) {
        return agentInstallCommandFor(family, isWindows(), isMac());
    }

    /**
     * Runnable install command for the selected assistant. Grok Build and Antigravity publish
     * native installers (no Node). {@code mac} is unused today; both Unix installers are the same
     * curl script.
     */
    static String agentInstallCommandFor(String family, boolean windows, boolean mac) {
        return switch (normalize(family)) {
            case "CLAUDE" -> "npm install -g @anthropic-ai/claude-code";
            case "COPILOT" -> "npm install -g @github/copilot";
            case "GROK" -> windows
                    ? "irm https://x.ai/cli/install.ps1 | iex"
                    : "curl -fsSL https://x.ai/cli/install.sh | bash";
            case "ANTIGRAVITY" -> windows
                    ? "irm https://antigravity.google/cli/install.ps1 | iex"
                    : "curl -fsSL https://antigravity.google/cli/install.sh | bash";
            case "CODEX" -> "npm install -g @openai/codex";
            default -> "";
        };
    }

    private static final String ENGINE_METADATA_URL =
            "https://repo1.maven.org/maven2/io/github/shafthq/SHAFT_ENGINE/maven-metadata.xml";
    private static final String MCP_METADATA_URL =
            "https://repo1.maven.org/maven2/io/github/shafthq/shaft-mcp/maven-metadata.xml";
    private static final Pattern METADATA_RELEASE = Pattern.compile("<release>\\s*([^<\\s]+)\\s*</release>");
    private static final Pattern VERSION_LIKE = Pattern.compile("\\d+(\\.\\d+)+");
    private static final AtomicReference<String> RESOLVED_ENGINE_VERSION = new AtomicReference<>();
    private static final AtomicReference<CompletableFuture<Void>> ENGINE_VERSION_PREFETCH = new AtomicReference<>();
    private static final AtomicReference<String> RESOLVED_MCP_VERSION = new AtomicReference<>();
    private static final AtomicReference<CompletableFuture<Void>> MCP_VERSION_PREFETCH = new AtomicReference<>();

    /**
     * Pre-downloads SHAFT Engine and its transitive dependencies into the local Maven repository so
     * every future SHAFT project on this machine builds without re-downloading them. The command
     * pins the real latest release resolved from Maven Central ({@code LATEST} is a Maven 3
     * meta-version that also matches snapshots, is unsupported by {@code dependency:get} on Maven 4,
     * and hides which version the user actually warmed up); when Central has not answered (yet),
     * the plugin's own release version — which tracks the engine release train — is used instead.
     */
    static String shaftEngineWarmupCommand() {
        prefetchLatestEngineVersion();
        // The -D argument is double-quoted so the command survives every shell it can be pasted
        // into: PowerShell tokenizes an unquoted -Dartifact=io.github... at the first dot, which
        // made Maven see a bogus ".github.shafthq:SHAFT_ENGINE" plugin goal (issue #3426 A1).
        // Double quotes are portable across PowerShell, cmd, bash, and zsh.
        return "mvn -B dependency:get \"-Dartifact=io.github.shafthq:SHAFT_ENGINE:" + latestEngineVersion() + "\"";
    }

    /**
     * Starts (at most once) a background resolution of the latest released engine version from
     * Maven Central, so a later EDT call to {@link #shaftEngineWarmupCommand()} can embed it without
     * ever blocking on the network. Callers that build setup UI should invoke this early.
     */
    static void prefetchLatestEngineVersion() {
        if (ENGINE_VERSION_PREFETCH.compareAndSet(null, new CompletableFuture<>())) {
            CompletableFuture
                    .runAsync(() -> {
                        String release = releaseVersionFromMavenCentral();
                        if (!release.isBlank()) {
                            RESOLVED_ENGINE_VERSION.set(release);
                        }
                    })
                    .whenComplete((ignored, error) -> ENGINE_VERSION_PREFETCH.get().complete(null));
        }
    }

    private static String latestEngineVersion() {
        return latestEngineVersion(RESOLVED_ENGINE_VERSION::get, SetupPrerequisites::bundledPluginVersion);
    }

    /**
     * Best-known latest engine release as a concrete version number, or blank when neither Maven
     * Central nor the bundled plugin version produced one (dev builds without network). Used by the
     * setup flow's real "is this project up to date" check (issue #3426 A4).
     */
    static String knownLatestEngineVersion() {
        String version = latestEngineVersion();
        return isVersionLike(version) ? version : "";
    }

    /**
     * Package-private for tests: picks the resolved Maven Central release when available, then the
     * bundled plugin release version, and only when neither looks like a real version number falls
     * back to Maven's {@code RELEASE} meta-version (latest non-snapshot) so the command still works
     * on dev builds without network access.
     */
    static String latestEngineVersion(Supplier<String> resolvedVersion, Supplier<String> bundledVersion) {
        String resolved = resolvedVersion.get();
        if (isVersionLike(resolved)) {
            return resolved;
        }
        String bundled = bundledVersion.get();
        if (isVersionLike(bundled)) {
            return bundled;
        }
        return "RELEASE";
    }

    private static boolean isVersionLike(String candidate) {
        return candidate != null && VERSION_LIKE.matcher(candidate.trim()).matches();
    }

    private static String bundledPluginVersion() {
        try {
            return ResourceBundle.getBundle("messages.ShaftBundle").getString("shaft.plugin.version");
        } catch (MissingResourceException exception) {
            return "";
        }
    }

    /**
     * Starts (at most once) a background resolution of the latest released shaft-mcp version from
     * Maven Central, mirroring {@link #prefetchLatestEngineVersion()}, so a later EDT call to
     * {@link #knownLatestMcpVersion()} never blocks on the network (issue #3538).
     */
    static void prefetchLatestMcpVersion() {
        if (MCP_VERSION_PREFETCH.compareAndSet(null, new CompletableFuture<>())) {
            CompletableFuture
                    .runAsync(() -> {
                        String release = releaseVersionFromMavenCentral(MCP_METADATA_URL);
                        if (!release.isBlank()) {
                            RESOLVED_MCP_VERSION.set(release);
                        }
                    })
                    .whenComplete((ignored, error) -> MCP_VERSION_PREFETCH.get().complete(null));
        }
    }

    /**
     * Best-known latest shaft-mcp release as a concrete version number, or blank when the background
     * resolution has not completed (yet) or the machine is offline. The setup flow's "SHAFT MCP
     * version" step treats blank as a neutral, non-blocking "unknown" state — it never fails setup
     * (issue #3538).
     */
    static String knownLatestMcpVersion() {
        String resolved = RESOLVED_MCP_VERSION.get();
        return isVersionLike(resolved) ? resolved : "";
    }

    private static String releaseVersionFromMavenCentral() {
        return releaseVersionFromMavenCentral(ENGINE_METADATA_URL);
    }

    private static String releaseVersionFromMavenCentral(String metadataUrl) {
        // No try-with-resources: HttpClient is only AutoCloseable on JDK 21+, and this module
        // still compiles with --release 17.
        try {
            HttpClient client = HttpClient.newBuilder()
                    .connectTimeout(Duration.ofSeconds(5))
                    .followRedirects(HttpClient.Redirect.NORMAL)
                    .build();
            HttpResponse<String> response = client.send(
                    HttpRequest.newBuilder(URI.create(metadataUrl))
                            .timeout(Duration.ofSeconds(10))
                            .GET()
                            .build(),
                    HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() != 200) {
                return "";
            }
            return releaseVersionFromMetadata(response.body());
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            return "";
        } catch (Exception exception) {
            return "";
        }
    }

    /**
     * Package-private for tests: extracts the {@code <release>} version from Maven repository
     * metadata XML, or blank when the document has none.
     */
    static String releaseVersionFromMetadata(String metadataXml) {
        if (metadataXml == null || metadataXml.isBlank()) {
            return "";
        }
        Matcher matcher = METADATA_RELEASE.matcher(metadataXml);
        return matcher.find() ? matcher.group(1) : "";
    }

    private static String pythonInstallCommand(boolean windows, boolean mac) {
        if (windows) {
            return "winget install -e --id Python.Python.3.12";
        }
        return mac ? "brew install python@3.12" : "sudo apt-get install -y python3";
    }

    private static String javaInstallCommand(boolean windows, boolean mac) {
        if (windows) {
            return "winget install -e --id EclipseAdoptium.Temurin.25.JDK";
        }
        return mac ? "brew install --cask temurin@25" : "sudo apt-get install -y openjdk-25-jdk";
    }

    private static String mavenInstallCommand(boolean windows, boolean mac) {
        if (windows) {
            return "winget install -e --id Apache.Maven";
        }
        return mac ? "brew install maven" : "sudo apt-get install -y maven";
    }

    private static String nodeInstallCommand(boolean windows, boolean mac) {
        if (windows) {
            return "winget install -e --id OpenJS.NodeJS.LTS";
        }
        return mac ? "brew install node" : "sudo apt-get install -y nodejs npm";
    }

    private static String normalize(String value) {
        return (value == null || value.isBlank() ? "" : value.trim())
                .toUpperCase(Locale.ROOT)
                .replace('-', '_')
                .replace(' ', '_');
    }

    private static boolean isWindows() {
        return System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("win");
    }

    private static boolean isMac() {
        return System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("mac");
    }
}
