package com.shaft.mcp;

import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Duration;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.regex.Matcher;

/**
 * MCP project lifecycle tools backed by existing SHAFT generator assets and upgrader scripts.
 */
@Service
public class ShaftProjectService {
    private static final String EXAMPLES_ROOT = "META-INF/shaft-mcp/examples";
    private static final String UPGRADER_RESOURCE = "META-INF/shaft-mcp/upgrade_to_modular_shaft.py";
    private static final String SHAFT_ENGINE_MAVEN_METADATA_URL =
            "https://repo.maven.apache.org/maven2/io/github/shafthq/shaft-engine/maven-metadata.xml";
    private static final int DEFAULT_COMPILE_TIMEOUT_SECONDS = 900;
    private static final List<String> OPTIONAL_MODULES = List.of(
            "shaft-capture",
            "shaft-doctor",
            "shaft-ai",
            "shaft-heal",
            "shaft-browserstack",
            "shaft-video",
            "shaft-visual",
            "shaft-sikulix",
            "shaft-mcp");
    private static final Map<String, Map<String, String>> PROJECTS = projects();

    private final McpWorkspacePolicy workspacePolicy;
    private final McpProcessRunner processRunner;
    private final Path upgraderScript;
    private final List<String> pythonCommand;
    private final Supplier<String> shaftVersionResolver;

    /**
     * Creates the default project lifecycle MCP service.
     */
    public ShaftProjectService() {
        this(McpWorkspacePolicy.current(), McpProcessRunner.system(),
                resolveUpgraderScript(), defaultPythonCommand(), ShaftProjectService::latestPublishedShaftEngineVersion);
    }

    ShaftProjectService(
            McpWorkspacePolicy workspacePolicy,
            McpProcessRunner processRunner,
            Path upgraderScript,
            List<String> pythonCommand) {
        this(workspacePolicy, processRunner, upgraderScript, pythonCommand,
                ShaftProjectService::latestPublishedShaftEngineVersion);
    }

    ShaftProjectService(
            McpWorkspacePolicy workspacePolicy,
            McpProcessRunner processRunner,
            Path upgraderScript,
            List<String> pythonCommand,
            Supplier<String> shaftVersionResolver) {
        this.workspacePolicy = Objects.requireNonNull(workspacePolicy, "workspacePolicy");
        this.processRunner = Objects.requireNonNull(processRunner, "processRunner");
        this.upgraderScript = Objects.requireNonNull(upgraderScript, "upgraderScript");
        this.pythonCommand = List.copyOf(Objects.requireNonNull(pythonCommand, "pythonCommand"));
        this.shaftVersionResolver = Objects.requireNonNull(shaftVersionResolver, "shaftVersionResolver");
    }

    /**
     * Creates a SHAFT Maven project from the same examples and rules used by the guide generator.
     *
     * @param outputDirectory workspace-relative output directory
     * @param runner TestNG, JUnit, or Cucumber
     * @param platform web, mobile, or api where supported by the selected runner
     * @param groupId Maven group ID
     * @param artifactId Maven artifact ID; defaults to the guide generator artifact name when blank
     * @param version Maven project version; defaults to 1.0.0 when blank
     * @param shaftVersion optional override for generated project {@code <shaft.version>}
     * @param optionalModules optional SHAFT module artifact IDs
     * @param includeGithubActions whether to include the generated workflow for web/api projects
     * @param includeDependabot whether to include Dependabot configuration
     * @param overwrite whether existing files may be replaced
     * @return generated project details
     */
    @Tool(name = "shaft_project_create",
            description = "creates a new SHAFT Maven project from the same examples and rules used by the guide generator")
    @SuppressWarnings("PMD.ExcessiveParameterList")
    public McpShaftProjectGenerationResult createProject(
            String outputDirectory,
            String runner,
            String platform,
            String groupId,
            String artifactId,
            String version,
            String shaftVersion,
            List<String> optionalModules,
            boolean includeGithubActions,
            boolean includeDependabot,
            boolean overwrite) {
        String selectedRunner = selectedRunner(runner);
        String selectedPlatform = selectedPlatform(selectedRunner, platform);
        String templateProject = PROJECTS.get(selectedRunner).get(selectedPlatform);
        String projectArtifactId = text(artifactId).isBlank()
                ? defaultArtifactId(selectedRunner, selectedPlatform)
                : text(artifactId);
        Path output = workspacePolicy.output(text(outputDirectory).isBlank() ? projectArtifactId : outputDirectory,
                "Project output directory");
        if (Files.exists(output) && !overwrite) {
            throw new IllegalArgumentException("Project output directory already exists.");
        }

        List<String> modules = checkedModules(optionalModules, selectedPlatform);
        List<String> warnings = List.of();
        try {
            copyResourceDirectory(EXAMPLES_ROOT + "/" + selectedRunner + "/" + templateProject, output, overwrite);
            Path pomPath = output.resolve("pom.xml");
            String pom = Files.readString(pomPath, StandardCharsets.UTF_8);
            pom = replaceFirst(pom, "<groupId>io\\.github\\.shafthq</groupId>",
                    "<groupId>" + escapeXml(defaultText(groupId, "io.github.yourUsername")) + "</groupId>");
            pom = replaceFirst(pom, "<artifactId>.*?</artifactId>",
                    "<artifactId>" + escapeXml(projectArtifactId) + "</artifactId>");
            pom = replaceFirst(pom, "<version>1\\.0-SNAPSHOT</version>",
                    "<version>" + escapeXml(defaultText(version, "1.0.0")) + "</version>");
            pom = replaceFirst(pom, "<shaft.version>.*?</shaft.version>",
                    "<shaft.version>" + escapeXml(resolveShaftVersion(shaftVersion)) + "</shaft.version>");
            Files.writeString(pomPath, addOptionalDependencies(pom, modules), StandardCharsets.UTF_8);

            if (includeGithubActions && !"mobile".equals(selectedPlatform)) {
                copyResourceFile(EXAMPLES_ROOT + "/.github/workflows/" + workflowName(selectedPlatform),
                        output.resolve(".github/workflows/" + workflowName(selectedPlatform)), output, overwrite);
            }
            if (includeDependabot) {
                copyResourceFile(EXAMPLES_ROOT + "/.github/dependabot.yml",
                        output.resolve(".github/dependabot.yml"), output, overwrite);
            }
            return new McpShaftProjectGenerationResult(
                    McpShaftProjectGenerationResult.CURRENT_SCHEMA_VERSION,
                    output,
                    pomPath,
                    selectedRunner,
                    selectedPlatform,
                    templateProject,
                    modules,
                    warnings);
        } catch (IOException exception) {
            throw new IllegalStateException("SHAFT project could not be generated.", exception);
        }
    }

    /**
     * Creates a SHAFT Maven project and resolves the generated {@code <shaft.version>} from Maven Central.
     *
     * @param outputDirectory workspace-relative output directory
     * @param runner TestNG, JUnit, or Cucumber
     * @param platform web, mobile, or api where supported by the selected runner
     * @param groupId Maven group ID
     * @param artifactId Maven artifact ID
     * @param version Maven project version
     * @param optionalModules optional SHAFT module artifact IDs
     * @param includeGithubActions whether to include a generated GitHub Actions workflow
     * @param includeDependabot whether to include Dependabot configuration
     * @param overwrite whether existing files may be replaced
     * @return generated project details
     */
    @SuppressWarnings("PMD.ExcessiveParameterList")
    public McpShaftProjectGenerationResult createProject(
            String outputDirectory,
            String runner,
            String platform,
            String groupId,
            String artifactId,
            String version,
            List<String> optionalModules,
            boolean includeGithubActions,
            boolean includeDependabot,
            boolean overwrite) {
        return createProject(outputDirectory, runner, platform, groupId, artifactId, version, "",
                optionalModules, includeGithubActions, includeDependabot, overwrite);
    }

    /**
     * Runs the existing modular SHAFT project upgrader script against a Java project.
     *
     * @param projectRoot workspace-relative Java/Maven project root
     * @param upgradeType basic, session, or full
     * @param dryRun whether to preview changes without writing files
     * @param approve explicit approval required when dryRun is false
     * @param shaftVersion optional target SHAFT version
     * @param compileCommand optional compile command string passed to the upgrader
     * @param compileTimeout compile timeout in seconds; defaults to 900 when non-positive
     * @param skipBaselineCompile whether to pass --skip-baseline-compile
     * @param allowAiRepair whether to allow the upgrader to use its configured AI repair path
     * @return upgrader process result
     */
    @Tool(name = "shaft_project_upgrade",
            description = "runs the existing SHAFT modular project upgrader script against the current Java project")
    public McpShaftProjectUpgradeResult upgradeProject(
            String projectRoot,
            String upgradeType,
            boolean dryRun,
            boolean approve,
            String shaftVersion,
            String compileCommand,
            int compileTimeout,
            boolean skipBaselineCompile,
            boolean allowAiRepair) {
        if (!dryRun && !approve) {
            throw new IllegalArgumentException("Project upgrade mutation requires approve=true.");
        }
        Path root = workspacePolicy.existing(projectRoot, "Project root");
        int timeoutSeconds = compileTimeout > 0 ? compileTimeout : DEFAULT_COMPILE_TIMEOUT_SECONDS;
        Path report = root.resolve("target/shaft-upgrader/upgrade-report.json").normalize();
        List<String> command = new ArrayList<>(pythonCommand);
        command.add(upgraderScript.toString());
        command.add("--project");
        command.add(root.toString());
        command.add("--upgrade-type");
        command.add(validUpgradeType(upgradeType));
        command.add("--compile-timeout");
        command.add(Integer.toString(timeoutSeconds));
        if (dryRun) {
            command.add("--dry-run");
        } else {
            command.add("--yes");
            command.add("--report");
            command.add(report.toString());
        }
        if (!text(shaftVersion).isBlank()) {
            command.add("--shaft-version");
            command.add(text(shaftVersion));
        }
        if (!text(compileCommand).isBlank()) {
            command.add("--compile-command");
            command.add(text(compileCommand));
        }
        if (skipBaselineCompile) {
            command.add("--skip-baseline-compile");
        }
        if (!allowAiRepair) {
            command.add("--no-ai");
        }

        McpProcessRunner.ProcessResult result = processRunner.run(
                command,
                root,
                Map.of(),
                Duration.ofSeconds(timeoutSeconds + 60L));
        return new McpShaftProjectUpgradeResult(
                McpShaftProjectUpgradeResult.CURRENT_SCHEMA_VERSION,
                root,
                report,
                dryRun,
                result.exitCode(),
                result.stdout(),
                result.stderr(),
                result.timedOut(),
                command);
    }

    private static Map<String, Map<String, String>> projects() {
        Map<String, Map<String, String>> projects = new LinkedHashMap<>();
        projects.put("TestNG", Map.of("web", "shaft-testng-web", "mobile", "shaft-testng-mobile",
                "api", "shaft-testng-api"));
        projects.put("JUnit", Map.of("web", "shaft-junit-web", "mobile", "shaft-junit-mobile",
                "api", "shaft-junit-api"));
        projects.put("Cucumber", Map.of("web", "shaft-cucumber-web"));
        return projects;
    }

    private static String selectedRunner(String value) {
        String normalized = text(value);
        return PROJECTS.keySet().stream()
                .filter(runner -> runner.equalsIgnoreCase(normalized))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Unsupported SHAFT project runner: " + value));
    }

    private static String selectedPlatform(String runner, String value) {
        String normalized = text(value).toLowerCase(Locale.ROOT);
        if (!PROJECTS.get(runner).containsKey(normalized)) {
            throw new IllegalArgumentException("Unsupported SHAFT project platform for " + runner + ": " + value);
        }
        return normalized;
    }

    private static List<String> checkedModules(List<String> modules, String platform) {
        Set<String> checked = new LinkedHashSet<>();
        if (modules != null) {
            for (String module : modules) {
                String normalized = text(module);
                if (!normalized.isBlank() && OPTIONAL_MODULES.contains(normalized)) {
                    checked.add(normalized);
                }
            }
        }
        if ("web".equals(platform)) {
            checked.add("shaft-visual");
        }
        return List.copyOf(checked);
    }

    private static String addOptionalDependencies(String pom, List<String> modules) {
        List<String> missing = modules.stream()
                .filter(module -> !pom.contains("<artifactId>" + module + "</artifactId>"))
                .toList();
        if (missing.isEmpty()) {
            return pom;
        }
        StringBuilder block = new StringBuilder();
        for (String module : missing) {
            block.append("        <dependency>\n")
                    .append("            <groupId>io.github.shafthq</groupId>\n")
                    .append("            <artifactId>").append(module).append("</artifactId>\n")
                    .append("        </dependency>\n");
        }
        String marker = "\\R    </dependencies>\\R    <build>";
        if (!java.util.regex.Pattern.compile(marker).matcher(pom).find()) {
            throw new IllegalArgumentException("Could not locate project dependency section in pom.xml");
        }
        return pom.replaceFirst(marker, Matcher.quoteReplacement("\n" + block + "    </dependencies>\n    <build>"));
    }

    private static void copyResourceDirectory(String resourceRoot, Path target, boolean overwrite) throws IOException {
        URL url = resource(resourceRoot);
        try {
            if ("file".equalsIgnoreCase(url.getProtocol())) {
                Path source = Path.of(url.toURI());
                Files.walkFileTree(source, new SimpleFileVisitor<>() {
                    @Override
                    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                        copyFile(file, target.resolve(source.relativize(file).toString()), target, overwrite);
                        return FileVisitResult.CONTINUE;
                    }
                });
                return;
            }
            if ("jar".equalsIgnoreCase(url.getProtocol())) {
                JarURLConnection connection = (JarURLConnection) url.openConnection();
                String prefix = connection.getEntryName() + "/";
                try (JarFile jar = connection.getJarFile()) {
                    for (JarEntry entry : jar.stream().filter(entry -> !entry.isDirectory())
                            .filter(entry -> entry.getName().startsWith(prefix)).toList()) {
                        Path destination = safeResourceDestination(target, entry.getName().substring(prefix.length()));
                        try (var input = jar.getInputStream(entry)) {
                            copyFile(input.readAllBytes(), destination, target, overwrite);
                        }
                    }
                }
                return;
            }
            throw new IOException("Unsupported project generator resource protocol: " + url.getProtocol());
        } catch (IOException exception) {
            throw exception;
        } catch (Exception exception) {
            throw new IOException("Project generator resources could not be read.", exception);
        }
    }

    private static void copyResourceFile(String resourceName, Path destination, Path targetRoot, boolean overwrite)
            throws IOException {
        try (var input = resource(resourceName).openStream()) {
            copyFile(input.readAllBytes(), destination, targetRoot, overwrite);
        }
    }

    private static void copyFile(Path source, Path destination, Path targetRoot, boolean overwrite) throws IOException {
        Path normalized = safeDestination(destination, targetRoot);
        Files.createDirectories(normalized.getParent());
        if (overwrite) {
            Files.copy(source, normalized, StandardCopyOption.REPLACE_EXISTING);
        } else {
            Files.copy(source, normalized);
        }
    }

    private static void copyFile(byte[] content, Path destination, Path targetRoot, boolean overwrite)
            throws IOException {
        Path normalized = safeDestination(destination, targetRoot);
        Files.createDirectories(normalized.getParent());
        if (overwrite) {
            Files.write(normalized, content);
        } else {
            Files.write(normalized, content, java.nio.file.StandardOpenOption.CREATE_NEW);
        }
    }

    private static Path safeDestination(Path destination, Path targetRoot) {
        Path normalized = destination.toAbsolutePath().normalize();
        if (targetRoot != null && !normalized.startsWith(targetRoot.toAbsolutePath().normalize())) {
            throw new IllegalArgumentException("Generated project file escaped the output directory.");
        }
        return normalized;
    }

    static Path safeResourceDestination(Path targetRoot, String resourceName) {
        Path normalizedRoot = targetRoot.toAbsolutePath().normalize();
        Path normalizedDestination = normalizedRoot.resolve(resourceName).normalize();
        if (!normalizedDestination.startsWith(normalizedRoot)) {
            throw new IllegalArgumentException("Generated project file escaped the output directory.");
        }
        return normalizedDestination;
    }

    private static URL resource(String resourceName) throws IOException {
        URL url = ShaftProjectService.class.getClassLoader().getResource(resourceName);
        if (url == null) {
            throw new IOException("Missing SHAFT project generator resource: " + resourceName);
        }
        return url;
    }

    private static Path resolveUpgraderScript() {
        Path sourceTreeScript = McpRuntimePaths.currentRoot()
                .resolve("shaft-upgrader")
                .resolve("upgrade_to_modular_shaft.py");
        if (Files.isRegularFile(sourceTreeScript)) {
            return sourceTreeScript;
        }
        Path cached = McpRuntimePaths.applicationDataRoot()
                .resolve("tools")
                .resolve("upgrade_to_modular_shaft.py");
        try {
            URL resource = resource(UPGRADER_RESOURCE);
            Files.createDirectories(cached.getParent());
            try (var input = resource.openStream()) {
                Files.write(cached, input.readAllBytes());
            }
            return cached;
        } catch (IOException exception) {
            throw new IllegalStateException("SHAFT upgrader script could not be resolved.", exception);
        }
    }

    private static List<String> defaultPythonCommand() {
        String os = System.getProperty("os.name", "").toLowerCase(Locale.ROOT);
        return os.contains("win") ? List.of("py", "-3") : List.of("python3");
    }

    private static String validUpgradeType(String value) {
        String normalized = text(value).toLowerCase(Locale.ROOT);
        if (Set.of("basic", "session", "full").contains(normalized)) {
            return normalized;
        }
        throw new IllegalArgumentException("Unsupported SHAFT project upgrade type: " + value);
    }

    private static String workflowName(String platform) {
        return "web".equals(platform) ? "web.yml" : "api.yml";
    }

    private String resolveShaftVersion(String requestedVersion) {
        String requested = text(requestedVersion);
        if (!requested.isBlank()) {
            return requested;
        }
        String resolved = text(shaftVersionResolver.get());
        if (resolved.isBlank()) {
            throw new IllegalStateException("Latest SHAFT Engine version could not be resolved.");
        }
        return resolved;
    }

    private static String latestPublishedShaftEngineVersion() {
        try {
            String metadata = readTextFromUrl(SHAFT_ENGINE_MAVEN_METADATA_URL);
            String release = firstXmlMatch(metadata, "<release>([^<]+)</release>");
            if (isStableVersion(release)) {
                return release;
            }
            String latest = firstXmlMatch(metadata, "<latest>([^<]+)</latest>");
            if (isStableVersion(latest)) {
                return latest;
            }
            return latestStableVersionFromVersions(metadata);
        } catch (IOException exception) {
            throw new IllegalStateException(
                    "Could not resolve latest SHAFT Engine version from Maven Central.",
                    exception);
        }
    }

    private static String firstXmlMatch(String metadata, String expression) {
        Matcher matcher = java.util.regex.Pattern.compile(expression).matcher(metadata);
        return matcher.find() ? text(matcher.group(1)) : "";
    }

    private static String latestStableVersionFromVersions(String metadata) {
        Matcher matcher = java.util.regex.Pattern.compile("<version>([^<]+)</version>").matcher(metadata);
        String selected = "";
        while (matcher.find()) {
            String candidate = text(matcher.group(1));
            if (!isStableVersion(candidate) || candidate.isBlank()) {
                continue;
            }
            if (compareVersions(candidate, selected) > 0) {
                selected = candidate;
            }
        }
        if (selected.isBlank()) {
            throw new IllegalStateException("Could not resolve a stable SHAFT Engine version.");
        }
        return selected;
    }

    private static boolean isStableVersion(String version) {
        String normalized = text(version).toLowerCase(Locale.ROOT);
        return !normalized.isBlank()
                && !normalized.contains("snapshot")
                && !normalized.contains("alpha")
                && !normalized.contains("beta")
                && !normalized.contains("rc")
                && !normalized.contains("milestone")
                && !normalized.contains("preview")
                && !normalized.contains("ea");
    }

    private static int compareVersions(String left, String right) {
        String[] leftParts = text(left).split("\\.");
        String[] rightParts = text(right).split("\\.");
        int maxLength = Math.max(leftParts.length, rightParts.length);
        for (int index = 0; index < maxLength; index++) {
            long leftValue = index < leftParts.length ? numericPart(leftParts[index]) : 0L;
            long rightValue = index < rightParts.length ? numericPart(rightParts[index]) : 0L;
            if (leftValue != rightValue) {
                return Long.compare(leftValue, rightValue);
            }
        }
        return Integer.compare(leftParts.length, rightParts.length);
    }

    private static long numericPart(String part) {
        String digits = text(part).replaceAll("[^0-9]", "");
        if (digits.isBlank()) {
            return 0L;
        }
        return Long.parseLong(digits);
    }

    private static String readTextFromUrl(String target) throws IOException {
        URL url = new URL(target);
        URLConnection connection = url.openConnection();
        connection.setConnectTimeout(5000);
        connection.setReadTimeout(5000);
        try (var input = connection.getInputStream()) {
            return new String(input.readAllBytes(), StandardCharsets.UTF_8);
        }
    }

    private static String defaultArtifactId(String runner, String platform) {
        return "shaft-" + platform.toLowerCase(Locale.ROOT) + "-" + runner.toLowerCase(Locale.ROOT);
    }

    private static String escapeXml(String value) {
        return value.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;")
                .replace("'", "&apos;");
    }

    private static String replaceFirst(String text, String regex, String replacement) {
        return text.replaceFirst(regex, Matcher.quoteReplacement(replacement));
    }

    private static String defaultText(String value, String defaultValue) {
        String text = text(value);
        return text.isBlank() ? defaultValue : text;
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
