package com.shaft.mcp;

import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.net.JarURLConnection;
import java.net.URI;
import java.net.URISyntaxException;
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
import java.util.Arrays;
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
    private static final String SHAFT_SKILLS_ROOT = "META-INF/shaft-mcp/shaft-skills";
    private static final String ROUTER_SKILL_NAME = "shaft-developer";
    private static final String SKILL_PACK_MANIFEST = ".shaft-mcp-managed-files";
    private static final String SKILL_PACK_MANIFEST_HEADER = "# SHAFT MCP managed skill files";
    private static final List<String> ALL_AGENT_HOSTS = List.of("codex", "claude", "vscode", "opencode", "grok");
    private static final Set<String> AGENT_LOOPS = Set.of("all", "claude", "codex", "opencode", "vscode", "grok");
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

    /**
     * Scaffolds SHAFT agent/skill bridge definitions into an existing test repo for a coding-agent loop.
     *
     * <p>Every bridge is generated at call time from the bundled {@code shaft-skills/&lt;skill&gt;/SKILL.md}
     * guides shipped inside the shaft-mcp jar, rather than a hand-duplicated copy of the full skill text,
     * so the set of skills and their descriptions always tracks {@code shaft-skills/} as of the shaft-mcp
     * build. Existing skill files are left untouched unless {@code overwrite} is {@code true}; host
     * instruction files always preserve user-authored bytes outside SHAFT's managed block. Existing
     * recognized host adapters are refreshed alongside the selected loop, and {@code all} installs every
     * supported host adapter.
     *
     * <p>A bundled skill whose {@code SKILL.md} frontmatter declares {@code distribution: full} is a
     * methodology skill whose value lives in its text, so it is scaffolded as its full body instead of a
     * thin bridge: verbatim for claude/codex/opencode, and with an injected {@code applyTo: "**"} frontmatter
     * key (body otherwise verbatim) for vscode. Skills without that key keep the thin bridge.
     *
     * @param loop all, claude, codex, opencode, vscode, or grok
     * @param targetDirectory workspace-relative existing test repo directory
     * @param overwrite whether existing SHAFT-owned skill files may be replaced
     * @return generated agent-scaffolding details
     */
    @Tool(name = "shaft_project_init_agents",
            description = "scaffolds SHAFT skill bridges and managed host-instruction blocks into an existing "
                    + "test repo for one or all coding-agent loops (all, claude, codex, opencode, vscode, grok); "
                    + "preserves user-authored instruction text and only overwrites SHAFT-owned skill files when "
                    + "overwrite=true")
    public McpShaftProjectInitAgentsResult initAgents(String loop, String targetDirectory, boolean overwrite) {
        String selectedLoop = selectedAgentLoop(loop);
        Path target = workspacePolicy.output(text(targetDirectory).isBlank() ? "." : targetDirectory,
                "Agent scaffolding target directory");
        List<Path> generatedFiles = new ArrayList<>();
        List<String> warnings = new ArrayList<>();
        try {
            Files.createDirectories(target);
            Set<String> hosts = agentHosts(target, selectedLoop);
            Map<String, String> skills = bundledSkillFiles();
            for (String host : hosts) {
                if ("grok".equals(host) && hosts.contains("codex")) {
                    continue;
                }
                installSkills(target, host, skills, overwrite, generatedFiles, warnings);
            }
            installHostAdapters(target, hosts, generatedFiles, warnings);
            return new McpShaftProjectInitAgentsResult(
                    McpShaftProjectInitAgentsResult.CURRENT_SCHEMA_VERSION,
                    target,
                    selectedLoop,
                    generatedFiles,
                    warnings);
        } catch (IOException exception) {
            throw new IllegalStateException("SHAFT agent scaffolding could not be generated.", exception);
        }
    }

    private static String selectedAgentLoop(String value) {
        String normalized = text(value).toLowerCase(Locale.ROOT);
        if (AGENT_LOOPS.contains(normalized)) {
            return normalized;
        }
        throw new IllegalArgumentException("Unsupported SHAFT agent loop: " + value);
    }

    private static String agentLoopSkillsDirectory(String loop) {
        return switch (loop) {
            case "codex", "grok" -> ".agents/skills";
            case "claude" -> ".claude/skills";
            case "opencode" -> ".opencode/skills";
            default -> throw new IllegalArgumentException("Unsupported skill host: " + loop);
        };
    }

    private static Set<String> agentHosts(Path target, String selectedLoop) throws IOException {
        Set<String> hosts = new LinkedHashSet<>();
        if ("all".equals(selectedLoop)) {
            hosts.addAll(ALL_AGENT_HOSTS);
        } else {
            hosts.add(selectedLoop);
        }
        Path agents = target.resolve("AGENTS.md");
        if (Files.exists(agents)) {
            String content = Files.readString(agents, StandardCharsets.UTF_8);
            boolean openCodeManaged = content.contains(managedBlockStart("opencode"));
            if (content.contains(managedBlockStart("codex")) || !openCodeManaged) {
                hosts.add("codex");
            }
            if (openCodeManaged) {
                hosts.add("opencode");
            }
            if (content.contains(managedBlockStart("grok"))) {
                hosts.add("grok");
            }
        }
        if (Files.exists(target.resolve("CLAUDE.md")) || Files.exists(target.resolve(".claude/CLAUDE.md"))) {
            hosts.add("claude");
        }
        if (Files.exists(target.resolve(".github/copilot-instructions.md"))) {
            hosts.add("vscode");
        }
        return hosts;
    }

    private static void installSkills(
            Path target,
            String host,
            Map<String, String> skillFiles,
            boolean overwrite,
            List<Path> generatedFiles,
            List<String> warnings) throws IOException {
        Path skillRoot = "vscode".equals(host)
                ? target.resolve(".github/instructions")
                : target.resolve(agentLoopSkillsDirectory(host));
        Set<String> managedFiles = managedSkillFiles(skillRoot, target, warnings);
        for (Map.Entry<String, String> skillFile : skillFiles.entrySet()) {
            String relativePath = skillFile.getKey();
            Path destination = safeArchiveDestination(skillRoot, relativePath);
            if (writeGeneratedFile(destination, target, skillFile.getValue(), relativePath,
                    managedFiles, overwrite, generatedFiles, warnings)) {
                managedFiles.add(relativePath);
            }
        }
        writeSkillPackManifest(skillRoot, target, managedFiles, generatedFiles, warnings);
    }

    private static void installHostAdapters(
            Path target,
            Set<String> hosts,
            List<Path> generatedFiles,
            List<String> warnings) throws IOException {
        if (hosts.contains("codex")) {
            writeManagedBlock(target.resolve("AGENTS.md"), target, "codex",
                    ".agents/skills/shaft-developer/SKILL.md", generatedFiles, warnings);
        }
        if (hosts.contains("grok")) {
            writeManagedBlock(target.resolve("AGENTS.md"), target, "grok",
                    ".agents/skills/shaft-developer/SKILL.md", generatedFiles, warnings);
        }
        if (hosts.contains("opencode")) {
            writeManagedBlock(target.resolve("AGENTS.md"), target, "opencode",
                    ".opencode/skills/shaft-developer/SKILL.md", generatedFiles, warnings);
        }
        if (hosts.contains("claude")) {
            Path rootAdapter = target.resolve("CLAUDE.md");
            Path nestedAdapter = target.resolve(".claude/CLAUDE.md");
            boolean rootExists = Files.exists(rootAdapter);
            boolean nestedExists = Files.exists(nestedAdapter);
            if (rootExists || !nestedExists) {
                writeManagedBlock(rootAdapter, target, "claude", ".claude/skills/shaft-developer/SKILL.md",
                        generatedFiles, warnings);
            }
            if (nestedExists) {
                writeManagedBlock(nestedAdapter, target, "claude", "skills/shaft-developer/SKILL.md",
                        generatedFiles, warnings);
            }
        }
        if (hosts.contains("vscode")) {
            writeManagedBlock(target.resolve(".github/copilot-instructions.md"), target, "vscode",
                    "instructions/shaft-developer/SKILL.md", generatedFiles, warnings);
        }
    }

    private static boolean writeGeneratedFile(
            Path destination,
            Path targetRoot,
            String content,
            String relativePath,
            Set<String> managedFiles,
            boolean overwrite,
            List<Path> generatedFiles,
            List<String> warnings) throws IOException {
        Path normalized = safeDestination(destination, targetRoot);
        if (Files.exists(normalized) && !overwrite) {
            warnings.add("Skipped existing file (overwrite=false): " + normalized);
            return managedFiles.contains(relativePath);
        }
        if (Files.exists(normalized) && !managedFiles.contains(relativePath)) {
            warnings.add("Skipped unowned existing file: " + normalized);
            return false;
        }
        Files.createDirectories(normalized.getParent());
        byte[] bytes = content.getBytes(StandardCharsets.UTF_8);
        if (Files.exists(normalized) && Arrays.equals(Files.readAllBytes(normalized), bytes)) {
            return true;
        }
        Files.write(normalized, bytes);
        generatedFiles.add(normalized);
        return true;
    }

    private static Set<String> managedSkillFiles(Path skillRoot, Path targetRoot, List<String> warnings)
            throws IOException {
        Path manifest = safeDestination(skillRoot.resolve(SKILL_PACK_MANIFEST), targetRoot);
        if (!Files.exists(manifest)) {
            return new LinkedHashSet<>();
        }
        List<String> lines = Files.readAllLines(manifest, StandardCharsets.UTF_8);
        if (lines.isEmpty() || !SKILL_PACK_MANIFEST_HEADER.equals(lines.getFirst())) {
            warnings.add("Skipped unowned skill-pack manifest: " + manifest);
            return new LinkedHashSet<>();
        }
        Set<String> managed = new LinkedHashSet<>();
        for (String line : lines.subList(1, lines.size())) {
            Path relative = Path.of(line).normalize();
            if (!line.isBlank() && !relative.isAbsolute() && !relative.startsWith("..")) {
                managed.add(relative.toString().replace('\\', '/'));
            }
        }
        return managed;
    }

    private static void writeSkillPackManifest(
            Path skillRoot,
            Path targetRoot,
            Set<String> managedFiles,
            List<Path> generatedFiles,
            List<String> warnings) throws IOException {
        Path manifest = safeDestination(skillRoot.resolve(SKILL_PACK_MANIFEST), targetRoot);
        if (Files.exists(manifest)) {
            List<String> lines = Files.readAllLines(manifest, StandardCharsets.UTF_8);
            if (lines.isEmpty() || !SKILL_PACK_MANIFEST_HEADER.equals(lines.getFirst())) {
                warnings.add("Skipped unowned skill-pack manifest: " + manifest);
                return;
            }
        }
        String content = SKILL_PACK_MANIFEST_HEADER + "\n"
                + managedFiles.stream().sorted().collect(java.util.stream.Collectors.joining("\n")) + "\n";
        if (Files.exists(manifest) && content.equals(Files.readString(manifest, StandardCharsets.UTF_8))) {
            return;
        }
        Files.createDirectories(manifest.getParent());
        Files.writeString(manifest, content, StandardCharsets.UTF_8);
        generatedFiles.add(manifest);
    }

    private static void writeManagedBlock(
            Path destination,
            Path targetRoot,
            String host,
            String routerPath,
            List<Path> generatedFiles,
            List<String> warnings) throws IOException {
        Path normalized = safeDestination(destination, targetRoot);
        String current = Files.exists(normalized) ? Files.readString(normalized, StandardCharsets.UTF_8) : "";
        String startMarker = managedBlockStart(host);
        String endMarker = managedBlockEnd(host);
        int start = current.indexOf(startMarker);
        int end = current.indexOf(endMarker, Math.max(0, start + startMarker.length()));
        int anyEnd = current.indexOf(endMarker);
        boolean duplicate = start >= 0 && (current.indexOf(startMarker, start + startMarker.length()) >= 0
                || (end >= 0 && current.indexOf(endMarker, end + endMarker.length()) >= 0));
        if ((start >= 0 && end < 0) || (start < 0 && anyEnd >= 0) || (anyEnd >= 0 && anyEnd < start) || duplicate) {
            warnings.add("Skipped file with malformed managed block: " + normalized);
            return;
        }

        String newline = newline(current);
        String block = managedBlock(host, routerPath, newline);
        String updated;
        if (start >= 0) {
            updated = current.substring(0, start) + block + current.substring(end + endMarker.length());
        } else {
            updated = current + managedBlockSeparator(current, newline) + block;
        }
        if (current.equals(updated)) {
            return;
        }
        Files.createDirectories(normalized.getParent());
        Files.writeString(normalized, updated, StandardCharsets.UTF_8);
        generatedFiles.add(normalized);
    }

    private static String managedBlock(String host, String routerPath, String newline) {
        return managedBlockStart(host) + newline
                + "## SHAFT " + titleCase(host) + newline + newline
                + "Before any SHAFT task, read and follow [`shaft-developer`](" + routerPath + ") as the "
                + "mandatory router before using any other SHAFT skill or tool." + newline
                + managedBlockEnd(host);
    }

    private static String managedBlockStart(String host) {
        return "<!-- SHAFT-MANAGED:BEGIN shaft-developer:" + host + " -->";
    }

    private static String managedBlockEnd(String host) {
        return "<!-- SHAFT-MANAGED:END shaft-developer:" + host + " -->";
    }

    private static String newline(String content) {
        return content.contains("\r\n") ? "\r\n" : "\n";
    }

    private static String managedBlockSeparator(String content, String newline) {
        if (content.isEmpty() || content.endsWith(newline + newline)) {
            return "";
        }
        return content.endsWith(newline) ? newline : newline + newline;
    }

    /**
     * Reads the canonical skill pack verbatim so that relative links between the hub, specialist
     * skills, playbooks, shared references, and examples remain valid in every supported host.
     */
    private static Map<String, String> bundledSkillFiles() throws IOException {
        Map<String, String> files = new LinkedHashMap<>();
        for (String relativePath : bundledSkillFileNames()) {
            files.put(relativePath, readResourceText(skillResource(relativePath)));
        }
        return files;
    }

    private static String skillResource(String relativePath) {
        String normalizedPath = normalizedSkillRelativePath(relativePath);
        String resourceName = SHAFT_SKILLS_ROOT + "/" + normalizedPath;
        if (!Path.of(resourceName).normalize().startsWith(Path.of(SHAFT_SKILLS_ROOT))) {
            throw new IllegalArgumentException("Unsafe SHAFT skill resource: " + relativePath);
        }
        return resourceName;
    }

    private static List<String> bundledSkillFileNames() throws IOException {
        URL url = resource(SHAFT_SKILLS_ROOT);
        try {
            if ("file".equalsIgnoreCase(url.getProtocol())) {
                Path source = Path.of(url.toURI());
                try (var files = Files.walk(source)) {
                    return files.filter(Files::isRegularFile)
                            .map(source::relativize)
                            .map(Path::toString)
                            .map(ShaftProjectService::normalizedSkillRelativePath)
                            .sorted()
                            .toList();
                }
            }
            if ("jar".equalsIgnoreCase(url.getProtocol())) {
                JarURLConnection connection = (JarURLConnection) url.openConnection();
                String prefix = connection.getEntryName() + "/";
                try (JarFile jar = connection.getJarFile()) {
                    Set<String> paths = new LinkedHashSet<>();
                    for (JarEntry entry : jar.stream().filter(candidate -> !candidate.isDirectory())
                            .filter(candidate -> candidate.getName().startsWith(prefix)).toList()) {
                        String remainder = entry.getName().substring(prefix.length());
                        paths.add(normalizedSkillRelativePath(remainder));
                    }
                    return paths.stream().sorted().toList();
                }
            }
            throw new IOException("Unsupported SHAFT skills resource protocol: " + url.getProtocol());
        } catch (IOException exception) {
            throw exception;
        } catch (Exception exception) {
            throw new IOException("SHAFT skills resources could not be read.", exception);
        }
    }

    private static String normalizedSkillRelativePath(String relativePath) {
        if (relativePath == null || relativePath.isBlank()) {
            throw new IllegalArgumentException("SHAFT skill resource path must not be blank.");
        }
        Path path = Path.of(relativePath).normalize();
        if (path.isAbsolute() || path.getNameCount() == 0 || path.startsWith("..")) {
            throw new IllegalArgumentException("Unsafe SHAFT skill resource: " + relativePath);
        }
        String normalized = path.toString().replace('\\', '/');
        if (normalized.equals(".") || normalized.startsWith("../") || normalized.contains(":")) {
            throw new IllegalArgumentException("Unsafe SHAFT skill resource: " + relativePath);
        }
        return normalized;
    }

    private static String readResourceText(String resourceName) throws IOException {
        try (var input = resource(resourceName).openStream()) {
            return new String(input.readAllBytes(), StandardCharsets.UTF_8);
        }
    }

    private static String titleCase(String skillName) {
        StringBuilder title = new StringBuilder();
        for (String word : skillName.split("-")) {
            if (title.length() > 0) {
                title.append(' ');
            }
            title.append("mcp".equals(word)
                    ? "MCP"
                    : Character.toUpperCase(word.charAt(0)) + word.substring(1));
        }
        return title.toString();
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
                    extractArchiveEntries(jar, prefix, target, overwrite);
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

    static void extractArchiveEntries(JarFile archive, String prefix, Path extractionRoot, boolean overwrite)
            throws IOException {
        for (JarEntry entry : archive.stream()
                .filter(candidate -> candidate.getName().startsWith(prefix)).toList()) {
            String relativePath = entry.getName().substring(prefix.length());
            if (relativePath.isEmpty()) {
                continue;
            }
            Path destination = safeArchiveDestination(extractionRoot, relativePath);
            if (entry.isDirectory()) {
                Files.createDirectories(destination);
                continue;
            }
            try (var input = archive.getInputStream(entry)) {
                copyFile(input.readAllBytes(), destination, extractionRoot, overwrite);
            }
        }
    }

    static Path safeArchiveDestination(Path extractionRoot, String archiveEntryName) {
        if (archiveEntryName == null || archiveEntryName.isBlank()) {
            throw new IllegalArgumentException("Archive entry name must not be blank.");
        }
        Path root = extractionRoot.toAbsolutePath().normalize();
        Path entryPath = Path.of(archiveEntryName.replace('\\', '/')).normalize();
        if (entryPath.isAbsolute()) {
            throw new IllegalArgumentException("Archive entry escaped the extraction directory.");
        }
        Path destination = root.resolve(entryPath).normalize();
        if (!destination.startsWith(root)) {
            throw new IllegalArgumentException("Archive entry escaped the extraction directory.");
        }
        return safeDestination(destination, root);
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
        if (targetRoot == null) {
            return normalized;
        }
        Path root = targetRoot.toAbsolutePath().normalize();
        if (!normalized.startsWith(root)) {
            throw new IllegalArgumentException("Generated project file escaped the output directory.");
        }
        try {
            Path existingRoot = root;
            while (existingRoot != null && !Files.exists(existingRoot)) {
                existingRoot = existingRoot.getParent();
            }
            if (existingRoot == null) {
                throw new IllegalArgumentException("Generated project file could not be safely resolved.");
            }
            Path realRoot = existingRoot.toRealPath();
            Path existing = normalized;
            while (existing != null && !Files.exists(existing)) {
                existing = existing.getParent();
            }
            if (existing == null || !existing.toRealPath().startsWith(realRoot)) {
                throw new IllegalArgumentException("Generated project file escaped the output directory.");
            }
        } catch (IOException exception) {
            throw new IllegalArgumentException("Generated project file could not be safely resolved.", exception);
        }
        return normalized;
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
        } catch (IOException | java.net.URISyntaxException exception) {
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

    private static String readTextFromUrl(String target) throws IOException, URISyntaxException {
        URL url = new URI(target).toURL();
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
