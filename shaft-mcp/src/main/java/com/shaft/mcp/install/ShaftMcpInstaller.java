package com.shaft.mcp.install;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.function.BiConsumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Installs the running shaft-mcp executable JAR and configures one local MCP client.
 */
public final class ShaftMcpInstaller {
    /** Manual setup and recovery documentation. */
    public static final String MANUAL_URL = "https://shaftengine.netlify.app/docs/agentic/mcp/manual";

    private static final Logger LOGGER = LoggerFactory.getLogger(ShaftMcpInstaller.class);
    private static final Pattern VERSION_PATTERN = Pattern.compile("([A-Za-z0-9][A-Za-z0-9._-]*)");
    private static final Pattern MAVEN_VERSION = Pattern.compile("Apache Maven\\s+(\\d+)\\.(\\d+)");
    private static final String SERVER_NAME = "shaft-mcp";

    private ShaftMcpInstaller() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Runs the local installer.
     *
     * @param arguments installer target flag
     * @return zero on success, nonzero on failure
     */
    public static int run(String[] arguments) {
        try {
            return run(arguments, InstallerEnvironment.system());
        } catch (Exception exception) {
            LOGGER.error("shaft-mcp installation failed. {} Manual setup: {}",
                    exception.getMessage(), MANUAL_URL);
            return 2;
        }
    }

    static int run(String[] arguments, InstallerEnvironment environment) {
        Objects.requireNonNull(environment, "environment");
        try {
            Target target = Target.parse(arguments);
            validatePrerequisites(target, environment);
            detectProjectOverride(target, environment);
            Path installedJar = installJar(environment);
            try {
                environment.probe().accept(environment.javaExecutable(), installedJar);
            } catch (ProbeFailure failure) {
                throw new InstallerFailure("The installed shaft-mcp JAR failed its stdio probe.", 4, failure);
            }
            configure(target, environment, installedJar);
            LOGGER.info("shaft-mcp {} is installed and configured for {}.",
                    environment.version(), target.displayName());
            target.restartMessage().ifPresent(message -> LOGGER.info(message));
            return 0;
        } catch (InstallerFailure failure) {
            LOGGER.error("{} Manual setup: {}", failure.getMessage(), MANUAL_URL);
            return failure.exitCode();
        }
    }

    private static void validatePrerequisites(Target target, InstallerEnvironment environment)
            throws InstallerFailure {
        if (environment.javaFeatureVersion() != 25) {
            throw new InstallerFailure("Java 25 is required; detected Java "
                    + environment.javaFeatureVersion() + ".", 3);
        }
        CommandResult maven = environment.commandRunner().run(List.of("mvn", "--version"));
        Matcher matcher = MAVEN_VERSION.matcher(maven.output());
        if (maven.exitCode() != 0 || !matcher.find()
                || Integer.parseInt(matcher.group(1)) < 3
                || Integer.parseInt(matcher.group(1)) == 3 && Integer.parseInt(matcher.group(2)) < 9) {
            throw new InstallerFailure("Maven 3.9 or newer is required.", 3);
        }
        if (!target.supportedOperatingSystems().contains(environment.operatingSystem())) {
            throw new InstallerFailure(target.displayName() + " is not supported on "
                    + environment.operatingSystem().displayName() + ".", 3);
        }
        if (!clientAvailable(target, environment)) {
            throw new InstallerFailure(target.displayName()
                    + " is not installed or its command is unavailable.", 3);
        }
    }

    private static boolean clientAvailable(Target target, InstallerEnvironment environment) {
        if (target == Target.CLAUDE_DESKTOP) {
            return environment.claudeDesktopInstalled();
        }
        return environment.commandRunner().run(List.of(target.clientCommand(), "--version")).exitCode() == 0;
    }

    private static Path installJar(InstallerEnvironment environment) throws InstallerFailure {
        if (!VERSION_PATTERN.matcher(environment.version()).matches()) {
            throw new InstallerFailure("The executable JAR does not contain a valid Implementation-Version.", 4);
        }
        if (!Files.isRegularFile(environment.sourceJar())) {
            throw new InstallerFailure("Run the installer from the executable shaft-mcp JAR.", 4);
        }
        Path destination = environment.applicationDataRoot()
                .resolve("versions")
                .resolve(environment.version())
                .resolve("shaft-mcp.jar")
                .toAbsolutePath()
                .normalize();
        try {
            InstallerFileOperations.copyVerifiedAtomically(environment.sourceJar(), destination);
            return destination;
        } catch (IOException exception) {
            throw new InstallerFailure("Could not install shaft-mcp into " + destination + ".", 4, exception);
        }
    }

    private static void configure(Target target, InstallerEnvironment environment, Path jar)
            throws InstallerFailure {
        Path configuration = configurationPath(target, environment);
        try (var backup = new InstallerFileOperations.ConfigurationBackup(configuration)) {
            try {
                switch (target) {
                    case CODEX, CODEX_APP -> configureCodex(environment, jar);
                    case CLAUDE -> configureClaudeCode(environment, jar);
                    case CLAUDE_DESKTOP -> configureClaudeDesktop(environment, jar);
                    case COPILOT -> configureCopilot(environment, jar);
                    case COPILOT_VSCODE -> configureVsCode(environment, jar);
                }
                verifyConfiguration(target, environment, jar);
                backup.commit();
            } catch (Exception failure) {
                restoreAfterFailure(backup, failure);
            }
        } catch (InstallerFailure failure) {
            throw failure;
        } catch (IOException exception) {
            throw new InstallerFailure("Could not create or remove the configuration backup.", 5, exception);
        }
    }

    private static void restoreAfterFailure(
            InstallerFileOperations.ConfigurationBackup backup,
            Exception failure) throws InstallerFailure {
        try {
            backup.restore();
            throw new InstallerFailure("Client configuration failed and configuration was restored.", 5, failure);
        } catch (IOException restorationFailure) {
            failure.addSuppressed(restorationFailure);
            throw new InstallerFailure("Client configuration failed and restoration also failed. Backup retained at "
                    + backup.backupPath() + ".", 6, failure);
        }
    }

    private static void configureCodex(InstallerEnvironment environment, Path jar) throws InstallerFailure {
        CommandResult result = environment.commandRunner().run(List.of(
                "codex", "mcp", "add", SERVER_NAME, "--",
                environment.javaExecutable().toString(), "-jar", jar.toString()));
        requireSuccess(result, "Codex MCP configuration command failed.");
    }

    private static void configureClaudeCode(InstallerEnvironment environment, Path jar) throws InstallerFailure {
        Path configuration = configurationPath(Target.CLAUDE, environment);
        try {
            JsonNode existing = InstallerFileOperations.readJsonObject(configuration)
                    .path("mcpServers").path(SERVER_NAME);
            if (!existing.isMissingNode()) {
                requireSuccess(
                        environment.commandRunner().run(
                                List.of("claude", "mcp", "remove", SERVER_NAME, "-s", "user")),
                        "Claude Code could not remove the previous shaft-mcp entry.");
            }
        } catch (IOException exception) {
            throw new InstallerFailure("Claude Code configuration is malformed.", 5, exception);
        }
        requireSuccess(environment.commandRunner().run(List.of(
                "claude", "mcp", "add", "-s", "user", SERVER_NAME, "--",
                environment.javaExecutable().toString(), "-jar", jar.toString())),
                "Claude Code MCP configuration command failed.");
    }

    private static void configureClaudeDesktop(InstallerEnvironment environment, Path jar)
            throws IOException {
        ObjectNode root = InstallerFileOperations.readJsonObject(
                configurationPath(Target.CLAUDE_DESKTOP, environment));
        ObjectNode servers = object(root, "mcpServers");
        servers.set(SERVER_NAME, stdioEntry(environment.javaExecutable(), jar, false));
        InstallerFileOperations.writeJsonAtomically(
                configurationPath(Target.CLAUDE_DESKTOP, environment), root);
    }

    private static void configureCopilot(InstallerEnvironment environment, Path jar) throws IOException {
        ObjectNode root = InstallerFileOperations.readJsonObject(configurationPath(Target.COPILOT, environment));
        ObjectNode servers = object(root, "mcpServers");
        ObjectNode entry = stdioEntry(environment.javaExecutable(), jar, true);
        entry.put("type", "local");
        ArrayNode tools = entry.putArray("tools");
        tools.add("*");
        servers.set(SERVER_NAME, entry);
        InstallerFileOperations.writeJsonAtomically(configurationPath(Target.COPILOT, environment), root);
    }

    private static void configureVsCode(InstallerEnvironment environment, Path jar)
            throws IOException, InstallerFailure {
        ObjectNode entry = stdioEntry(environment.javaExecutable(), jar, true);
        entry.put("name", SERVER_NAME);
        entry.put("type", "stdio");
        requireSuccess(environment.commandRunner().run(
                List.of("code", "--add-mcp", InstallerFileOperations.writeJson(entry))),
                "VS Code MCP configuration command failed.");
    }

    private static ObjectNode stdioEntry(Path javaExecutable, Path jar, boolean argsBeforeCommand) {
        ObjectNode entry = InstallerFileOperations.newJsonObject();
        if (argsBeforeCommand) {
            ArrayNode arguments = entry.putArray("args");
            arguments.add("-jar");
            arguments.add(jar.toString());
            entry.put("command", javaExecutable.toString());
        } else {
            entry.put("command", javaExecutable.toString());
            ArrayNode arguments = entry.putArray("args");
            arguments.add("-jar");
            arguments.add(jar.toString());
        }
        return entry;
    }

    private static ObjectNode object(ObjectNode parent, String property) throws IOException {
        JsonNode existing = parent.get(property);
        if (existing == null) {
            return parent.putObject(property);
        }
        if (existing instanceof ObjectNode object) {
            return object;
        }
        throw new IOException("Configuration property must be an object: " + property);
    }

    private static void verifyConfiguration(Target target, InstallerEnvironment environment, Path jar)
            throws InstallerFailure, IOException {
        if (target == Target.CODEX || target == Target.CODEX_APP) {
            CommandResult result = environment.commandRunner().run(
                    List.of("codex", "mcp", "get", SERVER_NAME, "--json"));
            requireSuccess(result, "Codex could not verify the shaft-mcp entry.");
            JsonNode entry = InstallerFileOperations.parseJson(result.output());
            verifyCommand(entry.path("transport"), environment.javaExecutable(), jar);
            return;
        }

        Path path = configurationPath(target, environment);
        JsonNode root = InstallerFileOperations.readJsonObject(path);
        JsonNode entry = switch (target) {
            case CLAUDE, CLAUDE_DESKTOP, COPILOT -> root.path("mcpServers").path(SERVER_NAME);
            case COPILOT_VSCODE -> root.path("servers").path(SERVER_NAME);
            default -> throw new IllegalStateException("Unexpected target: " + target);
        };
        verifyCommand(entry, environment.javaExecutable(), jar);
    }

    private static void verifyCommand(JsonNode entry, Path javaExecutable, Path jar) throws InstallerFailure {
        if (!javaExecutable.toString().equals(entry.path("command").asText())) {
            throw new InstallerFailure("The resulting shaft-mcp Java command is incorrect.", 5);
        }
        JsonNode arguments = entry.path("args");
        if (!arguments.isArray() || arguments.size() != 2
                || !"-jar".equals(arguments.get(0).asText())
                || !jar.toString().equals(arguments.get(1).asText())) {
            throw new InstallerFailure("The resulting shaft-mcp JAR arguments are incorrect.", 5);
        }
    }

    private static void requireSuccess(CommandResult result, String message) throws InstallerFailure {
        if (result.exitCode() != 0) {
            String detail = result.output().isBlank() ? "" : " " + result.output().strip();
            throw new InstallerFailure(message + detail, 5);
        }
    }

    private static Path configurationPath(Target target, InstallerEnvironment environment) {
        Path home = environment.userHome();
        Map<String, String> variables = environment.environmentVariables();
        return switch (target) {
            case CODEX, CODEX_APP -> optionalPath(variables.get("CODEX_HOME"))
                    .orElse(home.resolve(".codex"))
                    .resolve("config.toml");
            case CLAUDE -> home.resolve(".claude.json");
            case CLAUDE_DESKTOP -> environment.operatingSystem() == OperatingSystem.WINDOWS
                    ? Path.of(requiredVariable(variables, "APPDATA")).resolve("Claude").resolve("claude_desktop_config.json")
                    : home.resolve("Library").resolve("Application Support").resolve("Claude")
                            .resolve("claude_desktop_config.json");
            case COPILOT -> optionalPath(variables.get("COPILOT_HOME"))
                    .orElse(home.resolve(".copilot"))
                    .resolve("mcp-config.json");
            case COPILOT_VSCODE -> switch (environment.operatingSystem()) {
                case WINDOWS -> Path.of(requiredVariable(variables, "APPDATA"))
                        .resolve("Code").resolve("User").resolve("mcp.json");
                case MACOS -> home.resolve("Library").resolve("Application Support")
                        .resolve("Code").resolve("User").resolve("mcp.json");
                case LINUX -> optionalPath(variables.get("XDG_CONFIG_HOME"))
                        .orElse(home.resolve(".config"))
                        .resolve("Code").resolve("User").resolve("mcp.json");
            };
        };
    }

    private static Optional<Path> optionalPath(String value) {
        return value == null || value.isBlank() ? Optional.empty() : Optional.of(Path.of(value));
    }

    private static String requiredVariable(Map<String, String> variables, String name) {
        String value = variables.get(name);
        if (value == null || value.isBlank()) {
            throw new IllegalStateException(name + " is unavailable.");
        }
        return value;
    }

    private static void detectProjectOverride(Target target, InstallerEnvironment environment)
            throws InstallerFailure {
        List<Path> candidates = new ArrayList<>();
        for (Path directory = environment.workingDirectory().toAbsolutePath().normalize();
             directory != null;
             directory = directory.getParent()) {
            switch (target) {
                case CODEX, CODEX_APP -> candidates.add(directory.resolve(".codex").resolve("config.toml"));
                case CLAUDE, CLAUDE_DESKTOP -> candidates.add(directory.resolve(".mcp.json"));
                case COPILOT -> {
                    candidates.add(directory.resolve(".github").resolve("mcp.json"));
                    candidates.add(directory.resolve(".mcp.json"));
                }
                case COPILOT_VSCODE -> candidates.add(directory.resolve(".vscode").resolve("mcp.json"));
            }
            if (directory.equals(environment.userHome().toAbsolutePath().normalize())) {
                break;
            }
        }
        for (Path candidate : candidates) {
            if (containsProjectEntry(candidate, target)) {
                throw new InstallerFailure("Project configuration at " + candidate
                        + " defines shaft-mcp and would override the per-user entry.", 5);
            }
        }
    }

    private static boolean containsProjectEntry(Path path, Target target) throws InstallerFailure {
        if (!Files.isRegularFile(path)) {
            return false;
        }
        try {
            if (target == Target.CODEX || target == Target.CODEX_APP) {
                String toml = Files.readString(path, StandardCharsets.UTF_8);
                return Pattern.compile("(?m)^\\s*(?:"
                                + "\\[\\s*mcp_servers\\.(?:\"shaft-mcp\"|shaft-mcp)\\s*]"
                                + "|mcp_servers\\.(?:\"shaft-mcp\"|shaft-mcp)\\s*=)")
                        .matcher(toml)
                        .find();
            }
            JsonNode root = InstallerFileOperations.readJsonObject(path);
            return root.path("mcpServers").has(SERVER_NAME)
                    || root.path("servers").has(SERVER_NAME);
        } catch (IOException exception) {
            throw new InstallerFailure("Project MCP configuration is malformed: " + path, 5, exception);
        }
    }

    enum Target {
        CODEX("--codex", "Codex CLI and IDE extension", "codex", EnumSet.allOf(OperatingSystem.class), null),
        CODEX_APP("--codex-app", "Codex App", "codex", EnumSet.allOf(OperatingSystem.class),
                "Start a new Codex App session to load shaft-mcp."),
        CLAUDE("--claude", "Claude Code", "claude", EnumSet.allOf(OperatingSystem.class), null),
        CLAUDE_DESKTOP("--claude-desktop", "Claude Desktop", "",
                EnumSet.of(OperatingSystem.WINDOWS, OperatingSystem.MACOS),
                "Restart Claude Desktop to load shaft-mcp."),
        COPILOT("--copilot", "GitHub Copilot CLI", "copilot", EnumSet.allOf(OperatingSystem.class), null),
        COPILOT_VSCODE("--copilot-vscode", "GitHub Copilot in VS Code", "code",
                EnumSet.allOf(OperatingSystem.class), null);

        private final String flag;
        private final String displayName;
        private final String clientCommand;
        private final EnumSet<OperatingSystem> supportedOperatingSystems;
        private final String restartMessage;

        Target(
                String flag,
                String displayName,
                String clientCommand,
                EnumSet<OperatingSystem> supportedOperatingSystems,
                String restartMessage) {
            this.flag = flag;
            this.displayName = displayName;
            this.clientCommand = clientCommand;
            this.supportedOperatingSystems = supportedOperatingSystems;
            this.restartMessage = restartMessage;
        }

        static Target parse(String[] arguments) throws InstallerFailure {
            if (arguments == null || arguments.length != 1) {
                throw new InstallerFailure("Pass exactly one target: "
                        + Arrays.stream(values()).map(target -> target.flag).toList() + ".", 2);
            }
            return Arrays.stream(values())
                    .filter(target -> target.flag.equals(arguments[0]))
                    .findFirst()
                    .orElseThrow(() -> new InstallerFailure("Unsupported installer target: "
                            + arguments[0] + ".", 2));
        }

        String displayName() {
            return displayName;
        }

        String clientCommand() {
            return clientCommand;
        }

        EnumSet<OperatingSystem> supportedOperatingSystems() {
            return supportedOperatingSystems;
        }

        Optional<String> restartMessage() {
            return Optional.ofNullable(restartMessage);
        }
    }

    enum OperatingSystem {
        WINDOWS("Windows"),
        MACOS("macOS"),
        LINUX("Linux");

        private final String displayName;

        OperatingSystem(String displayName) {
            this.displayName = displayName;
        }

        String displayName() {
            return displayName;
        }

        static OperatingSystem current() throws InstallerFailure {
            String name = System.getProperty("os.name", "").toLowerCase(Locale.ROOT);
            if (name.contains("win")) {
                return WINDOWS;
            }
            if (name.contains("mac") || name.contains("darwin")) {
                return MACOS;
            }
            if (name.contains("linux")) {
                return LINUX;
            }
            throw new InstallerFailure("Unsupported operating system: "
                    + System.getProperty("os.name", "unknown") + ".", 3);
        }
    }

    record CommandResult(int exitCode, String output) {
        CommandResult {
            output = output == null ? "" : output;
        }
    }

    @FunctionalInterface
    interface CommandRunner {
        CommandResult run(List<String> command);
    }

    record InstallerEnvironment(
            OperatingSystem operatingSystem,
            Map<String, String> environmentVariables,
            Path userHome,
            Path workingDirectory,
            Path javaExecutable,
            int javaFeatureVersion,
            Path sourceJar,
            String version,
            Path applicationDataRoot,
            CommandRunner commandRunner,
            BiConsumer<Path, Path> probe,
            boolean claudeDesktopInstalled) {

        InstallerEnvironment {
            environmentVariables = Map.copyOf(environmentVariables);
            userHome = userHome.toAbsolutePath().normalize();
            workingDirectory = workingDirectory.toAbsolutePath().normalize();
            javaExecutable = javaExecutable.toAbsolutePath().normalize();
            sourceJar = sourceJar.toAbsolutePath().normalize();
            applicationDataRoot = applicationDataRoot.toAbsolutePath().normalize();
        }

        static InstallerEnvironment system() throws InstallerFailure {
            OperatingSystem os = OperatingSystem.current();
            Map<String, String> variables = new HashMap<>(System.getenv());
            Path home = Path.of(System.getProperty("user.home")).toAbsolutePath().normalize();
            Path java = ProcessHandle.current().info().command()
                    .map(Path::of)
                    .orElseGet(() -> Path.of(System.getProperty("java.home"), "bin",
                            os == OperatingSystem.WINDOWS ? "java.exe" : "java"))
                    .toAbsolutePath()
                    .normalize();
            Path source = discoverSourceJar();
            String implementationVersion = Optional.ofNullable(
                    ShaftMcpInstaller.class.getPackage().getImplementationVersion())
                    .orElse("");
            Path appData = applicationDataRoot(os, variables, home);
            return new InstallerEnvironment(
                    os,
                    variables,
                    home,
                    Path.of("").toAbsolutePath(),
                    java,
                    Runtime.version().feature(),
                    source,
                    implementationVersion,
                    appData,
                    new SystemCommandRunner(os),
                    (javaExecutable, jar) -> {
                        try {
                            InstallerStdioProbe.verify(javaExecutable, jar);
                        } catch (IOException exception) {
                            throw new ProbeFailure(exception);
                        }
                    },
                    claudeDesktopInstalled(os, variables, home));
        }

        private static Path discoverSourceJar() throws InstallerFailure {
            String[] processArguments = ProcessHandle.current().info().arguments().orElse(new String[0]);
            for (int index = 0; index + 1 < processArguments.length; index++) {
                if ("-jar".equals(processArguments[index])) {
                    Path launchedJar = Path.of(processArguments[index + 1])
                            .toAbsolutePath()
                            .normalize();
                    if (Files.isRegularFile(launchedJar)) {
                        return launchedJar;
                    }
                }
            }
            String[] classPathEntries = ManagementFactory.getRuntimeMXBean()
                    .getClassPath()
                    .split(Pattern.quote(File.pathSeparator));
            if (classPathEntries.length == 1) {
                Path classPathJar = Path.of(classPathEntries[0]).toAbsolutePath().normalize();
                if (Files.isRegularFile(classPathJar)
                        && classPathJar.getFileName().toString().endsWith(".jar")) {
                    return classPathJar;
                }
            }
            try {
                Path source = Path.of(ShaftMcpInstaller.class.getProtectionDomain()
                        .getCodeSource().getLocation().toURI())
                        .toAbsolutePath()
                        .normalize();
                if (!Files.isRegularFile(source)) {
                    throw new InstallerFailure("Run the installer from the executable shaft-mcp JAR.", 4);
                }
                return source;
            } catch (URISyntaxException exception) {
                throw new InstallerFailure("Could not locate the running shaft-mcp JAR.", 4, exception);
            }
        }

        private static Path applicationDataRoot(
                OperatingSystem os,
                Map<String, String> variables,
                Path home) throws InstallerFailure {
            return switch (os) {
                case WINDOWS -> {
                    String localAppData = variables.get("LOCALAPPDATA");
                    if (localAppData == null || localAppData.isBlank()) {
                        throw new InstallerFailure("LOCALAPPDATA is unavailable.", 3);
                    }
                    yield Path.of(localAppData).resolve("ShaftHQ").resolve("shaft-mcp");
                }
                case MACOS -> home.resolve("Library").resolve("Application Support")
                        .resolve("ShaftHQ").resolve("shaft-mcp");
                case LINUX -> optionalPath(variables.get("XDG_DATA_HOME"))
                        .orElse(home.resolve(".local").resolve("share"))
                        .resolve("shafthq").resolve("shaft-mcp");
            };
        }

        private static boolean claudeDesktopInstalled(
                OperatingSystem os,
                Map<String, String> variables,
                Path home) {
            if (os == OperatingSystem.MACOS) {
                return Files.isDirectory(Path.of("/Applications/Claude.app"))
                        || Files.isDirectory(home.resolve("Applications").resolve("Claude.app"));
            }
            if (os != OperatingSystem.WINDOWS) {
                return false;
            }
            List<Path> candidates = new ArrayList<>();
            optionalPath(variables.get("LOCALAPPDATA")).ifPresent(local -> {
                candidates.add(local.resolve("AnthropicClaude").resolve("claude.exe"));
                candidates.add(local.resolve("Programs").resolve("Claude").resolve("Claude.exe"));
            });
            optionalPath(variables.get("PROGRAMFILES")).ifPresent(programFiles ->
                    candidates.add(programFiles.resolve("Claude").resolve("Claude.exe")));
            return candidates.stream().anyMatch(Files::isRegularFile);
        }
    }

    static final class SystemCommandRunner implements CommandRunner {
        private final OperatingSystem operatingSystem;

        SystemCommandRunner(OperatingSystem operatingSystem) {
            this.operatingSystem = operatingSystem;
        }

        @Override
        public CommandResult run(List<String> command) {
            List<String> launchCommand = command;
            boolean nativeVsCode = false;
            if (operatingSystem == OperatingSystem.WINDOWS) {
                Optional<List<String>> nativeCommand = nativeVsCodeCommand(command);
                if (nativeCommand.isPresent()) {
                    launchCommand = nativeCommand.get();
                    nativeVsCode = true;
                } else {
                    launchCommand = windowsCommand(command)
                            .orElse(command);
                }
            }
            try {
                ProcessBuilder builder = new ProcessBuilder(launchCommand)
                        .redirectErrorStream(true);
                if (nativeVsCode) {
                    builder.environment().put("ELECTRON_RUN_AS_NODE", "1");
                }
                Process process = builder.start();
                boolean finished = process.waitFor(Duration.ofSeconds(30).toMillis(), TimeUnit.MILLISECONDS);
                if (!finished) {
                    process.destroyForcibly();
                    return new CommandResult(124, "Command timed out.");
                }
                return new CommandResult(
                        process.exitValue(),
                        new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8));
            } catch (IOException exception) {
                return new CommandResult(127, exception.getMessage());
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
                return new CommandResult(130, "Command interrupted.");
            }
        }

        private static Optional<List<String>> nativeVsCodeCommand(List<String> command) {
            if (command.isEmpty() || !"code".equalsIgnoreCase(command.getFirst())) {
                return Optional.empty();
            }
            List<Path> roots = new ArrayList<>();
            optionalPath(System.getenv("LOCALAPPDATA")).ifPresent(local ->
                    roots.add(local.resolve("Programs")));
            optionalPath(System.getenv("PROGRAMFILES")).ifPresent(roots::add);
            optionalPath(System.getenv("PROGRAMFILES(X86)")).ifPresent(roots::add);
            for (Path root : roots) {
                for (String directory : List.of("Microsoft VS Code", "Microsoft VS Code Insiders")) {
                    Path installation = root.resolve(directory);
                    Path executable = installation.resolve(
                            directory.endsWith("Insiders") ? "Code - Insiders.exe" : "Code.exe");
                    Path cli = installation.resolve("resources").resolve("app")
                            .resolve("out").resolve("cli.js");
                    if (Files.isRegularFile(executable) && Files.isRegularFile(cli)) {
                        List<String> nativeCommand = new ArrayList<>();
                        nativeCommand.add(executable.toString());
                        nativeCommand.add(cli.toString());
                        nativeCommand.addAll(command.subList(1, command.size()));
                        return Optional.of(nativeCommand);
                    }
                }
            }
            return Optional.empty();
        }

        private static Optional<List<String>> windowsCommand(List<String> command) {
            if (command.isEmpty()) {
                return Optional.empty();
            }
            Optional<Path> executable = resolveWindowsExecutable(command.getFirst());
            if (executable.isEmpty()) {
                return Optional.empty();
            }
            String fileName = executable.get().getFileName().toString().toLowerCase(Locale.ROOT);
            List<String> resolved = new ArrayList<>();
            if (fileName.endsWith(".cmd") || fileName.endsWith(".bat")) {
                resolved.addAll(List.of("cmd.exe", "/d", "/s", "/c", "call"));
            }
            resolved.add(executable.get().toString());
            resolved.addAll(command.subList(1, command.size()));
            return Optional.of(resolved);
        }

        private static Optional<Path> resolveWindowsExecutable(String command) {
            Path requested = Path.of(command);
            if (requested.getNameCount() > 1 || requested.isAbsolute()) {
                return Files.isRegularFile(requested)
                        ? Optional.of(requested.toAbsolutePath().normalize())
                        : Optional.empty();
            }
            String pathValue = System.getenv("PATH");
            if (pathValue == null || pathValue.isBlank()) {
                return Optional.empty();
            }
            String pathExtensions = Optional.ofNullable(System.getenv("PATHEXT"))
                    .filter(value -> !value.isBlank())
                    .orElse(".COM;.EXE;.BAT;.CMD");
            List<String> candidates = new ArrayList<>(Arrays.stream(pathExtensions.split(";"))
                    .filter(extension -> !extension.isBlank())
                    .toList());
            if (command.contains(".")) {
                candidates.addFirst("");
            }
            for (String directory : pathValue.split(Pattern.quote(File.pathSeparator))) {
                if (directory.isBlank()) {
                    continue;
                }
                for (String extension : candidates) {
                    Path candidate = Path.of(directory, command + extension);
                    if (Files.isRegularFile(candidate)) {
                        return Optional.of(candidate.toAbsolutePath().normalize());
                    }
                }
            }
            return Optional.empty();
        }
    }

    static final class ProbeFailure extends RuntimeException {
        ProbeFailure(IOException cause) {
            super(cause.getMessage(), cause);
        }
    }

    static class InstallerFailure extends Exception {
        private final int exitCode;

        InstallerFailure(String message, int exitCode) {
            super(message);
            this.exitCode = exitCode;
        }

        InstallerFailure(String message, int exitCode, Throwable cause) {
            super(message, cause);
            this.exitCode = exitCode;
        }

        int exitCode() {
            return exitCode;
        }
    }
}
