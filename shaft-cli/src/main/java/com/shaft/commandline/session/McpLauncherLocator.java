package com.shaft.commandline.session;

import com.shaft.commandline.mcp.McpException;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * Resolves a {@link LaunchSpec} for shaft-mcp by probing, in order: an explicit
 * {@code SHAFT_MCP_JAR} override, the installer's per-user versions directory, and finally a
 * dev-checkout fallback that reconstructs a classpath for the thin {@code shaft-mcp} target jar.
 */
public final class McpLauncherLocator {

    private static final String MAIN_CLASS = "com.shaft.mcp.ShaftMcpApplication";
    private static final String ARGS_FILE_NAME = "shaft-mcp.args";
    private static final String JAR_FILE_NAME = "shaft-mcp.jar";
    private static final String RUNTIME_DEPENDENCIES_ENTRY = "META-INF/shaft-mcp/runtime-dependencies.txt";

    private final Map<String, String> env;
    private final Path versionsRootOverride;
    private final Path devRepoRootOverride;

    /**
     * Creates a locator that uses the real process environment and real per-OS/dev roots.
     */
    public McpLauncherLocator() {
        this(System.getenv(), null, null);
    }

    /**
     * Creates a locator with overridable inputs, for tests driving it against temp directories.
     *
     * @param env                   the environment variables to read {@code SHAFT_MCP_JAR},
     *                              {@code LOCALAPPDATA}, and {@code XDG_DATA_HOME} from
     * @param versionsRootOverride  when non-null, used directly as the installer versions directory
     *                              instead of the computed per-OS default
     * @param devRepoRootOverride   when non-null, used as the working directory basis for the dev
     *                              fallback instead of the real {@code user.dir}
     */
    McpLauncherLocator(Map<String, String> env, Path versionsRootOverride, Path devRepoRootOverride) {
        this.env = env;
        this.versionsRootOverride = versionsRootOverride;
        this.devRepoRootOverride = devRepoRootOverride;
    }

    /**
     * Resolves how to launch shaft-mcp, trying the env override, then the installer versions
     * directory, then the dev-checkout fallback.
     *
     * @return a ready-to-run {@link LaunchSpec}
     * @throws McpException if no shaft-mcp installation or dev build could be found
     */
    public LaunchSpec locate() {
        String javaExe = javaExecutable();

        LaunchSpec fromEnv = tryEnvOverride(javaExe);
        if (fromEnv != null) {
            return fromEnv;
        }

        LaunchSpec fromVersionsDir = tryVersionsDirectory(javaExe);
        if (fromVersionsDir != null) {
            return fromVersionsDir;
        }

        LaunchSpec fromDevBuild = tryDevFallback(javaExe);
        if (fromDevBuild != null) {
            return fromDevBuild;
        }

        throw new McpException("Could not locate a shaft-mcp launcher. Install shaft-mcp (run the shaft-mcp "
                + "installer), set the SHAFT_MCP_JAR environment variable to a shaft-mcp jar or .args file, "
                + "or build shaft-mcp from a sibling checkout (../shaft-mcp or ./shaft-mcp).");
    }

    private LaunchSpec tryEnvOverride(String javaExe) {
        String override = env.get("SHAFT_MCP_JAR");
        if (override == null || override.isBlank()) {
            return null;
        }
        Path target = Path.of(override);
        if (!Files.isRegularFile(target)) {
            throw new McpException("SHAFT_MCP_JAR is set to '" + override + "' but that file does not exist.");
        }
        Path sibling = target.resolveSibling(ARGS_FILE_NAME);
        if (Files.isRegularFile(sibling)) {
            return new LaunchSpec(List.of(javaExe, "@" + sibling), target.toString());
        }
        if (target.getFileName().toString().endsWith(".args")) {
            return new LaunchSpec(List.of(javaExe, "@" + target), target.toString());
        }
        return new LaunchSpec(List.of(javaExe, "-cp", target.toString(), MAIN_CLASS), target.toString());
    }

    private LaunchSpec tryVersionsDirectory(String javaExe) {
        Path versionsRoot = versionsRootOverride != null ? versionsRootOverride : defaultVersionsRoot();
        if (!Files.isDirectory(versionsRoot)) {
            return null;
        }
        Optional<Path> newestVersionDir = newestVersionDirectory(versionsRoot);
        if (newestVersionDir.isEmpty()) {
            return null;
        }
        Path versionDir = newestVersionDir.get();
        Path argsFile = versionDir.resolve(ARGS_FILE_NAME);
        Path jarFile = versionDir.resolve(JAR_FILE_NAME);
        return new LaunchSpec(List.of(javaExe, "@" + argsFile), jarFile.toString());
    }

    /**
     * "Newest" is a lexicographic max over version-subdirectory names that contain a
     * {@code shaft-mcp.args} file. This is a simplification: it does not parse semantic-version
     * ordering, so a version scheme with inconsistent digit widths (e.g. {@code 9.0.0} vs.
     * {@code 10.0.0}) could sort incorrectly. shaft-mcp versions are date-based
     * ({@code 10.3.yyyymmddhh}) and sort correctly under this scheme in practice.
     */
    private Optional<Path> newestVersionDirectory(Path versionsRoot) {
        try (Stream<Path> entries = Files.list(versionsRoot)) {
            return entries
                    .filter(Files::isDirectory)
                    .filter(dir -> Files.isRegularFile(dir.resolve(ARGS_FILE_NAME)))
                    .max(Comparator.comparing(dir -> dir.getFileName().toString()));
        } catch (IOException exception) {
            return Optional.empty();
        }
    }

    private Path defaultVersionsRoot() {
        return applicationDataRoot().resolve("versions");
    }

    private Path applicationDataRoot() {
        String os = System.getProperty("os.name", "").toLowerCase(Locale.ROOT);
        Path home = Path.of(System.getProperty("user.home"));
        if (os.contains("win")) {
            String localAppData = env.get("LOCALAPPDATA");
            Path base = localAppData != null && !localAppData.isBlank()
                    ? Path.of(localAppData)
                    : home.resolve("AppData").resolve("Local");
            return base.resolve("ShaftHQ").resolve("shaft-mcp");
        }
        if (os.contains("mac") || os.contains("darwin")) {
            return home.resolve("Library").resolve("Application Support").resolve("ShaftHQ").resolve("shaft-mcp");
        }
        String xdgDataHome = env.get("XDG_DATA_HOME");
        Path base = xdgDataHome != null && !xdgDataHome.isBlank()
                ? Path.of(xdgDataHome)
                : home.resolve(".local").resolve("share");
        return base.resolve("shafthq").resolve("shaft-mcp");
    }

    private LaunchSpec tryDevFallback(String javaExe) {
        Path devBase = devRepoRootOverride != null ? devRepoRootOverride : Path.of(System.getProperty("user.dir"));
        List<Path> candidateDirs = List.of(
                devBase.resolve("..").resolve("shaft-mcp").resolve("target"),
                devBase.resolve("shaft-mcp").resolve("target"));
        for (Path candidateDir : candidateDirs) {
            Optional<Path> thinJar = findThinJar(candidateDir);
            if (thinJar.isPresent()) {
                return buildFromThinJar(thinJar.get(), javaExe);
            }
        }
        return null;
    }

    private Optional<Path> findThinJar(Path dir) {
        if (!Files.isDirectory(dir)) {
            return Optional.empty();
        }
        try (Stream<Path> entries = Files.list(dir)) {
            return entries
                    .filter(Files::isRegularFile)
                    .filter(path -> {
                        String name = path.getFileName().toString();
                        return name.startsWith("shaft-mcp-") && name.endsWith(".jar");
                    })
                    .max(Comparator.comparing(path -> path.getFileName().toString()));
        } catch (IOException exception) {
            return Optional.empty();
        }
    }

    private LaunchSpec buildFromThinJar(Path thinJar, String javaExe) {
        List<Path> dependencyJars = resolveRuntimeDependencies(thinJar);
        if (dependencyJars.isEmpty()) {
            throw new McpException("shaft-mcp dev jar at '" + thinJar + "' has no resolvable runtime dependencies "
                    + "in ~/.m2/repository. Build shaft-mcp first, run the shaft-mcp installer, "
                    + "or set SHAFT_MCP_JAR.");
        }
        String classpath = Stream.concat(Stream.of(thinJar), dependencyJars.stream())
                .map(Path::toString)
                .collect(Collectors.joining(File.pathSeparator));
        Path argsFile = writeDevArgsFile(thinJar, classpath);
        return new LaunchSpec(List.of(javaExe, "@" + argsFile), thinJar.toString());
    }

    /**
     * Writes a Java {@code @argfile} next to the dev thin jar: a dev checkout classpath spans every
     * SHAFT module's transitive jars, and passing that inline on Windows can exceed the ~32K
     * CreateProcess command-line limit ("The filename or extension is too long").
     */
    private Path writeDevArgsFile(Path thinJar, String classpath) {
        Path argsFile = thinJar.resolveSibling("shaft-mcp-dev.args");
        String content = "-cp" + System.lineSeparator()
                + '"' + classpath.replace("\\", "/").replace("\"", "\\\"") + '"' + System.lineSeparator()
                + MAIN_CLASS + System.lineSeparator();
        try {
            Files.writeString(argsFile, content, StandardCharsets.UTF_8);
        } catch (IOException exception) {
            throw new McpException("Could not write dev launch argfile next to '" + thinJar + "'.", exception);
        }
        return argsFile.toAbsolutePath().normalize();
    }

    private List<Path> resolveRuntimeDependencies(Path thinJar) {
        String manifest = readRuntimeDependencyManifest(thinJar);
        Path localRepository = Path.of(System.getProperty("user.home"), ".m2", "repository");
        List<Path> resolved = new ArrayList<>();
        for (String rawLine : manifest.split("\\R")) {
            String line = rawLine.strip();
            if (line.isEmpty() || line.startsWith("The following")) {
                continue;
            }
            Path dependencyJar = resolveDependencyLine(line, localRepository);
            if (dependencyJar != null && Files.isRegularFile(dependencyJar)) {
                resolved.add(dependencyJar);
            }
        }
        return resolved;
    }

    /**
     * Parses one {@code group:artifact:packaging[:classifier]:version:scope} coordinate line
     * (matching the installer's {@code parse_runtime_dependency_manifest}), skipping test-scope
     * and non-jar entries. Malformed lines are skipped rather than failing the whole resolution,
     * since this dev-fallback path is best-effort.
     */
    private Path resolveDependencyLine(String line, Path localRepository) {
        String token = line.split("\\s+")[0];
        String[] parts = token.split(":");
        String groupId;
        String artifactId;
        String packaging;
        String classifier = null;
        String version;
        String scope;
        if (parts.length == 5) {
            groupId = parts[0];
            artifactId = parts[1];
            packaging = parts[2];
            version = parts[3];
            scope = parts[4];
        } else if (parts.length == 6) {
            groupId = parts[0];
            artifactId = parts[1];
            packaging = parts[2];
            classifier = parts[3];
            version = parts[4];
            scope = parts[5];
        } else {
            return null;
        }
        if (!"jar".equals(packaging) || "test".equals(scope)) {
            return null;
        }
        String fileName = artifactId + "-" + version + (classifier != null ? "-" + classifier : "") + ".jar";
        Path groupPath = Path.of(groupId.replace('.', File.separatorChar));
        return localRepository.resolve(groupPath).resolve(artifactId).resolve(version).resolve(fileName);
    }

    private String readRuntimeDependencyManifest(Path thinJar) {
        try (ZipFile zip = new ZipFile(thinJar.toFile())) {
            ZipEntry entry = zip.getEntry(RUNTIME_DEPENDENCIES_ENTRY);
            if (entry == null) {
                throw new McpException("shaft-mcp dev jar at '" + thinJar + "' is missing "
                        + RUNTIME_DEPENDENCIES_ENTRY + "; run the shaft-mcp installer or set SHAFT_MCP_JAR.");
            }
            try (InputStream input = zip.getInputStream(entry)) {
                return new String(input.readAllBytes(), StandardCharsets.UTF_8);
            }
        } catch (IOException exception) {
            throw new McpException("Failed to read '" + thinJar + "': " + exception.getMessage(), exception);
        }
    }

    private static String javaExecutable() {
        Optional<String> currentCommand = ProcessHandle.current().info().command();
        if (currentCommand.isPresent() && looksLikeJava(currentCommand.get())) {
            return currentCommand.get();
        }
        String javaHome = System.getProperty("java.home");
        String exeName = System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("win")
                ? "java.exe"
                : "java";
        return Path.of(javaHome, "bin", exeName).toString();
    }

    private static boolean looksLikeJava(String command) {
        String fileName = Path.of(command).getFileName().toString().toLowerCase(Locale.ROOT);
        return fileName.equals("java") || fileName.equals("java.exe");
    }
}
