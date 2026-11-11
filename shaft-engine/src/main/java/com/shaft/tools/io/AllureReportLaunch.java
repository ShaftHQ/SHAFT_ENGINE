package com.shaft.tools.io;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.jar.JarFile;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * Prepares an out-of-process rebuild of {@link com.shaft.driver.SHAFT.Report#generateAllureReport}.
 * The public report method stays source-compatible and runs in the current JVM. A separate launcher
 * must use this helper: a JDK whose major version is at least the SHAFT class-file version, a Maven
 * classpath rebuild that uses {@code -o} only when {@code shaft-engine} is already in the local
 * repository, a hard failure when {@code com/shaft/driver/SHAFT.class} is missing, and a Java
 * {@code @argfile} so Windows can pass {@code -cp} without hitting the command-line length limit.
 */
public final class AllureReportLaunch {
    /** Entry that must be present before a report rebuild is launched. */
    public static final String SHAFT_CLASS_ENTRY = "com/shaft/driver/SHAFT.class";

    private static final Pattern JAVAC_VERSION = Pattern.compile("javac\\s+(\\d+)(?:\\.(\\d+))?");
    private static final String ENGINE_GROUP_PATH = "io/github/shafthq/shaft-engine";

    private AllureReportLaunch() {
    }

    /**
     * Reads the Java major version required by a class-file header. SHAFT is compiled to major 69,
     * which is Java 25.
     *
     * @param classBytes at least the first eight bytes of a class file
     * @return the Java major version ({@code classMajor - 44})
     */
    public static int javaMajorRequiredBy(byte[] classBytes) {
        if (classBytes == null || classBytes.length < 8) {
            throw new IllegalArgumentException("Class file header is too short.");
        }
        int classMajor = ((classBytes[6] & 0xFF) << 8) | (classBytes[7] & 0xFF);
        return classMajor - 44;
    }

    /**
     * Reads the Java major required by the running engine class file.
     *
     * @return the Java major version required by the running {@code SHAFT.class}
     */
    public static int engineJavaMajor() {
        try (InputStream classFile = com.shaft.driver.SHAFT.class.getResourceAsStream("SHAFT.class")) {
            if (classFile == null) {
                throw new IllegalStateException(SHAFT_CLASS_ENTRY + " is absent from the classpath.");
            }
            return javaMajorRequiredBy(classFile.readNBytes(8));
        } catch (IOException exception) {
            throw new IllegalStateException("Could not read " + SHAFT_CLASS_ENTRY + ".", exception);
        }
    }

    /**
     * Parses the major version from {@code javac -version} output.
     *
     * @param javacOutput combined stdout and stderr of {@code javac -version}
     * @return the javac major version; legacy {@code 1.8} is returned as 8
     */
    public static int javaMajorFromJavacOutput(String javacOutput) {
        Matcher matcher = JAVAC_VERSION.matcher(javacOutput == null ? "" : javacOutput);
        if (!matcher.find()) {
            throw new IllegalArgumentException("Unrecognized javac version output.");
        }
        int major = Integer.parseInt(matcher.group(1));
        if (major == 1 && matcher.group(2) != null) {
            return Integer.parseInt(matcher.group(2));
        }
        return major;
    }

    /**
     * Runs {@code bin/javac -version} when that executable exists.
     * The image is the absolute path from {@link #javacVersionCommand(Path)}. The JDK home is not used
     * as the process working directory, because Windows resolves a relative application name from the
     * parent directory.
     *
     * @param jdkHome a JDK home directory, not a JRE
     * @return the javac major, or {@code -1} when javac is missing or its output cannot be read
     */
    public static int jdkMajor(Path jdkHome) {
        if (jdkHome == null) {
            return -1;
        }
        List<String> command = javacVersionCommand(jdkHome);
        if (!Files.isRegularFile(Path.of(command.get(0)))) {
            return -1;
        }
        try {
            // Absolute image. ProcessBuilder is not a shell; SpotBugs still reports COMMAND_INJECTION.
            Process process = new ProcessBuilder(command).redirectErrorStream(true).start();
            String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
            process.waitFor();
            return javaMajorFromJavacOutput(output);
        } catch (IOException | InterruptedException | IllegalArgumentException exception) {
            if (exception instanceof InterruptedException) {
                Thread.currentThread().interrupt();
            }
            return -1;
        }
    }

    /**
     * Builds the {@code javac -version} command for one JDK home.
     * Element 0 is the absolute normalized path of {@code bin/javac.exe} on Windows and {@code bin/javac}
     * otherwise. It is never a relative {@code bin/javac} name.
     *
     * @param jdkHome a JDK home directory
     * @return {@code javac -version} with an absolute executable
     */
    public static List<String> javacVersionCommand(Path jdkHome) {
        boolean windows = System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("win");
        String executable = windows ? "javac.exe" : "javac";
        Path javac = jdkHome.resolve("bin").resolve(executable).toAbsolutePath().normalize();
        return List.of(javac.toString(), "-version");
    }

    /**
     * Selects {@code javaHome} when its javac major is at least {@code minimumMajor}; otherwise the
     * first fallback that qualifies.
     *
     * @param javaHome     preferred {@code JAVA_HOME}, or blank
     * @param fallbacks    extra JDK homes, such as a Temurin 25 directory
     * @param minimumMajor required major, normally {@link #engineJavaMajor()}
     * @param probe        how to read a JDK major; tests inject this and production uses {@link #jdkMajor(Path)}
     * @return the selected JDK home
     */
    public static Path resolveJdkHome(String javaHome, List<Path> fallbacks, int minimumMajor, JdkMajorProbe probe) {
        if (minimumMajor < 1) {
            throw new IllegalArgumentException("minimumMajor must be positive.");
        }
        if (javaHome != null && !javaHome.isBlank()) {
            Path preferred = Path.of(javaHome);
            if (probe.major(preferred) >= minimumMajor) {
                return preferred;
            }
        }
        if (fallbacks != null) {
            for (Path candidate : fallbacks) {
                if (candidate != null && probe.major(candidate) >= minimumMajor) {
                    return candidate;
                }
            }
        }
        throw new IllegalStateException("No JDK " + minimumMajor
                + " or newer with javac was found. Set JAVA_HOME to a JDK that can load SHAFT.");
    }

    /**
     * Maven classpath rebuild for a report launcher. {@code -o} is included only when a
     * {@code shaft-engine} jar is already in the local repository.
     *
     * @param localRepository the Maven local repository
     * @return the {@code mvn} argument list
     */
    public static List<String> mavenClasspathCommand(Path localRepository) {
        List<String> command = new ArrayList<>();
        command.add("mvn");
        if (shaftEnginePresent(localRepository)) {
            command.add("-o");
        }
        command.add("-q");
        command.add("dependency:build-classpath");
        return List.copyOf(command);
    }

    /**
     * Fails when no classpath entry contains {@code com/shaft/driver/SHAFT.class}.
     *
     * @param classpathEntries jars and directories
     */
    public static void requireShaftClass(List<Path> classpathEntries) {
        if (classpathEntries != null) {
            for (Path entry : classpathEntries) {
                if (containsShaftClass(entry)) {
                    return;
                }
            }
        }
        throw new IllegalStateException(SHAFT_CLASS_ENTRY + " is absent from the classpath.");
    }

    /**
     * Writes a Java argument file whose first argument is {@code -cp}. Launch with
     * {@link #javaCommand(Path, String...)} so the classpath is not placed on the process command line.
     *
     * @param argFile          destination, for example {@code target/shaft-report.args}
     * @param classpathEntries classpath entries; backslashes are written as {@code /} because Java
     *                         argument files treat {@code \} as an escape
     * @param trailingArgument program or source file that follows {@code -cp}, or blank
     * @return {@code argFile}
     * @throws IOException when the file cannot be written
     */
    public static Path writeClasspathArgFile(Path argFile, List<String> classpathEntries, String trailingArgument)
            throws IOException {
        String classpath = classpathEntries.stream()
                .map(entry -> entry.replace('\\', '/'))
                .reduce((left, right) -> left + java.io.File.pathSeparator + right)
                .orElse("");
        StringBuilder content = new StringBuilder();
        content.append("-cp").append(System.lineSeparator());
        content.append(quote(classpath)).append(System.lineSeparator());
        if (trailingArgument != null && !trailingArgument.isBlank()) {
            content.append(trailingArgument).append(System.lineSeparator());
        }
        Path parent = argFile.getParent();
        if (parent != null) {
            Files.createDirectories(parent);
        }
        Files.writeString(argFile, content.toString(), StandardCharsets.UTF_8);
        return argFile;
    }

    /**
     * Builds the {@code java} command that launches the report from an argument file.
     *
     * @param argFile  file produced by {@link #writeClasspathArgFile}
     * @param trailing arguments that follow the argument file, such as the source file and results directory
     * @return {@code java @argfile ...} with no inline classpath
     */
    public static List<String> javaCommand(Path argFile, String... trailing) {
        List<String> command = new ArrayList<>();
        command.add("java");
        command.add("@" + argFile.toAbsolutePath().normalize());
        if (trailing != null) {
            for (String argument : trailing) {
                command.add(argument);
            }
        }
        return List.copyOf(command);
    }

    private static boolean shaftEnginePresent(Path localRepository) {
        if (localRepository == null) {
            return false;
        }
        Path engine = localRepository.resolve(ENGINE_GROUP_PATH);
        if (!Files.isDirectory(engine)) {
            return false;
        }
        try (Stream<Path> files = Files.walk(engine, 4)) {
            return files.anyMatch(AllureReportLaunch::isEngineJar);
        } catch (IOException exception) {
            return false;
        }
    }

    private static boolean isEngineJar(Path path) {
        Path fileName = path == null ? null : path.getFileName();
        if (fileName == null) {
            return false;
        }
        String name = fileName.toString();
        return Files.isRegularFile(path)
                && name.startsWith("shaft-engine-")
                && name.endsWith(".jar")
                && !name.endsWith("-sources.jar")
                && !name.endsWith("-javadoc.jar");
    }

    private static boolean containsShaftClass(Path entry) {
        if (entry == null || !Files.exists(entry)) {
            return false;
        }
        if (Files.isDirectory(entry)) {
            return Files.isRegularFile(entry.resolve(SHAFT_CLASS_ENTRY));
        }
        Path fileName = entry.getFileName();
        if (fileName == null || !fileName.toString().endsWith(".jar")) {
            return false;
        }
        try (JarFile jar = new JarFile(entry.toFile())) {
            return jar.getJarEntry(SHAFT_CLASS_ENTRY) != null;
        } catch (IOException exception) {
            return false;
        }
    }

    private static String quote(String value) {
        if (value.indexOf(' ') < 0 && value.indexOf('\t') < 0) {
            return value;
        }
        return '"' + value.replace("\"", "\\\"") + '"';
    }

    /**
     * Reads the javac major of one JDK home.
     */
    @FunctionalInterface
    public interface JdkMajorProbe {
        /**
         * Reads the javac major for one JDK home.
         *
         * @param jdkHome JDK home to inspect
         * @return the javac major, or a value below the required major when the home does not qualify
         */
        int major(Path jdkHome);
    }
}
