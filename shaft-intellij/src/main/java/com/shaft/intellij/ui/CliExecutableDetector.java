package com.shaft.intellij.ui;

import java.io.File;
import java.util.List;
import java.util.Locale;

/**
 * Detects whether an agent CLI ({@code claude}, {@code codex}) is present on {@code PATH}, so the
 * CLI terminal actions (issue #3959) can point a user at the install command instead of opening a
 * terminal tab that just prints "command not found". Pure PATH/PATHEXT lookup -- no process is
 * spawned -- with the same injected-state DI shape {@link SetupPrerequisites} already uses so
 * this stays trivially testable without touching the real environment.
 */
final class CliExecutableDetector {
    private static final List<String> DEFAULT_WINDOWS_EXTENSIONS = List.of(".exe", ".cmd", ".bat", ".ps1");
    private static final List<String> NO_EXTENSION = List.of("");

    private CliExecutableDetector() {
        throw new IllegalStateException("Utility class");
    }

    static boolean isOnPath(String executable) {
        return isOnPath(executable, System.getenv("PATH"), System.getenv("PATHEXT"), isWindows());
    }

    /**
     * Package-private overload used by tests to stub {@code PATH}/{@code PATHEXT} and the OS.
     */
    static boolean isOnPath(String executable, String pathEnv, String pathExtEnv, boolean windows) {
        if (executable == null || executable.isBlank() || pathEnv == null || pathEnv.isBlank()) {
            return false;
        }
        List<String> extensions = windows ? windowsExtensions(pathExtEnv) : NO_EXTENSION;
        for (String directory : pathEnv.split(java.util.regex.Pattern.quote(File.pathSeparator))) {
            if (directory.isBlank()) {
                continue;
            }
            for (String extension : extensions) {
                String candidate = executable + extension;
                boolean found = windows
                        ? directoryContainsCaseInsensitive(directory, candidate)
                        : new File(directory, candidate).isFile();
                if (found) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Windows PATHEXT matching is case-insensitive by definition (cmd.exe resolves {@code .exe},
     * {@code .Exe} and {@code .EXE} identically), regardless of whether the underlying filesystem
     * happens to fold case itself. {@link File#isFile()} only matches case-insensitively on
     * filesystems (like NTFS) that fold case at the OS level -- on a case-sensitive filesystem
     * (e.g. the ext4 runners CI builds on) it would silently miss a real match. Listing the
     * directory and comparing names with {@link String#equalsIgnoreCase} implements the
     * case-insensitive match explicitly, so behavior no longer depends on the host filesystem.
     */
    private static boolean directoryContainsCaseInsensitive(String directory, String candidateName) {
        File[] entries = new File(directory).listFiles();
        if (entries == null) {
            return false;
        }
        for (File entry : entries) {
            if (entry.isFile() && entry.getName().equalsIgnoreCase(candidateName)) {
                return true;
            }
        }
        return false;
    }

    private static List<String> windowsExtensions(String pathExtEnv) {
        if (pathExtEnv == null || pathExtEnv.isBlank()) {
            return DEFAULT_WINDOWS_EXTENSIONS;
        }
        return List.of(pathExtEnv.split(";"));
    }

    private static boolean isWindows() {
        return System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("win");
    }
}
