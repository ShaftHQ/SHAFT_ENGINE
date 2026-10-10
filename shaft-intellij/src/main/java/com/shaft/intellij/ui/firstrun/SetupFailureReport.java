package com.shaft.intellij.ui.firstrun;

import com.intellij.ide.BrowserUtil;
import com.intellij.openapi.application.ApplicationManager;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

/**
 * Redacted setup-failure reports. Opt-in is required. {@code gh} runs only as an argument list.
 */
public final class SetupFailureReport {
    public static final String REPO = "ShaftHQ/SHAFT_ENGINE";
    private static final Set<String> CREATED = ConcurrentHashMap.newKeySet();
    private static final Set<String> BROWSED = ConcurrentHashMap.newKeySet();
    private static final Pattern GITHUB_TOKEN = Pattern.compile("ghp_[A-Za-z0-9]+");
    private static final Pattern SECRET_KEY = Pattern.compile("sk-[A-Za-z0-9_-]+");
    private static final Pattern BEARER = Pattern.compile("(?i)Bearer\\s+\\S+");
    private static final Pattern EMAIL = Pattern.compile("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}");

    private SetupFailureReport() {
    }

    @FunctionalInterface
    public interface GhProbe {
        boolean authenticated();
    }

    @FunctionalInterface
    public interface ProcessRunner {
        String run(List<String> command);
    }

    @FunctionalInterface
    public interface BrowserOpener {
        void open(String url);
    }

    public static void clearSession() {
        CREATED.clear();
        BROWSED.clear();
    }

    public static String redact(String raw) {
        return redact(raw, System.getProperty("user.home"), "");
    }

    public static String redact(String raw, String home, String project) {
        String text = raw == null ? "" : raw;
        text = GITHUB_TOKEN.matcher(text).replaceAll("[token]");
        text = SECRET_KEY.matcher(text).replaceAll("[token]");
        text = BEARER.matcher(text).replaceAll("Bearer [token]");
        text = EMAIL.matcher(text).replaceAll("[email]");
        text = replacePath(text, home, "[home]");
        text = replacePath(text, project, "[project]");
        return text;
    }

    public static String fingerprint(Throwable error, String stepId) {
        String type = error == null ? "unknown" : error.getClass().getName();
        String frame = "none";
        if (error != null) {
            for (StackTraceElement element : error.getStackTrace()) {
                if (element.getClassName().startsWith("com.shaft.")) {
                    frame = element.getClassName() + "." + element.getMethodName();
                    break;
                }
            }
        }
        String step = stepId == null ? "" : stepId;
        return type + "|" + frame + "|" + step;
    }

    public static List<String> ghCreateArgs(String repo, String title, String body) {
        return List.of("gh", "issue", "create", "--repo", REPO, "--title", title, "--body", body);
    }

    public static List<String> ghListArgs(String fingerprint) {
        return List.of("gh", "issue", "list", "--repo", REPO, "--state", "open", "--search", fingerprint);
    }

    public static String browserUrl(String title, String body) {
        return "https://github.com/" + REPO + "/issues/new?title="
                + encode(title) + "&body=" + encode(body);
    }

    public static void file(boolean optIn, String fingerprint, String title, String body,
                            GhProbe gh, ProcessRunner runner, BrowserOpener browser) {
        if (!optIn || fingerprint == null) {
            return;
        }
        if (gh != null && gh.authenticated()) {
            if (!CREATED.add(fingerprint)) {
                return;
            }
            String listed = runner.run(ghListArgs(fingerprint));
            if (listed != null && listed.contains(fingerprint)) {
                return;
            }
            runner.run(ghCreateArgs(REPO, title, body));
            return;
        }
        if (browser != null && BROWSED.add(fingerprint)) {
            browser.open(browserUrl(title, body));
        }
    }

    public static void fileDefault(boolean optIn, String fingerprint, String title, String body) {
        file(optIn, fingerprint, title, body, SetupFailureReport::authenticated,
                SetupFailureReport::runProcess, SetupFailureReport::openBrowser);
    }

    private static boolean authenticated() {
        return exitCode(List.of("gh", "auth", "status")) == 0;
    }

    private static String runProcess(List<String> command) {
        CommandResult result = run(command);
        return result == null ? "" : result.output;
    }

    private static int exitCode(List<String> command) {
        CommandResult result = run(command);
        return result == null ? -1 : result.exit;
    }

    private static void openBrowser(String url) {
        try {
            if (ApplicationManager.getApplication() == null) {
                return;
            }
            BrowserUtil.browse(url);
        } catch (Throwable ignored) {
            // No IDE application, or the browser handler is unavailable.
        }
    }

    private static CommandResult run(List<String> command) {
        try {
            Process process = new ProcessBuilder(command).redirectErrorStream(true).start();
            boolean finished = process.waitFor(20, TimeUnit.SECONDS);
            if (!finished) {
                process.destroyForcibly();
                return null;
            }
            String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
            return new CommandResult(process.exitValue(), output);
        } catch (Exception exception) {
            return null;
        }
    }

    private static String replacePath(String text, String path, String token) {
        if (path == null || path.isBlank()) {
            return text;
        }
        return text.replace(path, token);
    }

    private static String encode(String value) {
        return URLEncoder.encode(value == null ? "" : value, StandardCharsets.UTF_8);
    }

    private record CommandResult(int exit, String output) {
    }
}
