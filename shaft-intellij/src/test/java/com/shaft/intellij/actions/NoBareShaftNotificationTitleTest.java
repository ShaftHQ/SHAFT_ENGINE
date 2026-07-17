package com.shaft.intellij.actions;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Regression guard for issue #3638: every {@code ShaftNotifier.info}/{@code warn} call under
 * {@code shaft-intellij/src/main/java/com/shaft/intellij/actions} must pass a per-action-family
 * notification title instead of the bare literal {@code "SHAFT"} every action in this package
 * used to share, which made every Event Log entry from every action indistinguishable. Mirrors
 * the title convention {@code com.shaft.intellij.notifications.FailedRunDoctorNotifier} already
 * uses for its own notification.
 */
class NoBareShaftNotificationTitleTest {
    private static final Pattern BARE_TITLE =
            Pattern.compile("ShaftNotifier\\.(info|warn)\\(\\s*[A-Za-z_][A-Za-z0-9_]*\\s*,\\s*\"SHAFT\"\\s*,");

    @Test
    void noActionPassesTheBareSharedTitleToShaftNotifier() throws IOException {
        List<String> violations = new ArrayList<>();
        try (var paths = Files.walk(Path.of("src/main/java/com/shaft/intellij/actions"))) {
            for (Path path : paths.filter(p -> p.toString().endsWith(".java")).toList()) {
                String source = Files.readString(path);
                Matcher matcher = BARE_TITLE.matcher(source);
                while (matcher.find()) {
                    violations.add(path + " (offset " + matcher.start() + "): " + matcher.group());
                }
            }
        }
        assertTrue(violations.isEmpty(),
                "issue #3638: found ShaftNotifier.info/warn call(s) under actions/ using the bare "
                        + "literal \"SHAFT\" as their title instead of a per-action-family title: " + violations);
    }
}
