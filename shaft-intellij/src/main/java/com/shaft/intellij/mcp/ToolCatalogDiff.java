package com.shaft.intellij.mcp;

import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * Pure, UI-free comparison of a live SHAFT MCP {@code tools/list} response against the bundled
 * build-time catalog ({@link ToolCatalogIndex#toolNames()}), so the explicit "Refresh tools"
 * action in {@code ShaftFeaturePanel} can surface a stale bundled index instead of silently
 * shipping tool-selector/{@code /mcp} autocomplete metadata for tools the live server no longer
 * (or not yet) exposes (issue #3895, deferred from #3883(b)).
 *
 * <p>Deliberately guards both sides empty/null-safe, not just the live set: a live set that failed
 * to parse (server error, malformed response) must never be reported as "every bundled tool is
 * missing", and symmetrically a bundled set that is empty because the packaged resource is
 * missing/unreadable ({@link ToolCatalogIndex}'s own best-effort degrade) must never be reported as
 * "every live tool is unbundled". Either case is a real problem elsewhere, but not one this diff can
 * usefully name -- surfacing it here would just be a scary false positive on a routine refresh.</p>
 */
public final class ToolCatalogDiff {
    private static final int MAX_NAMES_SHOWN = 5;

    private ToolCatalogDiff() {
    }

    /**
     * Compares a live {@code tools/list} name set against the bundled catalog.
     *
     * @param liveToolNames    tool names from a fresh {@code tools/list} response, or {@code null}/empty
     *                         when the response could not be parsed
     * @param bundledToolNames tool names from {@link ToolCatalogIndex#toolNames()}, or {@code null}/empty
     *                         when the bundled resource is missing/unreadable
     * @return empty when either side is null/empty or the sets match exactly; otherwise one
     *     deterministic line naming the difference, e.g. {@code "Live MCP catalog differs from
     *     bundled index: +2 not in bundle (a, b); -1 missing live (c)"}
     */
    public static Optional<String> diff(Set<String> liveToolNames, Set<String> bundledToolNames) {
        if (liveToolNames == null || liveToolNames.isEmpty() || bundledToolNames == null || bundledToolNames.isEmpty()) {
            return Optional.empty();
        }
        Set<String> extra = new TreeSet<>(liveToolNames);
        extra.removeAll(bundledToolNames);
        Set<String> missing = new TreeSet<>(bundledToolNames);
        missing.removeAll(liveToolNames);
        if (extra.isEmpty() && missing.isEmpty()) {
            return Optional.empty();
        }
        StringBuilder message = new StringBuilder("Live MCP catalog differs from bundled index: ");
        if (!extra.isEmpty()) {
            message.append('+').append(extra.size()).append(" not in bundle (").append(names(extra)).append(')');
        }
        if (!missing.isEmpty()) {
            if (!extra.isEmpty()) {
                message.append("; ");
            }
            message.append('-').append(missing.size()).append(" missing live (").append(names(missing)).append(')');
        }
        return Optional.of(message.toString());
    }

    private static String names(Set<String> sortedNames) {
        if (sortedNames.size() <= MAX_NAMES_SHOWN) {
            return String.join(", ", sortedNames);
        }
        return sortedNames.stream().limit(MAX_NAMES_SHOWN).collect(Collectors.joining(", ")) + ", …";
    }
}
