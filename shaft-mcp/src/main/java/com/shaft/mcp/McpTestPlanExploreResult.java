package com.shaft.mcp;

import java.util.List;

/**
 * Result of a {@code test_plan_explore} breadth-first crawl: how many pages were visited, the
 * workspace-relative Markdown test-plan files written into {@code specs/}, and any bounded-crawl
 * or fail-soft warnings raised along the way.
 */
public record McpTestPlanExploreResult(
        String schemaVersion,
        int pagesVisited,
        List<String> plansWritten,
        List<String> warnings) {
}
