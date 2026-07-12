package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PlannerServiceTest {
    @TempDir
    Path temp;

    @Test
    void crawlRespectsMaxDepthClamp() {
        McpTestPlanExploreResult depthClampedToOne =
                service(chainGraph()).crawl("http://example.test/a", "", 0, 10);
        assertEquals(1, depthClampedToOne.pagesVisited());

        McpTestPlanExploreResult depthTwo =
                service(chainGraph()).crawl("http://example.test/a", "", 2, 10);
        assertEquals(3, depthTwo.pagesVisited());

        McpTestPlanExploreResult depthClampedToThree =
                service(chainGraph()).crawl("http://example.test/a", "", 10, 10);
        assertEquals(4, depthClampedToThree.pagesVisited());
    }

    @Test
    void crawlRespectsMaxPagesClampAndReportsBudgetWarning() {
        McpTestPlanExploreResult result = service(chainGraph()).crawl("http://example.test/a", "", 10, 1);

        assertEquals(1, result.pagesVisited());
        assertTrue(result.warnings().stream()
                .anyMatch(warning -> warning.contains("Page budget (1) reached; 2 discovered page(s)")));
    }

    @Test
    void crawlFiltersOffOriginLinks() {
        Map<String, PlannerService.PlannerPageSnapshot> pages = new LinkedHashMap<>();
        pages.put("http://example.test/a", new PlannerService.PlannerPageSnapshot(
                "http://example.test/a", "Home", List.of(),
                List.of("http://other.test/x", "http://example.test/a")));

        McpTestPlanExploreResult result = service(new FakeInspector(pages))
                .crawl("http://example.test/a", "", 3, 10);

        assertEquals(1, result.pagesVisited());
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("1 off-origin link(s)")));
    }

    @Test
    void crawlIsFailSoftWhenOnePageInspectionFails() {
        Map<String, PlannerService.PlannerPageSnapshot> pages = new LinkedHashMap<>();
        pages.put("http://example.test/a", new PlannerService.PlannerPageSnapshot(
                "http://example.test/a", "Home", List.of(),
                List.of("http://example.test/bad", "http://example.test/good")));
        pages.put("http://example.test/good", new PlannerService.PlannerPageSnapshot(
                "http://example.test/good", "Good", List.of(sampleFlow("Good flow")), List.of()));
        FakeInspector inspector = new FakeInspector(pages);
        inspector.failingUrls.add("http://example.test/bad");

        McpTestPlanExploreResult result = service(inspector).crawl("http://example.test/a", "", 3, 10);

        assertEquals(2, result.pagesVisited());
        assertEquals(1, result.plansWritten().size());
        assertTrue(result.warnings().stream()
                .anyMatch(warning -> warning.contains("http://example.test/bad") && warning.contains("boom")));
    }

    @Test
    void crawlWritesUniqueFileNamesForDuplicateFlowTitles() {
        Map<String, PlannerService.PlannerPageSnapshot> pages = new LinkedHashMap<>();
        pages.put("http://example.test/a", new PlannerService.PlannerPageSnapshot(
                "http://example.test/a", "Home",
                List.of(sampleFlow("Login form"), sampleFlow("Login form")), List.of()));

        McpTestPlanExploreResult result = service(new FakeInspector(pages))
                .crawl("http://example.test/a", "", 1, 10);

        assertEquals(2, result.plansWritten().size());
        assertEquals(2, Set.copyOf(result.plansWritten()).size());
        for (String writtenPath : result.plansWritten()) {
            assertTrue(Files.exists(Path.of(writtenPath)));
        }
        assertTrue(result.plansWritten().get(0).contains("01-login-form.md"));
        assertTrue(result.plansWritten().get(1).contains("02-login-form.md"));
    }

    @Test
    void renderPlanProducesExpectedMarkdownKeyLines() {
        PlannerService.PlannerLocator locator = new PlannerService.PlannerLocator(
                "ACCESSIBLE_NAME", "Email", "SHAFT.GUI.Locator.inputField(\"Email\")");
        PlannerService.PlannerStep step = new PlannerService.PlannerStep(
                "type", "Email", locator, "user@example.com");
        PlannerService.PlannerFlow flow = new PlannerService.PlannerFlow(
                "Login form", List.of(step), "Confirm the login succeeded.");

        String markdown = PlannerService.renderPlan(flow, "https://example.test/login");

        assertTrue(markdown.contains("# Login form"));
        assertTrue(markdown.contains("- Start at: https://example.test/login"));
        assertTrue(markdown.contains(
                "1. Type \"user@example.com\" into Email — `SHAFT.GUI.Locator.inputField(\"Email\")`"));
        assertTrue(markdown.contains("## Verify"));
        assertTrue(markdown.contains("- Confirm the login succeeded."));
    }

    @Test
    void buildSnapshotDiscoversFormFieldsSubmitAndLinks() {
        String html = """
                <html><body>
                <form id="loginForm">
                  <input type="email" name="email" placeholder="Email address"/>
                  <input type="password" name="password" placeholder="Password"/>
                  <button type="submit">Sign in</button>
                </form>
                <a href="/dashboard">Dashboard</a>
                </body></html>
                """;

        PlannerService.PlannerPageSnapshot snapshot =
                PlannerService.buildSnapshot("http://example.test/login", "Login", html);

        // One form flow (email, password, submit) plus one navigation flow for the outside-form link.
        assertEquals(2, snapshot.flows().size());
        PlannerService.PlannerFlow flow = snapshot.flows().getFirst();
        assertEquals(3, flow.steps().size());
        assertEquals("type", flow.steps().get(0).action());
        assertEquals("user@example.com", flow.steps().get(0).seedData());
        assertEquals("click", flow.steps().get(2).action());
        assertTrue(snapshot.links().stream().anyMatch(link -> link.contains("/dashboard")));
    }

    private PlannerService service(FakeInspector inspector) {
        return new PlannerService(McpWorkspacePolicy.of(temp), inspector, url -> { });
    }

    private static PlannerService.PlannerFlow sampleFlow(String title) {
        PlannerService.PlannerLocator locator = new PlannerService.PlannerLocator(
                "ID", "submit", "SHAFT.GUI.Locator.clickableField(\"Submit\")");
        PlannerService.PlannerStep step = new PlannerService.PlannerStep("click", "Submit", locator, "");
        return new PlannerService.PlannerFlow(title, List.of(step), "Confirm the expected outcome.");
    }

    private static FakeInspector chainGraph() {
        Map<String, PlannerService.PlannerPageSnapshot> pages = new LinkedHashMap<>();
        pages.put("http://example.test/a", new PlannerService.PlannerPageSnapshot(
                "http://example.test/a", "A", List.of(),
                List.of("http://example.test/b", "http://example.test/c")));
        pages.put("http://example.test/b", new PlannerService.PlannerPageSnapshot(
                "http://example.test/b", "B", List.of(), List.of("http://example.test/d")));
        pages.put("http://example.test/c", new PlannerService.PlannerPageSnapshot(
                "http://example.test/c", "C", List.of(), List.of()));
        pages.put("http://example.test/d", new PlannerService.PlannerPageSnapshot(
                "http://example.test/d", "D", List.of(), List.of()));
        return new FakeInspector(pages);
    }

    private static final class FakeInspector implements PlannerService.PlannerPageInspector {
        private final Map<String, PlannerService.PlannerPageSnapshot> pages;
        private final Set<String> failingUrls = new HashSet<>();

        private FakeInspector(Map<String, PlannerService.PlannerPageSnapshot> pages) {
            this.pages = pages;
        }

        @Override
        public PlannerService.PlannerPageSnapshot inspect(String url) {
            if (failingUrls.contains(url)) {
                throw new IllegalStateException("boom");
            }
            PlannerService.PlannerPageSnapshot snapshot = pages.get(url);
            if (snapshot == null) {
                throw new IllegalStateException("No fake snapshot registered for " + url);
            }
            return snapshot;
        }
    }
}
