package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GuideServiceOfflineBranchTest {
    @Test
    void searchSkipsDuplicateAndNonPublicRoutesAndClassifiesCodeBlocks() {
        Map<String, String> responses = new HashMap<>();
        responses.put(GuideService.GUIDE_INDEX_URI.toString(), """
                {
                  "documents": [
                    {"id": 1, "pageTitle": "Locators", "sectionTitle": "Builder", "sectionRoute": "/docs/guides/locators#builder", "type": "docs"},
                    {"id": 2, "pageTitle": "Locators duplicate", "sectionTitle": "Builder again", "sectionRoute": "/docs/guides/locators#builder", "type": "docs"},
                    {"id": 3, "pageTitle": "Tags", "sectionTitle": "Hidden", "sectionRoute": "/docs/tags/locator", "type": "docs"},
                    {"id": 4, "pageTitle": "Archive", "sectionTitle": "Old", "sectionRoute": "/docs/archive/locator", "type": "docs"},
                    {"id": 5, "pageTitle": "Maintainers", "sectionTitle": "Internal", "sectionRoute": "/docs/maintainers/locator", "type": "docs"}
                  ],
                  "index": {"invertedIndex": [["locator", {"title": {"1": {}, "2": {}}, "tags": {"1": {}}, "sidebarParentCategories": {"1": {}}, "content": {"1": {}}, "other": {"1": {}}}]]}
                }
                """);
        responses.put("https://shafthq.github.io/docs/guides/locators", """
                <html><body><main>
                  <h2 id="builder">Builder</h2>
                  <pre><code>public class LoginPage { }</code></pre>
                  <pre><code>By button = SHAFT.GUI.Locator.hasTagName("button").build();</code></pre>
                  <pre><code>   </code></pre>
                  <h2 id="next">Next</h2>
                  <p>ignored after the next heading</p>
                </main></body></html>
                """);
        ScriptedGuideHttp http = new ScriptedGuideHttp(responses);
        GuideService service = new GuideService(http);

        McpGuideSearchResult result = service.search("loca", 0);

        assertEquals(1, result.matches().size());
        assertEquals(McpCodeBlock.Kind.FULL_CLASS, result.matches().getFirst().codeBlocks().get(0).kind());
        assertEquals(McpCodeBlock.Kind.ACTION, result.matches().getFirst().codeBlocks().get(1).kind());
        assertEquals(2, result.matches().getFirst().codeBlocks().size());
        assertTrue(result.matches().getFirst().excerpt().contains("public class LoginPage"));
        assertFalse(result.matches().getFirst().excerpt().contains("ignored after"));
        assertTrue(http.requested().stream().noneMatch(uri -> uri.getPath().contains("/docs/tags")
                || uri.getPath().contains("/docs/archive")
                || uri.getPath().contains("/docs/maintainers")));
    }

    @Test
    void blankQueryUsesTheDefaultAndAFailedPageDoesNotDropOtherMatches() {
        Map<String, String> responses = new HashMap<>();
        responses.put(GuideService.GUIDE_INDEX_URI.toString(), """
                {
                  "documents": [
                    {"id": "ok", "pageTitle": "Actions", "sectionTitle": "Click", "sectionRoute": "/docs/actions/click", "type": "docs"},
                    {"id": "bad", "pageTitle": "Actions", "sectionTitle": "Type", "sectionRoute": "/docs/actions/type", "type": "docs"}
                  ],
                  "index": {"invertedIndex": [["actions", {"content": {"ok": {}, "bad": {}}}]]}
                }
                """);
        responses.put("https://shafthq.github.io/docs/actions/click",
                "<html><body><p>Click the button.</p></body></html>");
        ScriptedGuideHttp http = new ScriptedGuideHttp(responses);
        http.failures.put("https://shafthq.github.io/docs/actions/type", new IOException("missing page"));
        GuideService service = new GuideService(http);

        McpGuideSearchResult blank = service.search("  ", 1);
        assertTrue(blank.query().contains("SHAFT guide best practices"));

        McpGuideSearchResult result = service.search("actions", 5);
        assertEquals(1, result.matches().size());
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("unreadable official guide page")
                && warning.contains("/docs/actions/type")));
    }

    @Test
    void interruptedPageFetchStopsThatMatchAndKeepsTheWarning() {
        Map<String, String> responses = new HashMap<>();
        responses.put(GuideService.GUIDE_INDEX_URI.toString(), """
                {
                  "documents": [
                    {"id": "1", "pageTitle": "Wait", "sectionTitle": "Explicit", "sectionRoute": "/docs/wait/explicit", "type": "docs"}
                  ],
                  "index": {"invertedIndex": [["wait", {"title": {"1": {}}}]]}
                }
                """);
        ScriptedGuideHttp http = new ScriptedGuideHttp(responses);
        http.failures.put("https://shafthq.github.io/docs/wait/explicit", new InterruptedException("stopped"));
        GuideService service = new GuideService(http);

        McpGuideSearchResult result = service.search("wait", 5);

        assertTrue(result.matches().isEmpty());
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("unreadable official guide page")));
        assertTrue(Thread.currentThread().isInterrupted());
        Thread.interrupted();
    }

    private static final class ScriptedGuideHttp implements GuideService.GuideHttpClient {
        private final Map<String, String> responses;
        private final Map<String, Exception> failures = new HashMap<>();
        private final List<URI> requested = new ArrayList<>();

        private ScriptedGuideHttp(Map<String, String> responses) {
            this.responses = responses;
        }

        @Override
        public String get(URI uri) throws IOException, InterruptedException {
            requested.add(uri);
            Exception failure = failures.get(uri.toString());
            if (failure instanceof IOException ioException) {
                throw ioException;
            }
            if (failure instanceof InterruptedException interruptedException) {
                throw interruptedException;
            }
            String response = responses.get(uri.toString());
            if (response == null) {
                throw new IOException("No fixture for " + uri);
            }
            return response;
        }

        private List<URI> requested() {
            return List.copyOf(requested);
        }
    }
}
