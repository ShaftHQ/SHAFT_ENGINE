package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URI;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GuideServiceTest {
    @Test
    void searchReturnsOfficialPomLocatorGuidanceAndCode() {
        FakeGuideHttpClient http = new FakeGuideHttpClient(baseResponses());
        GuideService service = new GuideService(http);

        McpGuideSearchResult result = service.search("page object model locators SHAFT.GUI.Locator", 5);

        assertEquals(McpGuideSearchResult.CURRENT_SCHEMA_VERSION, result.schemaVersion());
        assertTrue(result.matches().stream()
                .anyMatch(match -> match.url().contains("Solution_Design#page-object-model-pom")));
        assertTrue(result.matches().stream()
                .anyMatch(match -> match.url().contains("Shaft_Locator_Builder#basic-syntax")
                        && match.codeBlocks().stream()
                        .anyMatch(block -> block.code().contains("SHAFT.GUI.Locator"))));
        assertTrue(result.guidanceRules().stream().anyMatch(rule -> rule.contains("avoid @FindBy")));
        assertTrue(result.guidanceRules().stream().anyMatch(rule -> rule.contains("do not invent APIs")));
        assertTrue(result.guidanceRules().stream().anyMatch(rule -> rule.contains("Thread.sleep")));
        assertTrue(result.guidanceRules().stream().anyMatch(rule -> rule.contains("request builders")));
        assertTrue(http.requested().stream()
                .allMatch(uri -> "shafthq.github.io".equalsIgnoreCase(uri.getHost())));
    }

    @Test
    void searchDoesNotFetchNonOfficialRoutesFromIndex() {
        FakeGuideHttpClient http = new FakeGuideHttpClient(baseResponses());
        GuideService service = new GuideService(http);

        McpGuideSearchResult result = service.search("external provider", 5);

        assertTrue(result.matches().isEmpty());
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("No official guide sections")));
        assertFalse(http.requested().stream().anyMatch(uri -> uri.toString().contains("evil.test")));
    }

    @Test
    void searchReturnsWarningWhenIndexIsUnavailable() {
        GuideService service = new GuideService(uri -> {
            throw new IOException("offline");
        });

        McpGuideSearchResult result = service.search("api testing", 5);

        assertTrue(result.matches().isEmpty());
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("Could not read")));
    }

    @Test
    void searchClampsRequestedResultCount() {
        Map<String, String> responses = manyApiResponses();
        GuideService service = new GuideService(new FakeGuideHttpClient(responses));

        McpGuideSearchResult result = service.search("api", 99);

        assertEquals(5, result.matches().size());
    }

    private static Map<String, String> baseResponses() {
        Map<String, String> responses = new HashMap<>();
        responses.put(GuideService.GUIDE_INDEX_URI.toString(), """
                {
                  "documents": [
                    {
                      "id": 1,
                      "pageTitle": "Test Automation Solution Design Patterns",
                      "sectionTitle": "Page Object Model (POM)",
                      "sectionRoute": "/docs/reference/guides/Solution_Design#page-object-model-pom",
                      "type": "docs"
                    },
                    {
                      "id": 2,
                      "pageTitle": "SHAFT Locator Builder",
                      "sectionTitle": "Basic Syntax",
                      "sectionRoute": "/docs/reference/actions/GUI/didYouKnow/Shaft_Locator_Builder#basic-syntax",
                      "type": "docs"
                    },
                    {
                      "id": 3,
                      "pageTitle": "Element Identification Best Practices",
                      "sectionTitle": "Use By Objects, Not FindBy",
                      "sectionRoute": "/docs/reference/guides/Element_Identification#use-by-objects-not-findby",
                      "type": "docs"
                    },
                    {
                      "id": 4,
                      "pageTitle": "External Provider",
                      "sectionTitle": "External Provider",
                      "sectionRoute": "https://evil.test/docs/external",
                      "type": "docs"
                    }
                  ],
                  "index": {
                    "invertedIndex": [
                      ["page", {"title": {"1": {}}, "content": {"1": {}}}],
                      ["object", {"title": {"1": {}}, "content": {"1": {}}}],
                      ["model", {"title": {"1": {}}, "content": {"1": {}}}],
                      ["pom", {"title": {"1": {}}, "content": {"1": {}}}],
                      ["locat", {"title": {"2": {}}, "content": {"2": {}, "3": {}}}],
                      ["shaft", {"content": {"1": {}, "2": {}, "3": {}}}],
                      ["gui", {"content": {"1": {}, "2": {}}}],
                      ["external", {"title": {"4": {}}, "content": {"4": {}}}]
                    ]
                  }
                }
                """);
        responses.put("https://shafthq.github.io/docs/reference/guides/Solution_Design", """
                <html><body><main>
                  <h1>Test Automation Solution Design Patterns</h1>
                  <h2 id="page-object-model-pom">Page Object Model (POM)</h2>
                  <p>The Page Object Model encapsulates each page in a class.</p>
                  <pre><code>public class LoginPage {
                    private final SHAFT.GUI.WebDriver driver;
                    public LoginPage(SHAFT.GUI.WebDriver driver) { this.driver = driver; }
                  }</code></pre>
                  <h2 id="fluent-page-object-method-chaining">Fluent Page Object</h2>
                </main></body></html>
                """);
        responses.put("https://shafthq.github.io/docs/reference/actions/GUI/didYouKnow/Shaft_Locator_Builder", """
                <html><body><main>
                  <h1>SHAFT Locator Builder</h1>
                  <h2 id="basic-syntax">Basic Syntax</h2>
                  <p>SHAFT.GUI.Locator builds Selenium By locators.</p>
                  <pre><code>By button = SHAFT.GUI.Locator
                      .hasTagName("button")
                      .hasText("Login")
                      .build();</code></pre>
                  <h2 id="available-builder-methods">Available Builder Methods</h2>
                </main></body></html>
                """);
        responses.put("https://shafthq.github.io/docs/reference/guides/Element_Identification", """
                <html><body><main>
                  <h1>Element Identification Best Practices</h1>
                  <h2 id="use-by-objects-not-findby">Use By Objects, Not FindBy</h2>
                  <p>Use By objects because SHAFT element handling already provides waits and retries.</p>
                </main></body></html>
                """);
        return responses;
    }

    private static Map<String, String> manyApiResponses() {
        Map<String, String> responses = new HashMap<>();
        StringBuilder documents = new StringBuilder();
        StringBuilder postings = new StringBuilder();
        for (int index = 1; index <= 6; index++) {
            if (index > 1) {
                documents.append(",");
                postings.append(",");
            }
            documents.append("""
                    {"id":%d,"pageTitle":"API Testing %d","sectionTitle":"API Section %d","sectionRoute":"/docs/testing/api-%d","type":"docs"}\
                    """.formatted(index, index, index, index));
            postings.append("\"").append(index).append("\":{}");
            responses.put("https://shafthq.github.io/docs/testing/api-" + index, """
                    <html><body><main><h1>API Testing</h1><p>Use official SHAFT API guide examples.</p></main></body></html>
                    """);
        }
        responses.put(GuideService.GUIDE_INDEX_URI.toString(), """
                {"documents":[%s],"index":{"invertedIndex":[["api",{"title":{%s},"content":{%s}}]]}}
                """.formatted(documents, postings, postings));
        return responses;
    }

    private static final class FakeGuideHttpClient implements GuideService.GuideHttpClient {
        private final Map<String, String> responses;
        private final List<URI> requested = new java.util.ArrayList<>();

        private FakeGuideHttpClient(Map<String, String> responses) {
            this.responses = responses;
        }

        @Override
        public String get(URI uri) throws IOException {
            requested.add(uri);
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
