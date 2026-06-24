package com.shaft.mcp;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Searches the live official SHAFT user guide for MCP agents before they write SHAFT code.
 */
@Service
public class GuideService {
    static final URI GUIDE_INDEX_URI = URI.create(
            "https://shafthq.github.io/search-index-docs-default-current.json");
    private static final String GUIDE_HOST = "shafthq.github.io";
    private static final String GUIDE_ROOT = "https://shafthq.github.io";
    private static final int DEFAULT_LIMIT = 5;
    private static final int MAX_LIMIT = 5;
    private static final int EXCERPT_LIMIT = 900;
    private static final int CODE_LIMIT = 1_500;
    private static final Pattern TOKEN_SPLIT = Pattern.compile("[^a-z0-9]+");
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final Set<String> STOP_WORDS = Set.of(
            "a", "an", "and", "are", "as", "at", "be", "by", "for", "from", "how", "in", "is", "it",
            "of", "on", "or", "that", "the", "this", "to", "use", "what", "when", "where", "which",
            "with", "your");
    private static final List<String> GUIDANCE_RULES = List.of(
            "Call shaft_guide_search before writing SHAFT code and cite the returned official guide URLs.",
            "Use SHAFT facade and method names exactly as shown in the guide examples; do not invent APIs.",
            "For GUI tests, prefer SHAFT.GUI.WebDriver with driver.browser(), driver.element(), driver.touch(), and driver.assertThat() patterns from the guide.",
            "Use Page Object Model for reusable UI flows; keep driver lifecycle in tests or a thin base class.",
            "Use Selenium By objects and SHAFT.GUI.Locator for locators; avoid @FindBy and PageFactory.",
            "Generated code must not contain Thread.sleep or absolute XPath locators.",
            "For API suites, keep reusable request builders and response validators outside test classes.",
            "For API, CLI, mobile, properties, troubleshooting, and reporting, search the exact topic and follow returned guide examples.",
            "If the official guide results do not cover the request, say that instead of hallucinating SHAFT behavior.");

    private final GuideHttpClient httpClient;

    /**
     * Creates the default live guide search service.
     */
    public GuideService() {
        this(new JdkGuideHttpClient());
    }

    GuideService(GuideHttpClient httpClient) {
        this.httpClient = httpClient;
    }

    /**
     * Searches the official SHAFT user guide and returns cited guidance, excerpts, and code examples.
     *
     * @param query topic to search for, such as page objects, locators, API, GUI, CLI, properties, or failures
     * @param maxResults maximum matches to return; values are clamped to 1 through 5
     * @return official guide search result
     */
    @Tool(name = "shaft_guide_search",
            description = "searches the live official SHAFT user guide before writing SHAFT tests, page objects,"
                    + " locators, API, GUI, CLI, mobile, troubleshooting, or best-practice code")
    public McpGuideSearchResult search(String query, int maxResults) {
        String effectiveQuery = normalizeQuery(query);
        int limit = clamp(maxResults);
        List<String> warnings = new ArrayList<>();
        try {
            GuideIndex index = readIndex(httpClient.get(GUIDE_INDEX_URI));
            List<ScoredDocument> scoredDocuments = score(index, effectiveQuery);
            List<McpGuideMatch> matches = new ArrayList<>();
            Set<String> seenRoutes = new HashSet<>();
            for (ScoredDocument scored : scoredDocuments) {
                if (matches.size() >= limit) {
                    break;
                }
                if (!seenRoutes.add(scored.document.sectionRoute())) {
                    continue;
                }
                match(scored, warnings).ifPresent(matches::add);
            }
            if (matches.isEmpty()) {
                warnings.add("No official guide sections matched the query. Use "
                        + GUIDE_ROOT + "/docs/start/overview and do not invent missing SHAFT APIs.");
            }
            return result(effectiveQuery, matches, warnings);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            warnings.add("Could not read the live SHAFT guide search index. Use "
                    + GUIDE_ROOT + "/docs/start/overview and do not invent missing SHAFT APIs.");
            return result(effectiveQuery, List.of(), warnings);
        } catch (IOException exception) {
            warnings.add("Could not read the live SHAFT guide search index. Use "
                    + GUIDE_ROOT + "/docs/start/overview and do not invent missing SHAFT APIs.");
            return result(effectiveQuery, List.of(), warnings);
        }
    }

    private java.util.Optional<McpGuideMatch> match(ScoredDocument scored, List<String> warnings) {
        try {
            URI source = officialUri(scored.document.sectionRoute());
            URI page = pageUri(source);
            Document html = Jsoup.parse(httpClient.get(page), source.toString());
            html.select("script, style, nav, footer").remove();
            SectionContent section = sectionContent(html, source.getFragment());
            return java.util.Optional.of(new McpGuideMatch(
                    scored.document.pageTitle(),
                    scored.document.sectionTitle(),
                    source.toString(),
                    scored.score(),
                    truncate(section.excerpt(), EXCERPT_LIMIT),
                    codeBlocks(source.toString(), section.codeBlocks())));
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            warnings.add("Skipped an unreadable official guide page: " + scored.document.sectionRoute());
            return java.util.Optional.empty();
        } catch (IOException | IllegalArgumentException exception) {
            warnings.add("Skipped an unreadable official guide page: " + scored.document.sectionRoute());
            return java.util.Optional.empty();
        }
    }

    private static McpGuideSearchResult result(
            String query,
            List<McpGuideMatch> matches,
            List<String> warnings) {
        return new McpGuideSearchResult(
                McpGuideSearchResult.CURRENT_SCHEMA_VERSION,
                query,
                GUIDE_INDEX_URI.toString(),
                matches,
                GUIDANCE_RULES,
                warnings);
    }

    private static GuideIndex readIndex(String json) throws IOException {
        JsonNode root = JSON.readTree(json);
        Map<String, GuideDocument> documents = new HashMap<>();
        for (JsonNode node : root.path("documents")) {
            GuideDocument document = new GuideDocument(
                    node.path("id").asText(),
                    node.path("pageTitle").asText(),
                    node.path("sectionTitle").asText(),
                    node.path("sectionRoute").asText());
            if (isPublicDocsRoute(document.sectionRoute())) {
                documents.put(document.id(), document);
            }
        }
        return new GuideIndex(documents, root.path("index").path("invertedIndex"));
    }

    private static List<ScoredDocument> score(GuideIndex index, String query) {
        Set<String> queryTokens = tokens(query);
        Map<String, Double> scores = new HashMap<>();
        for (JsonNode posting : index.invertedIndex()) {
            if (!posting.isArray() || posting.size() < 2) {
                continue;
            }
            String term = posting.get(0).asText();
            if (!matchesAnyToken(term, queryTokens)) {
                continue;
            }
            JsonNode fields = posting.get(1);
            for (Iterator<String> fieldNames = fields.fieldNames(); fieldNames.hasNext();) {
                String field = fieldNames.next();
                JsonNode ids = fields.path(field);
                for (Iterator<String> documentIds = ids.fieldNames(); documentIds.hasNext();) {
                    String id = documentIds.next();
                    if (index.documents().containsKey(id)) {
                        scores.merge(id, fieldWeight(field), Double::sum);
                    }
                }
            }
        }
        for (GuideDocument document : index.documents().values()) {
            String searchable = (document.pageTitle() + " " + document.sectionTitle() + " " + document.sectionRoute())
                    .toLowerCase(Locale.ROOT);
            for (String token : queryTokens) {
                if (searchable.contains(token)) {
                    scores.merge(document.id(), 3.0, Double::sum);
                }
            }
            if (searchable.contains(query.toLowerCase(Locale.ROOT))) {
                scores.merge(document.id(), 20.0, Double::sum);
            }
        }
        return scores.entrySet().stream()
                .map(entry -> new ScoredDocument(index.documents().get(entry.getKey()), entry.getValue()))
                .filter(scored -> scored.document() != null)
                .sorted(Comparator.comparingDouble(ScoredDocument::score).reversed()
                        .thenComparing(scored -> scored.document().sectionRoute()))
                .toList();
    }

    private static SectionContent sectionContent(Document document, String fragment) {
        Element scope = documentScope(document);
        Element heading = sectionHeading(scope, fragment);
        if (heading == null) {
            return new SectionContent(scope.text(), scope.select("pre").eachText());
        }
        return headingSectionContent(heading);
    }

    private static Element documentScope(Document document) {
        Element main = document.selectFirst("main");
        return main == null ? document.body() : main;
    }

    private static Element sectionHeading(Element scope, String fragment) {
        if (fragment == null || fragment.isBlank()) {
            return null;
        }
        Element heading = scope.ownerDocument().getElementById(fragment);
        if (heading == null || (!scope.equals(heading) && !scope.getAllElements().contains(heading))) {
            return null;
        }
        return heading;
    }

    private static SectionContent headingSectionContent(Element heading) {
        int headingLevel = headingLevel(heading);
        StringBuilder text = new StringBuilder(heading.text());
        List<String> codeBlocks = new ArrayList<>();
        for (Element sibling = heading.nextElementSibling(); sibling != null; sibling = sibling.nextElementSibling()) {
            if (startsNextSection(sibling, headingLevel)) {
                break;
            }
            String siblingText = sibling.text();
            if (!siblingText.isBlank()) {
                text.append("\n").append(siblingText);
            }
            codeBlocks.addAll(sibling.select("pre").eachText());
        }
        return new SectionContent(text.toString(), codeBlocks);
    }

    private static boolean startsNextSection(Element element, int currentHeadingLevel) {
        int siblingHeadingLevel = headingLevel(element);
        return siblingHeadingLevel > 0 && siblingHeadingLevel <= currentHeadingLevel;
    }

    private static List<McpCodeBlock> codeBlocks(String sourceUrl, List<String> examples) {
        List<McpCodeBlock> blocks = new ArrayList<>();
        int index = 1;
        for (String example : examples) {
            if (blocks.size() == 2) {
                break;
            }
            String code = truncate(example, CODE_LIMIT);
            if (code.isBlank()) {
                continue;
            }
            blocks.add(new McpCodeBlock(
                    "guide-example-" + index,
                    "Official SHAFT guide example",
                    code.contains(" class ") ? McpCodeBlock.Kind.FULL_CLASS : McpCodeBlock.Kind.ACTION,
                    "java",
                    List.of(),
                    code,
                    "Use only after adapting application URLs, data, and assertions.",
                    false,
                    List.of(sourceUrl),
                    List.of("Official guide example; adapt before use.")));
            index++;
        }
        return blocks;
    }

    private static URI officialUri(String route) {
        URI uri = route.startsWith("http://") || route.startsWith("https://")
                ? URI.create(route)
                : URI.create(GUIDE_ROOT).resolve(route);
        if (!"https".equalsIgnoreCase(uri.getScheme())
                || !GUIDE_HOST.equalsIgnoreCase(uri.getHost())
                || !isPublicDocsRoute(uri.getPath())) {
            throw new IllegalArgumentException("Unsupported guide route: " + route);
        }
        return uri;
    }

    private static URI pageUri(URI uri) {
        try {
            return new URI(uri.getScheme(), uri.getAuthority(), uri.getPath(), null, null);
        } catch (URISyntaxException exception) {
            throw new IllegalArgumentException("Unsupported guide URL: " + uri, exception);
        }
    }

    private static boolean isPublicDocsRoute(String route) {
        return route != null
                && route.startsWith("/docs/")
                && !route.startsWith("/docs/tags")
                && !route.startsWith("/docs/archive/")
                && !route.startsWith("/docs/maintainers/");
    }

    private static Set<String> tokens(String query) {
        Set<String> result = new LinkedHashSet<>();
        for (String token : TOKEN_SPLIT.split(query.toLowerCase(Locale.ROOT))) {
            if (token.length() > 1 && !STOP_WORDS.contains(token)) {
                result.add(token);
            }
        }
        if (query.toLowerCase(Locale.ROOT).contains("page object model")) {
            result.add("pom");
        }
        return result;
    }

    private static boolean matchesAnyToken(String term, Set<String> tokens) {
        for (String token : tokens) {
            if (term.equals(token)
                    || (token.length() >= 4 && term.startsWith(token))
                    || (term.length() >= 4 && token.startsWith(term))) {
                return true;
            }
        }
        return false;
    }

    private static double fieldWeight(String field) {
        return switch (field) {
            case "title" -> 12.0;
            case "tags" -> 8.0;
            case "sidebarParentCategories" -> 5.0;
            case "content" -> 2.0;
            default -> 1.0;
        };
    }

    private static int headingLevel(Element element) {
        String tag = element.tagName();
        if (tag.length() == 2 && tag.charAt(0) == 'h' && Character.isDigit(tag.charAt(1))) {
            return Character.digit(tag.charAt(1), 10);
        }
        return 0;
    }

    private static int clamp(int maxResults) {
        if (maxResults < 1) {
            return DEFAULT_LIMIT;
        }
        return Math.min(maxResults, MAX_LIMIT);
    }

    private static String normalizeQuery(String query) {
        return query == null || query.isBlank()
                ? "SHAFT guide best practices locators actions API GUI CLI troubleshooting"
                : query.trim();
    }

    private static String truncate(String value, int maxLength) {
        String text = value == null ? "" : value.trim();
        if (text.length() <= maxLength) {
            return text;
        }
        return text.substring(0, maxLength - 1).trim() + "...";
    }

    interface GuideHttpClient {
        String get(URI uri) throws IOException, InterruptedException;
    }

    private static final class JdkGuideHttpClient implements GuideHttpClient {
        private final HttpClient client = HttpClient.newBuilder()
                .connectTimeout(Duration.ofSeconds(10))
                .build();

        @Override
        public String get(URI uri) throws IOException, InterruptedException {
            HttpRequest request = HttpRequest.newBuilder(uri)
                    .timeout(Duration.ofSeconds(15))
                    .header("User-Agent", "shaft-mcp-guide-search")
                    .GET()
                    .build();
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() < 200 || response.statusCode() >= 300) {
                throw new IOException("Unexpected guide response status: " + response.statusCode());
            }
            return response.body();
        }
    }

    private record GuideDocument(String id, String pageTitle, String sectionTitle, String sectionRoute) {
    }

    private record GuideIndex(Map<String, GuideDocument> documents, JsonNode invertedIndex) {
    }

    private record ScoredDocument(GuideDocument document, double score) {
    }

    private record SectionContent(String excerpt, List<String> codeBlocks) {
    }
}
