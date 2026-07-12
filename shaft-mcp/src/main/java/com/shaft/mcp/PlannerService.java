package com.shaft.mcp;

import com.shaft.capture.generate.LocatorRanker;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.EventContext;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.capture.model.PageContext;
import com.shaft.driver.SHAFT;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.openqa.selenium.WebDriver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;

import static com.shaft.mcp.EngineService.getDriver;

/**
 * MCP planner: crawls the active SHAFT browser session breadth-first and writes deterministic,
 * review-first Markdown test plans per discovered flow. No AI calls are made; locator evidence and
 * ranking reuse the same primitives as {@link BrowserService#openForIntent}.
 */
@Service
public class PlannerService {
    private static final Logger logger = LoggerFactory.getLogger(PlannerService.class);
    private static final int MIN_DEPTH = 1;
    private static final int MAX_DEPTH = 3;
    private static final int DEFAULT_MAX_PAGES = 10;
    private static final int MAX_PAGES = 50;
    private static final int MAX_NAVIGATION_ACTIONS_PER_PAGE = 5;
    private static final String SPECS_DIRECTORY = "specs";

    private final McpWorkspacePolicy workspacePolicy;
    private final PlannerPageInspector pageInspector;
    private final Consumer<String> driverRestore;

    /**
     * Creates the default local MCP planner service.
     */
    public PlannerService() {
        this(McpWorkspacePolicy.current());
    }

    PlannerService(McpWorkspacePolicy workspacePolicy) {
        this(workspacePolicy, null, null);
    }

    PlannerService(
            McpWorkspacePolicy workspacePolicy,
            PlannerPageInspector pageInspector,
            Consumer<String> driverRestore) {
        this.workspacePolicy = Objects.requireNonNull(workspacePolicy, "workspacePolicy");
        this.pageInspector = pageInspector == null ? this::inspectLive : pageInspector;
        this.driverRestore = driverRestore == null ? PlannerService::restoreLiveDriver : driverRestore;
    }

    /**
     * Crawls the active SHAFT browser session breadth-first from {@code targetUrl}, discovering
     * forms, primary navigation actions, and same-origin links, and writes one Markdown test plan
     * per discovered flow into {@code specs/} inside the MCP workspace.
     *
     * @param targetUrl absolute URL to start the crawl from
     * @param goal      optional natural-language exploration goal used only to order the written
     *                  plans (flows whose text matches the goal are written first); never sent to
     *                  an AI provider
     * @param maxDepth  breadth-first depth from {@code targetUrl}, clamped to 1..3
     * @param maxPages  maximum number of pages visited, clamped to 1..50 (default 10)
     * @return pages visited, the plan files written, and any bounded-crawl warnings
     */
    @Tool(name = "test_plan_explore",
            description = "crawls the active SHAFT browser session breadth-first from a URL, discovering forms, "
                    + "primary navigation actions, and same-origin links, and writes one deterministic Markdown "
                    + "test plan per discovered flow into specs/ (no AI calls)")
    public McpTestPlanExploreResult exploreAndPlan(String targetUrl, String goal, int maxDepth, int maxPages) {
        try {
            getDriver();
            McpTestPlanExploreResult result = crawl(targetUrl, goal, maxDepth, maxPages);
            logger.info("Test plan exploration completed (pagesVisited: {}, plansWritten: {}).",
                    result.pagesVisited(), result.plansWritten().size());
            return result;
        } catch (Exception e) {
            logger.error("Failed to explore and plan from target URL (value redacted).", e);
            throw e;
        }
    }

    /**
     * Runs the breadth-first crawl and plan-writing pipeline against {@link #pageInspector}, without
     * requiring a live browser session. Package-private so unit tests can exercise the BFS budgets,
     * origin filtering, fail-soft page handling, and plan rendering with a fake inspector.
     *
     * @param targetUrl absolute URL to start the crawl from
     * @param goal      optional exploration goal used to order written plans
     * @param maxDepth  requested crawl depth before clamping
     * @param maxPages  requested page budget before clamping
     * @return the crawl and plan-writing result
     */
    McpTestPlanExploreResult crawl(String targetUrl, String goal, int maxDepth, int maxPages) {
        String startUrl = requireUrl(targetUrl);
        URI origin = originOf(startUrl);
        String normalizedStart = normalizeUrl(startUrl);
        if (normalizedStart == null) {
            throw new IllegalArgumentException("test_plan_explore requires a valid targetUrl.");
        }
        int depthLimit = Math.max(MIN_DEPTH, Math.min(maxDepth <= 0 ? MIN_DEPTH : maxDepth, MAX_DEPTH));
        int pagesLimit = Math.max(1, Math.min(maxPages <= 0 ? DEFAULT_MAX_PAGES : maxPages, MAX_PAGES));

        CrawlState state = new CrawlState();
        state.queue.add(new QueueEntry(startUrl, 1));
        state.enqueued.add(normalizedStart);

        while (!state.queue.isEmpty()) {
            visitNext(state, origin, depthLimit, pagesLimit);
        }

        List<PageFlow> ordered = orderByGoal(state.discovered, goal);
        List<String> plansWritten = writePlans(ordered);
        appendSummaryWarnings(state, pagesLimit);
        driverRestore.accept(startUrl);
        return new McpTestPlanExploreResult("1.0", state.visited.size(), plansWritten, List.copyOf(state.warnings));
    }

    /** Mutable breadth-first crawl bookkeeping shared by {@link #crawl} and its helpers. */
    private static final class CrawlState {
        final Deque<QueueEntry> queue = new ArrayDeque<>();
        final Set<String> visited = new LinkedHashSet<>();
        final Set<String> enqueued = new LinkedHashSet<>();
        final List<String> warnings = new ArrayList<>();
        final List<PageFlow> discovered = new ArrayList<>();
        int pageBudgetSkipped;
        int offOriginSkipped;
        int invalidLinksSkipped;
    }

    private void visitNext(CrawlState state, URI origin, int depthLimit, int pagesLimit) {
        QueueEntry entry = state.queue.poll();
        if (state.visited.size() >= pagesLimit) {
            state.pageBudgetSkipped++;
            return;
        }
        PlannerPageSnapshot snapshot;
        try {
            snapshot = pageInspector.inspect(entry.url());
        } catch (RuntimeException failure) {
            state.warnings.add("Skipped page " + entry.url() + " after an inspection failure: " + failure.getMessage());
            return;
        }
        String visitedKey = normalizeUrl(snapshot.url() == null || snapshot.url().isBlank()
                ? entry.url() : snapshot.url());
        if (visitedKey == null || !state.visited.add(visitedKey)) {
            return;
        }
        for (PlannerFlow flow : snapshot.flows()) {
            state.discovered.add(new PageFlow(snapshot.url(), flow));
        }
        if (entry.depth() < depthLimit) {
            enqueueLinks(state, snapshot, origin, entry.depth());
        }
    }

    private static void enqueueLinks(CrawlState state, PlannerPageSnapshot snapshot, URI origin, int depth) {
        for (String link : snapshot.links()) {
            String normalizedLink = normalizeUrl(link);
            if (normalizedLink == null) {
                state.invalidLinksSkipped++;
            } else if (!sameOrigin(normalizedLink, origin)) {
                state.offOriginSkipped++;
            } else if (state.enqueued.add(normalizedLink)) {
                state.queue.add(new QueueEntry(link, depth + 1));
            }
        }
    }

    private static void appendSummaryWarnings(CrawlState state, int pagesLimit) {
        if (state.pageBudgetSkipped > 0) {
            state.warnings.add("Page budget (" + pagesLimit + ") reached; " + state.pageBudgetSkipped
                    + " discovered page(s) were not visited.");
        }
        if (state.offOriginSkipped > 0) {
            state.warnings.add(state.offOriginSkipped + " off-origin link(s) were skipped.");
        }
        if (state.invalidLinksSkipped > 0) {
            state.warnings.add(state.invalidLinksSkipped + " unresolvable link(s) were skipped.");
        }
        if (state.discovered.isEmpty()) {
            state.warnings.add("No forms or primary navigation actions were discovered during exploration.");
        }
    }

    private List<String> writePlans(List<PageFlow> flows) {
        List<String> written = new ArrayList<>();
        int index = 0;
        for (PageFlow pageFlow : flows) {
            index++;
            String markdown = renderPlan(pageFlow.flow(), pageFlow.pageUrl());
            String fileName = String.format(Locale.ROOT, "%02d-%s.md", index, slugify(pageFlow.flow().title()));
            Path resolved = workspacePolicy.output(SPECS_DIRECTORY + "/" + fileName, "Test plan output path");
            try {
                Path parent = resolved.getParent();
                if (parent != null) {
                    Files.createDirectories(parent);
                }
                Files.writeString(resolved, markdown, StandardCharsets.UTF_8);
            } catch (IOException exception) {
                throw new IllegalArgumentException(
                        "Test plan output path cannot be written inside the MCP workspace.", exception);
            }
            written.add(resolved.toString());
        }
        return List.copyOf(written);
    }

    private PlannerPageSnapshot inspectLive(String url) {
        SHAFT.GUI.WebDriver driver = getDriver();
        driver.browser().navigateToURL(url);
        WebDriver seleniumDriver = driver.getDriver();
        String currentUrl = seleniumDriver.getCurrentUrl();
        String title = seleniumDriver.getTitle();
        String dom = seleniumDriver.getPageSource();
        return buildSnapshot(currentUrl, title, dom == null ? "" : dom);
    }

    private static void restoreLiveDriver(String url) {
        // Best-effort: exploration already succeeded, so a failure to restore the starting page
        // must never fail the tool call.
        try {
            getDriver().browser().navigateToURL(url);
        } catch (RuntimeException ignored) {
            logger.debug("Could not restore the browser to the exploration start URL after crawling.");
        }
    }

    /**
     * Parses one page's DOM into candidate flows (forms, primary navigation actions) and same-origin
     * link candidates, reusing {@link BrowserService}'s element-to-locator-evidence construction.
     * Pure/deterministic so it can be exercised directly in tests without a live browser.
     *
     * @param url   the page URL (post-navigation/redirect)
     * @param title the page title
     * @param dom   the page HTML
     * @return the page snapshot
     */
    static PlannerPageSnapshot buildSnapshot(String url, String title, String dom) {
        Document document = Jsoup.parse(dom == null ? "" : dom, url == null ? "" : url);
        EventContext context = new EventContext(
                1,
                Instant.EPOCH,
                new PageContext(url, title, "window-1", List.of(), 0, 0),
                EventContext.ReplayStatus.NOT_REPLAYED,
                List.of(),
                Map.of());
        List<PlannerFlow> flows = new ArrayList<>();
        int formIndex = 0;
        for (Element form : document.select("form")) {
            formIndex++;
            PlannerFlow flow = formFlow(document, context, form, formIndex, title);
            if (flow != null) {
                flows.add(flow);
            }
        }
        flows.addAll(navigationFlows(document, context));
        List<String> links = document.select("a[href]").stream()
                .map(anchor -> anchor.absUrl("href"))
                .filter(href -> !href.isBlank())
                .distinct()
                .toList();
        return new PlannerPageSnapshot(url, title == null ? "" : title, List.copyOf(flows), links);
    }

    private static PlannerFlow formFlow(
            Document document, EventContext context, Element form, int formIndex, String pageTitle) {
        List<Element> fields = form.select("input:not([type=hidden]):not([type=submit]):not([type=button])"
                + ":not([type=reset]):not([type=image]), textarea, select");
        Element submit = form.select("button[type=submit], input[type=submit], button:not([type])").first();
        if (fields.isEmpty() && submit == null) {
            return null;
        }
        List<PlannerStep> steps = new ArrayList<>();
        for (Element field : fields) {
            steps.add(fieldStep(document, context, field));
        }
        if (submit != null) {
            steps.add(clickStep(document, context, submit, describeControl(document, submit)));
        }
        if (steps.isEmpty()) {
            return null;
        }
        String title = flowTitle(form, pageTitle, formIndex);
        return new PlannerFlow(title, List.copyOf(steps),
                "Confirm the expected outcome of \"" + title + "\" (success message, redirect, or updated state).");
    }

    private static List<PlannerFlow> navigationFlows(Document document, EventContext context) {
        List<PlannerFlow> flows = new ArrayList<>();
        int count = 0;
        for (Element element : document.select("a[href], button")) {
            if (count >= MAX_NAVIGATION_ACTIONS_PER_PAGE) {
                break;
            }
            if (element.closest("form") != null) {
                continue;
            }
            if ("a".equals(element.tagName().toLowerCase(Locale.ROOT))) {
                String href = element.attr("href").trim().toLowerCase(Locale.ROOT);
                if (href.isEmpty() || href.equals("#") || href.startsWith("javascript:")) {
                    continue;
                }
            }
            String description = describeControl(document, element);
            if (description.isBlank()) {
                continue;
            }
            PlannerStep step = clickStep(document, context, element, description);
            flows.add(new PlannerFlow(
                    "Navigate: " + description,
                    List.of(step),
                    "Confirm that \"" + description + "\" leads to the expected page or state."));
            count++;
        }
        return flows;
    }

    private static PlannerStep fieldStep(Document document, EventContext context, Element field) {
        PlannerLocator locator = pickLocator(document, context, field);
        String description = describeControl(document, field);
        String tag = field.tagName().toLowerCase(Locale.ROOT);
        if ("select".equals(tag)) {
            return new PlannerStep("select", description, locator, "Select an appropriate option");
        }
        String type = field.attr("type").isBlank() ? "text" : field.attr("type").toLowerCase(Locale.ROOT);
        if ("checkbox".equals(type)) {
            return new PlannerStep("check", description, locator, "checked");
        }
        if ("radio".equals(type)) {
            return new PlannerStep("select", description, locator, "selected");
        }
        return new PlannerStep("type", description, locator, seedDataFor(type, description));
    }

    private static PlannerStep clickStep(
            Document document, EventContext context, Element element, String description) {
        PlannerLocator locator = pickLocator(document, context, element);
        return new PlannerStep("click", description, locator, "");
    }

    private static PlannerLocator pickLocator(Document document, EventContext context, Element element) {
        ElementSnapshot snapshot = BrowserService.snapshot(document, element);
        if (!snapshot.locatorCandidates().isEmpty()) {
            LocatorRanker.LocatorSelection selection = new LocatorRanker().select(snapshot, context, true);
            LocatorCandidate best = selection.selected().candidate();
            return new PlannerLocator(
                    best.strategy().name(), best.expression(), BrowserService.locatorCode(best, snapshot));
        }
        String cssFallback = element.cssSelector();
        return new PlannerLocator("CSS", cssFallback,
                "SHAFT.GUI.Locator.cssSelector(\"" + BrowserService.javaString(cssFallback) + "\")");
    }

    private static String seedDataFor(String type, String description) {
        return switch (type) {
            case "email" -> "user@example.com";
            case "password" -> "P@ssw0rd1";
            case "tel" -> "+15551234567";
            case "number" -> "1";
            case "date" -> "2026-01-01";
            case "url" -> "https://example.com";
            case "search" -> "sample search";
            default -> "Sample text for " + description;
        };
    }

    private static String describeControl(Document document, Element element) {
        String candidate = firstNonBlank(
                element.attr("aria-label"),
                BrowserService.label(document, element),
                element.attr("placeholder"),
                element.text(),
                element.attr("value"),
                element.attr("name"),
                element.attr("id"));
        return candidate.isBlank() ? capitalize(element.tagName()) : candidate;
    }

    private static String flowTitle(Element form, String pageTitle, int formIndex) {
        String label = firstNonBlank(form.attr("aria-label"), form.attr("name"), form.attr("id"));
        if (!label.isBlank()) {
            return humanize(label) + " form";
        }
        String base = pageTitle == null || pageTitle.isBlank() ? "Form" : pageTitle.trim();
        return base + " form " + formIndex;
    }

    private static List<PageFlow> orderByGoal(List<PageFlow> flows, String goal) {
        Set<String> tokens = tokenize(goal);
        if (tokens.isEmpty()) {
            return List.copyOf(flows);
        }
        List<PageFlow> matched = new ArrayList<>();
        List<PageFlow> unmatched = new ArrayList<>();
        for (PageFlow flow : flows) {
            if (matchesGoal(flow.flow(), tokens)) {
                matched.add(flow);
            } else {
                unmatched.add(flow);
            }
        }
        List<PageFlow> ordered = new ArrayList<>(matched);
        ordered.addAll(unmatched);
        return List.copyOf(ordered);
    }

    private static boolean matchesGoal(PlannerFlow flow, Set<String> tokens) {
        String searchable = flow.title().toLowerCase(Locale.ROOT);
        if (containsAny(searchable, tokens)) {
            return true;
        }
        for (PlannerStep step : flow.steps()) {
            if (containsAny(step.description().toLowerCase(Locale.ROOT), tokens)) {
                return true;
            }
        }
        return false;
    }

    private static boolean containsAny(String haystack, Set<String> tokens) {
        for (String token : tokens) {
            if (haystack.contains(token)) {
                return true;
            }
        }
        return false;
    }

    private static Set<String> tokenize(String value) {
        Set<String> tokens = new LinkedHashSet<>();
        if (value == null) {
            return tokens;
        }
        for (String token : value.toLowerCase(Locale.ROOT).split("[^a-z0-9]+")) {
            if (token.length() > 2) {
                tokens.add(token);
            }
        }
        return tokens;
    }

    static String renderPlan(PlannerFlow flow, String pageUrl) {
        StringBuilder markdown = new StringBuilder();
        markdown.append("# ").append(flow.title()).append(System.lineSeparator()).append(System.lineSeparator());
        markdown.append("## Preconditions").append(System.lineSeparator());
        markdown.append("- Start at: ")
                .append(pageUrl == null || pageUrl.isBlank() ? "(unknown URL)" : pageUrl)
                .append(System.lineSeparator()).append(System.lineSeparator());
        markdown.append("## Steps").append(System.lineSeparator());
        int number = 0;
        for (PlannerStep step : flow.steps()) {
            number++;
            markdown.append(number).append(". ").append(renderStepText(step))
                    .append(" — `").append(step.locator().shaftLocatorCode()).append('`')
                    .append(System.lineSeparator());
        }
        markdown.append(System.lineSeparator());
        markdown.append("## Verify").append(System.lineSeparator());
        markdown.append("- ").append(flow.verifySuggestion()).append(System.lineSeparator());
        return markdown.toString();
    }

    private static String renderStepText(PlannerStep step) {
        return switch (step.action()) {
            case "type" -> "Type \"" + step.seedData() + "\" into " + step.description();
            case "select" -> "Select \"" + step.seedData() + "\" for " + step.description();
            case "check" -> "Check " + step.description();
            default -> "Click " + step.description();
        };
    }

    private static String slugify(String value) {
        String slug = (value == null ? "" : value).toLowerCase(Locale.ROOT)
                .replaceAll("[^a-z0-9]+", "-")
                .replaceAll("(^-|-$)", "");
        if (slug.isBlank()) {
            slug = "flow";
        }
        return slug.length() > 60 ? slug.substring(0, 60) : slug;
    }

    private static String firstNonBlank(String... values) {
        for (String value : values) {
            if (value != null && !value.trim().isBlank()) {
                return value.trim();
            }
        }
        return "";
    }

    private static String capitalize(String value) {
        String text = value == null ? "" : value.trim();
        if (text.isEmpty()) {
            return text;
        }
        return Character.toUpperCase(text.charAt(0)) + text.substring(1);
    }

    private static String humanize(String value) {
        String spaced = value.replaceAll("[-_]+", " ").replaceAll("(?<=[a-z0-9])(?=[A-Z])", " ");
        return capitalize(spaced.trim().toLowerCase(Locale.ROOT));
    }

    private static String requireUrl(String value) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException("test_plan_explore requires a targetUrl.");
        }
        return value.trim();
    }

    private static URI originOf(String url) {
        try {
            URI uri = new URI(url);
            if (uri.getScheme() == null || uri.getHost() == null) {
                throw new IllegalArgumentException(
                        "test_plan_explore requires an absolute targetUrl with a scheme and host.");
            }
            return uri;
        } catch (URISyntaxException exception) {
            throw new IllegalArgumentException("test_plan_explore requires a valid targetUrl.", exception);
        }
    }

    private static String normalizeUrl(String raw) {
        if (raw == null || raw.isBlank()) {
            return null;
        }
        try {
            URI uri = new URI(raw.trim());
            if (uri.getScheme() == null || uri.getHost() == null) {
                return null;
            }
            URI normalized = new URI(uri.getScheme().toLowerCase(Locale.ROOT), uri.getAuthority(),
                    uri.getPath(), uri.getQuery(), null);
            String result = normalized.toString();
            return result.length() > 1 && result.endsWith("/") ? result.substring(0, result.length() - 1) : result;
        } catch (URISyntaxException exception) {
            return null;
        }
    }

    private static boolean sameOrigin(String url, URI origin) {
        try {
            URI uri = new URI(url);
            return Objects.equals(origin.getScheme(), uri.getScheme())
                    && Objects.equals(origin.getHost(), uri.getHost())
                    && effectivePort(origin.getScheme(), origin.getPort())
                    == effectivePort(uri.getScheme(), uri.getPort());
        } catch (URISyntaxException exception) {
            return false;
        }
    }

    private static int effectivePort(String scheme, int port) {
        if (port != -1) {
            return port;
        }
        return "https".equalsIgnoreCase(scheme) ? 443 : 80;
    }

    private record QueueEntry(String url, int depth) {
    }

    private record PageFlow(String pageUrl, PlannerFlow flow) {
    }

    /**
     * Seam between the breadth-first crawl/plan-writing logic and how one page's DOM is inspected.
     * The default implementation drives the live SHAFT browser session; tests supply a fake that
     * returns canned snapshots so the BFS budgets, origin filtering, and plan rendering can be
     * exercised without a browser.
     */
    @FunctionalInterface
    interface PlannerPageInspector {
        PlannerPageSnapshot inspect(String url);
    }

    /**
     * One visited page's discovered flows and outbound links.
     *
     * @param url   the page URL after navigation/redirects
     * @param title the page title
     * @param flows candidate flows discovered on this page (forms, primary navigation actions)
     * @param links every {@code a[href]} target found on the page, any origin; the crawl filters
     *              for same-origin links itself so that behavior stays testable
     */
    record PlannerPageSnapshot(String url, String title, List<PlannerFlow> flows, List<String> links) {
        PlannerPageSnapshot {
            flows = flows == null ? List.of() : List.copyOf(flows);
            links = links == null ? List.of() : List.copyOf(links);
        }
    }

    /**
     * One candidate test flow (a form submission or a primary navigation action).
     *
     * @param title           human-readable flow title, used to derive the plan file name
     * @param steps           ordered steps with candidate locators and seed data
     * @param verifySuggestion suggested verification step for the plan's Verify section
     */
    record PlannerFlow(String title, List<PlannerStep> steps, String verifySuggestion) {
        PlannerFlow {
            steps = steps == null ? List.of() : List.copyOf(steps);
        }
    }

    /**
     * One step within a flow.
     *
     * @param action      {@code type}, {@code select}, {@code check}, or {@code click}
     * @param description human-readable target description (label, placeholder, or text)
     * @param locator     candidate locator for the target element
     * @param seedData    type-appropriate seed value; blank for click-only steps
     */
    record PlannerStep(String action, String description, PlannerLocator locator, String seedData) {
    }

    /**
     * One candidate locator for a step's target element.
     *
     * @param strategy         locator strategy name
     * @param expression       locator expression
     * @param shaftLocatorCode ready-to-paste {@code SHAFT.GUI.Locator}/{@code By} snippet
     */
    record PlannerLocator(String strategy, String expression, String shaftLocatorCode) {
    }
}
