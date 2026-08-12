package com.shaft.tools.io.internal;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

/** Renders validated Playwright frame snapshots without a server or service worker. */
final class PlaywrightTraceOfflineAdapter {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final int MAX_MODEL_BYTES = 8 * 1024 * 1024;
    private static final int MAX_RENDER_BYTES = 8 * 1024 * 1024;
    private static final int MAX_RENDER_NODES = 200_000;
    private static final int MAX_RENDER_DEPTH = 512;
    private static final Pattern SAFE_NAME = Pattern.compile("[A-Za-z][A-Za-z0-9:_-]*");
    private static final Pattern CSS_URL = Pattern.compile("(?i)url\\s*\\(\\s*(['\"]?)([^)'\"]+)\\1\\s*\\)");
    private static final Pattern CSS_IMPORT = Pattern.compile(
            "(?i)@import\\s+(?:url\\(\\s*(?:(['\"])([^'\"]+)\\1|([^)'\"\\s]+))\\s*\\)"
                    + "|(['\"])([^'\"]+)\\4)\\s*([^;]*);");
    private static final Pattern STYLE_END = Pattern.compile("(?i)</style");
    private static final Set<String> VOID_ELEMENTS = Set.of(
            "AREA", "BASE", "BR", "COL", "COMMAND", "EMBED", "HR", "IMG", "INPUT", "KEYGEN", "LINK",
            "MENUITEM", "META", "PARAM", "SOURCE", "TRACK", "WBR");
    private static final Set<String> DROPPED_ELEMENTS = Set.of(
            "SCRIPT", "BASE", "IFRAME", "FRAME", "OBJECT", "EMBED", "VIDEO", "AUDIO", "SOURCE", "TRACK");
    private static final Set<String> URL_ATTRIBUTES = Set.of(
            "src", "href", "xlink:href", "poster", "background", "action", "formaction", "cite", "manifest");

    private PlaywrightTraceOfflineAdapter() {
    }

    static String render(PlaywrightTraceArchiveLoader.LoadedArchive archive, String snapshotName) {
        String document = snapshotDocument(archive, snapshotName);
        String csp = "<meta http-equiv=\"Content-Security-Policy\" content=\"default-src 'none'; "
                + "style-src 'unsafe-inline'; img-src data:; font-src data:; connect-src 'none'; object-src 'none'; "
                + "base-uri 'none'; form-action 'none'\">";
        String encoded = Base64.getEncoder().encodeToString((csp + document).getBytes(StandardCharsets.UTF_8));
        return """
                <!doctype html><html lang="en"><head><meta charset="utf-8">
                <meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline'; img-src data:; font-src data:; frame-src data:; connect-src 'none'; object-src 'none'; base-uri 'none'; form-action 'none'">
                <title>Playwright snapshot (offline)</title></head><body>
                <iframe id="playwright-snapshot" title="Playwright DOM snapshot" sandbox=""
                        src="data:text/html;charset=utf-8;base64,%s"></iframe></body></html>
                """.formatted(encoded);
    }

    static String snapshotDocument(PlaywrightTraceArchiveLoader.LoadedArchive archive, String snapshotName) {
        Model model = model(archive);
        SnapshotContext selected = model.snapshots().stream()
                .filter(context -> snapshotName.equals(context.snapshot().path("snapshotName").asText()))
                .filter(context -> context.snapshot().path("isMainFrame").asBoolean(false))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException(
                        "Main-frame Playwright snapshot is unavailable: " + snapshotName));
        RenderBudget output = new RenderBudget();
        renderNode(selected.snapshot().path("html"), selected, selected, model, output, 0);
        String doctype = selected.snapshot().path("doctype").asText().replaceAll("[^a-zA-Z0-9]", "");
        return (doctype.isBlank() ? "" : "<!DOCTYPE " + doctype + ">") + output.value();
    }

    private static Model model(PlaywrightTraceArchiveLoader.LoadedArchive archive) {
        List<SnapshotContext> snapshots = new ArrayList<>();
        List<ResourceRecord> resources = new ArrayList<>();
        Map<String, List<SnapshotContext>> histories = new HashMap<>();
        NodeTableBudget nodeTableBudget = new NodeTableBudget();
        int modelBytes = 0;
        for (String name : archive.traceEntryNames()) {
            if (!name.endsWith(".trace") && !name.endsWith(".network")) {
                continue;
            }
            int entrySize = archive.entrySize(name);
            modelBytes = Math.addExact(modelBytes, entrySize);
            if (modelBytes > MAX_MODEL_BYTES) {
                throw new IllegalArgumentException("Playwright snapshot model exceeds the offline model limit.");
            }
            byte[] bytes = archive.entry(name);
            for (JsonNode record : records(bytes)) {
                if ("frame-snapshot".equals(record.path("type").asText())) {
                    JsonNode snapshot = record.path("snapshot");
                    String frameId = snapshot.path("frameId").asText();
                    List<SnapshotContext> history = histories.computeIfAbsent(frameId, ignored -> new ArrayList<>());
                    SnapshotContext context = new SnapshotContext(snapshot, history, history.size(), nodeTableBudget);
                    history.add(context);
                    snapshots.add(context);
                } else if ("resource-snapshot".equals(record.path("type").asText())) {
                    JsonNode snapshot = record.path("snapshot");
                    resources.add(new ResourceRecord(snapshot.path("request").path("url").asText(),
                            snapshot.path("request").path("method").asText(),
                            snapshot.path("response").path("status").asInt(),
                            snapshot.path("response").path("content").path("_sha1").asText(),
                            snapshot.path("response").path("content").path("mimeType").asText(),
                            snapshot.path("_frameref").asText(), snapshot.path("_monotonicTime").asDouble()));
                }
            }
        }
        resources.sort(Comparator.comparingDouble(ResourceRecord::time));
        Map<String, List<ResourceRecord>> resourcesByUrl = new HashMap<>();
        for (ResourceRecord resource : resources) {
            resourcesByUrl.computeIfAbsent(resource.url(), ignored -> new ArrayList<>()).add(resource);
        }
        resourcesByUrl.replaceAll((ignored, records) -> List.copyOf(records));
        return new Model(List.copyOf(snapshots), Map.copyOf(resourcesByUrl), archive);
    }

    private static List<JsonNode> records(byte[] bytes) {
        List<JsonNode> records = new ArrayList<>();
        int start = 0;
        for (int index = 0; index <= bytes.length; index++) {
            if (index < bytes.length && bytes[index] != '\n') {
                continue;
            }
            int end = index > start && bytes[index - 1] == '\r' ? index - 1 : index;
            if (end > start) {
                records.add(JSON.readTree(bytes, start, end - start));
            }
            start = index + 1;
        }
        return records;
    }

    @SuppressWarnings("PMD.NPathComplexity")
    private static void renderNode(JsonNode node, SnapshotContext structureContext,
                                   SnapshotContext resourceContext, Model model, RenderBudget output, int depth) {
        output.visit(depth);
        if (node.isString()) {
            output.append(escapeText(node.asText()));
            return;
        }
        if (!node.isArray() || node.isEmpty()) {
            return;
        }
        JsonNode first = node.get(0);
        if (first.isArray() && first.size() == 2) {
            int delta = first.get(0).asInt();
            if (delta <= 0) {
                throw new IllegalArgumentException("Playwright snapshot reference must point backward.");
            }
            int referenceIndex = structureContext.index() - delta;
            if (referenceIndex < 0 || referenceIndex >= structureContext.history().size()) {
                return;
            }
            SnapshotContext referenced = structureContext.history().get(referenceIndex);
            int nodeIndex = first.get(1).asInt();
            List<JsonNode> cachedNodes = referenced.nodes();
            if (nodeIndex >= 0 && nodeIndex < cachedNodes.size()) {
                renderNode(cachedNodes.get(nodeIndex), referenced, resourceContext, model, output, depth + 1);
            }
            return;
        }
        if (!first.isString() || !SAFE_NAME.matcher(first.asText()).matches()) {
            return;
        }
        String name = first.asText();
        String upper = name.toUpperCase(Locale.ROOT);
        if (DROPPED_ELEMENTS.contains(upper) || "META".equals(upper)) {
            return;
        }
        JsonNode attributes = node.size() > 1 ? node.get(1) : null;
        if ("LINK".equals(upper) && attributes != null
                && "stylesheet".equalsIgnoreCase(attributes.path("rel").asText())) {
            Resource resource = resource(model, resourceContext, attributes.path("href").asText());
            if (resource != null && resource.mimeType().toLowerCase(Locale.ROOT).startsWith("text/css")) {
                String stylesheetUrl = resolveUrl(resourceContext.snapshot().path("frameUrl").asText(),
                        attributes.path("href").asText());
                output.append("<style>");
                rewriteCssResource(resource, stylesheetUrl, model, resourceContext, output, output.cssSources(),
                        new HashSet<>(), 0);
                output.append("</style>");
            }
            return;
        }
        output.append("<").append(name);
        if (attributes != null && attributes.isObject()) {
            for (Map.Entry<String, JsonNode> attribute : attributes.properties()) {
                String key = attribute.getKey();
                String lower = key.toLowerCase(Locale.ROOT);
                if (lower.equals("__playwright_value_")) {
                    key = "value";
                    lower = "value";
                }
                if (!SAFE_NAME.matcher(key).matches() || lower.startsWith("on") || lower.equals("srcdoc")
                        || lower.equals("sandbox") || lower.equals("srcset") || lower.equals("ping")) {
                    continue;
                }
                String value = attribute.getValue().asText();
                if (URL_ATTRIBUTES.contains(lower)) {
                    if (lower.equals("href") || lower.equals("xlink:href") || lower.equals("action")
                            || lower.equals("formaction")) {
                        value = "A".equals(upper) && lower.equals("href") ? "#" : "";
                    } else {
                        Resource resource = resource(model, resourceContext, value);
                        value = resource == null ? "" : resource.dataUri(output.remainingBytes());
                    }
                } else if (lower.equals("style")) {
                    value = rewriteCss(value, resourceContext.snapshot().path("frameUrl").asText(), model,
                            resourceContext,
                            output.remainingBytes(), output.cssSources(), new HashSet<>(), 0);
                }
                output.append(" ").append(key).append("=\"").append(escapeAttribute(value)).append("\"");
            }
        }
        output.append(">");
        for (int index = 2; index < node.size(); index++) {
            if ("STYLE".equals(upper) && node.get(index).isString()) {
                output.append(rewriteCss(node.get(index).asText(),
                        resourceContext.snapshot().path("frameUrl").asText(), model, resourceContext,
                        output.remainingBytes(), output.cssSources(), new HashSet<>(), 0));
            } else {
                renderNode(node.get(index), structureContext, resourceContext, model, output, depth + 1);
            }
        }
        if (!VOID_ELEMENTS.contains(upper)) {
            output.append("</").append(name).append(">");
        }
    }

    private static Resource resource(Model model, SnapshotContext context, String serializedUrl) {
        String url = resolveUrl(context.snapshot().path("frameUrl").asText(), serializedUrl);
        Optional<Resource> cached = context.resources().get(url);
        if (cached != null) {
            return cached.orElse(null);
        }
        Resource selected = selectResource(model, context, url);
        context.resources().put(url, Optional.ofNullable(selected));
        return selected;
    }

    @SuppressWarnings("PMD.NPathComplexity")
    private static Resource selectResource(Model model, SnapshotContext context, String url) {
        String overrideSha1 = overrideSha1(context, url);
        ResourceRecord sameFrame = null;
        ResourceRecord otherFrame = null;
        double snapshotTime = context.snapshot().path("timestamp").asDouble();
        for (ResourceRecord record : model.resourcesByUrl().getOrDefault(url, List.of())) {
            if (record.time() >= snapshotTime) {
                break;
            }
            if (record.status() == 304 || !"GET".equalsIgnoreCase(record.method()) || !url.equals(record.url())) {
                continue;
            }
            if (context.snapshot().path("frameId").asText().equals(record.frameId())) {
                sameFrame = record;
            } else {
                otherFrame = record;
            }
        }
        ResourceRecord chosen = sameFrame != null ? sameFrame : otherFrame;
        String sha1 = overrideSha1.isBlank() ? (chosen == null ? "" : chosen.sha1()) : overrideSha1;
        String entryName = "resources/" + sha1;
        int size = sha1.isBlank() ? -1 : model.archive().entrySize(entryName);
        if (size < 0) {
            return null;
        }
        String mime = chosen == null || chosen.mimeType().isBlank() ? "application/octet-stream" : chosen.mimeType();
        return new Resource(mime, entryName, size, model.archive());
    }

    private static void rewriteCssResource(Resource resource, String baseUrl, Model model, SnapshotContext context,
                                           RenderBudget output, CssSourceBudget sources,
                                           Set<String> imports, int depth) {
        if (resource.size() > output.remainingBytes()) {
            throw new IllegalArgumentException("Captured stylesheet exceeds the offline render limit.");
        }
        CssBuffer rewritten = new CssBuffer(output.remainingBytes());
        rewriteCss(resource.text(output.remainingBytes(), sources), baseUrl, model, context, rewritten, sources,
                imports, depth);
        output.append(rewritten.value());
    }

    private static String rewriteCss(String css, String baseUrl, Model model, SnapshotContext context,
                                     int maximumBytes, CssSourceBudget sources, Set<String> imports, int depth) {
        CssBuffer rewritten = new CssBuffer(maximumBytes);
        sources.add(css.getBytes(StandardCharsets.UTF_8).length);
        rewriteCss(css, baseUrl, model, context, rewritten, sources, imports, depth);
        return rewritten.value();
    }

    private static void rewriteCss(String css, String baseUrl, Model model, SnapshotContext context,
                                   CssBuffer rewritten, CssSourceBudget sources, Set<String> imports, int depth) {
        if (depth > 16) {
            throw new IllegalArgumentException("Captured stylesheet import depth exceeds the offline render limit.");
        }
        String safe = STYLE_END.matcher(css).replaceAll(Matcher.quoteReplacement("\\3C /style"));
        int offset = 0;
        while (offset < safe.length()) {
            Matcher importMatcher = CSS_IMPORT.matcher(safe);
            Matcher urlMatcher = CSS_URL.matcher(safe);
            boolean hasImport = importMatcher.find(offset);
            boolean hasUrl = urlMatcher.find(offset);
            if (!hasImport && !hasUrl) {
                rewritten.append(safe.substring(offset));
                break;
            }
            boolean useImport = hasImport && (!hasUrl || importMatcher.start() <= urlMatcher.start());
            Matcher match = useImport ? importMatcher : urlMatcher;
            rewritten.append(safe.substring(offset, match.start()));
            String importUrl = useImport ? firstNonBlank(match.group(2), match.group(3), match.group(5))
                    : match.group(2);
            String absolute = resolveUrl(baseUrl, importUrl);
            Resource resource = resource(model, context, absolute);
            if (useImport) {
                if (resource != null && resource.mimeType().toLowerCase(Locale.ROOT).startsWith("text/css")
                        && imports.add(absolute)) {
                    String condition = match.group(6).trim();
                    int conditionBlocks = openImportConditions(condition, rewritten);
                    rewriteCss(resource.text(rewritten.remainingBytes(), sources), absolute, model, context,
                            rewritten, sources, imports, depth + 1);
                    for (int block = 0; block < conditionBlocks; block++) {
                        rewritten.append("}");
                    }
                    imports.remove(absolute);
                }
            } else {
                rewritten.append("url(");
                rewritten.append(resource == null ? "data:," : resource.dataUri(rewritten.remainingBytes()));
                rewritten.append(")");
            }
            offset = match.end();
        }
    }

    private static String firstNonBlank(String... values) {
        for (String value : values) {
            if (value != null && !value.isBlank()) {
                return value;
            }
        }
        return "";
    }

    @SuppressWarnings("PMD.NPathComplexity")
    private static int openImportConditions(String condition, CssBuffer output) {
        int blocks = 0;
        String remaining = condition.trim();
        if (remaining.toLowerCase(Locale.ROOT).startsWith("layer")) {
            int close = remaining.length() > 5 && remaining.charAt(5) == '(' ? matchingParenthesis(remaining, 5) : 4;
            String name = close > 6 ? remaining.substring(6, close).trim() : "";
            output.append(name.isBlank() ? "@layer{" : "@layer " + name + "{");
            blocks++;
            remaining = close >= 0 && close + 1 < remaining.length() ? remaining.substring(close + 1).trim() : "";
        }
        if (remaining.toLowerCase(Locale.ROOT).startsWith("supports(")) {
            int close = matchingParenthesis(remaining, 8);
            if (close > 9) {
                output.append("@supports (" + remaining.substring(9, close).trim() + "){");
                blocks++;
                remaining = close + 1 < remaining.length() ? remaining.substring(close + 1).trim() : "";
            }
        }
        if (!remaining.isBlank()) {
            output.append("@media " + remaining + "{");
            blocks++;
        }
        return blocks;
    }

    private static int matchingParenthesis(String value, int open) {
        int depth = 0;
        char quote = 0;
        for (int index = open; index < value.length(); index++) {
            char current = value.charAt(index);
            if (quote != 0) {
                if (current == quote && (index == 0 || value.charAt(index - 1) != '\\')) {
                    quote = 0;
                }
            } else if (current == '\'' || current == '"') {
                quote = current;
            } else if (current == '(') {
                depth++;
            } else if (current == ')' && --depth == 0) {
                return index;
            }
        }
        return -1;
    }

    private static String overrideSha1(SnapshotContext context, String url) {
        SnapshotContext current = context;
        for (int guard = 0; guard < context.history().size(); guard++) {
            JsonNode override = resourceOverride(current, url);
            if (override == null) {
                return "";
            }
            if (!override.path("sha1").asText().isBlank()) {
                return override.path("sha1").asText();
            }
            int ref = override.path("ref").asInt();
            int target = current.index() - ref;
            if (ref <= 0 || target < 0) {
                return "";
            }
            current = current.history().get(target);
        }
        return "";
    }

    private static JsonNode resourceOverride(SnapshotContext context, String url) {
        for (JsonNode override : context.snapshot().path("resourceOverrides")) {
            if (url.equals(override.path("url").asText())) {
                return override;
            }
        }
        return null;
    }

    private static String resolveUrl(String base, String value) {
        try {
            return URI.create(base).resolve(value).toString();
        } catch (IllegalArgumentException exception) {
            return value;
        }
    }

    private static List<JsonNode> snapshotNodes(JsonNode root, NodeTableBudget budget) {
        List<JsonNode> nodes = new ArrayList<>();
        collectNodes(root, nodes, budget, 0);
        return nodes;
    }

    private static void collectNodes(JsonNode node, List<JsonNode> nodes, NodeTableBudget budget, int depth) {
        budget.visit();
        if (depth > MAX_RENDER_DEPTH) {
            throw new IllegalArgumentException("Playwright snapshot node table exceeds the offline render limit.");
        }
        if (node.isString()) {
            nodes.add(node);
        } else if (node.isArray() && !node.isEmpty() && node.get(0).isString()) {
            for (int index = 2; index < node.size(); index++) {
                collectNodes(node.get(index), nodes, budget, depth + 1);
            }
            nodes.add(node);
        }
    }

    private static String escapeText(String value) {
        return value.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;");
    }

    private static String escapeAttribute(String value) {
        return escapeText(value).replace("\"", "&quot;").replace("'", "&#39;");
    }

    private static final class SnapshotContext {
        private final JsonNode snapshot;
        private final List<SnapshotContext> history;
        private final int index;
        private final NodeTableBudget nodeTableBudget;
        private final Map<String, Optional<Resource>> resources = new HashMap<>();
        private List<JsonNode> nodes;

        private SnapshotContext(JsonNode snapshot, List<SnapshotContext> history, int index,
                                NodeTableBudget nodeTableBudget) {
            this.snapshot = snapshot;
            this.history = history;
            this.index = index;
            this.nodeTableBudget = nodeTableBudget;
        }

        JsonNode snapshot() {
            return snapshot;
        }

        List<SnapshotContext> history() {
            return history;
        }

        int index() {
            return index;
        }

        List<JsonNode> nodes() {
            if (nodes == null) {
                nodes = List.copyOf(snapshotNodes(snapshot.path("html"), nodeTableBudget));
            }
            return nodes;
        }

        Map<String, Optional<Resource>> resources() {
            return resources;
        }
    }

    private static final class NodeTableBudget {
        private int nodes;

        void visit() {
            nodes++;
            if (nodes > MAX_RENDER_NODES) {
                throw new IllegalArgumentException("Playwright snapshot node tables exceed the offline render limit.");
            }
        }
    }

    private record ResourceRecord(String url, String method, int status, String sha1, String mimeType,
                                  String frameId, double time) {
    }

    private record Model(List<SnapshotContext> snapshots, Map<String, List<ResourceRecord>> resourcesByUrl,
                         PlaywrightTraceArchiveLoader.LoadedArchive archive) {
    }

    private record Resource(String mimeType, String entryName, int size,
                            PlaywrightTraceArchiveLoader.LoadedArchive archive) {
        String dataUri(int maximumBytes) {
            String prefix = "data:" + mimeType.replaceAll("[^a-zA-Z0-9+./;=_-]", "") + ";base64,";
            long encodedSize = 4L * ((size + 2L) / 3L);
            if (prefix.length() + encodedSize > maximumBytes) {
                throw new IllegalArgumentException("Captured resource exceeds the offline render limit.");
            }
            return prefix + Base64.getEncoder().encodeToString(archive.entry(entryName));
        }

        String text(int maximumBytes, CssSourceBudget sources) {
            if (size > maximumBytes) {
                throw new IllegalArgumentException("Captured stylesheet exceeds the offline render limit.");
            }
            sources.add(size);
            return new String(archive.entry(entryName), StandardCharsets.UTF_8);
        }
    }

    private static final class CssSourceBudget {
        private int bytes;

        void add(int size) {
            bytes = Math.addExact(bytes, size);
            if (bytes > MAX_RENDER_BYTES) {
                throw new IllegalArgumentException("Captured stylesheet sources exceed the offline render limit.");
            }
        }
    }

    private static final class CssBuffer {
        private final StringBuilder value = new StringBuilder();
        private final int maximumBytes;
        private int bytes;

        private CssBuffer(int maximumBytes) {
            this.maximumBytes = maximumBytes;
        }

        CssBuffer append(String text) {
            bytes = Math.addExact(bytes, text.getBytes(StandardCharsets.UTF_8).length);
            if (bytes > maximumBytes) {
                throw new IllegalArgumentException("Captured CSS exceeds the offline render limit.");
            }
            value.append(text);
            return this;
        }

        int remainingBytes() {
            return maximumBytes - bytes;
        }

        String value() {
            return value.toString();
        }
    }

    private static final class RenderBudget {
        private final StringBuilder value = new StringBuilder();
        private final CssSourceBudget cssSources = new CssSourceBudget();
        private int bytes;
        private int nodes;

        void visit(int depth) {
            nodes++;
            if (nodes > MAX_RENDER_NODES || depth > MAX_RENDER_DEPTH) {
                throw new IllegalArgumentException("Playwright snapshot expansion exceeds the offline render limit.");
            }
        }

        RenderBudget append(String text) {
            bytes = Math.addExact(bytes, text.getBytes(StandardCharsets.UTF_8).length);
            if (bytes > MAX_RENDER_BYTES) {
                throw new IllegalArgumentException("Rendered Playwright snapshot exceeds the offline render limit.");
            }
            value.append(text);
            return this;
        }

        String value() {
            return value.toString();
        }

        int remainingBytes() {
            return MAX_RENDER_BYTES - bytes;
        }

        CssSourceBudget cssSources() {
            return cssSources;
        }
    }
}
