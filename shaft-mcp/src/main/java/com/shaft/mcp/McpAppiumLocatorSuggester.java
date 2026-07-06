package com.shaft.mcp;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Selects Appium locators from the current Inspector source using Appium Inspector's locator order.
 */
public final class McpAppiumLocatorSuggester {
    static final String COORDINATE_FALLBACK_WARNING = "Coordinate fallback used because no stable locator could be "
            + "resolved from the accessibility tree; this will probably fail when executed on a different device, "
            + "screen size, orientation, or app state.";

    private static final Pattern BOUNDS = Pattern.compile("\\[(-?\\d+),(-?\\d+)]\\[(-?\\d+),(-?\\d+)]");
    private static final String[][] SIMPLE_NATIVE_STRATEGIES = {
            {"name", "ACCESSIBILITY_ID"},
            {"content-desc", "ACCESSIBILITY_ID"},
            {"id", "ID"},
            {"rntestid", "ID"},
            {"resource-id", "ID"}
    };
    private static final String[][] UIAUTOMATOR_ATTRIBUTES = {
            {"resource-id", "resourceId"},
            {"text", "text"},
            {"content-desc", "description"},
            {"class", "className"}
    };
    private static final List<String> XPATH_ATTRIBUTES = List.of(
            "name", "content-desc", "id", "resource-id", "accessibility-id", "label", "text", "value");
    private static final List<String> ACCESSIBLE_NAME_ATTRIBUTES = List.of(
            "name", "content-desc", "text", "label", "resource-id");

    private final Document document;

    private McpAppiumLocatorSuggester(Document document) {
        this.document = document;
    }

    static boolean isCoordinateFallback(String action, locatorStrategy locatorStrategy) {
        return locatorStrategy == null
                && action != null
                && action.toLowerCase(java.util.Locale.ROOT).contains("coordinates");
    }

    public static Optional<McpAppiumLocatorSuggester> parse(String sourceXml) {
        if (sourceXml == null || sourceXml.isBlank()) {
            return Optional.empty();
        }
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
            factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            factory.setFeature("http://xml.org/sax/features/external-general-entities", false);
            factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
            factory.setXIncludeAware(false);
            factory.setExpandEntityReferences(false);
            Document document = factory.newDocumentBuilder().parse(new InputSource(new StringReader(sourceXml)));
            return Optional.of(new McpAppiumLocatorSuggester(document));
        } catch (Exception ignored) {
            return Optional.empty();
        }
    }

    public Optional<LocatorSuggestion> locatorAt(int x, int y) {
        return elements().stream()
                .filter(element -> bounds(element).map(rectangle -> rectangle.contains(x, y)).orElse(false))
                .min(Comparator.comparingInt(element -> bounds(element).map(Rectangle::area).orElse(Integer.MAX_VALUE)))
                .flatMap(this::bestLocator);
    }

    public Optional<LocatorSuggestion> locatorByAccessibleName(String accessibleName) {
        if (accessibleName == null || accessibleName.isBlank()) {
            return Optional.empty();
        }
        String targetName = accessibleName.trim();
        return elements().stream()
                .filter(element -> matchesAccessibleName(element, targetName))
                .findFirst()
                .flatMap(this::bestLocator);
    }

    private boolean matchesAccessibleName(Element element, String targetName) {
        return ACCESSIBLE_NAME_ATTRIBUTES.stream().anyMatch(name -> matchesAttribute(element, name, targetName));
    }

    private boolean matchesAttribute(Element element, String attributeName, String targetName) {
        String value = attribute(element, attributeName);
        return !value.isBlank() && value.equalsIgnoreCase(targetName);
    }

    private Optional<LocatorSuggestion> bestLocator(Element element) {
        for (String[] candidate : SIMPLE_NATIVE_STRATEGIES) {
            String value = attribute(element, candidate[0]);
            if (!value.isBlank() && attributeValueCount(candidate[0], value, null) == 1) {
                return Optional.of(new LocatorSuggestion(locatorStrategy.valueOf(candidate[1]), value));
            }
        }
        Optional<LocatorSuggestion> className = classNameLocator(element);
        if (className.isPresent()) {
            return className;
        }
        Optional<LocatorSuggestion> uiAutomator = uiAutomatorLocator(element);
        if (uiAutomator.isPresent()) {
            return uiAutomator;
        }
        return xpathLocator(element).map(xpath -> new LocatorSuggestion(locatorStrategy.XPATH, xpath));
    }

    private Optional<LocatorSuggestion> classNameLocator(Element element) {
        for (String value : List.of(
                attribute(element, "class"),
                attribute(element, "type"),
                tagClassName(element))) {
            if (!value.isBlank() && classNameCount(value) == 1) {
                return Optional.of(new LocatorSuggestion(locatorStrategy.CLASSNAME, value));
            }
        }
        return Optional.empty();
    }

    private Optional<LocatorSuggestion> uiAutomatorLocator(Element element) {
        String bestSelector = "";
        int bestCount = Integer.MAX_VALUE;
        for (String[] candidate : UIAUTOMATOR_ATTRIBUTES) {
            String value = uiAutomatorValue(element, candidate[0]);
            if (value.isBlank()) {
                continue;
            }
            List<Element> matches = elements().stream()
                    .filter(other -> value.equals(uiAutomatorValue(other, candidate[0])))
                    .toList();
            if (matches.isEmpty()) {
                continue;
            }
            String selector = "new UiSelector()." + candidate[1] + "(\"" + javaString(value) + "\")";
            if (matches.size() == 1) {
                return Optional.of(new LocatorSuggestion(locatorStrategy.ANDROID_UIAUTOMATOR, selector));
            }
            if (matches.size() < bestCount) {
                bestCount = matches.size();
                bestSelector = selector + ".instance(" + matches.indexOf(element) + ")";
            }
        }
        return bestSelector.isBlank()
                ? Optional.empty()
                : Optional.of(new LocatorSuggestion(locatorStrategy.ANDROID_UIAUTOMATOR, bestSelector));
    }

    private Optional<String> xpathLocator(Element element) {
        for (String attribute : XPATH_ATTRIBUTES) {
            String value = attribute(element, attribute);
            if (!value.isBlank() && attributeValueCount(attribute, value, element.getTagName()) == 1) {
                return Optional.of("//" + element.getTagName() + "[@" + attribute + "=" + xpathLiteral(value) + "]");
            }
        }
        return Optional.of(hierarchicalXPath(element));
    }

    private String hierarchicalXPath(Element element) {
        List<String> parts = new ArrayList<>();
        Node current = element;
        while (current instanceof Element currentElement) {
            String part = currentElement.getTagName();
            int index = sameTagSiblingIndex(currentElement);
            int count = sameTagSiblingCount(currentElement);
            if (count > 1) {
                part += "[" + index + "]";
            }
            parts.addFirst(part);
            current = currentElement.getParentNode();
        }
        return "/" + String.join("/", parts);
    }

    private int attributeValueCount(String name, String value, String tagName) {
        return (int) elements().stream()
                .filter(element -> tagName == null || tagName.equals(element.getTagName()))
                .filter(element -> value.equals(attribute(element, name)))
                .count();
    }

    private int classNameCount(String value) {
        return (int) elements().stream()
                .filter(element -> value.equals(className(element)))
                .count();
    }

    private List<Element> elements() {
        NodeList nodes = document.getElementsByTagName("*");
        List<Element> elements = new ArrayList<>(nodes.getLength());
        for (int index = 0; index < nodes.getLength(); index++) {
            if (nodes.item(index) instanceof Element element) {
                elements.add(element);
            }
        }
        return elements;
    }

    private Optional<Rectangle> bounds(Element element) {
        Matcher matcher = BOUNDS.matcher(attribute(element, "bounds"));
        if (matcher.matches()) {
            try {
                return Optional.of(new Rectangle(
                        Integer.parseInt(matcher.group(1)),
                        Integer.parseInt(matcher.group(2)),
                        Integer.parseInt(matcher.group(3)),
                        Integer.parseInt(matcher.group(4))));
            } catch (NumberFormatException exception) {
                return Optional.empty();
            }
        }
        String x = attribute(element, "x");
        String y = attribute(element, "y");
        String width = attribute(element, "width");
        String height = attribute(element, "height");
        if (x.isBlank() || y.isBlank() || width.isBlank() || height.isBlank()) {
            return Optional.empty();
        }
        try {
            int left = Integer.parseInt(x);
            int top = Integer.parseInt(y);
            return Optional.of(new Rectangle(left, top, left + Integer.parseInt(width), top + Integer.parseInt(height)));
        } catch (NumberFormatException exception) {
            return Optional.empty();
        }
    }

    private int sameTagSiblingIndex(Element element) {
        int index = 1;
        Node sibling = element.getPreviousSibling();
        while (sibling != null) {
            if (sibling instanceof Element previous && previous.getTagName().equals(element.getTagName())) {
                index++;
            }
            sibling = sibling.getPreviousSibling();
        }
        return index;
    }

    private int sameTagSiblingCount(Element element) {
        int count = 0;
        NodeList siblings = element.getParentNode().getChildNodes();
        for (int index = 0; index < siblings.getLength(); index++) {
            if (siblings.item(index) instanceof Element sibling
                    && sibling.getTagName().equals(element.getTagName())) {
                count++;
            }
        }
        return count;
    }

    private static String attribute(Element element, String name) {
        return element.hasAttribute(name) ? element.getAttribute(name).trim() : "";
    }

    private static String uiAutomatorValue(Element element, String name) {
        if (!"class".equals(name)) {
            return attribute(element, name);
        }
        return className(element);
    }

    private static String className(Element element) {
        String className = attribute(element, "class");
        if (!className.isBlank()) {
            return className;
        }
        String type = attribute(element, "type");
        return type.isBlank() ? tagClassName(element) : type;
    }

    private static String tagClassName(Element element) {
        String tagName = element.getTagName();
        if (tagName == null || tagName.isBlank() || "hierarchy".equals(tagName) || "node".equals(tagName)) {
            return "";
        }
        return tagName.contains(".") || tagName.startsWith("XCUIElementType") ? tagName : "";
    }

    private static String javaString(String value) {
        return value
                .replace("\\", "\\\\")
                .replace("\"", "\\\"");
    }

    private static String xpathLiteral(String value) {
        if (!value.contains("'")) {
            return "'" + value + "'";
        }
        if (!value.contains("\"")) {
            return "\"" + value + "\"";
        }
        StringBuilder literal = new StringBuilder("concat(");
        String[] parts = value.split("'", -1);
        for (int index = 0; index < parts.length; index++) {
            if (index > 0) {
                literal.append(", \"'\", ");
            }
            literal.append("'").append(parts[index]).append("'");
        }
        return literal.append(")").toString();
    }

    public record LocatorSuggestion(locatorStrategy strategy, String value) {
    }

    private record Rectangle(int left, int top, int right, int bottom) {
        boolean contains(int x, int y) {
            return x >= left && x <= right && y >= top && y <= bottom;
        }

        int area() {
            return Math.max(0, right - left) * Math.max(0, bottom - top);
        }
    }
}
