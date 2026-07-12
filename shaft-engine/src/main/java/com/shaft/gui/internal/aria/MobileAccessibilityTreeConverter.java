package com.shaft.gui.internal.aria;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/**
 * Converts Appium accessibility XML (UiAutomator2 {@code hierarchy}/{@code node} trees, or XCUITest
 * {@code XCUIElementType*} trees) into the {@link AriaNode} forest consumed by {@link AriaSnapshotHelper},
 * so {@code matchesAriaSnapshot()} baselines work the same way for native mobile as for web.
 */
public final class MobileAccessibilityTreeConverter {
    private static final String ANDROID_ROOT_TAG = "hierarchy";
    private static final String XCUI_ELEMENT_TYPE_PREFIX = "XCUIElementType";

    /**
     * Names, in priority order, that {@link #deriveName(Element)} checks to resolve an accessible name.
     * Mirrors {@code McpAppiumLocatorSuggester.ACCESSIBLE_NAME_ATTRIBUTES}.
     */
    private static final List<String> ACCESSIBLE_NAME_ATTRIBUTES = List.of(
            "content-desc", "name", "label", "text", "value");

    /**
     * Generic layout-only roles that are filtered out (with their children hoisted up) when the node
     * has no accessible name of its own.
     */
    private static final Set<String> GENERIC_CONTAINER_ROLES = Set.of(
            "framelayout", "linearlayout", "relativelayout", "viewgroup", "other", "window");

    private MobileAccessibilityTreeConverter() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Converts Appium accessibility XML (either a full page source or a single element's
     * {@code outerXML} subtree) into an {@link AriaNode} forest.
     *
     * <p>The synthetic Android root wrapper tag {@code hierarchy} is skipped; its children become the
     * top-level forest. Nameless generic-container nodes (e.g. {@code FrameLayout}, {@code LinearLayout},
     * {@code XCUIElementTypeOther}, {@code XCUIElementTypeWindow}) are dropped and their children hoisted
     * up in their place; a nameless generic container with no children is dropped entirely. Every other
     * node is kept, whether or not it has a name.</p>
     *
     * @param pageSourceXml the Appium accessibility XML to convert
     * @return the converted top-level nodes
     * @throws IllegalArgumentException if {@code pageSourceXml} is blank or is not well-formed XML
     */
    public static List<AriaNode> convert(String pageSourceXml) {
        if (pageSourceXml == null || pageSourceXml.isBlank()) {
            throw new IllegalArgumentException("Appium accessibility XML must not be blank.");
        }
        Element root = parseDocument(pageSourceXml).getDocumentElement();
        if (root == null) {
            return List.of();
        }
        List<AriaNode> forest = new ArrayList<>();
        if (ANDROID_ROOT_TAG.equals(root.getTagName())) {
            for (Element child : childElements(root)) {
                forest.addAll(convertElement(child));
            }
        } else {
            forest.addAll(convertElement(root));
        }
        return forest;
    }

    /**
     * Captures an aria snapshot of the element matched by {@code locator} on a mobile-native session:
     * reads the element's {@code outerXML} attribute (Appium UiAutomator2/XCUITest subtree XML), falling
     * back to the full {@link WebDriver#getPageSource()} when {@code outerXML} is unavailable, then
     * converts and serializes it.
     *
     * @param driver active mobile-native WebDriver instance
     * @param locator locator of the element to snapshot
     * @return the captured snapshot serialized as YAML
     */
    public static String captureAriaSnapshot(WebDriver driver, By locator) {
        WebElement element = driver.findElement(locator);
        String xml = element.getAttribute("outerXML");
        if (xml == null || xml.isBlank()) {
            xml = driver.getPageSource();
        }
        return AriaSnapshotHelper.serialize(convert(xml));
    }

    /**
     * Converts a single element, and recursively its children, returning the list of nodes it
     * contributes to its parent's child list: normally a single-element list, an empty list when the
     * element is a nameless generic container with no surviving children, or the element's own
     * (already-converted) children when it is a nameless generic container that does have children.
     */
    private static List<AriaNode> convertElement(Element element) {
        List<AriaNode> children = new ArrayList<>();
        for (Element childElement : childElements(element)) {
            children.addAll(convertElement(childElement));
        }
        String role = deriveRole(element);
        String name = deriveName(element);
        if (name.isBlank() && GENERIC_CONTAINER_ROLES.contains(role)) {
            return children;
        }
        return List.of(new AriaNode(role, name, children));
    }

    private static String deriveRole(Element element) {
        String tagName = element.getTagName();
        if (tagName.startsWith(XCUI_ELEMENT_TYPE_PREFIX)) {
            String stripped = tagName.substring(XCUI_ELEMENT_TYPE_PREFIX.length());
            return stripped.isBlank() ? tagName.toLowerCase(Locale.ROOT) : stripped.toLowerCase(Locale.ROOT);
        }
        String classAttribute = attribute(element, "class");
        String source = classAttribute.isBlank() ? tagName : classAttribute;
        int lastDot = source.lastIndexOf('.');
        String role = lastDot >= 0 ? source.substring(lastDot + 1) : source;
        return role.toLowerCase(Locale.ROOT);
    }

    private static String deriveName(Element element) {
        for (String attributeName : ACCESSIBLE_NAME_ATTRIBUTES) {
            String value = attribute(element, attributeName);
            if (!value.isBlank()) {
                return value;
            }
        }
        return "";
    }

    private static String attribute(Element element, String name) {
        return element.hasAttribute(name) ? element.getAttribute(name).trim() : "";
    }

    private static List<Element> childElements(Element element) {
        NodeList childNodes = element.getChildNodes();
        List<Element> elements = new ArrayList<>();
        for (int index = 0; index < childNodes.getLength(); index++) {
            Node child = childNodes.item(index);
            if (child instanceof Element childElement) {
                elements.add(childElement);
            }
        }
        return elements;
    }

    private static Document parseDocument(String xml) {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
            factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            factory.setFeature("http://xml.org/sax/features/external-general-entities", false);
            factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
            factory.setXIncludeAware(false);
            factory.setExpandEntityReferences(false);
            return factory.newDocumentBuilder().parse(new InputSource(new StringReader(xml)));
        } catch (Exception exception) {
            throw new IllegalArgumentException("Failed to parse Appium accessibility XML: " + exception.getMessage(), exception);
        }
    }
}
