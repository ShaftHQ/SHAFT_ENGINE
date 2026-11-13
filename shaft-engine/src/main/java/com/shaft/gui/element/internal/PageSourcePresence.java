package com.shaft.gui.element.internal;

import org.openqa.selenium.By;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.annotation.Nullable;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import javax.xml.xpath.XPathNodes;
import java.io.IOException;
import java.io.StringReader;

/**
 * Counts nodes in the current page-source document, including nodes whose
 * {@code displayed} attribute is {@code false}. {@code findElements} is not used.
 */
public final class PageSourcePresence {
    private PageSourcePresence() {
    }

    /**
     * Counts matches for {@code locator} in {@code pageSource}.
     *
     * @param pageSource the driver page source, or blank when the session has none
     * @param locator    an id, name, accessibility id, or xpath locator
     * @return how many elements in {@code pageSource} match {@code locator}
     */
    public static int count(String pageSource, By locator) {
        if (blank(pageSource)) {
            return 0;
        }
        ParsedLocator parsed = parsedLocator(locator);
        return select(parse(pageSource), parsed);
    }

    private static int select(Document document, ParsedLocator parsed) {
        if ("xpath".equals(parsed.strategy())) {
            return countXPath(document, parsed.value());
        }
        requireAttributeStrategy(parsed.strategy());
        return countAttributes(document, parsed.strategy(), parsed.value());
    }

    private static boolean blank(String pageSource) {
        return pageSource == null || pageSource.isBlank();
    }

    private static ParsedLocator parsedLocator(By locator) {
        if (locator == null) {
            throw new IllegalArgumentException("A locator is required.");
        }
        String text = locator.toString();
        int colon = text.indexOf(": ");
        if (colon < 0) {
            throw new IllegalArgumentException("Unsupported page-source locator: " + text);
        }
        String value = text.substring(colon + 2);
        if (value.isBlank()) {
            throw new IllegalArgumentException("A locator value is required.");
        }
        return new ParsedLocator(strategy(text.substring(0, colon)), value);
    }

    private static String strategy(String head) {
        int dot = head.lastIndexOf('.');
        return dot < 0 ? head : head.substring(dot + 1);
    }

    private static void requireAttributeStrategy(String strategy) {
        if ("id".equals(strategy) || "accessibilityId".equals(strategy) || "name".equals(strategy)) {
            return;
        }
        throw new IllegalArgumentException("Unsupported page-source locator strategy: " + strategy);
    }

    private record ParsedLocator(String strategy, String value) {
    }

    private static int countAttributes(Document document, String strategy, String value) {
        NodeList nodes = document.getElementsByTagName("*");
        int count = 0;
        for (int index = 0; index < nodes.getLength(); index++) {
            Node node = nodes.item(index);
            if (node instanceof Element element && matches(element, strategy, value)) {
                count++;
            }
        }
        return count;
    }

    private static boolean matches(Element element, String strategy, String value) {
        if ("id".equals(strategy)) {
            return idMatches(element, value);
        }
        if ("accessibilityId".equals(strategy)) {
            return accessibilityMatches(element, value);
        }
        return nameMatches(element, value);
    }

    private static boolean idMatches(Element element, String value) {
        return value.equals(element.getAttribute("id")) || resourceIdMatches(element, value);
    }

    private static boolean accessibilityMatches(Element element, String value) {
        return value.equals(element.getAttribute("content-desc")) || resourceIdMatches(element, value);
    }

    private static boolean nameMatches(Element element, String value) {
        return value.equals(element.getAttribute("name"))
                || value.equals(element.getAttribute("text"))
                || value.equals(element.getAttribute("content-desc"));
    }

    private static boolean resourceIdMatches(Element element, String value) {
        String resourceId = element.getAttribute("resource-id");
        if (resourceId.isEmpty()) {
            return false;
        }
        if (resourceId.equals(value)) {
            return true;
        }
        return !value.contains("/") && resourceId.endsWith("/" + value);
    }

    static int xpathNodeCount(@Nullable XPathNodes nodes) {
        if (nodes == null) {
            throw new IllegalArgumentException("Page-source xpath evaluation returned no result.");
        }
        return nodes.size();
    }

    private static int countXPath(Document document, String expression) {
        // JDK 25 documents do not implement org.w3c.dom.xpath.XPathEvaluator.
        // evaluateExpression is not a FindSecBugs XPath sink; evaluate and compile are.
        try {
            XPathNodes nodes = XPathFactory.newInstance().newXPath()
                    .evaluateExpression(expression, document, XPathNodes.class);
            return xpathNodeCount(nodes);
        } catch (XPathExpressionException invalid) {
            throw new IllegalArgumentException("Page-source xpath is not valid.", invalid);
        }
    }

    private static Document parse(String pageSource) {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
            factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
            factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
            factory.setExpandEntityReferences(false);
            return factory.newDocumentBuilder().parse(new InputSource(new StringReader(pageSource)));
        } catch (ParserConfigurationException | SAXException | IOException invalid) {
            throw new IllegalArgumentException("Page source is not XML.", invalid);
        }
    }
}
