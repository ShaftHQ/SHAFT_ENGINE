package com.shaft.intellij.ui;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Locale;

/**
 * Real, on-disk check for the setup flow's "Upgrade project" step (issue #3426 A4/A5): reads the
 * project's {@code pom.xml}, finds the SHAFT ({@code io.github.shafthq}) version it depends on,
 * and compares it against the latest released engine version. A project already on the latest
 * release — or on a newer local development build — counts as up to date.
 */
final class ShaftProjectVersionCheck {
    private static final String SHAFT_GROUP_ID = "io.github.shafthq";

    /**
     * Outcome of one check.
     */
    enum State {
        /** Project depends on SHAFT at or above the latest known release. */
        UP_TO_DATE,
        /** Project depends on SHAFT below the latest known release. */
        UPGRADE_AVAILABLE,
        /** Project has no readable SHAFT dependency (no pom, or pom without io.github.shafthq). */
        NOT_A_SHAFT_PROJECT,
        /** Project uses SHAFT but the latest release could not be determined (offline). */
        LATEST_UNKNOWN
    }

    /**
     * One check result.
     *
     * @param state comparison outcome
     * @param projectVersion SHAFT version found in the project pom, or blank
     * @param latestVersion latest released version used for comparison, or blank when unknown
     * @param pomPresent whether the project root has a readable {@code pom.xml} at all — separates
     *                   "plain Maven project without SHAFT" from "empty folder" so the setup wizard
     *                   can point at the right next action (issue #3425 A5)
     */
    record Result(State state, String projectVersion, String latestVersion, boolean pomPresent) {
        Result(State state, String projectVersion, String latestVersion) {
            this(state, projectVersion, latestVersion, true);
        }
    }

    private ShaftProjectVersionCheck() {
        throw new IllegalStateException("Utility class");
    }

    static Result check(Path projectRoot, String latestVersion) {
        Path pomFile = projectRoot == null ? null : projectRoot.resolve("pom.xml");
        boolean pomPresent = pomFile != null && Files.isRegularFile(pomFile);
        String projectVersion = shaftVersionFromPom(pomFile);
        if (projectVersion.isBlank()) {
            return new Result(State.NOT_A_SHAFT_PROJECT, "", latestVersion == null ? "" : latestVersion, pomPresent);
        }
        if (latestVersion == null || latestVersion.isBlank() || !isVersionLike(latestVersion)) {
            return new Result(State.LATEST_UNKNOWN, projectVersion, "", true);
        }
        return new Result(
                compareVersions(projectVersion, latestVersion) >= 0 ? State.UP_TO_DATE : State.UPGRADE_AVAILABLE,
                projectVersion,
                latestVersion,
                true);
    }

    /**
     * Extracts the version of the first {@code io.github.shafthq} dependency (or parent) declared
     * in the pom, resolving one level of {@code ${property}} indirection against
     * {@code <properties>}. Returns blank when the pom is missing, unreadable, or SHAFT-free.
     */
    static String shaftVersionFromPom(Path pomFile) {
        if (pomFile == null || !Files.isRegularFile(pomFile)) {
            return "";
        }
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
            factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
            factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
            Document document = factory.newDocumentBuilder().parse(pomFile.toFile());
            String version = firstShaftVersion(document);
            return resolveProperty(document, version);
        } catch (Exception unreadablePom) {
            return "";
        }
    }

    private static String firstShaftVersion(Document document) {
        NodeList groupIds = document.getElementsByTagName("groupId");
        for (int index = 0; index < groupIds.getLength(); index++) {
            Node groupId = groupIds.item(index);
            if (!SHAFT_GROUP_ID.equals(groupId.getTextContent() == null ? "" : groupId.getTextContent().trim())) {
                continue;
            }
            Node declaration = groupId.getParentNode();
            String version = childText(declaration, "version");
            if (!version.isBlank()) {
                return version;
            }
        }
        return "";
    }

    private static String childText(Node parent, String childName) {
        if (!(parent instanceof Element element)) {
            return "";
        }
        NodeList children = element.getChildNodes();
        for (int index = 0; index < children.getLength(); index++) {
            Node child = children.item(index);
            if (childName.equals(child.getNodeName())) {
                return child.getTextContent() == null ? "" : child.getTextContent().trim();
            }
        }
        return "";
    }

    private static String resolveProperty(Document document, String version) {
        if (!version.startsWith("${") || !version.endsWith("}")) {
            return version;
        }
        String propertyName = version.substring(2, version.length() - 1).trim();
        NodeList propertiesBlocks = document.getElementsByTagName("properties");
        for (int index = 0; index < propertiesBlocks.getLength(); index++) {
            String resolved = childText(propertiesBlocks.item(index), propertyName);
            if (!resolved.isBlank()) {
                return resolved;
            }
        }
        return "";
    }

    static boolean isVersionLike(String candidate) {
        return candidate != null && candidate.trim().matches("\\d+(\\.\\d+)+");
    }

    /**
     * Numeric dot-component comparison; non-numeric components compare lexicographically as a
     * fallback so dev suffixes never crash the check.
     */
    static int compareVersions(String left, String right) {
        String[] leftParts = left.trim().split("\\.");
        String[] rightParts = right.trim().split("\\.");
        int length = Math.max(leftParts.length, rightParts.length);
        for (int index = 0; index < length; index++) {
            String leftPart = index < leftParts.length ? leftParts[index] : "0";
            String rightPart = index < rightParts.length ? rightParts[index] : "0";
            int comparison = compareVersionComponent(leftPart, rightPart);
            if (comparison != 0) {
                return comparison;
            }
        }
        return 0;
    }

    private static int compareVersionComponent(String left, String right) {
        try {
            return Long.compare(Long.parseLong(left), Long.parseLong(right));
        } catch (NumberFormatException notNumeric) {
            return left.toLowerCase(Locale.ROOT).compareTo(right.toLowerCase(Locale.ROOT));
        }
    }
}
