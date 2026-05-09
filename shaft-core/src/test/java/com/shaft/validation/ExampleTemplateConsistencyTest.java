package com.shaft.validation;

import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

import javax.xml.parsers.DocumentBuilderFactory;
import java.io.File;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

class ExampleTemplateConsistencyTest {

    private static final String EXAMPLES_ROOT = "../shaft-web/src/main/resources/examples";

    private static final List<String> ALL_TEMPLATES = List.of(
        "Cucumber/shaft-cucumber-web",
        "JUnit/shaft-junit-api",
        "JUnit/shaft-junit-mobile",
        "JUnit/shaft-junit-web",
        "TestNG/shaft-testng-api",
        "TestNG/shaft-testng-mobile",
        "TestNG/shaft-testng-web"
    );

    private static final List<String> API_TEMPLATES = List.of(
        "JUnit/shaft-junit-api",
        "TestNG/shaft-testng-api"
    );

    private static final Set<String> VALID_SHAFT_ARTIFACT_IDS = Set.of(
        "SHAFT_ENGINE", "shaft-engine", "shaft-web", "shaft-api", "shaft-db", "shaft-core", "shaft-bom"
    );

    private Document parse(String templatePath) throws Exception {
        File pom = Path.of(EXAMPLES_ROOT, templatePath, "pom.xml").toFile();
        assertTrue(pom.exists(), "Template pom.xml not found: " + pom.getAbsolutePath());
        return DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(pom);
    }

    private String shaftArtifactId(Document doc) {
        NodeList deps = doc.getElementsByTagName("dependency");
        for (int i = 0; i < deps.getLength(); i++) {
            NodeList children = deps.item(i).getChildNodes();
            String groupId = null, artifactId = null;
            for (int j = 0; j < children.getLength(); j++) {
                if ("groupId".equals(children.item(j).getNodeName()))
                    groupId = children.item(j).getTextContent().trim();
                if ("artifactId".equals(children.item(j).getNodeName()))
                    artifactId = children.item(j).getTextContent().trim();
            }
            if ("io.github.shafthq".equals(groupId) && artifactId != null
                    && VALID_SHAFT_ARTIFACT_IDS.contains(artifactId)) {
                return artifactId;
            }
        }
        return null;
    }

    @Test
    void allTemplatesShouldReferenceValidArtifactIds() throws Exception {
        for (String template : ALL_TEMPLATES) {
            Document doc = parse(template);
            String artifactId = shaftArtifactId(doc);
            assertNotNull(artifactId,
                template + ": no io.github.shafthq dependency with a valid SHAFT artifactId found");
            assertTrue(VALID_SHAFT_ARTIFACT_IDS.contains(artifactId),
                template + ": artifactId '" + artifactId + "' not in valid set " + VALID_SHAFT_ARTIFACT_IDS);
        }
    }

    @Test
    void allTemplatesShouldHaveNonBlankVersion() throws Exception {
        for (String template : ALL_TEMPLATES) {
            Document doc = parse(template);
            NodeList props = doc.getElementsByTagName("shaft_engine.version");
            assertTrue(props.getLength() > 0,
                template + ": <shaft_engine.version> property not found");
            String version = props.item(0).getTextContent().trim();
            assertFalse(version.isBlank(),
                template + ": <shaft_engine.version> is blank");
        }
    }

    @Test
    void apiTemplatesShouldDeclareShaftApiNotUmbrellaArtifact() throws Exception {
        for (String template : API_TEMPLATES) {
            Document doc = parse(template);
            String artifactId = shaftArtifactId(doc);
            assertEquals("shaft-api", artifactId,
                template + ": API template should declare shaft-api, got: " + artifactId);
        }
    }

    @Test
    void apiTemplatesShouldNotDeclareSeleniumDirectly() throws Exception {
        for (String template : API_TEMPLATES) {
            Document doc = parse(template);
            NodeList deps = doc.getElementsByTagName("dependency");
            for (int i = 0; i < deps.getLength(); i++) {
                NodeList children = deps.item(i).getChildNodes();
                for (int j = 0; j < children.getLength(); j++) {
                    if ("artifactId".equals(children.item(j).getNodeName())) {
                        String artifactId = children.item(j).getTextContent().trim();
                        assertFalse(artifactId.startsWith("selenium-"),
                            template + ": must not directly declare selenium, found: " + artifactId);
                    }
                }
            }
        }
    }
}
