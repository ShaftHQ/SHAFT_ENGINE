package com.shaft.intellij;

import org.junit.jupiter.api.Test;

import javax.xml.parsers.DocumentBuilderFactory;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Every descriptor under META-INF must be well-formed XML: IntelliJ silently fails to load an
 * optional-dependency config file whose XML is invalid (the Plugin Verifier flagged
 * {@code io.github.shafthq.shaft-withJUnit.xml} for a "--" inside a comment, which the XML spec
 * forbids). The string-based descriptor tests never parse these files, so this is the only guard.
 */
class ShaftPluginDescriptorXmlTest {

    @Test
    void allPluginDescriptorsAreWellFormedXml() throws IOException {
        List<Path> descriptors;
        try (Stream<Path> files = Files.list(Path.of("src/main/resources/META-INF"))) {
            descriptors = files.filter(path -> path.getFileName().toString().endsWith(".xml")).toList();
        }
        assertFalse(descriptors.isEmpty(), "expected plugin descriptors under META-INF");
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setAttribute("http://javax.xml.XMLConstants/property/accessExternalDTD", "");
        assertAll(descriptors.stream().map(descriptor ->
                () -> assertDoesNotThrow(
                        () -> factory.newDocumentBuilder().parse(descriptor.toFile()),
                        descriptor + " is not well-formed XML")));
    }

    /**
     * Plugin Verifier 1.410 emits UndeclaredKotlinK2CompatibilityMode when a descriptor depends on
     * org.jetbrains.kotlin and its own kotlinPluginMode stays Implicit. A declaration that lives
     * only in the optional config-file does not clear that warning.
     */
    @Test
    void kotlinDependencyDeclaresK2ModeInTheSameDescriptor() throws Exception {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setAttribute("http://javax.xml.XMLConstants/property/accessExternalDTD", "");
        Element root = factory.newDocumentBuilder()
                .parse(Path.of("src/main/resources/META-INF/plugin.xml").toFile())
                .getDocumentElement();
        NodeList depends = root.getElementsByTagName("depends");
        boolean kotlinDependency = false;
        for (int i = 0; i < depends.getLength(); i++) {
            if ("org.jetbrains.kotlin".equals(depends.item(i).getTextContent().trim())) {
                kotlinDependency = true;
            }
        }
        assertTrue(kotlinDependency, "plugin.xml must depend on org.jetbrains.kotlin");
        NodeList modes = root.getElementsByTagName("supportsKotlinPluginMode");
        assertEquals(1, modes.getLength(), "K2 mode must be declared in plugin.xml itself");
        assertEquals("true", ((Element) modes.item(0)).getAttribute("supportsK2"));
    }
}
