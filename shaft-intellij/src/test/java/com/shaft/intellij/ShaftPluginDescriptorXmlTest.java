package com.shaft.intellij;

import org.junit.jupiter.api.Test;

import javax.xml.parsers.DocumentBuilderFactory;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;

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
}
