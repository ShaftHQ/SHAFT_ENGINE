package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class SourceCodeContextTest {
    @Test
    public void neverDoesNotResolveLifecycleOrReadSource() {
        com.shaft.driver.SHAFT.Properties.reporting.set().whenToAttachSourceCode("Never");
        try (var source = org.mockito.Mockito.mockStatic(SourceCodeContext.class)) {
            SourceCodeAttachment collector = new SourceCodeAttachment();
            var step = new io.qameta.allure.model.StepResult().setStatus(io.qameta.allure.model.Status.FAILED);
            collector.started(step, () -> { throw new AssertionError("Never must not resolve the lifecycle"); });
            collector.stopping(step);
            source.verifyNoInteractions();
        } finally {
            com.shaft.properties.internal.Properties.clearForCurrentThread();
        }
    }

    @Test
    public void boundedWindowClampsToFirstAndLastLinesAndRejectsInvalidLine() throws Exception {
        Path source = Files.createTempFile("source-window", ".java");
        try {
            Files.writeString(source, "first\nsecond\nthird\nfourth\nfifth\nsixth\n");
            Assert.assertEquals(SourceCodeContext.read(source, 1).lines().size(), 3);
            Assert.assertEquals(SourceCodeContext.read(source, 3).lines().size(), 5);
            Assert.assertEquals(SourceCodeContext.read(source, 6).lines().size(), 3);
            Assert.assertEquals(SourceCodeContext.read(source, 3).lines().get(2).text(), "third");
            Assert.assertTrue(SourceCodeContext.read(source, 7).lines().isEmpty());
            Assert.assertTrue(SourceCodeContext.read(source, -1).lines().isEmpty());
            Assert.assertTrue(SourceCodeContext.read(null, 1).lines().isEmpty());
            Files.writeString(source, "");
            Assert.assertTrue(SourceCodeContext.read(source, 1).lines().isEmpty());
        } finally {
            Files.deleteIfExists(source);
        }
    }

    @Test
    public void oversizedSourceIsOmittedWithoutPublishingPartialLines() throws Exception {
        Path source = Files.createTempFile("source-size", ".java");
        try {
            Files.writeString(source, "x".repeat(17_000));
            var window = SourceCodeContext.read(source, 1);
            Assert.assertTrue(window.lines().isEmpty());
            Assert.assertTrue(window.unavailableReason().contains("size limit"));
        } finally {
            Files.deleteIfExists(source);
        }
    }

    @Test
    public void sourceMetadataResolvesNestedClassButCannotTraversePaths() {
        var nested = new StackTraceElement(getClass().getName() + "$Nested", "call", "SourceCodeContextTest.java", 1);
        Path source = SourceCodeContext.findSourceFile(nested, getClass());
        Assert.assertNotNull(source);
        String relative = SourceCodeContext.relative(source, getClass());
        Assert.assertTrue(relative.contains("src/test/java/com/shaft/tools/io/internal/SourceCodeContextTest.java"), relative);
        Assert.assertFalse(Path.of(relative).isAbsolute());
        var traversal = new StackTraceElement(getClass().getName(), "call", "../SourceCodeContextTest.java", 1);
        Assert.assertNull(SourceCodeContext.findSourceFile(traversal, getClass()));
        var kotlin = new StackTraceElement("customer.Test", "call", "Test.kt", 1);
        Assert.assertNull(SourceCodeContext.findSourceFile(kotlin, getClass()));
    }

    @Test
    public void renderedSnippetRedactsSecretsBeforeEscapingMarkup() {
        String credential = "source-context-test-private-value";
        FailureTraceReporter.registerSensitiveSourceValue(credential);
        ReportSecrets.registerValue("source-context-property-secret");
        try {
            var frame = new StackTraceElement("customer.Test", "execute", "Test.java", 42);
            var window = new SourceCodeContext.Window(List.of(
                    new SourceCodeContext.Line(41, "String value = \"" + credential + "\";"),
                    new SourceCodeContext.Line(42, "call(\"<script>alert('x')</script>\");"),
                    new SourceCodeContext.Line(43, "String other = \"source-context-property-secret\";")), "");
            String html = SourceCodeAttachment.render(frame, "src/test/java/customer/Test.java", window);
            Assert.assertFalse(html.contains(credential));
            Assert.assertFalse(html.contains("source-context-property-secret"));
            Assert.assertFalse(html.contains("<script>"));
            Assert.assertTrue(html.contains("&lt;script&gt;"));
            Assert.assertTrue(html.contains("class=\"source-line active\""));
            Assert.assertEquals(html.split("&#8594;", -1).length - 1, 1);
            Assert.assertFalse(html.contains("file:"));
        } finally {
            FailureTraceReporter.clearSensitiveValues();
        }
    }

    @Test
    public void unavailableSourceShowsKnownLocationWithoutInventingSnippet() {
        var frame = new StackTraceElement("customer.Test", "execute", "Test.java", -1);
        String html = SourceCodeAttachment.render(frame, "Test.java", SourceCodeContext.read(null, -1));
        Assert.assertTrue(html.contains("customer.Test.execute"));
        Assert.assertTrue(html.contains("metadata is unavailable"));
        Assert.assertFalse(html.contains("Recorded line:"));
        Assert.assertFalse(html.contains("class=\"source-line active\""));
    }

    @Test
    public void unsafeShortCredentialOmitsTheWholeSourceWindow() {
        FailureTraceReporter.registerSensitiveSourceValue("xy");
        try {
            var frame = new StackTraceElement("customer.Test", "execute", "Test.java", 2);
            var window = new SourceCodeContext.Window(List.of(new SourceCodeContext.Line(1, "before();"),
                    new SourceCodeContext.Line(2, "enter(\"xy\");"), new SourceCodeContext.Line(3, "after();")), "");
            String html = SourceCodeAttachment.render(frame, "Test.java", window);
            Assert.assertTrue(html.contains("Source context omitted"));
            Assert.assertFalse(html.contains("before();"));
            Assert.assertFalse(html.contains("enter("));
            Assert.assertFalse(html.contains("source-line active"));
        } finally {
            FailureTraceReporter.clearSensitiveValues();
        }
    }
}
